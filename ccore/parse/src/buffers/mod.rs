mod parse;

use crate::literal::Literal;
use crate::parse::{Error, Parse, ParseStream, Result};
use crate::tokens::{Delimeter, Punct, TokenTree, TokenTreeTy};
use std::cell::Cell;
use std::{fmt::Display, marker::PhantomData};

pub struct TokenStream {
    entries: Box<[TokenTree]>,
}

impl Display for TokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            &self
                .entries
                .iter()
                .fold(String::new(), |acc, n| format!("{acc}{n} ")),
        )
    }
}

impl TokenStream {
    pub fn from_str(str: &str) -> Result<Self> {
        Ok(Self {
            entries: parse::parse_str(str)?,
        })
    }

    pub(crate) fn new(entries: Box<[TokenTree]>) -> Self {
        Self { entries }
    }

    pub fn parse<'a, P: Parse>(self) -> Result<P>
    where
        Self: 'a,
    {
        let cursor = self.cursor();
        let parse_buffer = ParseBuffer::new::<'a>(cursor);
        P::parse(&parse_buffer)
    }

    fn cursor(&self) -> Cursor<'static> {
        let entries = &self.entries;
        unsafe { Cursor::new(entries.as_ptr(), entries.len()) }
    }
}

#[derive(Clone, Copy)]
pub struct Cursor<'a> {
    pos: *const TokenTree,
    end: *const TokenTree,
    marker: PhantomData<&'a ()>,
}

impl<'a> Cursor<'a> {
    /// # Safety
    /// if len is larger than the referenced slice, it will either
    /// cause undefined behaviour, or othe program bugs.
    pub unsafe fn new(pos: *const TokenTree, len: usize) -> Self {
        Self {
            pos,
            end: pos.add(len),
            marker: Default::default(),
        }
    }

    fn next(self) -> Self {
        Self {
            pos: unsafe { self.pos.add(1) },
            ..self
        }
    }

    fn entry(self) -> Option<&'static TokenTree> {
        if self.pos < self.end {
            let val = unsafe { &*self.pos };
            Some(val)
        } else {
            None
        }
    }

    fn is_empty(&self) -> bool {
        self.pos >= self.end
    }

    pub fn ident(self) -> Option<(String, Cursor<'a>)> {
        match &self.entry()?.ty {
            TokenTreeTy::Ident(ident) => Some((ident.clone(), self.next())),
            _ => None,
        }
    }

    pub fn literal(self) -> Option<(Literal, Cursor<'a>)> {
        match &self.entry()?.ty {
            TokenTreeTy::Literal(lit) => Some((lit.clone(), self.next())),
            _ => None,
        }
    }

    pub fn punct(self) -> Option<(Punct, Cursor<'a>)> {
        match self.entry()?.ty {
            TokenTreeTy::Punct(punct) => Some((punct.clone(), self.next())),
            _ => None,
        }
    }

    pub fn delim(self) -> Option<(Delimeter, Cursor<'a>)> {
        match self.entry()?.ty {
            TokenTreeTy::Group(group, _) => Some((group.clone(), self.next())),
            _ => None,
        }
    }

    pub fn group(self, delim: Delimeter) -> Option<(Box<[TokenTree]>, Cursor<'a>)> {
        match &self.entry()?.ty {
            TokenTreeTy::Group(cmp, entries) if *cmp == delim => {
                Some((entries.clone(), self.next()))
            }
            _ => None,
        }
    }

    pub fn token_tree(self) -> Option<(TokenTree, Cursor<'a>)> {
        if let Some(tt) = self.entry() {
            Some((tt.clone(), self.next()))
        } else {
            None
        }
    }

    pub fn token_stream(mut self) -> TokenStream {
        let mut vec = vec![];
        while let Some((tt, next)) = self.token_tree() {
            self = next;
            vec.push(tt);
        }
        TokenStream::new(vec.into_boxed_slice())
    }

    pub fn error(self, err: impl Display) -> Error {
        let entry = self.entry();
        let (col, row) = if let Some(entry) = entry {
            (entry.col, entry.row)
        } else {
            (usize::MAX, usize::MAX)
        };
        Error::new(err.to_string(), None, col, row)
    }
}

#[derive(Clone)]
pub struct ParseBuffer<'a> {
    cursor: Cell<Cursor<'static>>,
    mark: PhantomData<Cursor<'a>>,
}

impl<'a> ParseBuffer<'a> {
    pub(crate) fn new<'b>(cursor: Cursor<'static>) -> ParseBuffer<'b> {
        ParseBuffer {
            cursor: Cell::new(cursor),
            mark: Default::default(),
        }
    }

    pub(crate) fn update_cursor(&self, cursor: Cursor<'static>) {
        self.cursor.set(cursor)
    }

    pub fn is_empty(&self) -> bool {
        self.cursor().is_empty()
    }

    pub fn parse<P: Parse>(&self) -> Result<P> {
        P::parse(self)
    }

    pub fn peek<P: Parse>(&self) -> bool {
        self.clone().parse::<P>().is_ok()
    }

    pub fn eat<P: Parse>(&self) -> Option<P> {
        let clone = self.clone();
        if let Ok(parsed) = clone.parse::<P>() {
            self.update_cursor(clone.cursor());
            Some(parsed)
        } else {
            None
        }
    }

    pub fn expect<P: Parse>(&self) -> bool {
        self.eat::<P>().is_some()
    }

    pub fn lookahead1<P: Parse>(&self) -> bool {
        let clone = self.clone();
        clone.skip();
        clone.peek::<P>()
    }

    pub fn lookahead2<P: Parse>(&self) -> bool {
        let clone = self.clone();
        clone.skip();
        clone.skip();
        clone.peek::<P>()
    }

    pub fn call<P, F: Fn(ParseStream) -> Result<P>>(&self, call: F) -> Result<P> {
        call(self)
    }

    pub fn skip(&self) {
        self.update_cursor(self.cursor().next())
    }

    pub fn error(&self, err: impl Display) -> Error {
        self.cursor.get().error(err)
    }

    pub fn cursor(&self) -> Cursor<'static> {
        self.cursor.get()
    }

    pub(crate) fn ident(&self) -> Option<String> {
        if let Some((ident, next)) = self.cursor.get().ident() {
            self.cursor.set(next);
            Some(ident)
        } else {
            None
        }
    }

    pub(crate) fn literal(&self) -> Option<Literal> {
        if let Some((lit, next)) = self.cursor.get().literal() {
            self.cursor.set(next);
            Some(lit)
        } else {
            None
        }
    }

    pub(crate) fn punct(&self) -> Option<Punct> {
        if let Some((punct, next)) = self.cursor.get().punct() {
            self.cursor.set(next);
            Some(punct)
        } else {
            None
        }
    }

    pub(crate) fn group(&self, delim: Delimeter) -> Option<Box<[TokenTree]>> {
        if let Some((group, next)) = self.cursor.get().group(delim) {
            self.cursor.set(next);
            Some(group)
        } else {
            None
        }
    }
}
