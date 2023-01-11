use std::cell::Cell;

use crate::literal::{LitInt, LitStr, Literal};
use crate::parse::{Error, Result};
use crate::tokens::{self, Punct};
use crate::tokens::{Delimeter, TokenTree, TokenTreeTy};
use peeking_take_while::PeekableExt;

#[derive(Debug)]
enum ParsedTy {
    Ident(String),
    Literal(Literal),
    Punct(Punct),
    Group(Delimeter),
    End(Delimeter),
}

#[derive(Debug)]
struct Parsed {
    col: usize,
    row: usize,
    ty: ParsedTy,
}

fn split(mut iter: impl Iterator<Item = char>) -> Result<Vec<Parsed>> {
    let mut vec = vec![];
    let col_initial = 1;
    let row_initial = 1;
    let col_old = Cell::new(col_initial);
    let row_old = Cell::new(row_initial);
    let col = Cell::new(col_initial);
    let row = Cell::new(row_initial);
    let mut iter = std::iter::from_fn(|| {
        let c = iter.next();
        c.map(|c| {
            match c {
                '\n' => {
                    col.set(0);
                    row.set(row.get() + 1)
                }
                _ => col.set(col.get() + 1),
            };
            c
        })
    })
    .peekable();
    macro_rules! p {
        ($e:expr) => {{
            let c = col_old.get();
            let r = row_old.get();
            col_old.set(col.get());
            row_old.set(row.get());
            vec.push(Parsed {
                col: c,
                row: r,
                ty: $e,
            })
        }};
    }
    macro_rules! e {
        ($e:expr) => {
            Err(Error::new($e, None, col_old.get(), row_old.get()))
        };
    }
    while let Some(peek) = iter.peek() {
        match peek {
            c if c.is_alphabetic() => {
                let iter = iter.by_ref().peeking_take_while(|c| match c {
                    c if c.is_alphanumeric() => true,
                    '_' => true,
                    _ => false,
                });
                let ident: String = iter.collect();
                p!(ParsedTy::Ident(ident))
            }
            c if c.is_numeric() => {
                let iter = iter.by_ref().peeking_take_while(|c| match c {
                    c if c.is_numeric() => true,
                    '.' => true,
                    _ => false,
                });
                let number: String = iter.collect();
                p!(ParsedTy::Literal(Literal::Int(LitInt::new(
                    number.parse::<i128>().unwrap()
                ))))
            }
            '(' => {
                iter.next();
                p!(ParsedTy::Group(Delimeter::Paren))
            }
            ')' => {
                iter.next();
                p!(ParsedTy::End(Delimeter::Paren))
            }
            '{' => {
                iter.next();
                p!(ParsedTy::Group(Delimeter::Brace))
            }
            '}' => {
                iter.next();
                p!(ParsedTy::End(Delimeter::Brace))
            }
            '[' => {
                iter.next();
                p!(ParsedTy::Group(Delimeter::Bracket))
            }
            ']' => {
                iter.next();
                p!(ParsedTy::End(Delimeter::Bracket))
            }
            '\"' => {
                let mut found = false;
                iter.next();
                let string: String = iter
                    .by_ref()
                    .take_while(|&c| {
                        found = c == '\"';
                        !found
                    })
                    .collect();
                if found {
                    p!(ParsedTy::Literal(Literal::String(LitStr::new(string))))
                } else {
                    return e!("missing closing '\"' for string literal");
                }
            }
            c if c.is_whitespace() => {
                iter.by_ref()
                    .peeking_take_while(|c| c.is_whitespace())
                    .for_each(|_| {});
                col_old.set(col.get());
                row_old.set(row.get());
            }
            _ => {
                let mut punct = String::new();
                'exit: {
                    while let Some(next) = iter.next() {
                        match next {
                            c if c.is_whitespace() => break,
                            c => {
                                punct.push(c);
                                match tokens::match_punct(&punct) {
                                    tokens::PunctMatch::Matched(matched) => {
                                        p!(ParsedTy::Punct(matched));
                                        break 'exit;
                                    }
                                    tokens::PunctMatch::Partial => {}
                                    tokens::PunctMatch::None => break,
                                }
                            }
                        }
                    }
                    return e!(format!("Expected punctuator, got '{punct}'"));
                }
            }
        };
    }
    Ok(vec)
}

pub(super) fn parse_str(str: &str) -> Result<Box<[TokenTree]>> {
    let split = split(str.chars())?;
    parsed_into_boxed_entries(split)
}

fn parsed_into_boxed_entries(parsed: Vec<Parsed>) -> Result<Box<[TokenTree]>> {
    fn into_boxed(mut iter: impl Iterator<Item = Parsed>) -> Result<Box<[TokenTree]>> {
        let mut vec = vec![];
        while let Some(next) = iter.next() {
            vec.push(TokenTree {
                col: next.col,
                row: next.row,
                ty: match next.ty {
                    ParsedTy::Ident(ident) => TokenTreeTy::Ident(ident),
                    ParsedTy::Literal(lit) => TokenTreeTy::Literal(lit),
                    ParsedTy::Punct(punct) => TokenTreeTy::Punct(punct),
                    ParsedTy::Group(group) => TokenTreeTy::Group(
                        group,
                        parsed_into_boxed_entries(
                            iter.by_ref()
                                .take_while(|cur| match cur.ty {
                                    ParsedTy::End(end) if group == end => false,
                                    _ => true,
                                })
                                .collect(),
                        )?,
                    ),
                    ParsedTy::End(_) => {
                        return Err(Error::new(
                            format!("{}:{} unexpected group delimiter.", next.row, next.col),
                            None,
                            next.col,
                            next.row,
                        ))
                    }
                },
            })
        }
        Ok(vec.into_boxed_slice())
    }
    let iter = parsed.into_iter();
    into_boxed(iter)
}
