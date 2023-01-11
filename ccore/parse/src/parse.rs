use crate::buffers::ParseBuffer;
use std::fmt::{Debug, Display, self};
use std::marker::PhantomData;
use std::path::PathBuf;

pub type ParseStream<'a> = &'a ParseBuffer<'a>;

#[derive(Debug)]
pub struct Error {
    msg: String,
    file: Option<PathBuf>,
    col: usize,
    row: usize,
}

impl Error{
    pub fn new(msg: impl Display, file: Option<PathBuf>, col: usize, row: usize) -> Self{
        Self { msg: msg.to_string(), file, col, row }
    }

    pub fn msg(&self) -> &String {
        &self.msg
    }

    pub fn file(&self) -> &Option<PathBuf>{
        &self.file
    }

    pub fn col(&self) -> usize{
        self.col
    }

    pub fn row(&self) -> usize{
        self.row
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            msg,
            file,
            col,
            row,
        } = self;
        f.write_str(&match file {
            Some(file) if let Some(file) = file.to_str() => {
                format!("{file} {row}:{col} {msg}")
            }
            _ => 
                format!("{row}:{col} {msg}"),
        })
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait Parse: Sized {
    fn parse(parse: ParseStream) -> Result<Self>;
}

impl<T: Parse> Parse for Option<T> {
    fn parse(parse: ParseStream) -> Result<Self> {
        let parse_clone = parse.clone();
        if let Ok(parsed) = parse_clone.parse::<T>() {
            parse.update_cursor(parse_clone.cursor());
            Ok(Some(parsed))
        } else {
            Ok(None)
        }
    }
}

impl<T: Parse> Parse for Vec<T>{
    fn parse(parse: ParseStream) -> Result<Self> {
        let mut vec = vec![];
        while !parse.is_empty() {
            vec.push(parse.parse()?);
        }
        Ok(vec)
    }
}

#[derive(Debug)]
pub struct Punctuated<T, P>{
    punctuated: Vec<T>,
    mark: PhantomData<P>,
}

impl<T: Parse, P: Parse> Punctuated<T,P>{
    pub fn parse_non_terminated(parse: ParseStream) -> Result<Self>{
        let mut vec = vec![];
        if parse.peek::<T>(){
            loop {
                vec.push(parse.parse()?);
                if parse.peek::<P>(){
                    parse.parse::<P>()?;
                } else{
                    break;
                }
            }
        }
        Ok(Self{
            punctuated: vec,
            mark: Default::default(),
        })
    }

    pub fn parse_terminated(parse: ParseStream) -> Result<Self>{
        unimplemented!()
    }
}

#[macro_export]
macro_rules! delim {
    ($ty:ident, $out:ident in $parse:expr) => {
        {
            $out = {
                let parse = $crate::tokens::$ty::parse_inner($parse)?;
                let cursor = unsafe { $crate::buffers::Cursor::new(parse.as_ptr(), parse.len()) };
                $crate::buffers::ParseBuffer::new(cursor)
            };
            Ok($crate::tokens::$ty)
        }
    };
}  

#[macro_export]
macro_rules! braced {
    ($out:ident in $parse:expr) => {
        $crate::delim!(Brace, $out in $parse)
    };
} 

#[macro_export]
macro_rules! parenthesized {
    ($out:ident in $parse:expr) => {
        $crate::delim!(Paren, $out in $parse)
    };
} 

#[macro_export]
macro_rules! bracketed {
    ($out:ident in $parse:expr) => {
        $crate::delim!(Bracket, $out in $parse)
    };
} 

