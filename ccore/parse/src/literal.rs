use crate::parse::{Parse, ParseStream, Result};
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct LitInt(i128);

impl LitInt {
    pub fn new(value: i128) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone)]
pub struct LitFloat(f64);

#[derive(Debug, Clone)]
pub struct LitStr(String);

impl LitStr {
    pub fn new(string: String) -> Self {
        Self(string)
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(LitInt),
    Float(LitFloat),
    String(LitStr),
}

impl Parse for Literal {
    fn parse(parse: ParseStream) -> Result<Self> {
        parse.literal().ok_or(parse.error("expected literal"))
    }
}

impl Display for LitInt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0.to_string())
    }
}

impl Display for LitStr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&format!("\"{}\"", self.0))
    }
}

impl Display for LitFloat {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0.to_string())
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&match self {
            Literal::Int(int) => int.to_string(),
            Literal::Float(float) => float.to_string(),
            Literal::String(string) => string.to_string(),
        })
    }
}
