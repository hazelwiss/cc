use crate::parse::{Parse, ParseStream, Result};

#[derive(Debug)]
pub struct Ident(String);

impl Ident {
    pub fn new(ident: String) -> Option<Self> {
        if Self::valid_ident(&ident) {
            Some(Self(ident))
        } else {
            None
        }
    }

    fn valid_ident(ident: &str) -> bool {
        true
    }
}

impl Parse for Ident {
    fn parse(parse: ParseStream) -> Result<Self> {
        let ident = parse.ident().ok_or(parse.error("expected identifier"))?;
        let err = &format!("invalid identifier {ident}");
        Ok(Ident::new(ident).ok_or(parse.error(err))?)
    }
}
