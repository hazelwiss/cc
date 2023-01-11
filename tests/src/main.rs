use ccore::parse::declarations::Declaration;
use ccore::parse::{self, Parse, ParseStream, TokenStream};

#[derive(Debug)]
pub struct Test {
    declarations: Vec<Declaration>,
}

impl Parse for Test {
    fn parse(parse: ParseStream) -> parse::Result<Self> {
        let declarations = parse.parse()?;
        Ok(Self { declarations })
    }
}

fn main() {
    let input = include_str!("../input/input.c");
    let ts = match TokenStream::from_str(input) {
        Ok(ts) => ts,
        Err(err) => panic!("{err}",),
    };
    println!("tokenstream:\n{ts}");
    println!("\n\n\n");
    let res = ts.parse::<Test>();
    let test = match res {
        Ok(res) => res,
        Err(err) => panic!("{err}"),
    };
    println!("parsed:\n{test:?}")
}
