use crate::buffers::Cursor;
use crate::literal::Literal;
use crate::parse::{self, Parse, ParseStream, Result};
use std::fmt::Display;

pub trait Token: std::default::Default {
    fn peek(cursor: Cursor) -> bool;

    fn display() -> &'static str;
}

impl<T: Token> Parse for T {
    fn parse(parse: ParseStream) -> parse::Result<Self> {
        if T::peek(parse.cursor()) {
            parse.skip();
            Ok(T::default())
        } else {
            Err(parse.error(format!(
                "expected token '{}', got '{}'",
                T::display(),
                parse
                    .cursor()
                    .token_tree()
                    .map_or("end of buffer".to_string(), |(tt, _)| tt.to_string())
            )))
        }
    }
}

pub(crate) enum PunctMatch {
    Matched(Punct),
    Partial,
    None,
}

pub(crate) fn match_punct(str: &str) -> PunctMatch {
    let mut iter = MATCH_PUNCT.iter().filter(|(cmp, _)| cmp.starts_with(str));
    let partial = iter.clone().count() > 0;
    while let Some((cmp, val)) = iter.next() {
        if *cmp == str {
            return PunctMatch::Matched(*val);
        }
    }
    if partial {
        PunctMatch::Partial
    } else {
        PunctMatch::None
    }
}

#[derive(Clone)]
pub enum TokenTreeTy {
    Ident(String),
    Literal(Literal),
    Punct(Punct),
    Group(Delimeter, Box<[TokenTree]>),
}

#[derive(Clone)]
pub struct TokenTree {
    pub col: usize,
    pub row: usize,
    pub ty: TokenTreeTy,
}

impl Display for TokenTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&match &self.ty {
            TokenTreeTy::Ident(ident) => ident.clone(),
            TokenTreeTy::Literal(lit) => lit.to_string(),
            TokenTreeTy::Punct(punct) => punct.to_string(),
            TokenTreeTy::Group(group, entries) => {
                let string = entries
                    .iter()
                    .fold(String::new(), |acc, n| format!("{acc}{n} "))
                    .to_string();
                match group {
                    Delimeter::Paren => format!("( {string} )"),
                    Delimeter::Bracket => format!("[ {string} ]"),
                    Delimeter::Brace => format!("{{ {string} }}"),
                }
            }
        })
    }
}

macro_rules! define_keywords {
    (
        $($str:literal $vis:vis struct $ident:ident),* $(,)?
    ) => {
        $(
            #[allow(non_camel_case_types)]
            #[derive(Clone, Copy, Debug)]
            $vis struct $ident;

            impl Token for $ident {
                fn peek(cursor: Cursor) -> bool{
                   if let Some((ident, _)) = cursor.ident(){
                        ident == $str
                   } else{
                       false
                   }
                }

                fn display() -> &'static str{
                    $str
                }
            }

            impl Display for $ident {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.write_str($str)
                }
            }

            impl std::default::Default for $ident{
                fn default() -> Self{
                    Self
                }
            }
        )*
    };
}

define_keywords!(
    "auto"              pub struct Auto,
    "extern"            pub struct Extern,
    "short"             pub struct Short,
    "while"             pub struct While,
    "break"             pub struct Break,
    "float"             pub struct Float,
    "signed"            pub struct Signed,
    "_Alignas"          pub struct _Alignas,
    "case"              pub struct Case,
    "for"               pub struct For,
    "sizeof"            pub struct Sizeof,
    "_Alignof"          pub struct _Alignof,
    "char"              pub struct Char,
    "goto"              pub struct Goto,
    "static"            pub struct Static,
    "_Atomic"           pub struct _Atomic,
    "const"             pub struct Const,
    "if"                pub struct If,
    "struct"            pub struct Struct,
    "_Bool"             pub struct _Bool,
    "Continue"          pub struct Continue,
    "inline"            pub struct Inline,
    "switch"            pub struct Switch,
    "_Complex"          pub struct _Complex,
    "default"           pub struct Default,
    "int"               pub struct Int,
    "typedef"           pub struct Typedef,
    "_Generic"          pub struct _Generic,
    "do"                pub struct Do,
    "long"              pub struct Long,
    "union"             pub struct Union,
    "_Imaginary"        pub struct _Imaginary,
    "double"            pub struct Double,
    "register"          pub struct Register,
    "unsigned"          pub struct Unsigned,
    "_Noreturn"         pub struct _Noreturn,
    "else"              pub struct Else,
    "restrict"          pub struct Restrict,
    "void"              pub struct Void,
    "_Static_assert"    pub struct _Static_assert,
    "enum"              pub struct Enum,
    "return"            pub struct Return,
    "volatile"          pub struct Volatile,
    "_Thread_local"     pub struct _Thread_local,
);

macro_rules! define_punctuator {
    ($($str:literal $vis:vis struct $ident:ident),* $(,)?) => {
        $(
            #[allow(non_camel_case_types)]
            #[derive(Clone, Copy, Debug)]
            $vis struct $ident;

            impl Token for $ident {
                fn peek(cursor: Cursor) -> bool{
                   if let Some((punct, _)) = cursor.punct(){
                        match punct{
                            Punct::$ident(_) => true,
                            _ => false,
                        }
                   } else{
                       false
                   }
                }

                fn display() -> &'static str{
                    $str
                }
            }

            impl Display for $ident {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.write_str($str)
                }
            }

            impl std::default::Default for $ident{
                fn default() -> Self{
                    Self
                }
            }
        )*

        #[derive(Debug, Clone, Copy)]
        pub enum Punct{
            $(
                $ident($ident)
            ),*
        }

        impl Display for Punct{
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(match self{
                    $(
                        Self::$ident(_) => $str,
                    )*
                })
            }
        }

        const MATCH_PUNCT: &[(&str, Punct)] = &[
            $(
                ($str, Punct::$ident($ident))
            ),*
        ];
    };
}

define_punctuator!(
    "."     pub struct Dot,
    "->"    pub struct Arrow,
    "++"    pub struct PlusPlus,
    "--"    pub struct MinusMinus,
    "&"     pub struct Ampersand,
    "*"     pub struct Asterix,
    "+"     pub struct Plus,
    "-"     pub struct Minus,
    "~"     pub struct Tilde,
    "!"     pub struct ExclamationMark,
    "/"     pub struct Slash,
    "%"     pub struct Percent,
    "<<"    pub struct DoubleLeftArrow,
    ">>"    pub struct DoubleRightArrow,
    "<"     pub struct LeftArrow,
    ">"     pub struct RightArrow,
    "<="    pub struct LeftEqualArrow,
    ">="    pub struct RightEqualArrow,
    "=="    pub struct EqualEqual,
    "!="    pub struct ExclamationEqual,
    "^"     pub struct Carat,
    "|"     pub struct VerticalBar,
    "&&"    pub struct DoubleAmpersand,
    "||"    pub struct DoubleVerticalBar,
    "?"     pub struct QuestionMark,
    ":"     pub struct Colon,
    ";"     pub struct SemiColon,
    "..."   pub struct DotDotDot,
    "="     pub struct Equal,
    "*="    pub struct AsterixEqual,
    "/="    pub struct SlashEqual,
    "%="    pub struct PercentEqual,
    "+="    pub struct PlusEqual,
    "-="    pub struct MinusEqual,
    "<<="   pub struct DoubleLeftArrowEqual,
    ">>="   pub struct DoubleRightArrowEqual,
    "&="    pub struct AmpersandEqual,
    "^="    pub struct CaratEqual,
    "|="    pub struct VerticalBarEqual,
    ","     pub struct Comma,
    "#"     pub struct Hashtag,
    "##"    pub struct DoubleHashtag,
    "<:"    pub struct LeftArrowColon,
    ":>"    pub struct ColonRightArrow,
    "<%"    pub struct LeftArrowPercent,
    "%>"    pub struct PercentRightArrow,
    "%:"    pub struct PercentColon,
    "%:%:"  pub struct DoublePercentColon,
);

macro_rules! define_delimeter {
    ($($str:literal $vis:vis struct $ident:ident),* $(,)?) => {
        $(
            #[derive(Clone, Copy, Debug)]
            $vis struct $ident;

            impl $ident{
                pub fn parse_inner(parse: ParseStream) -> Result<Box<[TokenTree]>>{
                    if let Some(entries) = parse.group(Delimeter::$ident){
                        Ok(entries)
                    } else{
                        Err(parse.error(&format!("expected {}", $str)))
                    }
                }
            }

            impl Token for $ident {
                fn peek(cursor: Cursor) -> bool{
                   if let Some((punct, _)) = cursor.delim(){
                        match punct {
                            Delimeter::$ident => true,
                            _ => false,
                        }
                   } else{
                       false
                   }
                }

                fn display() -> &'static str{
                    $str
                }
            }

            impl Display for $ident {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.write_str($str)
                }
            }

            impl std::default::Default for $ident{
                fn default() -> Self{
                    Self
                }
            }
        )*

        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum Delimeter {
            $(
                $ident
            ),*
        }
    };
}

define_delimeter!(
    "(" pub struct Paren,
    "{" pub struct Brace,
    "[" pub struct Bracket,
);

#[macro_export]
macro_rules! token {
    [auto] => { $crate::tokens::Auto };
    [extern] => { $crate::tokens::Extern };
    [short] => { $crate::tokens::Short};
    [while] => { $crate::tokens::While };
    [break] => { $crate::tokens::Break };
    [float] => { $crate::tokens::Float };
    [signed] => { $crate::tokens::Signed };
    [_Alignas] => { $crate::tokens::_Alignas };
    [case] => { $crate::tokens::Case };
    [for] => { $crate::tokens::For };
    [sizeof] => { $crate::tokens::Sizeof };
    [_Alignof] => { $crate::tokens::_Alignof };
    [char] => { $crate::tokens::Char };
    [goto] => { $crate::tokens::Goto };
    [static] => { $crate::tokens::Static };
    [_Atomic] => { $crate::tokens::_Atomic };
    [const] => { $crate::tokens::Const };
    [if] => { $crate::tokens::If };
    [struct] => { $crate::tokens::Struct };
    [_Bool] => { $crate::tokens::_Bool };
    [continue] => { $crate::tokens::Continue };
    [inline] => { $crate::tokens::Inline };
    [switch] => { $crate::tokens::Switch };
    [_Complex] => { $crate::tokens::_Complex };
    [default] => { $crate::tokens::Default };
    [int] => { $crate::tokens::Int };
    [typedef] => { $crate::tokens::Typedef };
    [_Generic] => { $crate::tokens::_Generic };
    [do] => { $crate::tokens::Do };
    [long] => { $crate::tokens::Long };
    [union] => { $crate::tokens::Union };
    [_Imaginary] => { $crate::tokens::_Imaginary };
    [double] => { $crate::tokens::Double };
    [register] => { $crate::tokens::Register };
    [unsigned] => { $crate::tokens::Unsigned };
    [_Noreturn] => { $crate::tokens::_Noreturn };
    [else] => { $crate::tokens::Else };
    [restrict] => { $crate::tokens::Restrict };
    [void] => { $crate::tokens::Void };
    [_Static_assert] => { $crate::tokens::_Static_assert };
    [enum] => { $crate::tokens::Enum };
    [return] => { $crate::tokens::Return };
    [volatile] => { $crate::tokens::Volatile };
    [_Thread_local] => { $crate::tokens::_Thread_local };
    [.] => { $crate::tokens::Dot };
    [->] => { $crate::tokens::Arrow };
    [++] => { $crate::tokens::PlusPlus };
    [--] => { $crate::tokens::MinusMinus };
    [&] => { $crate::tokens::Ampersand };
    [*] => { $crate::tokens::Asterix };
    [+] => { $crate::tokens::Plus };
    [-] => { $crate::tokens::Minus };
    [~] => { $crate::tokens::Tilde };
    [!] => { $crate::tokens::ExclamationMark };
    [/] => { $crate::tokens::Slash };
    [%] => { $crate::tokens::Percent };
    [<<] => { $crate::tokens::DoubleLeftArrow };
    [>>] => { $crate::tokens::DoubleRightArrow };
    [<] => { $crate::tokens::LeftArrow };
    [>] => { $crate::tokens::RightArrow };
    [<=] => { $crate::tokens::LeftEqualArrow };
    [>=] => { $crate::tokens::RightEqualArrow };
    [==] => { $crate::tokens::EqualEqual };
    [!=] => { $crate::tokens::ExclamationEqual };
    [^] => { $crate::tokens::Carat };
    [|] => { $crate::tokens::VerticalBar };
    [&&] => { $crate::tokens::DoubleAmpersand };
    [||] => { $crate::tokens::DoubleVerticalBar };
    [?] => { $crate::tokens::QuestionMark };
    [:] => { $crate::tokens::Colon };
    [;] => { $crate::tokens::SemiColon };
    [...] => { $crate::tokens::DotDotDot };
    [=] => { $crate::tokens::Equal };
    [*=] => { $crate::tokens::AsterixEqual };
    [/=] => { $crate::tokens::SlashEqual };
    [%=] => { $crate::tokens::PercentEqual };
    [+=] => { $crate::tokens::PlusEqual };
    [-=] => { $crate::tokens::MinusEqual };
    [<<=] => { $crate::tokens::DoubleLeftArrowEqual };
    [>>=] => { $crate::tokens::DoubleRightArrowEqual };
    [&=] => { $crate::tokens::AmpersandEqual };
    [^=] => { $crate::tokens::CaratEqual };
    [|=] => { $crate::tokens::VerticalBarEqual };
    [,] => { $crate::tokens::Comma };
    [#] => { $crate::tokens::Hashtag };
    [##] => { $crate::tokens::DoubleHashtag };
    [<:] => { $crate::tokens::LeftArrowColon };
    [:>] => { $crate::tokens::ColonRightArrow };
    [<%] => { $crate::tokens::LeftArrowPercent };
    [%>] => { $crate::tokens::PercentRightArrow };
    [%:] => { $crate::tokens::PercentColon };
    [%:%:] => { $crate::tokens::DoublePercentColon };
}

macro_rules! match_tokens {
    ($parse:expr; $($t:ty => $v:expr,)*; $def:expr $(,)? ) => {
        'b: {
            let parse = $parse.clone();
            $(
                if <$t as $crate::tokens::Token>::peek(parse.cursor()){
                    $parse.update_cursor(parse.cursor());
                    break 'b ($v)
                }
            )*
            $def
        }
    };
}
pub(crate) use match_tokens;
