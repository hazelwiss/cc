use crate::braced;
use crate::ident::Ident;
use crate::literal::{LitInt, LitStr, Literal};
use crate::parse::{Parse, ParseStream, Punctuated, Result};
use crate::{
    parenthesized, token,
    tokens::{self, match_tokens},
};

macro_rules! def_binary {
    ($($ident:ident);* $(;)?) => {
        $(
            #[derive(Debug)]
            pub struct $ident{
                pub l: Box<Expr>,
                pub r: Box<Expr>,
            }
        )*
    };
}

def_binary!(
    Mul;
    Div;
    Mod;
    Add;
    Sub;
    ShiftL;
    ShiftR;
    GreaterEq;
    LessEq;
    Greater;
    Less;
    NotEq;
    Eq;
    BitwiseAnd;
    BitwiseXor;
    BitwiseOr;
    LogicalAnd;
    LogicalOr;
    AssignOr;
    AssignXor;
    AssignAnd;
    AssignRShift;
    AssignLShift;
    AssignSub;
    AssignAdd;
    AssignMod;
    AssignDiv;
    AssignMul;
    AssignEq;
    Comma;
);

macro_rules! def_unary {
    ($($ident:ident);* $(;)?) => {
        $(
            #[derive(Debug)]
            pub struct $ident{
                pub expr: Box<Expr>,
            }
        )*
    };
}

def_unary!(
    PreIncrement;
    PreDecrement;
    Negate;
    Promote;
    Address;
    Deref;
    BitwiseInvert;
    LogicalNot;
    Sizeof;
);

#[derive(Debug)]
pub struct Alignof {
    pub type_name: Ident,
}

#[derive(Debug)]
pub struct Subscript {
    pub outer: Box<Expr>,
    pub bracket: tokens::Bracket,
    pub inner: Box<Expr>,
}

#[derive(Debug)]
pub struct FnCall {
    pub func: Box<Expr>,
    pub paren: tokens::Paren,
    pub args: Punctuated<Expr, token![,]>,
}

#[derive(Debug)]
pub struct Member {
    pub expr: Box<Expr>,
    pub member: Ident,
}

#[derive(Debug)]
pub struct MemberPtr {
    pub expr: Box<Expr>,
    pub member: Ident,
}

#[derive(Debug)]
pub struct PostIncrement {
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct PostDecrement {
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct Construct {
    pub ty: Ident,
    pub initializer_list: Punctuated<Expr, token![,]>,
}

#[derive(Debug)]
pub struct Cast {
    pub paren: tokens::Paren,
    pub ty: Ident,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct Ternary {
    pub cond_expr: Box<Expr>,
    pub true_expr: Box<Expr>,
    pub false_expr: Box<Expr>,
}

macro_rules! def_expr {
    ($($ident:ident),* $(,)?) => {
        #[derive(Debug)]
        pub enum Expr{
            Ident(Ident),
            LitInt(LitInt),
            LitStr(LitStr),
            Expr(Box<Expr>),
            $(
                $ident($ident)
            ),*
        }
    };
}

def_expr!(
    Subscript,
    FnCall,
    Member,
    MemberPtr,
    PostIncrement,
    PostDecrement,
    Construct,
    PreIncrement,
    PreDecrement,
    Negate,
    Promote,
    Address,
    Deref,
    BitwiseInvert,
    LogicalNot,
    Sizeof,
    Alignof,
    Cast,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    ShiftL,
    ShiftR,
    GreaterEq,
    LessEq,
    Greater,
    Less,
    NotEq,
    Eq,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    LogicalAnd,
    LogicalOr,
    Ternary,
    AssignOr,
    AssignXor,
    AssignAnd,
    AssignRShift,
    AssignLShift,
    AssignSub,
    AssignAdd,
    AssignMod,
    AssignDiv,
    AssignMul,
    AssignEq,
    Comma,
);

impl Parse for Expr {
    fn parse(parse: ParseStream) -> Result<Self> {
        parse_comma(parse)
    }
}

fn parse_primary(parse: ParseStream) -> Result<Expr> {
    Ok(if let Ok(ident) = parse.parse::<Ident>() {
        Expr::Ident(ident)
    } else if let Ok(lit) = parse.parse::<Literal>() {
        match lit {
            Literal::Int(int) => Expr::LitInt(int),
            Literal::Float(_) => unimplemented!(),
            Literal::String(str) => Expr::LitStr(str),
        }
    } else if parse.peek::<tokens::Paren>() {
        let content;
        parenthesized!(content in parse)?;
        let expr = Box::new(content.parse()?);
        Expr::Expr(expr)
    } else {
        return Err(parse.error("expected primary expression."));
    })
}

fn parse_postfix(parse: ParseStream) -> Result<Expr> {
    Ok(if parse.peek::<tokens::Paren>() {
        let content;
        let _paren = parenthesized!(content in parse)?;
        let ty = content.parse()?;
        let content;
        let _brace = braced!(content in parse)?;
        let initializer_list = Punctuated::parse_terminated(&content)?;
        Expr::Construct(Construct {
            ty,
            initializer_list,
        })
    } else {
        let mut expr = parse_primary(parse)?;
        'l: loop {
            match_tokens!(
                parse;
                token![.] => {
                    let ident = parse.parse()?;
                    expr = Expr::Member(Member{
                        expr: Box::new(expr),
                        member: ident
                    })
                },
                token![->] => {
                    let ident = parse.parse()?;
                    expr = Expr::MemberPtr(MemberPtr{
                        expr: Box::new(expr),
                        member: ident,
                    })
                },
                token![++] => {
                    expr = Expr::PostIncrement(PostIncrement{
                        expr: Box::new(expr),
                    })
                },
                token![--] => {
                    expr = Expr::PostDecrement( PostDecrement{
                        expr: Box::new(expr),
                    })
                },
                ; break 'l,
            )
        }
        expr
    })
}

fn parse_unary(parse: ParseStream) -> Result<Expr> {
    macro_rules! def {
        ($ident:ident) => {
            Expr::$ident($ident {
                expr: Box::new(parse_cast(&parse)?),
            })
        };
    }
    Ok(match_tokens!(
        parse;
        token![++] => Expr::PreIncrement(PreIncrement{
            expr: Box::new(parse_unary(&parse)?),
        }),
        token![--] => Expr::PreDecrement(PreDecrement{
            expr: Box::new(parse_unary(&parse)?),
        }),
        token![&] => def!(Address),
        token![*] => def!(Deref),
        token![+] => def!(Promote),
        token![-] => def!(Negate),
        token![~] => def!(BitwiseInvert),
        token![!] => def!(LogicalNot),
        token![sizeof] => def!(Sizeof),
        token![_Alignof] => {
            let content;
            let _paren = parenthesized!(content in &parse)?;
            let type_name = content.parse()?;
            Expr::Alignof(Alignof { type_name })
        },
        ; return parse_postfix(&parse)
    ))
}

fn parse_cast(parse: ParseStream) -> Result<Expr> {
    if parse.peek::<tokens::Paren>() {
        let content;
        let paren = parenthesized!(content in parse)?;
        let ty = content.parse()?;
        let expr = Box::new(parse.parse()?);
        Ok(Expr::Cast(Cast { paren, ty, expr }))
    } else {
        parse_unary(parse)
    }
}

fn parse_mul(parse: ParseStream) -> Result<Expr> {
    let cast = parse_cast(parse)?;
    macro_rules! def {
        ($ident:ident) => {
            Expr::$ident($ident {
                l: Box::new(cast),
                r: Box::new(parse_cast(&parse)?),
            })
        };
    }
    Ok(match_tokens!(
        parse;
        token![*] => def!(Mul),
        token![/] => def!(Div),
        token![%] => def!(Mod),
        ; cast
    ))
}

fn parse_add(parse: ParseStream) -> Result<Expr> {
    let mul = parse_mul(parse)?;
    macro_rules! def {
        ($ident:ident) => {
            Expr::$ident($ident {
                l: Box::new(mul),
                r: Box::new(parse_add(&parse)?),
            })
        };
    }
    Ok(match_tokens!(
        parse;
        token![+] => def!(Add),
        token![-] => def!(Sub),
        ; mul
    ))
}

fn parse_bwshift(parse: ParseStream) -> Result<Expr> {
    let add = parse_add(parse)?;
    macro_rules! def {
        ($ident:ident) => {
            Expr::$ident($ident {
                l: Box::new(add),
                r: Box::new(parse_bwshift(&parse)?),
            })
        };
    }
    Ok(match_tokens!(
        parse;
        token![<<] => def!(ShiftL),
        token![>>] => def!(ShiftR),
        ; add
    ))
}

fn parse_relational(parse: ParseStream) -> Result<Expr> {
    let shift = parse_bwshift(parse)?;
    macro_rules! def {
        ($ident:ident) => {
            Expr::$ident($ident {
                l: Box::new(shift),
                r: Box::new(parse_relational(&parse)?),
            })
        };
    }
    Ok(match_tokens!(
        parse;
        token![<] => def!(Less),
        token![>] => def!(Greater),
        token![<=] => def!(LessEq),
        token![>=] => def!(GreaterEq),
        ; shift
    ))
}

fn parse_eq(parse: ParseStream) -> Result<Expr> {
    let relational = parse_relational(parse)?;
    macro_rules! def {
        ($ident:ident) => {
            Expr::$ident($ident {
                l: Box::new(relational),
                r: Box::new(parse_eq(&parse)?),
            })
        };
    }
    Ok(match_tokens!(
        parse;
        token![==] => def!(Eq),
        token![!=] => def!(NotEq),
        ; relational
    ))
}

fn parse_bwand(parse: ParseStream) -> Result<Expr> {
    let eq = parse_eq(parse)?;
    Ok(if parse.expect::<token![&]>() {
        Expr::BitwiseAnd(BitwiseAnd {
            l: Box::new(eq),
            r: Box::new(parse_bwand(parse)?),
        })
    } else {
        eq
    })
}

fn parse_bwxor(parse: ParseStream) -> Result<Expr> {
    let and = parse_bwand(parse)?;
    Ok(if parse.expect::<token![^]>() {
        Expr::BitwiseXor(BitwiseXor {
            l: Box::new(and),
            r: Box::new(parse_bwxor(parse)?),
        })
    } else {
        and
    })
}

fn parse_bwor(parse: ParseStream) -> Result<Expr> {
    let xor = parse_bwxor(parse)?;
    Ok(if parse.expect::<token![|]>() {
        Expr::BitwiseOr(BitwiseOr {
            l: Box::new(xor),
            r: Box::new(parse_bwor(parse)?),
        })
    } else {
        xor
    })
}

fn parse_land(parse: ParseStream) -> Result<Expr> {
    let or = parse_bwor(parse)?;
    Ok(if parse.expect::<token![&&]>() {
        Expr::LogicalAnd(LogicalAnd {
            l: Box::new(or),
            r: Box::new(parse_land(parse)?),
        })
    } else {
        or
    })
}

fn parse_lor(parse: ParseStream) -> Result<Expr> {
    let and = parse_land(parse)?;
    Ok(if parse.expect::<token![||]>() {
        Expr::LogicalOr(LogicalOr {
            l: Box::new(and),
            r: Box::new(parse_lor(parse)?),
        })
    } else {
        and
    })
}

fn parse_cond(parse: ParseStream) -> Result<Expr> {
    let cond_expr = parse_lor(parse)?;
    Ok(if parse.expect::<token![?]>() {
        let true_expr = Box::new(parse.parse()?);
        let colon = parse.parse::<token![:]>()?;
        let false_expr = Box::new(parse_cond(parse)?);
        Expr::Ternary(Ternary {
            cond_expr: Box::new(cond_expr),
            true_expr,
            false_expr,
        })
    } else {
        cond_expr
    })
}

fn parse_assign(parse: ParseStream) -> Result<Expr> {
    let clone = parse.clone();
    if let Ok(unary) = parse_unary(&clone) {
        macro_rules! def {
            ($ident:ident) => {
                Expr::$ident($ident {
                    l: Box::new(unary),
                    r: Box::new(parse_assign(&clone)?),
                })
            };
        }
        let expr = match_tokens!(
            clone;
            token![=] => def!(AssignEq),
            token![*=] => def!(AssignMul),
            token![/=] => def!(AssignDiv),
            token![%=] => def!(AssignMod),
            token![+=] => def!(AssignAdd),
            token![-=] => def!(AssignSub),
            token![<<=] => def!(AssignLShift),
            token![>>=] => def!(AssignRShift),
            token![&=] => def!(AssignAnd),
            token![^=] => def!(AssignXor),
            token![|=] => def!(AssignOr),
            ; return parse_cond(&parse)
        );
        parse.update_cursor(clone.cursor());
        Ok(expr)
    } else {
        parse_cond(parse)
    }
}

fn parse_comma(parse: ParseStream) -> Result<Expr> {
    let assingment_expr = parse_assign(parse)?;
    Ok(if parse.expect::<token![,]>() {
        Expr::Comma(Comma {
            l: Box::new(assingment_expr),
            r: Box::new(parse_comma(parse)?),
        })
    } else {
        assingment_expr
    })
}
