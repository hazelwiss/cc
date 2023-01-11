use crate::parse::{Parse, ParseStream, Result};
use crate::Punctuated;
use crate::{token, tokens::match_tokens};

pub struct Pointer {}

impl Parse for Pointer {}

pub struct Array {}

impl Parse for Array {}

pub struct Function {}

impl Parse for Function {}

pub struct Declarator {}

impl Parse for Declarator {}
