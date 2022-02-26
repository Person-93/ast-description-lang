pub use self::generated::*;
use logos::Lexer;
use std::{num::ParseFloatError, str::FromStr};

mod generated;

pub struct Ident(String);

pub struct NumLit(f64);

pub struct StrLit(String);

impl Ident {
  fn new(lexer: Lexer<Token>) -> Self {
    Self(String::from(lexer.slice()))
  }
}

impl NumLit {
  fn new(lexer: Lexer<Token>) -> Result<Self, ParseFloatError> {
    f64::from_str(lexer.slice()).map(Self)
  }
}

impl StrLit {
  fn new(lexer: Lexer<Token>) -> Self {
    Self(String::from(lexer.slice()))
  }
}
