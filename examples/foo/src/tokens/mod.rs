pub use self::generated::*;
use logos::Lexer;
use std::fmt::{self, Display, Formatter};

mod generated;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Ident(String);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NumLit(i128, Option<u128>);

impl Ident {
  fn new(lexer: &mut Lexer<Token>) -> Self {
    Self(String::from(lexer.slice()))
  }
}

impl Display for Ident {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

impl NumLit {
  fn new(lexer: &mut Lexer<Token>) -> Self {
    let mut iter = lexer.slice().split('.');
    Self(
      iter.next().unwrap().parse().unwrap(),
      iter.next().map(|text| text.parse().unwrap()),
    )
  }
}

impl Display for NumLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)?;
    if let Some(dec) = self.1 {
      write!(f, "{dec}")?;
    }
    Ok(())
  }
}
