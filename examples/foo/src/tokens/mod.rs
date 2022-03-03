pub use self::generated::*;
use logos::Lexer;
use std::fmt::{self, Display, Formatter};

mod generated;

fn make_ident(lexer: &mut Lexer<Token>) -> Ident {
  Ident(String::from(lexer.slice()))
}

fn make_num_lit(lexer: &mut Lexer<Token>) -> NumLit {
  let mut iter = lexer.slice().split('.');
  NumLit(
    iter.next().unwrap().parse().unwrap(),
    iter.next().map(|text| text.parse().unwrap()),
  )
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Ident(String);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NumLit(i128, Option<u128>);

impl Display for Ident {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
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
