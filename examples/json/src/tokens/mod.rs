pub use self::generated::*;
use logos::Lexer;
use std::fmt::{self, Display, Formatter};

mod generated;

fn make_num_lit(lexer: &mut Lexer<Token>) -> NumLit {
  let mut iter = lexer.slice().split('.');
  NumLit(
    iter.next().unwrap().parse().unwrap(),
    iter.next().map(|text| text.parse().unwrap()),
  )
}

fn make_str_lit(lexer: &mut Lexer<Token>) -> StrLit {
  StrLit(String::from(lexer.slice()))
}

// the float in the number literal is split into two parts
// the part before the decimal and the part after it
// this is because all tokens must be hashable, but floats
// can't be hashed
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NumLit(i128, Option<u128>);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StrLit(String);

impl Display for NumLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)?;
    if let Some(dec) = self.1 {
      write!(f, ".{dec}")?;
    }
    Ok(())
  }
}

impl Display for StrLit {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    Display::fmt(&self.0, f)
  }
}
