use crate::tokens::Token;
use anyhow::Result;
use chumsky::{error::Simple, Parser, Stream};
use logos::Logos;
use std::fmt::{self, Display, Formatter};

mod ast;
mod tokens;

fn main() -> Result<()> {
  let foo = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/sample.foo"))?;
  let iter = Token::lexer(&foo).spanned();
  let parsed = ast::parse::file()
    .parse(Stream::from_iter(foo.len()..foo.len() + 1, iter))
    .map_err(Error)?;
  println!("{parsed:#?}");
  Ok(())
}

#[derive(Clone, Debug)]
struct Error(Vec<Simple<Token>>);

impl Display for Error {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    for (idx, err) in self.0.iter().enumerate() {
      write!(f, "{idx}: {err}")?;
    }
    Ok(())
  }
}

impl std::error::Error for Error {}
