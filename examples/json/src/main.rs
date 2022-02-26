use crate::tokens::Token;
use chumsky::{Parser, Stream};
use logos::Logos;

mod ast;
mod tokens;

fn main() {
  let json = std::fs::read_to_string("sample.json").unwrap();
  let iter = Token::lexer(&json).spanned();
  let parsed = ast::parse::json()
    .parse(Stream::from_iter(json.len()..json.len() + 1, iter))
    .unwrap();
  println!("{parsed:#?}")
}
