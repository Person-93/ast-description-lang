use crate::{Ident, InvalidIdent, Specs};
use anyhow::{anyhow, Context, Result};
use proc_macro2::TokenStream;
use std::{
  fmt::{self, Display, Formatter},
  str::FromStr,
};

mod collapsed;
mod lex;
mod parsed;
mod print;
mod raw;

pub fn generate_ast_mod(text: &str, specs: &Specs<'_>, config: Config<'_>) -> Result<TokenStream> {
  let error = TokenStream::from_str(config.error)
    .map_err(|err| anyhow!("{err}"))
    .context("failed to lex the error type")?;
  let tokens_mod = TokenStream::from_str(config.tokens_mod)
    .map_err(|err| anyhow!("{err}"))
    .context("failed to lex the tokens mod path")?;
  let ast = raw::Ast::parse(text).context("failed to lex AST description")?;
  let ast = ast
    .transform(specs)
    .context("failed to parse AST description")?;
  let ast = ast
    .transform()
    .context("failed to collapse AST description")?;
  let ast = ast.print(specs, error, tokens_mod);
  Ok(ast)
}

#[non_exhaustive]
pub struct Config<'c> {
  pub error: &'c str,
  pub tokens_mod: &'c str,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum Modifier {
  Repeat,
  Csv,
  OnePlus,
  CsvOnePlus,
  Optional,
  Boxed,
}

impl<'i> Ident<'i> {
  pub fn new(ident: &'i str) -> Result<Self, InvalidIdent> {
    if ident.chars().all(|c| raw::IDENT_CHARS.contains(c)) {
      Ok(Ident(ident))
    } else {
      Err(InvalidIdent(String::from(ident)))
    }
  }
}

impl Default for Config<'_> {
  fn default() -> Self {
    Self {
      error: "::chumsky::error::Simple<tokens::Token>",
      tokens_mod: "crate::tokens",
    }
  }
}

impl Display for Modifier {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Modifier::Repeat => write!(f, "*"),
      Modifier::Csv => write!(f, ",*"),
      Modifier::OnePlus => write!(f, "+"),
      Modifier::CsvOnePlus => write!(f, ",+"),
      Modifier::Optional => write!(f, "?"),
      Modifier::Boxed => write!(f, "~"),
    }
  }
}
