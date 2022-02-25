pub use self::collections::{NamedItem, NamedSet, Unnamed};
use anyhow::{anyhow, Context, Result};
use heck::ToPascalCase;
use indexmap::IndexMap;
use proc_macro2::TokenStream;
use serde::Deserialize;
use std::{
  fmt::{self, Display, Formatter},
  str::FromStr,
};
use thiserror::Error;

mod collapsed;
mod collections;
mod lex;
mod parsed;
mod print;
mod raw;

#[non_exhaustive]
pub struct Config<'c> {
  pub error: &'c str,
  pub tokens_mod: &'c str,
}

impl Default for Config<'_> {
  fn default() -> Self {
    Self {
      error: "::chumsky::error::Simple<tokens::Token>",
      tokens_mod: "crate::tokens",
    }
  }
}

#[derive(Clone, Debug, Deserialize)]
#[serde(bound(deserialize = "'de: 's"))]
pub struct Specs<'s> {
  pub static_tokens: IndexMap<Ident<'s>, &'s str>,
  pub dynamic_tokens: IndexMap<Ident<'s>, &'s str>,
  pub delimiters: IndexMap<Ident<'s>, Delimiter<'s>>,
}

pub fn generate(text: &str, specs: &Specs<'_>, config: Config<'_>) -> Result<TokenStream> {
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

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Deserialize)]
#[serde(transparent)]
pub struct Ident<'a>(&'a str);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Modifier {
  Repeat,
  Csv,
  OnePlus,
  CsvOnePlus,
  Optional,
  Boxed,
}

#[derive(Copy, Clone, Debug, Deserialize)]
pub struct Delimiter<'d> {
  open: &'d str,
  close: &'d str,
}

impl<'i> Ident<'i> {
  pub fn new(ident: &'i str) -> Result<Self, InvalidIdent> {
    if ident.chars().all(|c| raw::IDENT_CHARS.contains(c)) {
      Ok(Ident(ident))
    } else {
      Err(InvalidIdent(String::from(ident)))
    }
  }

  pub fn as_type(&self) -> proc_macro2::Ident {
    quote::format_ident!("{}", self.0.to_pascal_case())
  }
}

impl Display for Ident<'_> {
  #[inline(always)]
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    Display::fmt(self.0, f)
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

pub fn is_rust_keyword(word: &str) -> bool {
  #[rustfmt::skip]
  const KEYWORDS: &[&str] = &[
    // strict keywords
    "as", "async", "await", "break", "const", "continue", "crate", "dyn", "else", "enum", "extern",
    "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub",
    "ref", "return", "self", "Self", "static", "struct", "super", "trait", "true", "type",
    "unsafe", "use", "where", "while",

    // reserved keywords
    "abstract", "become", "box", "do", "final", "macro", "override", "priv", "try", "typeof",
    "unsized", "virtual", "yield",
  ];

  KEYWORDS.contains(&word)
}

#[cfg(test)]
#[allow(clippy::type_complexity)]
const SNAPSHOT_CASES: &[(&str, fn() -> Specs<'static>)] =
  &[("empty", Specs::empty), ("json", Specs::json)];

#[cfg(test)]
impl Specs<'_> {
  fn empty() -> Specs<'static> {
    Specs {
      delimiters: Default::default(),
      static_tokens: Default::default(),
      dynamic_tokens: Default::default(),
    }
  }

  fn json() -> Specs<'static> {
    use indexmap::indexmap;

    Specs {
      delimiters: indexmap! {
        Ident("brace") => Delimiter{open: "left_brace", close: "right_brace"},
        Ident("bracket") => Delimiter{open: "left_bracket", close: "right_bracket"},
      },
      static_tokens: indexmap! {
        Ident("left_brace") => "{",
        Ident("right_brace") => "}",
        Ident("left_bracket") => "[",
        Ident("right_bracket") => "]",
        Ident("colon") => ":",
        Ident("kw_true") => "true",
        Ident("kw_false") => "false",
      },
      dynamic_tokens: indexmap! {
        Ident("ident") => "[[:alpha:]_][[:alnum:]_]*",
        Ident("num_lit") => r"\d+(\.\d+)?",
        Ident("str_lit") => r#""([^"]|(\\"))*""#,
      },
    }
  }
}

#[derive(Clone, Debug, Error)]
#[error("invalid ident: {0}")]
pub struct InvalidIdent(String);
