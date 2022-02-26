pub use self::{
  ast::*,
  collections::{NamedItem, NamedSet, Unnamed},
};
use heck::ToPascalCase;
use indexmap::IndexMap;
use serde::Deserialize;
use std::fmt::{Display, Formatter};
use thiserror::Error;

mod ast;
mod collections;
mod tokens;

#[derive(Clone, Debug, Deserialize)]
#[serde(bound(deserialize = "'de: 's"))]
pub struct Specs<'s> {
  pub static_tokens: IndexMap<Ident<'s>, &'s str>,
  pub dynamic_tokens: IndexMap<Ident<'s>, &'s str>,
  pub delimiters: IndexMap<Ident<'s>, Delimiter<'s>>,
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Deserialize)]
#[serde(transparent)]
pub struct Ident<'a>(&'a str);

#[derive(Copy, Clone, Debug, Deserialize)]
pub struct Delimiter<'d> {
  open: &'d str,
  close: &'d str,
}

impl<'i> Ident<'i> {
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

#[derive(Clone, Debug, Error)]
#[error("invalid ident: {0}")]
pub struct InvalidIdent(String);

fn is_rust_keyword(word: &str) -> bool {
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
const SNAPSHOT_CASES: &[&str] = &["json", "foo"];
