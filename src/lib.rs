pub use self::collections::{NamedItem, NamedSet, Unnamed};
use anyhow::{Context, Result};
use heck::ToPascalCase;
use indexmap::IndexMap;
use proc_macro2::TokenStream;
use serde::Deserialize;
use std::fmt::{self, Display, Formatter};

mod collapsed;
mod collections;
mod lex;
mod parsed;
mod print;
mod raw;

pub trait Specs<'s> {
  fn delimiters(&self) -> &NamedSet<'s, Delimiter<'s>>;
  fn static_tokens(&self) -> &IndexMap<Ident<'s>, &'s str>;
  fn dynamic_tokens(&self) -> &IndexMap<Ident<'s>, &'s str>;
}

pub fn generate<'s, S: Specs<'s>>(text: &'s str, specs: &'s S) -> Result<TokenStream> {
  let ast = raw::Ast::parse(text).context("failed to lex AST description")?;
  let ast = ast
    .transform(specs)
    .context("failed to parse AST description")?;
  let ast = ast
    .transform()
    .context("failed to collapse AST description")?;
  let ast = ast.print(specs);
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

#[derive(Copy, Clone, Debug)]
pub struct Delimiter<'d> {
  name: Ident<'d>,
  open: &'d str,
  close: &'d str,
}

impl Ident<'_> {
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

impl<'d> self::collections::NamedItem<'d> for Delimiter<'d> {
  type Name = Ident<'d>;
  type Unnamed = UnnamedDelimiter<'d>;

  fn name(&self) -> Self::Name {
    self.name
  }

  fn dummy(name: Self::Name) -> Self {
    Self {
      name,
      open: "",
      close: "",
    }
  }
}

#[derive(Deserialize)]
pub struct UnnamedDelimiter<'d> {
  open: &'d str,
  close: &'d str,
}

impl<'d> Unnamed<'d> for UnnamedDelimiter<'d> {
  type Named = Delimiter<'d>;

  fn add_name(self, name: <Self::Named as NamedItem<'d>>::Name) -> Self::Named {
    Delimiter {
      name,
      open: self.open,
      close: self.close,
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
