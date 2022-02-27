use super::{
  super::{
    raw::{Ast as RawAst, NodeDef, NodeKindDef},
    Modifier,
  },
  Ast, Node, NodeKind, TaggedNodeKind,
};
use crate::{Ident, Specs};
use std::{cell::RefCell, collections::hash_map::HashMap, marker::PhantomData};
use thiserror::Error;

impl<'a> RawAst<'a> {
  pub fn transform(&'a self, specs: &'a Specs<'a>) -> Result<Ast<'a>, Error> {
    let validator = Validator::new(self, specs);
    Ok(Ast(
      self
        .iter()
        .map(|entry| -> Result<_, _> {
          validator
            .validate_node_def(&entry.node)
            .map(|node_def| Node {
              ident: entry.ident,
              kind: node_from_def(node_def.kind),
              tag: node_def.tag,
            })
        })
        .collect::<Result<_, _>>()?,
    ))
  }
}

fn node_from_def(node_def_kind: ValidNodeKind) -> NodeKind {
  match node_def_kind {
    ValidNodeKind::Node(ident) => NodeKind::Node(ident),
    ValidNodeKind::Modified(inner, modifier) => {
      NodeKind::Modified(Box::new(node_from_def(inner.kind)), modifier)
    }
    ValidNodeKind::StaticToken(ident) => NodeKind::StaticToken(ident),
    ValidNodeKind::DynamicToken(ident) => NodeKind::DynamicToken(ident),
    ValidNodeKind::Group { nodes, inline } => NodeKind::Group {
      nodes: (nodes
        .into_iter()
        .map(|node| TaggedNodeKind {
          kind: node_from_def(node.kind),
          tag: node.tag,
        })
        .collect::<Vec<_>>()),
      inline,
    },
    ValidNodeKind::Choice { nodes, inline } => NodeKind::Choice {
      nodes: nodes
        .into_iter()
        .map(|node| TaggedNodeKind {
          kind: node_from_def(node.kind),
          tag: node.tag,
        })
        .collect::<Vec<_>>(),
      inline,
    },
    ValidNodeKind::Delimited(inner, delimiter) => {
      NodeKind::Delimited(Box::new(node_from_def(inner.kind)), delimiter)
    }
    ValidNodeKind::Todo => NodeKind::Todo,
    ValidNodeKind::End => NodeKind::End,
  }
}

struct Validator<'v, 'ast: 'v, 's: 'ast> {
  ast: &'v RawAst<'ast>,
  specs: &'v Specs<'s>,
  cache: RefCell<HashMap<&'v NodeKindDef<'ast>, ValidNodeKind<'ast>>>,
  _marker: PhantomData<&'s ()>,
}

impl<'v, 'ast: 'v, 's: 'ast> Validator<'v, 'ast, 's> {
  fn new(ast: &'v RawAst<'ast>, specs: &'v Specs<'s>) -> Validator<'v, 'ast, 's> {
    Validator {
      ast,
      specs,
      cache: Default::default(),
      _marker: PhantomData,
    }
  }

  fn validate_node_def(&self, node_def: &'v NodeDef<'ast>) -> Result<ValidNode<'ast>, Error> {
    let cache = self.cache.borrow();
    Ok(ValidNode {
      tag: node_def.tag,
      kind: if let Some(valid_node_def) = cache.get(&node_def.kind) {
        valid_node_def.clone()
      } else {
        drop(cache);
        let valid_node = match &node_def.kind {
          NodeKindDef::Simple(ident) => {
            let ident = *ident;
            if self.ast.get(ident).is_some() {
              ValidNodeKind::Node(ident)
            } else if self.specs.static_tokens.keys().copied().any(|t| t == ident) {
              ValidNodeKind::StaticToken(ident)
            } else if self
              .specs
              .dynamic_tokens
              .keys()
              .copied()
              .any(|t| t == ident)
            {
              ValidNodeKind::DynamicToken(ident)
            } else {
              return Err(Error::UnknownIdent(String::from(ident.0)));
            }
          }
          NodeKindDef::Modified(inner, modifier) => {
            ValidNodeKind::Modified(Box::new(self.validate_node_def(inner)?), *modifier)
          }
          NodeKindDef::Delimiter(inner, delimiter) => {
            if self.specs.delimiters.contains_key(delimiter) {
              ValidNodeKind::Delimited(Box::new(self.validate_node_def(inner)?), *delimiter)
            } else {
              return Err(Error::UnknownDelim(String::from(delimiter.0)));
            }
          }
          NodeKindDef::Group { nodes, inline } => ValidNodeKind::Group {
            nodes: nodes
              .iter()
              .map(|node| self.validate_node_def(node))
              .collect::<Result<_, _>>()?,
            inline: *inline,
          },
          NodeKindDef::Choice {
            first,
            second,
            inline,
          } => ValidNodeKind::Choice {
            nodes: self.validate_choices(first, second, Vec::with_capacity(1))?,
            inline: *inline,
          },
          NodeKindDef::Todo => ValidNodeKind::Todo,
          NodeKindDef::End => ValidNodeKind::End,
        };
        self
          .cache
          .borrow_mut()
          .insert(&node_def.kind, valid_node.clone());
        valid_node
      },
    })
  }

  fn validate_choices(
    &self,
    first: &'v NodeDef<'ast>,
    second: &'v NodeDef<'ast>,
    mut processed: Vec<ValidNode<'ast>>,
  ) -> Result<Vec<ValidNode<'ast>>, Error> {
    processed.push(self.validate_node_def(first)?);
    match &second.kind {
      NodeKindDef::Choice {
        first,
        second,
        inline: _,
      } => self.validate_choices(first, second, processed),
      _ => {
        processed.push(self.validate_node_def(second)?);
        Ok(processed)
      }
    }
  }
}

#[derive(Clone, Debug)]
struct ValidNode<'a> {
  kind: ValidNodeKind<'a>,
  tag: Option<Ident<'a>>,
}

#[derive(Clone, Debug)]
enum ValidNodeKind<'a> {
  Node(Ident<'a>),
  Modified(Box<ValidNode<'a>>, Modifier),
  StaticToken(Ident<'a>),
  DynamicToken(Ident<'a>),
  Group {
    nodes: Vec<ValidNode<'a>>,
    inline: bool,
  },
  Choice {
    nodes: Vec<ValidNode<'a>>,
    inline: bool,
  },
  Delimited(Box<ValidNode<'a>>, Ident<'a>),
  Todo,
  End,
}

#[derive(Clone, Debug, Error)]
pub enum Error {
  #[error("unknown identifier: `{0}`")]
  UnknownIdent(String),
  #[error("unknown delimiter: `{0}`")]
  UnknownDelim(String),
}
