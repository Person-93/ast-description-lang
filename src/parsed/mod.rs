use super::Modifier;
use crate::collections::{NamedItem, NamedSet, Unnamed};
use crate::Ident;
use std::{
  fmt::{self, Display, Formatter},
  ops::{Deref, DerefMut},
};

mod transform;

#[derive(Debug, Clone)]
pub struct Ast<'a>(NamedSet<'a, Node<'a>>);

#[derive(Clone, Debug)]
pub struct Node<'n> {
  pub ident: Ident<'n>,
  pub kind: NodeKind<'n>,
  pub tag: Option<Ident<'n>>,
}

#[derive(Clone, Debug)]
pub struct TaggedNodeKind<'n> {
  pub kind: NodeKind<'n>,
  pub tag: Option<Ident<'n>>,
}

#[derive(Clone, Debug)]
pub enum NodeKind<'n> {
  Node(Ident<'n>),
  StaticToken(Ident<'n>),
  DynamicToken(Ident<'n>),
  Group {
    nodes: Vec<TaggedNodeKind<'n>>,
    inline: bool,
  },
  Choice {
    nodes: Vec<TaggedNodeKind<'n>>,
    inline: bool,
  },
  Delimited(Box<NodeKind<'n>>, Ident<'n>),
  Modified(Box<NodeKind<'n>>, Modifier),
  Todo,
}

impl<'a, 'n: 'a> FromIterator<Node<'n>> for Ast<'a> {
  fn from_iter<T: IntoIterator<Item = Node<'n>>>(iter: T) -> Self {
    Ast(iter.into_iter().collect())
  }
}

impl<'a> DerefMut for Ast<'a> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl<'a> Deref for Ast<'a> {
  type Target = NamedSet<'a, Node<'a>>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl Display for Node<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{} = {}", self.ident, self.kind)
  }
}

impl Display for TaggedNodeKind<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.kind)?;
    if let Some(tag) = self.tag {
      write!(f, ":{}", tag)?;
    }
    Ok(())
  }
}

impl Display for NodeKind<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      NodeKind::Node(ident) | NodeKind::StaticToken(ident) | NodeKind::DynamicToken(ident) => {
        write!(f, "{ident}")
      }
      NodeKind::Group { nodes, inline: _ } => write!(
        f,
        "{}",
        nodes
          .iter()
          .map(ToString::to_string)
          .collect::<Vec<_>>()
          .join(" ")
      ),
      NodeKind::Choice { nodes, inline: _ } => write!(
        f,
        "{}",
        nodes
          .iter()
          .map(ToString::to_string)
          .collect::<Vec<_>>()
          .join(" | ")
      ),
      NodeKind::Delimited(inner, delimiter) => write!(f, "delim[{delimiter}]<{inner}>"),
      NodeKind::Modified(inner, modifier) => match inner.as_ref() {
        NodeKind::Group { .. } | NodeKind::Choice { .. } => write!(f, "({inner}){modifier}"),
        NodeKind::Node(_)
        | NodeKind::StaticToken(_)
        | NodeKind::DynamicToken(_)
        | NodeKind::Delimited(..) => write!(f, "{inner}{modifier}"),
        NodeKind::Modified(..) => unreachable!(),
        NodeKind::Todo => write!(f, "!todo"),
      },
      NodeKind::Todo => write!(f, "!todo"),
    }
  }
}

impl<'n> NamedItem<'n> for Node<'n> {
  type Name = Ident<'n>;
  type Unnamed = TaggedNodeKind<'n>;

  fn name(&self) -> Ident<'n> {
    self.ident
  }

  fn dummy(name: Ident<'n>) -> Self {
    Node {
      ident: name,
      kind: NodeKind::Node(Ident("")),
      tag: None,
    }
  }
}

impl<'n> Unnamed<'n> for TaggedNodeKind<'n> {
  type Named = Node<'n>;

  fn add_name(self, name: Ident<'n>) -> Self::Named {
    Node {
      ident: name,
      kind: self.kind,
      tag: self.tag,
    }
  }
}

#[cfg(test)]
#[test]
fn snapshots() {
  use crate::raw::Ast;
  use insta::{assert_debug_snapshot, with_settings};
  use std::{fs, path::Path};

  for (name, specs) in super::SNAPSHOT_CASES {
    let mut path = Path::new(env!("CARGO_MANIFEST_DIR"))
      .join("examples")
      .join(name);
    path.set_extension("ast");
    let text = fs::read_to_string(&path).unwrap();
    with_settings!({input_file => Some(path)}, {
      assert_debug_snapshot!(Ast::parse(&text).unwrap().transform(&specs()));
    });
  }
}
