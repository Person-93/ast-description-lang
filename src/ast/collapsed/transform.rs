use super::{
  super::parsed::{
    Ast as ParsedAst, NodeKind as ParsedNodeKind, TaggedNodeKind as ParsedTaggedNodeKind,
  },
  Ast, Choice, ChoiceKind, Group, GroupKind, Node, NodeKind, TaggedNodeKind,
};
use crate::{
  collections::{NamedSet, Unnamed},
  Ident,
};
use std::{collections::HashSet, fmt::Display};
use thiserror::Error;

impl<'a> ParsedAst<'a> {
  pub fn transform(&'a self) -> Result<Ast<'a>, Error> {
    let nodes = Nodes(
      self
        .iter()
        .map(|node| {
          self
            .transform_node_kind(&node.kind, Some(node.ident))
            .and_then(|(kind, hint)| {
              Ok(Node::try_from((
                TaggedNodeKind {
                  kind,
                  tag: node.tag,
                },
                hint,
              ))?)
            })
            .map_err(|err| err.context(format!("failed to parse node: {node}")))
        })
        .collect::<Result<NamedSet<_>, _>>()?,
    );

    let ast = nodes.handle_cycles();

    println!(
      "cyclic nodes: {}",
      ast
        .cyclic
        .iter()
        .copied()
        .enumerate()
        .filter_map(|(idx, cycle)| cycle.then(|| ast.nodes[idx].ident.to_string()))
        .collect::<Vec<_>>()
        .join(", ")
    );

    Ok(ast)
  }

  fn transform_node_kind(
    &'a self,
    kind: &'a ParsedNodeKind<'a>,
    hint: Option<Ident<'a>>,
  ) -> Result<(NodeKind<'a>, Option<Ident<'a>>), Error> {
    match kind {
      ParsedNodeKind::Node(child) => {
        let ident = self.get(*child).unwrap().ident;
        Ok((NodeKind::Node(ident), hint))
      }
      ParsedNodeKind::StaticToken(ident) => Ok((NodeKind::StaticToken(*ident), hint)),
      ParsedNodeKind::DynamicToken(ident) => Ok((NodeKind::DynamicToken(*ident), hint)),
      ParsedNodeKind::Group { nodes, inline } => self.transform_group(nodes, *inline, hint),
      ParsedNodeKind::Choice { nodes, inline } => self.transform_choice(nodes, *inline, hint),
      ParsedNodeKind::Delimited(inner, delimiter) => {
        self.transform_delimited(*delimiter, inner, hint)
      }
      ParsedNodeKind::Modified(inner, modifier) => self
        .transform_node_kind(inner, hint)
        .map(|(inner, hint)| (NodeKind::Modified(Box::new(inner), *modifier), hint)),
      ParsedNodeKind::Todo => Ok((NodeKind::Todo, hint)),
      ParsedNodeKind::End => Ok((NodeKind::End, hint)),
    }
  }

  fn transform_group(
    &'a self,
    nodes: &'a [ParsedTaggedNodeKind<'a>],
    inline: bool,
    hint: Option<Ident<'a>>,
  ) -> Result<(NodeKind<'a>, Option<Ident<'a>>), Error> {
    let indices: Vec<_> = nodes
      .iter()
      .enumerate()
      .filter_map(|(idx, kind)| (!self.is_zero_sized(&kind.kind)).then(|| idx))
      .collect();

    let nodes: Vec<_> = nodes
      .iter()
      .map(|child| {
        self
          .transform_node_kind(&child.kind, None)
          .map(|(kind, hint)| {
            (
              TaggedNodeKind {
                kind,
                tag: child.tag,
              },
              hint,
            )
          })
      })
      .collect::<Result<_, _>>()
      .map_err(|err| {
        err.context(format!(
          "failed to collapse group: {}",
          nodes
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(" ")
        ))
      })?;

    let make_nodes = |nodes: Vec<(TaggedNodeKind<'a>, Option<Ident<'a>>)>| {
      nodes
        .into_iter()
        .map(|(kind, hint)| match hint {
          Some(ident) => Ok(Node {
            ident,
            kind: kind.kind,
            tag: kind.tag,
          }),
          None => Node::try_from((kind, hint)),
        })
        .collect::<Result<_, _>>()
    };

    let (kind, generated_hint) = match indices.len() {
      0 => return Ok((NodeKind::SubGroup(make_nodes(nodes)?), None)),
      1 => (GroupKind::One(indices[0]), nodes[indices[0]].1),
      _ => (GroupKind::Many(indices), None),
    };

    Ok((
      NodeKind::Group(Group {
        members: make_nodes(nodes)?,
        kind,
        inline,
      }),
      hint.or(generated_hint),
    ))
  }

  fn transform_choice(
    &'a self,
    nodes: &'a [ParsedTaggedNodeKind<'a>],
    inline: bool,
    hint: Option<Ident<'a>>,
  ) -> Result<(NodeKind<'a>, Option<Ident<'a>>), Error> {
    let nodes: Vec<_> = nodes
      .iter()
      .map(|node_kind| {
        Ok(Node::try_from(
          self
            .transform_node_kind(&node_kind.kind, None)
            .map(|(untagged_kind, hint)| {
              (
                TaggedNodeKind {
                  kind: untagged_kind,
                  tag: node_kind.tag,
                },
                hint,
              )
            })?,
        )?)
      })
      .collect::<Result<_, Error>>()
      .map_err(|err| {
        err.context(format!(
          "failed to collapse choice: {}",
          nodes
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(" | ")
        ))
      })?;

    Ok((
      NodeKind::Choice(match nodes.len() {
        0 | 1 => unreachable!(),
        2 => match (&nodes[0].kind, &nodes[1].kind) {
          (NodeKind::StaticToken(_), NodeKind::StaticToken(_)) => Choice {
            kind: ChoiceKind::Regular(nodes),
            inline,
          },
          (NodeKind::StaticToken(ident), _) => Choice {
            kind: ChoiceKind::Option {
              secondary: *ident,
              primary: Box::new(nodes.into_iter().nth(1).unwrap()),
            },
            inline,
          },
          (_, NodeKind::StaticToken(ident)) => Choice {
            kind: ChoiceKind::Option {
              secondary: *ident,
              primary: Box::new(nodes.into_iter().next().unwrap()),
            },
            inline,
          },
          (_, _) => Choice {
            kind: ChoiceKind::Regular(nodes),
            inline,
          },
        },
        _ => Choice {
          kind: ChoiceKind::Regular(nodes),
          inline,
        },
      }),
      hint,
    ))
  }

  fn transform_delimited(
    &'a self,
    delimiter: Ident<'a>,
    inner: &'a ParsedNodeKind<'a>,
    hint: Option<Ident<'a>>,
  ) -> Result<(NodeKind<'a>, Option<Ident<'a>>), Error> {
    self
      .transform_node_kind(inner, hint)
      .map(|(inner, hint)| (NodeKind::Delimited(Box::new(inner), delimiter), hint))
  }

  #[must_use]
  fn is_zero_sized(&self, kind: &ParsedNodeKind<'_>) -> bool {
    match kind {
      ParsedNodeKind::Node(child) => self.is_zero_sized(&self.get(*child).unwrap().kind),
      ParsedNodeKind::StaticToken(_) => true,
      ParsedNodeKind::DynamicToken(_) => false,
      ParsedNodeKind::Group { nodes, inline: _ } => {
        nodes.iter().all(|node| self.is_zero_sized(&node.kind))
      }
      ParsedNodeKind::Choice { .. } => false,
      ParsedNodeKind::Delimited(inner, _) => self.is_zero_sized(inner),
      ParsedNodeKind::Modified(_, _) => false,
      ParsedNodeKind::Todo | ParsedNodeKind::End => true,
    }
  }
}

struct Nodes<'n>(NamedSet<'n, Node<'n>>);

impl<'n> Nodes<'n> {
  fn handle_cycles(self) -> Ast<'n> {
    let cyclic = self
      .0
      .iter()
      .map(|node| self.is_node_cyclic(node))
      .collect::<Vec<_>>();
    Ast {
      nodes: self.0,
      cyclic,
    }
  }

  #[must_use]
  fn is_node_cyclic(&self, node: &Node<'_>) -> bool {
    self.traverse_node(node.ident, &node.kind, &mut Default::default())
  }

  #[must_use]
  fn traverse_node(
    &self,
    ident: Ident<'n>,
    kind: &NodeKind<'n>,
    visited: &mut HashSet<Ident<'n>>,
  ) -> bool {
    let mut delegate_to_child = |child: &Node<'n>| {
      ident == child.ident
        || (visited.insert(child.ident) && self.traverse_node(ident, &child.kind, visited))
    };
    match kind {
      NodeKind::Node(child) => {
        if ident == *child {
          true
        } else {
          let node = self.0.get(*child).unwrap();
          visited.insert(node.ident);
          self.traverse_node(ident, &node.kind, visited)
        }
      }
      NodeKind::StaticToken(_) => false,
      NodeKind::DynamicToken(_) => false,
      NodeKind::Group(Group {
        members: nodes,
        kind: _,
        inline: _,
      })
      | NodeKind::SubGroup(nodes) => nodes.iter().any(delegate_to_child),
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Option {
          primary,
          secondary: _,
        },
        inline: _,
      }) => delegate_to_child(primary),
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Regular(choices),
        inline: _,
      }) => choices.iter().any(delegate_to_child),
      NodeKind::Delimited(inner, _) => self.traverse_node(ident, inner, visited),
      NodeKind::Modified(inner, _) => self.traverse_node(ident, inner, visited),
      NodeKind::Todo => false,
      NodeKind::End => false,
    }
  }
}

#[derive(Clone, Debug, Error)]
pub enum Error {
  #[error(transparent)]
  MissingName(#[from] MissingName),
  #[error("{1}")]
  Context(#[source] Box<Error>, String),
}

impl Error {
  fn context<T: Display>(self, message: T) -> Self {
    Error::Context(Box::new(self), message.to_string())
  }
}

impl<'n> TryFrom<(TaggedNodeKind<'n>, Option<Ident<'n>>)> for Node<'n> {
  type Error = MissingName;

  fn try_from((kind, hint): (TaggedNodeKind<'n>, Option<Ident<'n>>)) -> Result<Self, Self::Error> {
    match hint {
      Some(ident) => Ok(kind.add_name(ident)),
      None => match kind.kind {
        NodeKind::Node(ident) | NodeKind::StaticToken(ident) | NodeKind::DynamicToken(ident) => {
          Ok(kind.add_name(ident))
        }
        NodeKind::Group(ref group) => match &group.kind {
          GroupKind::One(idx) => Ok(Node {
            ident: group.members.get(*idx).unwrap().ident,
            kind: kind.kind,
            tag: kind.tag,
          }),
          GroupKind::Many(_) => Err(MissingName::new("failed to name group", &kind)),
        },
        NodeKind::SubGroup(_) => Err(MissingName::new("failed to name sub-group", &kind)),
        NodeKind::Choice(Choice {
          kind: ChoiceKind::Regular(_),
          inline: _,
        }) => Err(MissingName::new("failed to name choice", &kind)),
        NodeKind::Choice(Choice {
          kind: ChoiceKind::Option {
            ref primary,
            secondary: _,
          },
          inline: _,
        }) => Ok(Node {
          ident: primary.ident,
          kind: kind.kind,
          tag: kind.tag,
        }),
        NodeKind::Delimited(inner, delimiter) => Node::try_from((
          TaggedNodeKind {
            kind: *inner,
            tag: None,
          },
          hint,
        ))
        .map(|node| Node {
          ident: node.ident,
          kind: NodeKind::Delimited(Box::new(node.kind), delimiter),
          tag: kind.tag,
        }),
        NodeKind::Modified(inner, modifier) => Node::try_from((
          TaggedNodeKind {
            kind: *inner,
            tag: None,
          },
          hint.or(kind.tag),
        ))
        .map(|node| Node {
          ident: node.ident,
          kind: NodeKind::Modified(Box::new(node.kind), modifier),
          tag: kind.tag,
        }),
        NodeKind::Todo => Node::try_from((
          TaggedNodeKind {
            kind: NodeKind::Todo,
            tag: kind.tag,
          },
          Some(hint.unwrap_or(Ident("todo"))),
        )),
        NodeKind::End => Node::try_from((
          TaggedNodeKind {
            kind: NodeKind::End,
            tag: kind.tag,
          },
          Some(hint.unwrap_or(Ident("end"))),
        )),
      },
    }
  }
}

#[derive(Clone, Debug, Error)]
#[error("{0}")]
pub struct MissingName(String);

impl MissingName {
  fn new<T: Display>(message: T, node: &TaggedNodeKind<'_>) -> Self {
    Self(format!("{message}: {node}"))
  }
}
