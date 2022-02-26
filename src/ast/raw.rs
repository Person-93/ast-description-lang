use super::{lex::Lexer, Modifier};
use crate::{
  collections::{NamedItem, NamedSet, Unnamed},
  Ident,
};
use std::{
  fmt::{self, Display, Formatter},
  ops::{Deref, DerefMut, Range},
};
use thiserror::Error;

#[derive(Debug)]
pub(super) struct Ast<'a>(NamedSet<'a, Entry<'a>>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) struct Entry<'a> {
  pub ident: Ident<'a>,
  pub node: NodeDef<'a>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(super) struct NodeDef<'a> {
  pub kind: NodeKindDef<'a>,
  pub tag: Option<Ident<'a>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(super) enum NodeKindDef<'a> {
  Simple(Ident<'a>),
  Modified(Box<NodeDef<'a>>, Modifier),
  Delimiter(Box<NodeDef<'a>>, Ident<'a>),
  Group {
    nodes: Vec<NodeDef<'a>>,
    inline: bool,
  },
  Choice {
    first: Box<NodeDef<'a>>,
    second: Box<NodeDef<'a>>,
    inline: bool,
  },
  Todo,
}

impl<'a> DerefMut for Ast<'a> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl<'a> Deref for Ast<'a> {
  type Target = NamedSet<'a, Entry<'a>>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl Ast<'_> {
  pub fn parse(text: &str) -> Result<Ast, Error> {
    let mut ast = Ast(Default::default());
    let mut fragment: Option<Range<_>> = None;

    for range in LineRanges::new(text) {
      let range = match &fragment {
        Some(f) => f.start..range.end,
        None => range,
      };
      let line = &text[range.clone()];
      if line.starts_with('#') || line.is_empty() {
        continue;
      };
      if line.ends_with('\\') {
        fragment = Some(range);
        continue;
      }

      if !ast.insert(Entry::parse(line)?) {
        return Err(Error {
          text: String::from(line),
          index: None,
          kind: ErrorKind::Duplicate,
        });
      }

      fragment = None;
    }

    Ok(ast)
  }
}

impl Entry<'_> {
  fn parse(line: &str) -> Result<Entry, Error> {
    let lexer = Lexer::new(line);
    let node_type = Ident::parse(&lexer)?;

    lexer.skip_whitespace();
    if !lexer.parse_literal("=") {
      return Err(Error::with_full_text(&lexer, ErrorKind::MissingEquals));
    }
    lexer.skip_whitespace();

    let node = NodeDef::parse(&lexer, false)?;

    Ok(Entry {
      ident: node_type,
      node,
    })
  }
}

impl<'e> NamedItem<'e> for Entry<'e> {
  type Name = Ident<'e>;
  type Unnamed = NodeDef<'e>;

  fn name(&self) -> Ident<'e> {
    self.ident
  }

  fn dummy(name: Ident<'e>) -> Self {
    Entry {
      ident: name,
      node: NodeDef {
        kind: NodeKindDef::Simple(Ident("")),
        tag: None,
      },
    }
  }
}

impl<'e> Unnamed<'e> for NodeDef<'e> {
  type Named = Entry<'e>;

  fn add_name(self, name: Ident<'e>) -> Self::Named {
    Entry {
      ident: name,
      node: self,
    }
  }
}

impl NodeDef<'_> {
  fn parse<'p>(lexer: &Lexer<'p>, inline: bool) -> Result<NodeDef<'p>, Error> {
    let mut nodes = Vec::new();
    while matches!(lexer.peek(), Some(c) if !IDENT_TERMINALS.contains(c)) {
      if lexer.parse_literal("|") {
        lexer.skip_whitespace();
        return Ok(NodeDef {
          kind: NodeKindDef::Choice {
            first: Box::new(NodeDef::from_vec(lexer, nodes, inline)?),
            second: Box::new(NodeDef::parse(lexer, inline)?),
            inline,
          },
          tag: parse_tag(lexer)?,
        });
      }

      let mut node = if lexer.parse_literal("(") {
        let node = NodeDef::parse(lexer, true)?;
        if lexer.parse_literal(")") {
          node
        } else {
          return Err(Error::with_full_text(lexer, ErrorKind::MismatchedDelimiter));
        }
      } else if lexer.parse_literal("delim[") {
        let ident = Ident::parse(lexer)?;

        if !lexer.parse_literal("]<") {
          return Err(Error::with_full_text(lexer, ErrorKind::MismatchedDelimiter));
        }

        let node = NodeDef::parse(lexer, false)?;

        if !lexer.parse_literal(">") {
          return Err(Error::with_full_text(lexer, ErrorKind::MismatchedDelimiter));
        }

        NodeDef {
          kind: NodeKindDef::Delimiter(Box::new(node), ident),
          tag: None,
        }
      } else if lexer.parse_literal("!todo") {
        NodeDef {
          kind: NodeKindDef::Todo,
          tag: None,
        }
      } else {
        NodeDef {
          kind: NodeKindDef::Simple(Ident::parse(lexer)?),
          tag: None,
        }
      };

      while let Some(modifier) = Modifier::parse(lexer) {
        if node.tag.is_some() {
          return Err(Error {
            text: format!("{}{modifier}", node.kind),
            index: Some(lexer.index()),
            kind: ErrorKind::TagNotAllowed,
          });
        }
        node = NodeDef {
          kind: NodeKindDef::Modified(Box::new(node), modifier),
          tag: None,
        };
      }

      node.tag = parse_tag(lexer)?;

      nodes.push(node);

      lexer.skip_whitespace();
    }

    NodeDef::from_vec(lexer, nodes, inline)
  }

  fn from_vec<'a>(
    lexer: &Lexer<'a>,
    mut nodes: Vec<NodeDef<'a>>,
    inline: bool,
  ) -> Result<NodeDef<'a>, Error> {
    match nodes.len() {
      0 => Err(Error::new(lexer, ErrorKind::MissingNodeDef)),
      1 => Ok(nodes.pop().unwrap()),
      _ => Ok(NodeDef {
        kind: NodeKindDef::Group { nodes, inline },
        tag: None,
      }),
    }
  }
}

fn parse_tag<'p>(lexer: &Lexer<'p>) -> Result<Option<Ident<'p>>, Error> {
  if lexer.parse_literal(":") {
    let ident = Ident::parse(lexer).map(Some)?;
    if Modifier::parse(lexer).is_some() {
      Err(Error::new(lexer, ErrorKind::TagNotAllowed))
    } else {
      Ok(ident)
    }
  } else {
    Ok(None)
  }
}

impl Modifier {
  fn parse(lexer: &Lexer) -> Option<Modifier> {
    if lexer.parse_literal("*") {
      Some(Modifier::Repeat)
    } else if lexer.parse_literal(",*") {
      Some(Modifier::Csv)
    } else if lexer.parse_literal("+") {
      Some(Modifier::OnePlus)
    } else if lexer.parse_literal(",+") {
      Some(Modifier::CsvOnePlus)
    } else if lexer.parse_literal("?") {
      Some(Modifier::Optional)
    } else if lexer.parse_literal("~") {
      Some(Modifier::Boxed)
    } else {
      None
    }
  }
}

impl Ident<'_> {
  fn parse<'p>(lexer: &Lexer<'p>) -> Result<Ident<'p>, Error> {
    lexer.bump_while(|c| IDENT_CHARS.contains(c));
    if lexer.peek().map_or(true, |c| IDENT_TERMINALS.contains(c)) {
      Ok(Ident(lexer.extract().unwrap()))
    } else {
      Err(Error::new(lexer, ErrorKind::InvalidIdent))
    }
  }
}

const IDENT_TERMINALS: &str = " ,+*?)]>~:";
pub(crate) const IDENT_CHARS: &str = "abcdefghijklmnopqurstuvwxyz_";

struct LineRanges<'a> {
  text: &'a str,
  index: usize,
}

impl LineRanges<'_> {
  fn new(text: &str) -> LineRanges {
    LineRanges { text, index: 0 }
  }
}

impl Iterator for LineRanges<'_> {
  type Item = Range<usize>;

  fn next(&mut self) -> Option<Self::Item> {
    (self.text.len() > self.index).then(|| {
      let range = match self.text[self.index..self.text.len()].find('\n') {
        Some(i) => {
          let r = self.index..self.index + i;
          self.index += i + 1;
          r
        }
        None => {
          let r = self.index..self.text.len();
          self.index = self.text.len();
          r
        }
      };
      if self.text[range.clone()].ends_with('\r') {
        range.start..range.end - 1
      } else {
        range
      }
    })
  }
}

impl Display for NodeDef<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.kind)?;
    if let Some(tag) = self.tag {
      write!(f, ":{tag}")?;
    }
    Ok(())
  }
}

impl Display for NodeKindDef<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      NodeKindDef::Simple(ident) => write!(f, "{ident}"),
      NodeKindDef::Modified(inner, modifier) => write!(f, "{inner}{modifier}"),
      NodeKindDef::Delimiter(inner, delimiter) => write!(f, "delim[{delimiter}<{inner}>"),
      NodeKindDef::Group { nodes, inline } => {
        if *inline {
          write!(f, "(")?;
        }

        let mut nodes = nodes.iter();
        let first = nodes.next().unwrap();
        write!(f, "{first}")?;
        for node in nodes {
          write!(f, " {node}")?;
        }

        if *inline {
          write!(f, ")")?;
        }
        Ok(())
      }
      NodeKindDef::Choice {
        first,
        second,
        inline,
      } => {
        if *inline {
          write!(f, "(")?;
        }

        write!(f, "{first}")?;
        write!(f, " | {second}")?;

        if *inline {
          write!(f, ")")?;
        }
        Ok(())
      }
      NodeKindDef::Todo => write!(f, "!todo"),
    }
  }
}

#[derive(Clone, Debug, Error)]
pub struct Error {
  text: String,
  index: Option<usize>,
  kind: ErrorKind,
}

#[derive(Copy, Clone, Debug)]
enum ErrorKind {
  InvalidIdent,
  MismatchedDelimiter,
  MissingEquals,
  MissingNodeDef,
  Duplicate,
  TagNotAllowed,
}

impl Error {
  fn new(lexer: &Lexer<'_>, kind: ErrorKind) -> Self {
    lexer.bump_while(|c| c != ' ');
    let (text, index) = match lexer.extract() {
      Some(text) => (text, Some(lexer.index())),
      None => (lexer.text(), None),
    };
    let text = String::from(text);
    Self { text, index, kind }
  }

  fn with_full_text(lexer: &Lexer<'_>, kind: ErrorKind) -> Self {
    Self {
      text: String::from(lexer.text()),
      index: None,
      kind,
    }
  }
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}: ", self.kind)?;
    if let Some(index) = self.index {
      write!(f, "{index}: ")?;
    }
    write!(f, "{}", self.text)
  }
}

impl Display for ErrorKind {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      ErrorKind::InvalidIdent => write!(f, "invalid identifier"),
      ErrorKind::MismatchedDelimiter => write!(f, "mismatched delimiter"),
      ErrorKind::MissingEquals => write!(f, "expected '='"),
      ErrorKind::MissingNodeDef => write!(f, "expected node definition"),
      ErrorKind::Duplicate => write!(f, "duplicate key"),
      ErrorKind::TagNotAllowed => write!(f, "tags should be after modifiers"),
    }
  }
}

#[cfg(test)]
#[test]
fn snapshots() {
  use insta::{assert_debug_snapshot, with_settings};
  use std::{fs, path::Path};

  for name in crate::SNAPSHOT_CASES {
    let mut path = Path::new(env!("CARGO_MANIFEST_DIR"))
      .join("examples")
      .join(name);
    path.set_extension("ast");
    let text = fs::read_to_string(&path).unwrap();
    with_settings!({input_file => Some(path)}, {
      assert_debug_snapshot!(Ast::parse(&text));
    });
  }
}
