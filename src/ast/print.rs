use super::{
  collapsed::{Ast, Choice, ChoiceKind, Group, GroupKind, Node, NodeKind},
  Modifier,
};
use crate::{collections::NamedItem, is_rust_keyword, Ident, Specs};
use heck::ToPascalCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};

impl Ast<'_> {
  pub fn print(
    &self,
    specs: &Specs<'_>,
    error: TokenStream,
    tokens_mod: TokenStream,
    span: Option<TokenStream>,
  ) -> TokenStream {
    let span = span.map(|span| quote! { type Span = #span; });
    let member_span = span.as_ref().map(|_| quote! { pub span: Span });

    let nodes = self.iter().filter_map(|node| match &node.kind {
      NodeKind::Node(_)
      | NodeKind::StaticToken(_)
      | NodeKind::DynamicToken(_)
      | NodeKind::Choice(Choice {
        kind: ChoiceKind::Option { .. },
        inline: _,
      }) => None,
      NodeKind::Group(Group {
        members,
        kind,
        inline: _,
      }) => match kind {
        GroupKind::One(_) => None,
        GroupKind::Many(indices) => {
          let mut members = indices
            .iter()
            .copied()
            .map(|idx| &members[idx])
            .filter(|node| !self.is_node_kind_zero_sized(&node.kind))
            .map(|node| {
              let ident = node.tag.unwrap_or(node.ident);
              let ty = self.print_as_type(&node.kind, Some(node.ident));
              quote! { pub #ident: #ty }
            })
            .peekable();

          if members.peek().is_none() {
            None
          } else {
            let ident = node.ident.as_type();
            Some(quote! {
              #[derive(Clone, Debug)]
              pub struct #ident { #(#members),*, #member_span }
            })
          }
        }
      },
      NodeKind::SubGroup(_) => unreachable!("top level sub-group"),
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Regular(choices),
        inline: _,
      }) => {
        let variants = choices.iter().map(|choice| {
          let name = choice
            .tag
            .unwrap_or(choice.ident)
            .to_string()
            .to_pascal_case();
          let name = Ident(&name);
          let body = if self.is_node_kind_zero_sized(&choice.kind) {
            quote! {}
          } else {
            let ty = self.print_as_type(&choice.kind, Some(choice.ident));
            quote! { (#ty) }
          };
          quote! { #name #body }
        });

        let ident = format_ident!("{}", node.ident.0.to_pascal_case());
        Some(quote! {
          #[derive(Clone, Debug)]
          pub enum #ident { #(#variants),* }
        })
      }
      NodeKind::Delimited(inner, _delimiter) => match &**inner {
        NodeKind::Node(_) | NodeKind::StaticToken(_) | NodeKind::DynamicToken(_) => None,
        NodeKind::Group(Group {
          members,
          kind,
          inline: _,
        }) => match kind {
          GroupKind::One(_) => None,
          GroupKind::Many(indices) => {
            let members = indices
              .iter()
              .copied()
              .map(|idx| &members[idx])
              .filter(|node| !self.is_node_kind_zero_sized(&node.kind))
              .map(|node| {
                let ident = node.tag.unwrap_or(node.ident);
                let ty = self.print_as_type(&node.kind, Some(node.ident));
                quote! { pub #ident: #ty }
              });

            let ident = node.ident.as_type();
            Some(quote! {
              #[derive(Clone, Debug)]
              pub struct #ident { #(#members),*, #member_span }
            })
          }
        },
        NodeKind::SubGroup(_)
        | NodeKind::Choice(_)
        | NodeKind::Delimited(_, _)
        | NodeKind::Modified(_, _)
        | NodeKind::Todo => None,
        NodeKind::End => unreachable!(),
      },
      NodeKind::Modified(..) => None,
      NodeKind::Todo => None,
      NodeKind::End => None,
    });

    let parsers = self
      .nodes
      .iter()
      .map(|node| self.print_parser(node, specs, span.is_some()));

    let recursive_parsers = self.print_recursive_parsers(specs, span.is_some());

    quote! {
      #![allow(dead_code)]

      use #tokens_mod::*;
      #(#nodes)*

      #span

      pub mod parse {
        use super::*;
        use chumsky::prelude::*;
        use #tokens_mod::{parse::*, Token};

        type Error = #error;

        #(#parsers)*
        #recursive_parsers
      }
    }
  }

  fn print_as_type(&self, kind: &NodeKind<'_>, hint: Option<Ident<'_>>) -> TokenStream {
    match kind {
      NodeKind::Node(child) => match self.get(*child) {
        Some(child) => self.print_as_type(&child.kind, Some(child.ident)),
        None => {
          let message = format!("missing node `{child}`");
          quote! { compile_error!(#message) }
        }
      },
      NodeKind::StaticToken(_) => unreachable!("tried to print type of static token"),
      NodeKind::DynamicToken(ident) => ident.as_type().to_token_stream(),
      NodeKind::Group(Group {
        members,
        kind,
        inline: _,
      }) => match kind {
        GroupKind::One(idx) => {
          let node = &members[*idx];
          self.print_as_type(&node.kind, Some(node.ident))
        }
        GroupKind::Many(_) => hint.unwrap().as_type().to_token_stream(),
      },
      NodeKind::SubGroup(_) => quote! { () },
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Regular(_),
        inline: _,
      }) => hint.unwrap().as_type().to_token_stream(),
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Option {
          primary,
          secondary: _,
        },
        inline: _,
      }) => {
        let ty = self.print_as_type(&primary.kind, Some(primary.ident));
        quote! { Option<#ty> }
      }
      NodeKind::Delimited(inner, _) => self.print_as_type(inner, hint),
      NodeKind::Modified(inner, modifier) => self.print_modified_type(&**inner, None, *modifier),
      NodeKind::Todo | NodeKind::End => quote! { () },
    }
  }

  fn print_modified_type(
    &self,
    kind: &NodeKind<'_>,
    hint: Option<Ident<'_>>,
    modifier: Modifier,
  ) -> TokenStream {
    match modifier {
      Modifier::Repeat | Modifier::Csv | Modifier::OnePlus | Modifier::CsvOnePlus => {
        if self.is_node_kind_zero_sized(kind) {
          quote! { usize }
        } else {
          let ty = self.print_as_type(kind, hint);
          quote! { Vec<#ty> }
        }
      }
      Modifier::Optional => {
        if self.is_node_kind_zero_sized(kind) {
          quote! { bool }
        } else {
          let ty = self.print_as_type(kind, None);
          quote! { Option<#ty> }
        }
      }
      Modifier::Boxed => {
        let ty = self.print_as_type(kind, hint);
        quote! { Box<#ty> }
      }
    }
  }

  fn print_parser(&self, node: &Node<'_>, specs: &Specs<'_>, include_span: bool) -> TokenStream {
    let ident = node.ident;
    let ty = if self.is_node_kind_zero_sized(&node.kind) {
      quote! { () }
    } else {
      self.print_as_type(&node.kind, Some(node.ident))
    };
    let body = if self.is_node_cyclic(node) {
      let idx = self.index_of(node.ident).unwrap();
      let idx = self.cyclic.binary_search(&idx).unwrap();
      let idx = proc_macro2::Literal::usize_unsuffixed(idx);
      quote! { RECURSIVE.with(|parsers| parsers.borrow().#idx.clone()) }
    } else {
      self.print_parser_body(&node.kind, Some(node.ident), specs, include_span)
    };

    quote! {
      pub fn #ident() -> impl Parser<Token, #ty, Error = Error> { #body }
    }
  }

  fn print_parser_body(
    &self,
    node_kind: &NodeKind<'_>,
    hint: Option<Ident<'_>>,
    specs: &Specs<'_>,
    include_span: bool,
  ) -> TokenStream {
    match node_kind {
      NodeKind::Node(child) => quote! { #child() },
      NodeKind::StaticToken(ident) | NodeKind::DynamicToken(ident) => quote! { #ident() },
      NodeKind::Group(group) => self.print_group_parser(group, hint, specs, include_span),
      NodeKind::SubGroup(members) => {
        let mut members = members.iter();
        let first = self.print_sub_parser(members.next().unwrap(), specs, false, include_span);
        let members: Vec<_> = members
          .map(|node| {
            let parser = self.print_sub_parser(node, specs, false, include_span);
            quote! { .then(#parser) }
          })
          .collect();

        if members.is_empty() {
          first
        } else {
          quote! { #first #(#members)*.ignored() }
        }
      }
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Regular(choices),
        inline: _,
      }) => {
        let ty = self.print_as_type(node_kind, hint);
        let choices = choices.iter().enumerate().map(|(idx, choice)| {
          let variant = choice.tag.unwrap_or(choice.ident).as_type();

          let parser = self.print_sub_parser(choice, specs, false, include_span);

          let func =
            self.print_func_for_choice_mapping(&choice.kind, ty.clone(), variant.to_token_stream());

          let choice = quote! { #parser.map(#func) };
          if idx == 0 {
            choice
          } else {
            quote! { .or(#choice) }
          }
        });
        quote! { #(#choices)* }
      }
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Option { primary, secondary },
        inline: _,
      }) => {
        let primary = self.print_sub_parser(primary, specs, false, include_span);
        quote! { #primary.map(Some).or(#secondary().map(|_| None)) }
      }
      NodeKind::Delimited(inner, delimiter) => self.delimit_parser(
        self.print_parser_body(inner, hint, specs, include_span),
        *delimiter,
        specs,
      ),
      NodeKind::Modified(inner, modifier) => {
        self.print_modified_parser(inner, *modifier, specs, false, include_span)
      }
      NodeKind::Todo => print_todo(),
      NodeKind::End => quote! { end() },
    }
  }

  fn print_sub_parser(
    &self,
    node: &Node<'_>,
    specs: &Specs<'_>,
    inline: bool,
    include_span: bool,
  ) -> TokenStream {
    if inline {
      self.print_parser_body(&node.kind, None, specs, include_span)
    } else {
      let ident = node.ident;
      let parser = quote! { #ident() };
      match &node.kind {
        NodeKind::Node(child) => quote! { #child() },
        NodeKind::StaticToken(_) | NodeKind::DynamicToken(_) => parser,
        NodeKind::Group(group @ Group { inline: true, .. }) => {
          self.print_group_parser(group, None, specs, include_span)
        }
        NodeKind::Group(Group { inline: false, .. }) => parser,
        NodeKind::SubGroup(_) => self.print_parser_body(&node.kind, None, specs, include_span),
        NodeKind::Choice(Choice { inline: true, .. }) => {
          self.print_parser_body(&node.kind, None, specs, include_span)
        }
        NodeKind::Choice(Choice { inline: false, .. }) => parser,
        NodeKind::Delimited(inner, delimiter) => {
          let parser = match &**inner {
            NodeKind::Node(child) => quote! { #child() },
            NodeKind::Modified(inner, modifier) => {
              self.print_modified_parser(inner, *modifier, specs, false, include_span)
            }
            _ => parser,
          };
          self.delimit_parser(parser, *delimiter, specs)
        }
        NodeKind::Modified(inner, modifier) => {
          self.print_modified_parser(inner, *modifier, specs, false, include_span)
        }
        NodeKind::Todo => print_todo(),
        NodeKind::End => parser,
      }
    }
  }

  fn print_group_parser(
    &self,
    Group {
      members,
      kind,
      inline,
    }: &Group<'_>,
    hint: Option<Ident<'_>>,
    specs: &Specs<'_>,
    include_span: bool,
  ) -> TokenStream {
    match kind {
      GroupKind::One(idx) => {
        let binding = print_tuple_binding(members.len());
        let members = self.print_group_members(members, specs, *inline, include_span);
        let idx = ident_from_idx(*idx);
        quote! { #members.map(|#binding| #idx ) }
      }
      GroupKind::Many(_) => {
        let ty = hint.unwrap().as_type().to_token_stream();
        let member_init = self.print_group_members_init(members, include_span);
        let binding = print_tuple_binding(members.len());
        let binding = if include_span {
          quote! { #binding, span }
        } else {
          binding
        };
        let members = self.print_group_members(members, specs, *inline, include_span);
        let map_fn = if include_span {
          quote! { map_with_span }
        } else {
          quote! { map }
        };
        quote! { #members.#map_fn(|#binding| #ty { #member_init }) }
      }
    }
  }

  fn print_group_members_init(&self, members: &[Node<'_>], include_span: bool) -> TokenStream {
    let members = members
      .iter()
      .enumerate()
      .filter(|(_, member)| !self.is_node_kind_zero_sized(&member.kind))
      .map(|(idx, member)| {
        let ident = member.tag.unwrap_or(member.ident);
        let idx = ident_from_idx(idx);
        quote! { #ident: #idx }
      });
    let span = include_span.then(|| quote! { span });
    quote! { #(#members),*, #span }
  }

  fn print_group_members(
    &self,
    members: &[Node<'_>],
    specs: &Specs<'_>,
    inline: bool,
    include_span: bool,
  ) -> TokenStream {
    let members = members.iter().enumerate().map(|(current_idx, node)| {
      let parser = self.print_sub_parser(node, specs, inline, include_span);
      if current_idx == 0 {
        parser
      } else {
        quote! { .then(#parser) }
      }
    });
    quote! { #(#members)* }
  }

  fn is_node_cyclic(&self, node: &Node<'_>) -> bool {
    match node.kind {
      NodeKind::Node(_) | NodeKind::Group(_) | NodeKind::Choice(_) => {
        let idx = self.nodes.index_of(node.name()).unwrap();
        self.cyclic.binary_search(&idx).is_ok()
      }
      NodeKind::StaticToken(_)
      | NodeKind::DynamicToken(_)
      | NodeKind::Delimited(_, _)
      | NodeKind::SubGroup(_)
      | NodeKind::Modified(_, _)
      | NodeKind::Todo
      | NodeKind::End => false,
    }
  }

  fn delimit_parser(
    &self,
    parser: TokenStream,
    delimiter: Ident<'_>,
    specs: &Specs<'_>,
  ) -> TokenStream {
    match specs.delimiters.get(&delimiter) {
      Some(delimiter) => {
        let open = format_ident!("{}", delimiter.open);
        let close = format_ident!("{}", delimiter.close);
        quote! { #parser.delimited_by(#open(), #close()) }
      }
      None => {
        let message = format!("missing delimiter: {delimiter}");
        quote! { compile_error!(#message) }
      }
    }
  }

  fn print_recursive_parsers(&self, specs: &Specs<'_>, include_span: bool) -> TokenStream {
    let nodes: Vec<_> = self
      .cyclic
      .iter()
      .map(|idx| self.nodes.get_index(*idx).unwrap())
      .collect();

    let idents = nodes.iter().map(|node| node.ident.to_token_stream());
    let idents = quote! { (#(#idents),*) };

    let parser_types = nodes.iter().map(|node| {
      let ty = self.print_as_type(&node.kind, Some(node.ident));
      quote! { Recursive<'static, Token, #ty, Error> }
    });

    let decls = nodes.iter().map(|node| {
      let ident = node.ident;
      quote! {
        #[allow(unused_assignments)]
        let mut #ident = recursive(|_| todo());
      }
    });

    let body = nodes.iter().fold(TokenStream::new(), |accum, node| {
      let ident = node.ident;
      let parser =
        self.print_recursive_sub_parser(&node.kind, Some(node.ident), specs, include_span);
      quote! {
        #ident = recursive(|#[allow(unused_variables)] #ident| {
          #accum
          #parser
        });
      }
    });

    quote! {
      thread_local! {
        static RECURSIVE: std::cell::RefCell<RecursiveParsers> =
          std::cell::RefCell::new(recursive_parsers());
      }
      type RecursiveParsers = (#(#parser_types),*);
      fn recursive_parsers() -> RecursiveParsers {
        #(#decls)*
        #body
        #idents
      }
    }
  }

  fn print_recursive_sub_parser(
    &self,
    node_kind: &NodeKind<'_>,
    hint: Option<Ident<'_>>,
    specs: &Specs<'_>,
    include_span: bool,
  ) -> TokenStream {
    let do_recursion = |node: &Node<'_>, hint: Option<Ident<'_>>| {
      let ident = node.ident;
      if self.is_node_cyclic(node) {
        quote! { #ident.clone() }
      } else {
        self.print_recursive_sub_parser(
          &node.kind,
          Some(hint.unwrap_or(ident)),
          specs,
          include_span,
        )
      }
    };

    match node_kind {
      NodeKind::Node(child) => do_recursion(self.get(*child).unwrap(), None),
      NodeKind::StaticToken(ident) | NodeKind::DynamicToken(ident) => quote! { #ident() },
      NodeKind::Group(Group {
        members,
        kind,
        inline: _,
      }) => match kind {
        GroupKind::One(idx) => {
          let binding = print_tuple_binding(members.len());

          let members = members.iter().enumerate().map(|(current_idx, node)| {
            let parser = if self.is_node_cyclic(node) {
              let ident = node.ident;
              quote! { #ident.clone() }
            } else {
              self.print_recursive_sub_parser(&node.kind, Some(node.ident), specs, include_span)
            };
            if current_idx == 0 {
              parser
            } else {
              quote! { .then(#parser) }
            }
          });

          let idx = ident_from_idx(*idx);

          quote! { #(#members)*.map(|#binding| #idx ) }
        }
        GroupKind::Many(_) => {
          let ty = self.print_as_type(node_kind, hint);
          let member_init = self.print_group_members_init(members, include_span);
          let binding = print_tuple_binding(members.len());
          let map_fn = if include_span {
            quote! { map_with_span }
          } else {
            quote! { map }
          };
          let binding = if include_span {
            quote! { #binding, span }
          } else {
            binding
          };

          let members = members.iter().enumerate().map(|(current_idx, node)| {
            let parser = do_recursion(node, None);
            if current_idx == 0 {
              parser
            } else {
              quote! { .then(#parser) }
            }
          });

          quote! { #(#members)*.#map_fn(|#binding| #ty { #member_init }) }
        }
      },
      NodeKind::SubGroup(_) => self.print_parser_body(node_kind, None, specs, include_span),
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Regular(choices),
        inline: _,
      }) => {
        let ty = self.print_as_type(node_kind, hint);
        let choices = choices.iter().enumerate().map(|(idx, node)| {
          let variant = node.tag.unwrap_or(node.ident).as_type();
          let parser = do_recursion(node, None);
          let func =
            self.print_func_for_choice_mapping(&node.kind, ty.clone(), variant.to_token_stream());
          let choice = quote! { #parser.map(#func) };
          if idx == 0 {
            choice
          } else {
            quote! { .or(#choice) }
          }
        });
        quote! { #(#choices)* }
      }
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Option { primary, secondary },
        inline: _,
      }) => {
        let primary = if self.is_node_cyclic(primary) {
          let ident = primary.ident;
          quote! { #ident.clone() }
        } else {
          match &primary.kind {
            NodeKind::Group(_) => self.print_recursive_sub_parser(
              &primary.kind,
              Some(primary.ident),
              specs,
              include_span,
            ),
            _ => {
              let ident = primary.ident;
              quote! { #ident() }
            }
          }
        };
        quote! { #primary.map(Some).or(#secondary().map(|_| None)) }
      }
      NodeKind::Delimited(inner, delimiter) => self.delimit_parser(
        self.print_recursive_sub_parser(inner, hint, specs, include_span),
        *delimiter,
        specs,
      ),
      NodeKind::Modified(inner, modifier) => {
        self.print_modified_parser(inner, *modifier, specs, true, include_span)
      }
      NodeKind::Todo => print_todo(),
      NodeKind::End => quote! { end() },
    }
  }

  fn print_func_for_choice_mapping(
    &self,
    kind: &NodeKind<'_>,
    ty: TokenStream,
    variant: TokenStream,
  ) -> TokenStream {
    match kind {
      NodeKind::Node(child) => {
        self.print_func_for_choice_mapping(&self.get(*child).unwrap().kind, ty, variant)
      }
      NodeKind::StaticToken(_) => quote! { |_| #ty::#variant },
      NodeKind::DynamicToken(_) => quote! { #ty::#variant },
      NodeKind::Group(Group {
        members: _,
        kind,
        inline: _,
      }) => match kind {
        GroupKind::One(_) => quote! { #ty::#variant },
        GroupKind::Many(_) => quote! { #ty::#variant },
      },
      NodeKind::SubGroup(_) => quote! { |_| #ty::#variant },
      NodeKind::Choice(_) => quote! { #ty::#variant },
      NodeKind::Delimited(_, _) => quote! { #ty::#variant },
      NodeKind::Modified(_, _) => quote! { #ty::#variant },
      NodeKind::Todo => quote! { |_| #ty::#variant },
      NodeKind::End => quote! { |_| #ty::#variant },
    }
  }

  fn print_modified_parser(
    &self,
    inner: &NodeKind<'_>,
    modifier: Modifier,
    specs: &Specs<'_>,
    recursive: bool,
    include_span: bool,
  ) -> TokenStream {
    if self.is_node_kind_zero_sized(inner) {
      let inner = self.print_parser_body(inner, None, specs, include_span);

      match modifier {
        Modifier::Repeat => quote! { #inner.repeated().map(|items| items.len()) },
        Modifier::Csv => quote! { #inner.separated_by(comma()).map(|items| items.len()) },
        Modifier::OnePlus => quote! { #inner.repeated().at_least(1).map(|items| items.len()) },
        Modifier::CsvOnePlus => {
          quote! { #inner.separated_by(comma()).at_least(1).map(|items| items.len()) }
        }
        Modifier::Optional => quote! { #inner.or_not().map(|opt| opt.is_some()) },
        Modifier::Boxed => unreachable!("empty box"),
      }
    } else {
      let inner = if recursive {
        self.print_recursive_sub_parser(inner, None, specs, include_span)
      } else {
        self.print_parser_body(inner, None, specs, include_span)
      };

      match modifier {
        Modifier::Repeat => quote! { #inner.repeated() },
        Modifier::Csv => quote! { #inner.separated_by(comma()) },
        Modifier::OnePlus => quote! { #inner.repeated().at_least(1) },
        Modifier::CsvOnePlus => {
          quote! { #inner.separated_by(comma()).at_least(1) }
        }
        Modifier::Optional => quote! { #inner.or_not() },
        Modifier::Boxed => quote! { #inner.map(Box::new) },
      }
    }
  }

  #[must_use]
  fn is_node_kind_zero_sized(&self, node_kind: &NodeKind<'_>) -> bool {
    match node_kind {
      NodeKind::Node(child) => self.is_node_kind_zero_sized(&self.get(*child).unwrap().kind),
      NodeKind::StaticToken(_) => true,
      NodeKind::DynamicToken(_) => false,
      NodeKind::Group(_) => false,
      NodeKind::SubGroup(_) => true,
      NodeKind::Choice(_) => false,
      NodeKind::Delimited(inner, _) => self.is_node_kind_zero_sized(inner),
      NodeKind::Modified(_, _) => false,
      NodeKind::Todo => true,
      NodeKind::End => true,
    }
  }
}

impl ToTokens for Ident<'_> {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    let ident = if is_rust_keyword(self.0) {
      format_ident!("{}_", self.0)
    } else {
      format_ident!("{}", self.0)
    };
    tokens.extend(quote! { #ident })
  }
}

fn print_todo() -> TokenStream {
  quote! { todo::<Token, (), Error>() }
}

fn print_tuple_binding(len: usize) -> TokenStream {
  (1..len).fold(ident_from_idx(0).to_token_stream(), |accum, idx| {
    let ident = ident_from_idx(idx);
    quote! { (#accum, #ident) }
  })
}

fn ident_from_idx(idx: usize) -> proc_macro2::Ident {
  format_ident!("_{idx}")
}

#[cfg(test)]
#[test]
fn snapshots() {
  use super::raw::Ast;
  use crate::Config;
  use insta::{assert_snapshot, with_settings};
  use std::{
    fs,
    io::Write,
    path::Path,
    process::{Command, Stdio},
    str::FromStr,
  };

  for name in crate::SNAPSHOT_CASES {
    let mut path = Path::new(env!("CARGO_MANIFEST_DIR"))
      .join("examples")
      .join(name)
      .join("example");

    path.set_extension("toml");
    let specs = fs::read_to_string(&path).unwrap();
    let specs = toml::de::from_str(&specs).unwrap();

    path.set_extension("ast");
    let text = fs::read_to_string(&path).unwrap();

    let config = Config::default();

    let printed = Ast::parse(&text)
      .unwrap()
      .transform(&specs)
      .unwrap()
      .transform()
      .unwrap()
      .print(
        &specs,
        TokenStream::from_str(config.error).unwrap(),
        TokenStream::from_str(config.tokens_mod).unwrap(),
        None,
      )
      .to_string();
    let rustfmt = Command::new("rustfmt")
      .stdin(Stdio::piped())
      .stdout(Stdio::piped())
      .spawn()
      .expect("starting rustfmt process");
    rustfmt
      .stdin
      .as_ref()
      .unwrap()
      .write_all(printed.as_bytes())
      .unwrap();
    let output = rustfmt
      .wait_with_output()
      .expect("collect output from rustfmt");
    let status = output.status;
    if !status.success() {
      let message = std::str::from_utf8(&output.stderr).unwrap();
      panic!("rustfmt failed: {status}\n{message}");
    }
    let printed = std::str::from_utf8(&output.stdout).unwrap();

    with_settings!({input_file => Some(path)}, {
      assert_snapshot!(printed);
    });
  }
}
