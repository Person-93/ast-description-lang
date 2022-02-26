use ast_description_lang::{Config, Specs};
use std::fs;

fn main() {
  println!("cargo:rerun-if-changed=example.toml");
  println!("cargo:rerun-if-changed=example.ast");

  let specs = fs::read_to_string("example.toml").unwrap();
  let specs: Specs = toml::de::from_str(&specs).unwrap();

  let tokens = specs.generate_tokens_mod().unwrap();
  fs::write("src/tokens/generated.rs", tokens.to_string()).unwrap();

  let config = Config::default();

  let ast = fs::read_to_string("example.ast").unwrap();
  let ast = ast_description_lang::generate_ast_mod(&ast, &specs, config).unwrap();
  fs::write("src/ast/generated.rs", ast.to_string()).unwrap();
}
