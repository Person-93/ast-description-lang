use std::cell::Cell;

#[derive(Clone)]
pub struct Lexer<'l> {
  text: &'l str,
  start: Cell<usize>,
  index: Cell<usize>,
}

impl<'l> Lexer<'l> {
  pub fn new(text: &str) -> Lexer {
    Lexer {
      text,
      start: Cell::new(0),
      index: Cell::new(0),
    }
  }

  pub fn text(&self) -> &str {
    self.text
  }

  pub fn index(&self) -> usize {
    self.index.get()
  }

  pub fn extract(&self) -> Option<&'l str> {
    let val = (self.start < self.index).then(|| &self.text[self.start.get()..self.index.get()]);
    self.start.set(self.index.get());
    val
  }

  pub fn peek(&self) -> Option<char> {
    (self.index.get() < self.text.len()).then(|| self.text.as_bytes()[self.index.get()] as char)
  }

  pub fn skip_whitespace(&self) {
    self.bump_while(|c| c == ' ' || c == '\\' || c == '\n');
    self.start.set(self.index.get());
  }

  #[must_use]
  pub fn parse_literal(&self, value: &str) -> bool {
    for i in 0..value.len() {
      if self
        .peek()
        .map_or(false, |c| value.as_bytes()[i] as char == c)
      {
        self.increment();
      } else {
        return false;
      }
    }
    self.start.set(self.index.get());
    true
  }

  pub fn bump_while(&self, f: fn(char) -> bool) {
    while self.peek().map_or(false, f) {
      self.increment()
    }
  }

  fn increment(&self) {
    self.index.set(self.index.get() + 1);
  }
}
