use indexmap::IndexSet;
use std::fmt::Display;
use std::hash::Hash;
use std::marker::PhantomData;

mod impls;

#[derive(Debug, Clone)]
pub struct NamedSet<'ni, T: NamedItem<'ni>>(IndexSet<Wrapper<'ni, T>>);

#[derive(Debug, Clone)]
struct Wrapper<'ni, T: NamedItem<'ni>>(T, PhantomData<&'ni ()>);

pub trait NamedItem<'ni> {
  type Name: Hash + Ord + Copy + Display;
  type Unnamed: Unnamed<'ni, Named = Self>;

  fn name(&self) -> Self::Name;
  fn dummy(name: Self::Name) -> Self;
}

pub trait Unnamed<'ni> {
  type Named: NamedItem<'ni>;

  fn add_name(self, name: <Self::Named as NamedItem<'ni>>::Name) -> Self::Named;
}

impl<'ni, T: NamedItem<'ni>> NamedSet<'ni, T> {
  pub fn insert(&mut self, item: T) -> bool {
    self.0.insert(Wrapper::new(item))
  }

  pub fn get(&self, name: T::Name) -> Option<&T> {
    self
      .0
      .get(&Wrapper::new(T::dummy(name)))
      .map(|wrapper| &wrapper.0)
  }

  pub fn get_index(&self, index: usize) -> Option<&T> {
    self.0.get_index(index).map(|wrapper| &wrapper.0)
  }

  pub fn index_of(&self, name: T::Name) -> Option<usize> {
    self.0.get_index_of(&Wrapper::new(T::dummy(name)))
  }

  pub fn contains(&self, name: T::Name) -> bool {
    self.0.contains(&Wrapper(T::dummy(name), PhantomData))
  }

  pub fn iter(&'ni self) -> impl Iterator<Item = &'ni T> + 'ni {
    self.0.iter().map(|wrapper| &wrapper.0)
  }
}

impl<'ni, T: NamedItem<'ni>> Wrapper<'ni, T> {
  fn new(item: T) -> Self {
    Self(item, PhantomData)
  }
}
