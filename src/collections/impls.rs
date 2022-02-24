use super::{NamedItem, NamedSet, Unnamed, Wrapper};
use serde::{
  de::{self, Error, MapAccess},
  Deserialize, Deserializer,
};
use std::{
  any,
  cmp::Ordering,
  fmt::{self, Formatter},
  hash::{Hash, Hasher},
  marker::PhantomData,
  ops::Index,
};

impl<'ni, T: NamedItem<'ni>> FromIterator<T> for NamedSet<'ni, T> {
  fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
    NamedSet(
      iter
        .into_iter()
        .map(|item| Wrapper(item, PhantomData))
        .collect(),
    )
  }
}

impl<'ni, T: NamedItem<'ni>> FromIterator<(T::Name, T::Unnamed)> for NamedSet<'ni, T> {
  fn from_iter<I: IntoIterator<Item = (T::Name, T::Unnamed)>>(iter: I) -> Self {
    NamedSet::from_iter(
      iter
        .into_iter()
        .map(|(name, unnamed)| unnamed.add_name(name)),
    )
  }
}

impl<'ni, T: NamedItem<'ni>> Default for NamedSet<'ni, T> {
  fn default() -> Self {
    NamedSet(Default::default())
  }
}

impl<'ni, T: NamedItem<'ni>> Index<usize> for NamedSet<'ni, T> {
  type Output = T;

  fn index(&self, index: usize) -> &Self::Output {
    &self.0[index].0
  }
}

impl<'de, 'ni, T> Deserialize<'de> for NamedSet<'ni, T>
where
  'de: 'ni,
  T: NamedItem<'ni>,
  <T as NamedItem<'ni>>::Name: Deserialize<'de>,
  <T as NamedItem<'ni>>::Unnamed: Deserialize<'de>,
{
  fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
    return deserializer.deserialize_map(Visitor(PhantomData, PhantomData));

    struct Visitor<'de, 'ni, T>(PhantomData<&'ni &'de ()>, PhantomData<T>)
    where
      'de: 'ni,
      T: NamedItem<'ni>,
      <T as NamedItem<'ni>>::Name: Deserialize<'de>,
      <T as NamedItem<'ni>>::Unnamed: Deserialize<'de>;

    impl<'de, 'ni, T> de::Visitor<'de> for Visitor<'de, 'ni, T>
    where
      'de: 'ni,
      T: NamedItem<'ni>,
      <T as NamedItem<'ni>>::Name: Deserialize<'de>,
      <T as NamedItem<'ni>>::Unnamed: Deserialize<'de>,
    {
      type Value = NamedSet<'ni, T>;

      fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "a collection of type `{}`", any::type_name::<T>())
      }

      fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
        let mut named_set = Self::Value::default();

        while let Some((name, unnamed)) = map.next_entry()? {
          let unnamed: T::Unnamed = unnamed;
          if !named_set.insert(unnamed.add_name(name)) {
            return Err(A::Error::custom(format!(
              "duplicate entry `{name}` in collection of type `{}`",
              any::type_name::<T>()
            )));
          }
        }

        Ok(named_set)
      }
    }
  }
}

impl<'ni, T: NamedItem<'ni>> Hash for Wrapper<'ni, T> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.0.name().hash(state)
  }
}

impl<'ni, T: NamedItem<'ni>> Ord for Wrapper<'ni, T> {
  fn cmp(&self, other: &Self) -> Ordering {
    self.0.name().cmp(&other.0.name())
  }
}

impl<'ni, T: NamedItem<'ni>> PartialOrd for Wrapper<'ni, T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<'ni, T: NamedItem<'ni>> Eq for Wrapper<'ni, T> {}

impl<'ni, T: NamedItem<'ni>> PartialEq for Wrapper<'ni, T> {
  fn eq(&self, other: &Self) -> bool {
    self.cmp(other) == Ordering::Equal
  }
}
