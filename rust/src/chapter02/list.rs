use std::sync::Arc;

pub trait List<T> {}

pub trait ListView<T>: Sized {
    type Owned: List<T>;
    fn append(&self, item: T) -> Self::Owned;
    fn head(&self) -> Option<&T>;
    fn tail(&self) -> Option<Self>;

    fn len(&self) -> usize {
        self.tail().as_ref().map(Self::len).unwrap_or(0)
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<T> List<T> for Vec<T> {}

impl<T: Clone> ListView<T> for &[T] {
    type Owned = Vec<T>;
    fn append(&self, item: T) -> Vec<T> {
        let mut list = self.to_vec();
        list.push(item);
        list
    }

    fn head(&self) -> Option<&T> {
        self.get(0)
    }

    fn tail(&self) -> Option<Self> {
        if self.len() == 0 {
            return None;
        }
        Some(&self[1..])
    }

    fn len(&self) -> usize {
        <[T]>::len(self)
    }
}

#[derive(Debug, PartialEq)]
pub enum SharedList<T> {
    Empty,
    Cons(Arc<(T, Self)>),
}

impl<T> SharedList<T> {
    pub fn new(item: T) -> Self {
        SharedList::Cons(Arc::new((item, SharedList::Empty)))
    }
}

impl<T> Clone for SharedList<T> {
    fn clone(&self) -> Self {
        match self {
            SharedList::Empty => SharedList::Empty,
            SharedList::Cons(entry) => SharedList::Cons(entry.clone()),
        }
    }
}

impl<T> List<T> for SharedList<T> {}

impl<T> ListView<T> for SharedList<T> {
    type Owned = SharedList<T>;

    fn append(&self, item: T) -> Self::Owned {
        Self::Cons(Arc::new((item, self.clone())))
    }

    fn head(&self) -> Option<&T> {
        match self {
            SharedList::Empty => None,
            SharedList::Cons(entry) => Some(&entry.0),
        }
    }

    fn tail(&self) -> Option<Self> {
        match self {
            SharedList::Empty => None,
            SharedList::Cons(entry) => Some(entry.1.clone()),
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            SharedList::Empty => true,
            SharedList::Cons(_) => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vec_implements_list_interface() {
        fn is_empty<T>(list: impl ListView<T>) -> bool {
            list.len() == 0
        }
        let empty: Vec<i32> = vec![];
        assert!(is_empty(empty.as_slice()));
    }
}
