use std::any::{Any, TypeId};

pub trait Type: 'static + std::fmt::Debug {
    fn type_id(&self) -> TypeId;
    fn as_any(&self) -> &dyn Any;
    fn parent(&self) -> Option<&dyn Type>;
}

// some impls to make Type work with generic procedures
impl Type for () {
    fn type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn parent(&self) -> Option<&dyn Type> {
        None
    }
}
impl Type for i64 {
    fn type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn parent(&self) -> Option<&dyn Type> {
        None
    }
}

impl Type for String {
    fn type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn parent(&self) -> Option<&dyn Type> {
        None
    }
}

pub trait TypeHierarchy {
    fn upcast<T: Type>(&self) -> Option<&T>;

    fn is_a<T: Type>(&self) -> bool {
        self.upcast::<T>().is_some()
    }
}

impl<T: Type> TypeHierarchy for T {
    fn upcast<X: Type>(&self) -> Option<&X> {
        if self.type_id() == TypeId::of::<X>() {
            return Some(unsafe { std::mem::transmute(self) });
        }

        if let Some(p) = self.parent() {
            p.upcast::<X>()
        } else {
            None
        }
    }
}

impl TypeHierarchy for dyn Type {
    fn upcast<X: Type>(&self) -> Option<&X> {
        if self.type_id() == TypeId::of::<X>() {
            return unsafe { Some(&*(self as *const dyn Type as *const X)) };
        }

        if let Some(p) = self.parent() {
            p.upcast::<X>()
        } else {
            None
        }
    }
}

#[macro_export]
macro_rules! type_hierarchy {
    (($typename:ident $fields:tt $($subtypes:tt)*)) => {
        recurse_type_hierarchy!{
            ($typename $fields $($subtypes)*)
        }
    }
}

#[macro_export]
macro_rules! recurse_type_hierarchy {
    (
        ($typename:ident
            $fields:tt
            $($subtype:tt)*
        )
    ) => {
        make_type!{$typename, $fields}
        impl_type!{$typename}
        make_fields!{$typename, $fields}
        $(
            recurse_type_hierarchy!{
                $subtype,
                $typename,
                [$fields, []]
            }
        )*
    };

    (
        ($typename:ident
            $fields:tt
            $($subtype:tt)*
        ),
        $supertype:ty,
        $inherit:tt
    ) => {
        make_type!{$typename($supertype), $fields}
        impl_type!{$supertype > $typename}
        make_fields!{$typename, $fields}
        make_inherited_fields!{$typename, $inherit}
        $(
            recurse_type_hierarchy!{
                $subtype,
                $typename,
                [$fields, $inherit]
            }
        )*
    };
}

#[macro_export]
macro_rules! make_type {
    ($typename:ident, []) => {
        #[derive(Debug, Clone)]
        pub struct $typename;
    };

    ($typename:ident, [$($field_name:ident: $field_type:ty,)*]) => {
        #[derive(Debug, Clone)]
        pub struct $typename {
            $($field_name: $field_type,)*
        }
    };

    ($typename:ident($parent:ty), [$($field_name:ident: $field_type:ty,)*]) => {
        make_type! {
            $typename, [parent: $parent, $($field_name: $field_type,)*]
        }
    };
}

#[macro_export]
macro_rules! impl_type {
    ($typename:ty) => {
        impl $crate::chapter03::type_structure::Type for $typename {
            fn type_id(&self) -> ::std::any::TypeId {
                ::std::any::TypeId::of::<Self>()
            }

            fn as_any(&self) -> &dyn ::std::any::Any {
                self
            }

            fn parent(&self) -> Option<&dyn $crate::chapter03::type_structure::Type> {
                None
            }
        }
    };

    ($supertype:ty > $typename:ty) => {
        impl $crate::chapter03::type_structure::Type for $typename {
            fn type_id(&self) -> ::std::any::TypeId {
                ::std::any::TypeId::of::<Self>()
            }

            fn as_any(&self) -> &dyn ::std::any::Any {
                self
            }

            fn parent(&self) -> Option<&dyn $crate::chapter03::type_structure::Type> {
                Some(&self.parent)
            }
        }
    };
}

#[macro_export]
macro_rules! make_fields {
    ($typename:ident, [$($field_name:ident: $field_type:ty,)*]) => {
        impl $typename {
            $(
                pub fn $field_name(&self) -> &$field_type { &self.$field_name }
            )*
        }
    }
}

#[macro_export]
macro_rules! make_inherited_fields {
    ($typename:ident, []) => { };

    ($typename:ident, [$parent_fields:tt, $grandparent_fields:tt]) => {
        make_inherited_fields!(@impl $typename, $parent_fields);
        make_inherited_fields!($typename, $grandparent_fields);
    };

    (@impl $typename:ident, [$($field_name:ident: $field_type:ty,)*]) => {
        impl $typename {
            $(
                pub fn $field_name(&self) -> &$field_type { &self.parent.$field_name() }
            )*
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type_hierarchy! {
        (Object []
            (NamedObject [name: String,]
                (Person [age: u8,])
                (Pet [])
            )
        )
    }

    impl NamedObject {
        fn new(name: String) -> Self {
            NamedObject {
                parent: Object,
                name,
            }
        }
    }

    impl Person {
        fn new(name: impl ToString, age: u8) -> Self {
            Person {
                parent: NamedObject::new(name.to_string()),
                age,
            }
        }
    }

    impl Pet {
        fn new(name: impl ToString) -> Self {
            Pet {
                parent: NamedObject::new(name.to_string()),
            }
        }
    }

    #[test]
    fn it_works() {
        let bob: Box<dyn Type> = Box::new(Person::new("Bob", 42));
        let pet: Box<dyn Type> = Box::new(Pet::new("Gerfield"));

        assert!(!bob.is_a::<Pet>());
        assert!(bob.is_a::<Person>());
        assert!(bob.is_a::<NamedObject>());
        assert!(bob.is_a::<Object>());

        assert_eq!(bob.upcast::<NamedObject>().unwrap().name, "Bob");
        assert_eq!(bob.upcast::<Person>().unwrap().age(), &42);
        assert_eq!(bob.upcast::<Person>().unwrap().name(), "Bob");

        assert_eq!(pet.upcast::<Pet>().unwrap().name(), "Gerfield");
        assert!(pet.upcast::<Object>().is_some());
    }
}
