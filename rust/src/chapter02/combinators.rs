pub fn compose<T, U, V>(f: impl Fn(U) -> V, g: impl Fn(T) -> U) -> impl Fn(T) -> V {
    move |x| f(g(x))
}

pub fn iterate<T, F>(n: usize) -> impl Fn(F) -> Box<dyn Fn(T) -> T>
where
    T: 'static,
    F: 'static + Clone + Fn(T) -> T,
{
    move |f| {
        if n == 0 {
            Box::new(identity)
        } else {
            Box::new(compose(f.clone(), iterate(n - 1)(f)))
        }
    }
}

pub fn parallel_combine<T: Clone, X, Y, Z>(
    h: impl Fn(X, Y) -> Z,
    f: impl Fn(T) -> X,
    g: impl Fn(T) -> Y,
) -> impl Fn(T) -> Z {
    move |x| h(f(x.clone()), g(x))
}

pub fn identity<T>(x: T) -> T {
    x
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chapter02::utils::square;

    #[test]
    fn compose_acceptance_test() {
        assert_eq!(
            compose(|x| ("foo", x), |x| ("bar", x))("z"),
            ("foo", ("bar", "z"))
        );
    }

    #[test]
    fn compose_returns_result_of_first_arg() {
        assert_eq!(compose(|_| "f-result", |_| 0)(0), "f-result");
    }

    #[test]
    fn compose_passes_result_of_second_arg_to_first() {
        compose(|x| assert_eq!(x, "g-result"), |_| "g-result")(0);
    }

    #[test]
    fn compose_passes_input_to_second_arg() {
        compose(|_| 0, |x| assert_eq!(x, "input"))("input");
    }

    #[test]
    fn iterate_acceptance_test() {
        assert_eq!(iterate(3)(square)(5), 390625)
    }

    #[test]
    fn iterate_zero_is_the_identity() {
        assert_eq!(iterate(0)(square)(5), 5)
    }

    #[test]
    fn iterate_zero_never_call_the_procedure() {
        iterate(0)(|_| panic!("should not be called"))(5);
    }

    #[test]
    fn parallel_combine_acceptance_test() {
        assert_eq!(
            parallel_combine(
                |a, b| (a, b),
                |(x, y, z)| ("foo", x, y, z),
                |(u, v, w)| ("bar", u, v, w)
            )(("a", "b", "c")),
            (("foo", "a", "b", "c"), ("bar", "a", "b", "c"))
        );
    }
}
