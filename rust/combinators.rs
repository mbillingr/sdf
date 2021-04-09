fn compose<T, U, V>(f: impl Fn(U) -> V, g: impl Fn(T) -> U) -> impl Fn(T) -> V {
    move |x| f(g(x))
}

fn iterate<T, F>(n: usize) -> impl Fn(F) -> Box<dyn Fn(T) -> T>
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

fn parallel_combine<T: Clone, X, Y, Z>(
    h: impl Fn(X, Y) -> Z,
    f: impl Fn(T) -> X,
    g: impl Fn(T) -> Y,
) -> impl Fn(T) -> Z {
    move |x| h(f(x.clone()), g(x))
}

fn identity<T>(x: T) -> T {
    x
}

fn square(x: i64) -> i64 {
    x * x
}

fn main() {
    println!("{:?}", compose(|x| (1, x), |x| (2, x))(0));
    println!("{}", iterate(3)(square)(5));
    println!(
        "{:?}",
        parallel_combine(|a, b| (a, b), |x| (x,), |x| [x])(42)
    );
}
