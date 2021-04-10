--- some scheme-like helper functions

function cons(a, d)
    function the_pair(operator)
        return operator(a, d)
    end
    return the_pair
end

function car(pair)
    return pair(function(a, d)
        return a
    end)
end

function cdr(pair)
    return pair(function(a, d)
        return d
    end)
end

function print_pair(pair)
    print("(", car(pair), " . ", cdr(pair), ")")
end

function print_list(list)
    io.write("(")
    if list ~= nil then
        io.write(car(list))
        list = cdr(list)
        while (list ~= nil) do
            io.write(" ", car(list))
            list = cdr(list)
        end
    end
    io.write(")\n")
end

------------------------------------------------------------------

function compose(f, g)
    function the_composition (...)
        return f(g(...))
    end
    return the_composition
end

ans = compose(
        function(x)
            return cons("foo", x)
        end,
        function(x)
            return cons("bar", x)
        end)(nil)
print_list(ans)

function iterate(n)
    function the_iterator(f)
        if n == 0 then
            return identity
        else
            return compose(f, iterate(n - 1)(f))
        end
    end
    return the_iterator
end

function identity(...)
    return ...
end

function square(x)
    return x * x
end

print(iterate(3)(square)(5))

function parallel_combine(h, f, g)
    function the_combination(...)
        return h(f(...), g(...))
    end
    return the_combination
end

print_pair(parallel_combine(cons, identity, square)(2))
