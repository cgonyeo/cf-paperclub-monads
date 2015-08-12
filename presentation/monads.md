% Monads for functional programming
% Derek Gonyeo

## The Paper

Monads for functional programming

By Philip Wadler at the University of Glasgow

<!-- 
The paper introduces the concept of a monad and explains what it is, and uses
three case studies to demonstrate that it is a useful pattern for emulating
impure style effects in a pure environment.
-->

# A Go Example

## First: a wrapper type

Only one of the two fields is set. This will probably seem unnecessary at first.

```go
type Either struct {
    err error
    val []byte
}
```

## Random function #1

Takes a byte slice. If it starts with `"CloudFlare"`, it will remove the first
10 characters and return the rest of the slice. Otherwise it returns an error.

```go
func parseCloudFlare(val []byte) Either {
    if bytes.HasPrefix(val, []byte("CloudFlare")) {
        return Either {
            err: nil,
            val: val[10:],
        }
    }
    return Either {
        err: fmt.Errorf("Doesn't start with \"CloudFlare\": %s", string(val)),
        val: nil,
    }
}
```

## Random function #2

Takes a byte slice. If it starts with a digit, it will remove the first
character and return the rest of the slice. Otherwise it returns an error.

```go
func parseDigit(val []byte) Either {
    if len(val) > 0 {
        first := val[0]
        if first >= 48 && first <= 57 {
            return Either{
                err: nil,
                val: val[1:],
            }
        }
    }
    return Either {
        err: fmt.Errorf("Doesn't start with a digit: %s", string(val)),
        val: nil,
    }
}
```

## Error handling in Go

A function that uses the two functions above. Crazy stuff here.

```go
func normalImpl(val []byte) ([]byte, error) {
    res := parseDigit(val)
    if res.err != nil {
        return nil, res.err
    }

    res = parseCloudFlare(res.val)
    if res.err != nil {
        return nil, res.err
    }

    return res.val, res.err
}
```

## What would happen?

```go
res, err := normalImpl([]byte("1CloudFlare23CloudFlare"))
if err != nil {
    fmt.Printf("Error encountered: %v\n", err)
} else {
    fmt.Printf("Success. Resulting value: %s\n", string(res))
}

res, err = normalImpl([]byte("2Akamai23CloudFlare"))
if err != nil {
    fmt.Printf("Error encountered: %v\n", err)
} else {
    fmt.Printf("Success. Resulting value: %s\n", string(res))
}
```

## Remove the pattern

Having four lines per function call to have error handling is obnoxious,
especially when every block does the exact same thing. Let's make an
abstraction to do that for us.

## Essentials of a Monad

Unit takes a byte slice, and puts it in an `Either.` Chain take an `Either` and
a function that takes a byte slice, and applies the function to the value in the
`Either`.

```go
func Unit(val []byte) Either {
    return Either{
        err: nil,
        val: val,
    }
}

func Chain(val Either, f func([]byte) Either) Either {
    if val.err != nil {
        return Either{val.err, nil}
    } else {
        return f(val.val)
    }
}
```

## Using the monad

The monadic implementation inserts the value into an `Either` with `Unit`, and
then uses `Chain` to connect the functions we want to call together.

```go
func monadicImpl(val []byte) ([]byte, error) {
    res := Chain(Chain(
             Unit(val),
             parseDigit),
             parseCloudFlare)
    return res.val, res.err
}
```

## Demo

# Maybe

## New wrapper struct

```go
type mystr struct {
    num int
    msg string
}

type Maybe struct {
    str *mystr
}
```

## Functions using mystr

```go
func incNum(str *mystr) Maybe {
    str.num++
    str.msg = str.msg + " incNum"
    return Maybe {
        str: str,
    }
}

func decNum(str *mystr) Maybe {
    if str.num > 0 {
        str.num--
        str.msg = str.msg + " decNum"
        return Maybe {
            str: str,
        }
    }
    return Maybe {
        str: nil,
    }
}
```

## Normal usage

```go
func normalImpl(str *mystr) *mystr {
    res := incNum(str)
    if res.str == nil {
        return nil
    }

    res = decNum(res.str)
    if res.str == nil {
        return nil
    }

    res = incNum(res.str)
    if res.str == nil {
        return nil
    }

    res = decNum(res.str)
    if res.str == nil {
        return nil
    }

    res = decNum(res.str)
    if res.str == nil {
        return nil
    }

    res = decNum(res.str)
    return res.str
}
```

## What happens?

```go
str := normalImpl(&mystr{num: 3, msg: ""})
if str == nil {
    fmt.Printf("Computation failed\n")
} else {
    fmt.Printf("Computation Result. num: %d, msg: %s\n",
                        str.num, str.msg)
}

str = normalImpl(&mystr{num: 1, msg: ""})
if str == nil {
    fmt.Printf("Computation failed\n")
} else {
    fmt.Printf("Computation Result. num: %d, msg: %s\n",
                        str.num, str.msg)
}
```

## More monads!

```go
func Unit(val *mystr) Maybe {
    return Maybe{
        str: val,
    }
}

func Chain(val Maybe, f func(*mystr) Maybe) Maybe {
    if val.str == nil {
        return Maybe{nil}
    } else {
        return f(val.str)
    }
}
```

## Monadic usage

```go
func monadicImpl(str *mystr) *mystr {
    res := Chain(Chain(Chain(Chain(Chain(Chain(
            Unit(str),
            incNum),
            decNum),
            incNum),
            decNum),
            decNum),
            decNum)
    return res.str
}
```

## What happens?

```go
str = monadicImpl(&mystr{num: 3, msg: ""})
if str == nil {
    fmt.Printf("Computation failed\n")
} else {
    fmt.Printf("Computation Result. num: %d, msg: %s\n",
                        str.num, str.msg)
}

str = monadicImpl(&mystr{num: 1, msg: ""})
if str == nil {
    fmt.Printf("Computation failed\n")
} else {
    fmt.Printf("Computation Result. num: %d, msg: %s\n",
                        str.num, str.msg)
}
```

# Monad Laws

## Law 1

### Left Identity

> If we take a value, put it in a default context with return and then feed it
> to a function by using >>=, it's the same as just taking the value and
> applying the function to it.

_Learn You a Haskell For Great Good_

## Law 1

Haskell:

```haskell
(return x) >>= f

f x
```

Go:

```go
Chain(Unit(x), f)

f(x)
```

## Law 2

### Right Identity

> If we have a monadic value and we use >>= to feed it to return, the result is
> our original monadic value.

_Learn You a Haskell For Great Good_

## Law 2

Haskell:

```haskell
x >>= return

x
```

Go:

```go
Chain(x, Unit)

x
```

## Law 3

### Associativity

> When we have a chain of monadic function applications with >>=, it shouldn't
> matter how they're nested.

_Learn You a Haskell For Great Good_

## Law 3

Haskell:

```haskell
(x >>= f) >>= g

x >>= (\y -> f y >>= g)
```

Go:

```go
Chain(Chain(x, f), g)

func apply(y []byte) Either {
    Chain(f(y), g)
}
Chain(x, apply)
```
