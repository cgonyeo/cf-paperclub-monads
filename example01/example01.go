package main

import (
    "bytes"
    "fmt"
)

type Either struct {
    err error
    val []byte
}

// :: (Monad m) => a -> m a
func Unit(val []byte) Either {
    return Either{
        err: nil,
        val: val,
    }
}

// :: (Monad m) => m a -> (a -> m b) -> m b
func Chain(val Either, f func([]byte) Either) Either {
    if val.err != nil {
        return Either{val.err, nil}
    } else {
        return f(val.val)
    }
}

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

func normalImpl(val []byte) ([]byte, error) {
    res := parseDigit(val)
    if res.err != nil {
        return nil, res.err
    }

    res = parseCloudFlare(res.val)
    if res.err != nil {
        return nil, res.err
    }

    res = parseDigit(res.val)
    if res.err != nil {
        return nil, res.err
    }

    res = parseDigit(res.val)
    return res.val, res.err
}

func monadicImpl(val []byte) ([]byte, error) {
    res := Chain(Chain(Chain(Chain(
             Unit(val),
             parseDigit),
             parseCloudFlare),
             parseDigit),
             parseDigit)
    return res.val, res.err
}

func main() {
    res, err := normalImpl([]byte("1CloudFlare23CloudFlare"))
    if err != nil {
        fmt.Printf("Error encountered: %v\n", err)
    } else {
        fmt.Printf("Success. Resulting value: %s\n", string(res))
    }

    res, err = normalImpl([]byte("1Akamai23CloudFlare"))
    if err != nil {
        fmt.Printf("Error encountered: %v\n", err)
    } else {
        fmt.Printf("Success. Resulting value: %s\n", string(res))
    }

    res, err = monadicImpl([]byte("1CloudFlare23CloudFlare"))
    if err != nil {
        fmt.Printf("Error encountered: %v\n", err)
    } else {
        fmt.Printf("Success. Resulting value: %s\n", string(res))
    }

    res, err = monadicImpl([]byte("1Akamai23CloudFlare"))
    if err != nil {
        fmt.Printf("Error encountered: %v\n", err)
    } else {
        fmt.Printf("Success. Resulting value: %s\n", string(res))
    }
}
