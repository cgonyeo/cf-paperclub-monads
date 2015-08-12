package main

import (
    "fmt"
)

type mystr struct {
    num int
    msg string
}

type Maybe struct {
    str *mystr
}

// :: (Monad m) => a -> m a
func Unit(val *mystr) Maybe {
    return Maybe{
        str: val,
    }
}

// :: (Monad m) => m a -> (a -> m b) -> m b
func Chain(val Maybe, f func(*mystr) Maybe) Maybe {
    if val.str == nil {
        return Maybe{nil}
    } else {
        return f(val.str)
    }
}

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

func main() {
    str := normalImpl(&mystr{num: 3, msg: ""})
    if str == nil {
        fmt.Printf("Computation failed\n")
    } else {
        fmt.Printf("Computation Result. num: %d, msg: %s\n",
                            str.num, str.msg)
    }

    str = monadicImpl(&mystr{num: 3, msg: ""})
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

    str = monadicImpl(&mystr{num: 1, msg: ""})
    if str == nil {
        fmt.Printf("Computation failed\n")
    } else {
        fmt.Printf("Computation Result. num: %d, msg: %s\n",
                            str.num, str.msg)
    }
}
