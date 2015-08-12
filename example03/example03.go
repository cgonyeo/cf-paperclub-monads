package main

import (
    "fmt"
)

type Position struct {
    x int
    y int
}

// :: (Monad m) => a -> m a
func Unit(val Position) []Position {
    return []Position{val}
}

// :: (Monad m) => m a -> (a -> m b) -> m b
func Chain(vals []Position, f func(Position) []Position) []Position {
    var results []Position
    for _, val := range vals {
        positions := f(val)
        results = append(results, positions...)
    }
    return results
}

func moveKnight(p Position) []Position {
    var results []Position
    if p.x - 3 >= 1 && p.y - 1 >= 1 {
        results = append(results, Position{p.x - 3, p.y - 1})
    }
    if p.x - 3 >= 1 && p.y + 1 <= 8 {
        results = append(results, Position{p.x - 3, p.y + 1})
    }
    if p.x + 3 <= 8 && p.y - 1 >= 1 {
        results = append(results, Position{p.x + 3, p.y - 1})
    }
    if p.x + 3 <= 8 && p.y + 1 <= 8 {
        results = append(results, Position{p.x + 3, p.y + 1})
    }
    if p.x - 1 >= 1 && p.y - 3 >= 1 {
        results = append(results, Position{p.x - 1, p.y - 3})
    }
    if p.x - 1 >= 1 && p.y + 3 <= 8 {
        results = append(results, Position{p.x - 1, p.y + 3})
    }
    if p.x + 1 <= 8 && p.y - 3 >= 1 {
        results = append(results, Position{p.x + 1, p.y - 3})
    }
    if p.x + 1 <= 8 && p.y + 3 <= 8 {
        results = append(results, Position{p.x + 1, p.y + 3})
    }
    return results
}

func normalImpl(p Position) []Position {
    possibilities := moveKnight(p)
    var results []Position
    for _, pos := range possibilities {
        more_possibs := moveKnight(pos)
        results = append(results, more_possibs...)
    }

    var more_results []Position
    for _, pos := range results {
        more_possibs := moveKnight(pos)
        more_results = append(more_results, more_possibs...)
    }

    return more_results
}

func monadicImpl(p Position) []Position {
    return Chain(Chain(Chain(
                Unit(p),
                moveKnight),
                moveKnight),
                moveKnight)
}

func main() {
    possibilities := normalImpl(Position{1,2})
    fmt.Printf("Possible locations after 3 turns: \n")
    for _, pos := range possibilities {
        fmt.Printf("(%d,%d), ", pos.x, pos.y)
    }
    fmt.Printf("\n")

    possibilities = monadicImpl(Position{1,2})
    fmt.Printf("Possible locations after 3 turns: \n")
    for _, pos := range possibilities {
        fmt.Printf("(%d,%d), ", pos.x, pos.y)
    }
    fmt.Printf("\n")
}
