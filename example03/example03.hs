data Position = Position Int Int

moveKnight :: Position -> [Position]
moveKnight (Position x y) =
        filter (\(Position x y) -> x <= 8 && x >= 1 && y <= 8 && y >= 1) 
                        [ Position (x - 3) (y - 1)
                        , Position (x - 3) (y + 1)
                        , Position (x + 3) (y - 1)
                        , Position (x + 3) (y + 1)
                        , Position (x - 1) (y - 3)
                        , Position (x - 1) (y + 3)
                        , Position (x + 1) (y - 3)
                        , Position (x + 1) (y + 3)
                        ]

monadicImpl :: Position -> [Position]
monadicImpl p = return p >>= moveKnight >>= moveKnight >>= moveKnight

main :: IO ()
main = do
    putStrLn "Possible locations after 3 turns:"
    let possibilities = monadicImpl (Position 1 2)
    putStrLn
        $ concat
        $ map (\(Position x y) -> "(" ++ show x ++ "," ++ show y ++ "), ")
        $ possibilities
