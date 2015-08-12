data MyStr = MyStr Int String

incNum :: MyStr -> Maybe MyStr
incNum (MyStr n s) = Just (MyStr (n + 1) (s ++ " incnum"))

decNum :: MyStr -> Maybe MyStr
decNum (MyStr n s)
    | n > 0    = Just (MyStr (n - 1) (s ++ " decnum"))
    | otherwise = Nothing

monadicImpl :: MyStr -> Maybe MyStr
monadicImpl mystr = return mystr
                    >>= incNum
                    >>= decNum
                    >>= incNum
                    >>= decNum
                    >>= decNum
                    >>= decNum

main :: IO ()
main = do
    case monadicImpl (MyStr 3 "") of
        Nothing -> putStrLn "Computation Failed"
        Just (MyStr n s) -> putStrLn $ "Computation Result. num: " ++ show n
                                                      ++ ", msg: " ++ s

    case monadicImpl (MyStr 1 "") of
        Nothing -> putStrLn "Computation Failed"
        Just (MyStr n s) -> putStrLn $ "Computation Result. num: " ++ show n
                                                      ++ ", msg: " ++ s
