type Err = String

parseCloudFlare :: String -> Either Err String
parseCloudFlare val =
        if take 10 val == "CloudFlare"
            then Right (drop 10 val)
            else Left ("Doesn't start with \"CloudFlare\": " ++ val)

parseDigit :: String -> Either Err String
parseDigit val =
        if head val `elem` ['0'..'9']
            then Right (tail val)
            else Left ("Doesn't start with a digit: " ++ val)

monadicImpl :: String -> Either Err String
monadicImpl val = return val
                    >>= parseDigit
                    >>= parseCloudFlare
                    >>= parseDigit
                    >>= parseDigit

main :: IO ()
main = do
    case monadicImpl "1CloudFlare23CloudFlare" of
        Left err -> putStrLn ("Error encountered: " ++ err)
        Right val -> putStrLn ("Success. Resulting value: " ++ val)

    case monadicImpl "1Akamai23CloudFlare" of
        Left err -> putStrLn ("Error encountered: " ++ err)
        Right val -> putStrLn ("Success. Resulting value: " ++ val)

