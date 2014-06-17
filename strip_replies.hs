import Text.Regex.PCRE

main :: IO ()
main = interact (unlines . stripQuoted . lines)

-- | sequence of filters to tidy quoted messages
stripQuoted :: [String] -> [String]
stripQuoted = removeDoubles . tweakAttachments . removeSignature . removeQuoteAttrib . removeLevel2

-- | remove any line starting with more than one '>'
removeLevel2 :: [String] -> [String]
removeLevel2 = filter (\x -> take 2 x /= ">>")

-- | remove attribution line for level 2 quote
removeQuoteAttrib :: [String] -> [String]
removeQuoteAttrib =  filterOutRegexList badstuff
    where badstuff = [
            "^>[[:space:]]*(From|Forwarded message|Begin forwarded message|Original Message|Reply message|Mensaje original)(:)?[[:space:]]+",
            "^>[[:space:]]*On .* wrote:",
            "^>[[:space:]]*Quoting .*? <" ]

-- | remove signature for quote at level 1
removeSignature :: [String] -> [String]
removeSignature =  filterOutRegexList badstuff
    where badstuff = [
            "^> -- $",
            "^> --$",
            "^> ___+$",
            "^> Sent from my HTC$",
            "^> Sent from my iPhone$",
            "^> The University of Edinburgh is a charitable body, registered in$",
            "^> Scotland, with registration number SC005336\\.$"]

-- | return only lines that don't match list of regexs (1st arg)
-- note: regexs are used first to last
filterOutRegexList :: [String] -> [String] -> [String]
filterOutRegexList = foldr (\x acc -> acc . filterOutRegex x) id

-- | return only lines that don't match regex (1st arg)
filterOutRegex :: String -> [String] -> [String]
filterOutRegex pat = filter (\x -> not (x =~ pat :: Bool)) 

-- | tweak attachment text from [x] to `[x]`
tweakAttachments :: [String] -> [String]
tweakAttachments = map (\x -> if take 3 x == "> [" && last x == ']' 
                                then "> `[" ++ (drop 3 (init x)) ++ "]`" 
                                else x)

-- | remove double newlines in quoted message
-- also removes trailing newlines
removeDoubles :: [String] -> [String]
removeDoubles = foldr (\x acc -> if x == ">" && (acc == [] || head acc == ">") then acc else x:acc) []
