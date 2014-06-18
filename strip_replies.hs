import Text.Regex.PCRE
import Data.List

main :: IO ()
main = interact (unlines . stripQuoted . lines)

-- | sequence of filters to tidy quoted messages
stripQuoted :: [String] -> [String]
stripQuoted = removeDoubles . tweakAttachments . removeLine . removeAfterLine . removeLevel2

-- | remove any line starting with more than one '>'
removeLevel2 :: [String] -> [String]
removeLevel2 = filter killRule
    where killRule ('>':'>':_) = False
          killRule _           = True

-- | remove lines after (and including) those that match badstuff
removeAfterLine :: [String] -> [String]
removeAfterLine =  takeWhile (\x -> not (any (x=~) badstuff))
    where badstuff = [ "^>\\sFrom:\\s"
                     , "^>\\s(-+)\\s?(:?Forwarded message|Original Message|Reply message|Mensaje original)\\s?\\1$" ]

-- | remove lines that match badstuff
removeLine :: [String] -> [String]
removeLine =  filter (\x -> not (any (x=~) badstuff))
    where badstuff = [ "^> On .* wrote:$" 
                     , "^> Quoting .*? <"
                     , "^> Begin forwarded message:$"
                     , "^> --\\s?$"
                     , "^> ___+$"
                     , "^> Sent from my \\w+$"
                     , "^> The University of Edinburgh is a charitable body, registered in$"
                     , "^> Scotland, with registration number SC005336\\.$" ]

-- | tweak attachment text from [x] to `[x]`
tweakAttachments :: [String] -> [String]
tweakAttachments = map rewriteRule
    where rewriteRule x = if "> [" `isPrefixOf` x && "]" `isSuffixOf` x
                          then "> `[" ++ drop 3 (init x) ++ "]`"
                          else x

-- | remove double lines of ">" in quoted message
-- | also removes trailing lines of ">" at end
removeDoubles :: [String] -> [String]
removeDoubles = foldr accRule []
    where accRule ">" []           = []
          accRule ">" (">":others) = others
          accRule x acc            = x:acc
