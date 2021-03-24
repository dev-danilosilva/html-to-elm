module Extra.Utils exposing (isSpaceChar)


isSpaceChar : Char -> Bool
isSpaceChar c =
    c == ' ' || c == '\t' || c == '\u{000D}' || c == '\u{000C}' || c == '\u{00A0}'
