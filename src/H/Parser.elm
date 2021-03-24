module H.Parser exposing (Node(..), Attribute, run)

import Parser exposing ( (|.)
                       , (|=)
                       , Parser
                       )

import Extra.Utils exposing (isSpaceChar)


type Node
    = Text String
    | Element String (List Attribute) (List Node)


type alias Attribute =
    ( String, String )




-- MAIN --

run : String -> Result (List Parser.DeadEnd) (List Node)
run input =
    if String.isEmpty input then
        Ok []
    else
        Parser.run (oneOrMore "node" node) input


node : Parser Node
node =
  Parser.oneOf
    [ text
    , element
    ]
    


text : Parser Node
text =
    Parser.oneOf
        [ Parser.getChompedString <|
            chompOneOrMore (\c -> c /= '<' && c /= '&')
        ]
        |> oneOrMore "text element"
        |> Parser.map (String.join "" >> Text) 


element : Parser Node
element =
    Parser.succeed Tuple.pair
        |. Parser.chompIf ((==) '<')
        |= tagName
        |. Parser.chompWhile isSpaceChar
        |= tagAttributes
        |> Parser.andThen
            (\(name, attributes) ->
                Parser.succeed (Element name attributes)
                    |. Parser.chompIf ((==) '>')
                    |= many (Parser.backtrackable node)
                    |. closingTag name
            )


tagName : Parser String
tagName =
    Parser.getChompedString
        ( Parser.chompIf Char.isAlphaNum
            |. Parser.chompWhile (\c -> Char.isAlphaNum c || c == '-')
        )
        |> Parser.map String.toLower


tagAttributes : Parser (List Attribute)
tagAttributes = many tagAttribute

tagAttribute : Parser Attribute
tagAttribute =
    Parser.succeed Tuple.pair
        |= tagAttributeName
        |. Parser.chompWhile isSpaceChar
        |= tagAttributeValue
        |. Parser.chompWhile isSpaceChar

tagAttributeName : Parser String
tagAttributeName =
    let
        isTagAttributeChar : Char -> Bool
        isTagAttributeChar c =
            not (isSpaceChar c) && c /= '"' && c /= '\'' && c /= '>' && c /= '/' && c /= '='
    in
        Parser.getChompedString (chompOneOrMore isTagAttributeChar)
            |> Parser.map String.toLower

tagAttributeValue : Parser String
tagAttributeValue =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.chompIf ((==) '=')
            |. Parser.chompWhile isSpaceChar
            |= Parser.oneOf
                [ tagAttributeUnquotedValue
                , tagAttributeQuotedValue '"'
                , tagAttributeQuotedValue '\''
                ]
        ]

tagAttributeUnquotedValue : Parser String
tagAttributeUnquotedValue =
    let
        isUnquotedValueChar c =
            not (isSpaceChar c) && c /= '"' && c /= '\'' && c /= '=' && c /= '<' && c /= '>' && c /= '`' && c /= '&'
    in
        ( chompOneOrMore isUnquotedValueChar
            |> Parser.getChompedString
        )
        |> oneOrMore "attribute value"
        |> Parser.map (String.join "")


tagAttributeQuotedValue : Char -> Parser String

tagAttributeQuotedValue quote =
    let
        isQuotedValueChar c =
            c /= quote && c /= '&'
    in
    Parser.succeed identity
        |. Parser.chompIf ((==) quote)
        |= ( Parser.getChompedString (chompOneOrMore isQuotedValueChar)
                |> many
                |> Parser.map (String.join "")
           )
        |. Parser.chompIf ((==) quote)


closingTag : String -> Parser ()
closingTag name =
    let
        isClosingTagChar c=
            (not <| isSpaceChar c)
            && c /= '>' 
        chompName =
            chompOneOrMore isClosingTagChar
                |> Parser.getChompedString
                |> Parser.andThen
                    (\closingName ->
                        if String.toLower closingName == name then
                            Parser.succeed ()
                        else
                            Parser.problem ("Closing tag does not match opening tag" ++ name)
                    )
    in
        Parser.chompIf ((==) '<')
            |. Parser.chompIf ((==) '/')
            |. chompName
            |. Parser.chompWhile isSpaceChar
            |. Parser.chompIf ((==) '>')


chompOneOrMore : (Char -> Bool) -> Parser ()
chompOneOrMore pred =
    Parser.chompIf pred
      |. Parser.chompWhile pred


oneOrMore : String -> Parser a -> Parser (List a)
oneOrMore type_ parser_ =
    Parser.loop []
        (\list ->
            Parser.oneOf
                [ parser_ |> Parser.map (\new -> Parser.Loop (new :: list))
                , if List.isEmpty list then
                    Parser.problem ("Expecting at least one " ++ type_)
                  else
                    Parser.succeed (Parser.Done (List.reverse list))
                ]
        )

many : Parser a -> Parser (List a)
many parser_ =
    Parser.loop []
        (\list ->
            Parser.oneOf
                [ parser_ |> Parser.map (\new -> Parser.Loop (new :: list))
                , Parser.succeed <| (Parser.Done <| List.reverse list)
                ]
        )
