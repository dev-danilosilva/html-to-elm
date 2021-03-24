module H.Render exposing (node2Elm, node2String, renderElm)

import H.Parser exposing (run, Node(..), Attribute)


renderElm : String -> Result String String
renderElm input =
    case run input of
        Ok ns ->
            case List.length ns of
                0 -> Err "Did not found any node"

                1 -> case List.head ns of
                        Just node -> Ok <| node2Elm node
                        Nothing -> Err "Did not found any node"
                _ -> Err "Should Send a Single Node"

        Err _ -> Err "Cannot Parse this Html"

node2Elm : Node -> String
node2Elm node_ =
    let
        renderAttr (attrName, attrValue) =
            attrName ++ " \"" ++ attrValue ++ "\""
        
        renderAttrs : List Attribute -> String
        renderAttrs attrs =
            String.join ", " <|
                List.map renderAttr attrs
    in
        case node_ of
            Text txt ->
                txt
            
            Element el attrs children ->
                String.concat
                    [ el
                    , " "
                    , "["
                    , renderAttrs attrs
                    , "]"
                    , " "
                    , "["
                    , String.concat <| List.map node2Elm children
                    , "]"
                    ]


node2String : Node -> String
node2String node_ =
  let
    attr2String : Attribute -> String
    attr2String (attrName, value) = String.concat [attrName, "=", "\"", value, "\""]
  in
    case node_ of
      Text txt -> 
          txt

      Element el attrs children ->
          let
            attrs2String : List Attribute -> String
            attrs2String at = case at of
                [] -> ""
                (x :: xs) -> attr2String x ++ " " ++ attrs2String xs
          in
            String.concat
                    [ "<"
                    , el
                    , " "
                    , attrs2String attrs
                    , ">"
                    , List.map node2String children
                          |> String.concat
                    , "</"
                    , el
                    , ">"
                    ]