import Text.Parsec
import Text.Parsec.String (Parser)


expression :: Parser Float
expression =
  chainActions "+-" (chainActions "/*" number)


number :: Parser Float
number = fmap read $ withWhitespace numberString
  where
    numberString :: Parser String
    numberString = many1 $ oneOf $ '.' : ['0' .. '9']

    withWhitespace :: Parser a -> Parser a
    withWhitespace = between spaces spaces


chainActions :: String -> Parser Float -> Parser Float
chainActions actionsStr arguments =
  chainl1 arguments action
    where
      action :: Parser (Float -> Float -> Float)
      action = fmap toAction (oneOf actionsStr)

      toAction :: Char -> (Float -> Float -> Float)
      toAction '+' = (+)
      toAction '-' = (-)
      toAction '*' = (*)
      toAction '/' = (/)
