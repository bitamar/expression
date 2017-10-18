import Text.Parsec
import Text.Parsec.String (Parser)


expression :: Parser Float
expression = chainActions "+-" (chainActions "/*" expressionOrNumber)
  where
    expressionOrNumber :: Parser Float
    expressionOrNumber = withWhitespace $ withParens expression <|> number

    withWhitespace :: Parser a -> Parser a
    withWhitespace = between spaces spaces

    withParens :: Parser a -> Parser a
    withParens = between (char '(') (char ')')


number :: Parser Float
number = fmap read numberString
  where
    numberString :: Parser String
    numberString = many1 $ oneOf $ '.' : ['0' .. '9']


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
