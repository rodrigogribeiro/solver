Constraint Parser
=================


> module Parser.ConstraintParser (parser) where

> import Data.Functor
> import Data.Functor.Identity
  
> import Text.Parsec
> import Text.Parsec.Char
> import Text.Parsec.Language
> import Text.Parsec.Token (TokenParser)    
> import qualified Text.Parsec.Token as Tk
> import qualified Text.Parsec.Expr as Ex

> import Data.Type
> import Data.Constraints

A type for parsers
  
> type Parser a = ParsecT String () Identity a

Top level parsing function
  
> parser :: String -> Either String Constraint
> parser = either (Left . show) Right . parse constraintParser ""


Constraint parser

> constraintParser :: Parser Constraint
> constraintParser = Ex.buildExpressionParser table ctrParser
>                    where
>                      table = [[ Ex.Infix conjParser Ex.AssocLeft ]]
>                      conjParser = (:&:) <$ comma         

> ctrParser :: Parser Constraint
> ctrParser = choice [
>                      eqParser
>                    , ascriptionParser
>                    , hasParser
>                    , defParser  
>                    , existsParser
>                    , typeDefParser
>                    ]

> eqParser :: Parser Constraint
> eqParser = (:=:) <$> typeParser <* reservedOp "="
>                                 <*> typeParser

> ascriptionParser :: Parser Constraint
> ascriptionParser = build <$> reserved "typeof"   <*>
>                              (parens nameParser) <*>
>                              reservedOp "="      <*>
>                              typeParser
>                    where
>                      build _ n _ t = n :<-: t

> hasParser :: Parser Constraint
> hasParser = reserved "has" *>
>             parens (build <$> typeParser <*>
>                               comma      <*>
>                               nameParser <*>
>                               colon      <*>
>                               typeParser)
>             where
>               build t _ n _ t' = Has t (Field n t')

> defParser :: Parser Constraint
> defParser = build <$> reserved "def" <*> nameParser <*>
>                       colon          <*> typeParser <*>
>                       reserved "in"  <*> constraintParser
>             where
>               build _ n _ t _ ctr = Def n t ctr

> existsParser :: Parser Constraint
> existsParser = build <$> reserved "exists" <*> nameParser <*>
>                          reservedOp "."    <*> constraintParser
>                where
>                  build _ n _ ctr = Exists n ctr 

> typeDefParser :: Parser Constraint
> typeDefParser = build <$> reserved "typedef" <*> typeParser <*>
>                           reserved "as"      <*> typeParser
>                 where
>                    build _ n _ t = TypeDef n t
  
Type parser

> typeParser :: Parser Ty
> typeParser = option id (Pointer <$ string "*") <*> typeParser'
     
> typeParser' :: Parser Ty
> typeParser' = choice [ tyVarParser
>                      , tyConParser
>                      , tyFunParser
>                      , structParser
>                     ]  

> tyVarParser :: Parser Ty
> tyVarParser = TyVar <$> name'
>               where
>                  name' = f <$> string "alpha"
>                            <*> (show <$> Tk.integer constrLexer)
>                  f x y = Name (x ++ y)              

> tyConParser :: Parser Ty
> tyConParser = (TyCon . Name) <$> (Tk.identifier constrLexer)

> tyFunParser :: Parser Ty
> tyFunParser = f <$> parens (typeParser `sepBy1` comma)
>               where
>                 f ts = FunTy (last ts) (init ts)

> structParser :: Parser Ty
> structParser = f <$> reserved "struct" <*>
>                      nameParser  <*>
>                      braces (fieldParser `endBy1` semi)
>                where
>                  f _ n fs = Struct fs n

> fieldParser :: Parser Field
> fieldParser = flip Field <$> typeParser <*> nameParser
  
Lexer definition
  
> constrLexer :: TokenParser st
> constrLexer = Tk.makeTokenParser constrDef

> nameParser :: Parser Name
> nameParser = Name <$> (Tk.identifier constrLexer <|>
>                        Tk.operator constrLexer)

> reserved :: String -> Parser ()
> reserved = Tk.reserved constrLexer           

> reservedOp :: String -> Parser ()
> reservedOp = Tk.reservedOp constrLexer

> braces :: Parser a -> Parser a
> braces = Tk.braces constrLexer

> parens :: Parser a -> Parser a
> parens = Tk.parens constrLexer          

> comma :: Parser ()
> comma = () <$ Tk.comma constrLexer

> semi :: Parser ()
> semi = () <$ Tk.semi constrLexer

> starParser :: Parser ()
> starParser = () <$ Tk.symbol constrLexer "*"

> colon :: Parser ()
> colon = () <$ Tk.colon constrLexer

> dot :: Parser ()
> dot = () <$ Tk.dot constrLexer
  
Constraint language def

> constrDef :: LanguageDef st
> constrDef = emptyDef {
>               Tk.reservedOpNames = [":", "=", "->"] 
>             , Tk.reservedNames = ["exists", "def", "in", "typedef"
>                                  , "as", "void", "struct", "has", "typeof"]
>             }             
