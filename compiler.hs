import Data.Char
import Control.Monad.Trans.State
import Control.Monad
import Control.Monad
import Control.Applicative

data Keyword = Class | Constructor | Function | Method | Field | Static | Var | KeywordInt |
    KeywordChar | Boolean | Void | KeywordTrue | KeywordFalse | KeywordNull | This | Let |
    Do | If | Else | While | KeywordReturn deriving (Show, Eq)
getKeyword :: String -> (Maybe(Keyword), String)
getKeyword('c':'l':'a':'s':'s': rest) = (Just(Class), rest) 
getKeyword('c':'o':'n':'s':'t':'r':'u':'c':'t':'o':'r':rest) = (Just(Constructor), rest)
getKeyword('f':'u':'n':'c':'t':'i':'o':'n':rest) = (Just(Function), rest)
getKeyword('m':'e':'t':'h':'o':'d':rest) = (Just(Method), rest)
getKeyword('f':'i':'e':'l':'d':rest) = ( Just(Field), rest )
getKeyword('s':'t':'a':'t':'i':'c':rest) =( Just(Static), rest )
getKeyword('v':'a':'r':rest) = ( Just(Var), rest)
getKeyword('i':'n':'t':rest) = ( Just(KeywordInt), rest)
getKeyword('c':'h':'a':'r':rest) = ( Just(KeywordChar), rest )
getKeyword('b':'o':'o':'l':'e':'a':'n':rest) = ( Just(Boolean), rest )
getKeyword('v':'o':'i':'d':rest) =(Just(Void), rest)
getKeyword('t':'r':'u':'e':rest) = (Just(KeywordTrue), rest)
getKeyword('n':'u':'l':'l':rest) = (Just(KeywordNull), rest)
getKeyword('t':'h':'i':'s':rest) = (Just(This), rest)
getKeyword('l':'e':'t':rest) = (Just(Let), rest)
getKeyword('d':'o':rest) = (Just(Do), rest)
getKeyword('i':'f':rest) = (Just(If), rest)
getKeyword('e':'l':'s':'e':rest) = (Just(Else), rest)
getKeyword('w':'h':'i':'l':'e':rest) = (Just(While), rest)
getKeyword('r':'e':'t':'u':'r':'n':rest) = (Just(KeywordReturn), rest)
getKeyword(x) = (Nothing, x)

data Symbol = LeftBracket | RightBracket | LeftParen | RightParen | 
    LeftBox | RightBox | DotOperator | Comma | SemiColon | PlusOperator | 
    MinusOperator | TimesOperator | DivisionOperator | AndOperator | VerticalBar | 
    LessThanOperator | GreaterThanOperator | EqualityOperator | Tilde deriving (Show, Eq)
getSymbol :: String -> (Maybe Symbol, String)
getSymbol('{':rest) = ( Just(LeftBracket), rest )
getSymbol('}':rest) = ( Just(RightBracket), rest )
getSymbol('(':rest) = ( Just(LeftParen), rest )
getSymbol(')':rest) = ( Just(RightParen), rest )
getSymbol('[':rest) = ( Just(LeftBox), rest )
getSymbol(']':rest) = ( Just(RightBox), rest )
getSymbol('.':rest) = ( Just(DotOperator), rest )
getSymbol(',':rest) = ( Just(Comma), rest )
getSymbol(';':rest) = ( Just(SemiColon), rest )
getSymbol('+':rest) = ( Just(PlusOperator), rest )
getSymbol('-':rest) = ( Just(MinusOperator), rest )
getSymbol('*':rest) = ( Just(TimesOperator), rest )
getSymbol('/':rest) = ( Just(DivisionOperator), rest )
getSymbol('&':rest) = ( Just(AndOperator), rest )
getSymbol('|':rest) = ( Just(VerticalBar), rest )
getSymbol('<':rest) = ( Just(LessThanOperator), rest )
getSymbol('>':rest) = ( Just(GreaterThanOperator), rest )
getSymbol('=':rest) = ( Just(EqualityOperator), rest )
getSymbol('~':rest) = ( Just(Tilde), rest )
getSymbol(rest) = ( Nothing, rest )

data IntegerConstant = IntegerConstant Integer deriving Show

getInteger :: String -> (Maybe IntegerConstant, String)
getInteger(input) = case span isDigit input of
    ([], rest) -> (Nothing, rest)
    (integer, rest) -> ( Just( IntegerConstant( read( integer ))), rest)


data StringConstant = StringConstant String deriving Show
getStringConstant :: String -> (Maybe StringConstant, String)
getStringConstant('"':rest) = case span (\c -> c /= '"') rest of
    ([], x) -> (Nothing, '"':x)
    (stringConstant, '"':xs) -> (Just(StringConstant(stringConstant)), xs)
    (_, []) -> (Nothing, '"':rest) --parse error, didn't end with " 
getStringConstant(input) = (Nothing, input)


data Identifier = Identifier(String) deriving Show
getIdentifier :: String -> (Maybe(Identifier), String)
getIdentifier(input) = case span isIdentifierChar input of
    ([], x) -> (Nothing, x)
    (a, b) -> (Just(Identifier(a)), b)

isIdentifierChar :: Char -> Bool
isIdentifierChar(x) = isAlphaNum(x) || (x == '_')

data Token = TokensKeyword(Keyword) | 
    TokensSymbol(Symbol) | 
    TokensInteger(IntegerConstant) | 
    TokensString(StringConstant) | 
    TokensIdentifier(Identifier) deriving Show

getLexemeToken :: ( (String -> (Maybe(a), String)) , a -> Token) -> (String -> (Maybe(Token), String))
getLexemeToken(getLexeme, toToken) = 
    \input -> let (a, b) = getLexeme(input) in
        ((fmap toToken a), b)

getKeywordToken = getLexemeToken(getKeyword, TokensKeyword)
getSymbolToken = getLexemeToken(getSymbol, TokensSymbol)
getIntegerToken = getLexemeToken(getInteger, TokensInteger)
getStringToken = getLexemeToken(getStringConstant, TokensString)
getIdentifierToken = getLexemeToken(getIdentifier, TokensIdentifier)


lexCombine :: (String -> (Maybe(Token), String)) -> (String ->(Maybe(Token), String)) -> (String ->(Maybe(Token), String))
lexCombine f g input = 
    case g(input) of
        (Just a, b) -> (Just a, b)
        (Nothing, b) -> case f(b) of
                            (Just a, rest) -> (Just a, rest)
                            (Nothing, rest) -> (Nothing, rest)

                            
getToken :: String -> (Maybe Token, String)
getToken = 
    getIdentifierToken `lexCombine` 
    getStringToken `lexCombine` 
    getIntegerToken `lexCombine` 
    getSymbolToken `lexCombine` 
    getKeywordToken 

getTokens :: String -> [Token]
getTokens(input) = fst(getTokensHelper([], input)) 

getTokensHelper :: ([Token], String) -> ([Token], String)
getTokensHelper(a, input) = case getToken(flushWhiteSpace(input)) of
    (Just token, rest) -> getTokensHelper(a ++ [token], rest)
    (Nothing, rest) -> (a, rest)

flushWhiteSpace = dropWhile isSpace


data Parser a = Parser ([Token] -> Maybe(a, [Token]))

instance Monad Parser where
    (Parser f) >>= (Parser g) = Parser $ \tokens -> case f(tokens) of
                                    Nothing -> Nothing
                                    (a, rest) -> g(a)(rest) 
    return a = Parser $ \tokens-> Just(a, tokens) 

instance Alternative Parser where
    empty = Parser $ \tokens -> Nothing
    (Parser f) <|> (Parser g) = Parser $ \tokens ->
                                    case f(tokens) of
                                        Nothing -> case g(tokens) of
                                                        Nothing -> Nothing
                                                        x -> x
                                        x -> x


parseKeyword :: Parser Keyword
parseKeyword = Parser $ 
    \tokens -> case tokens of
        TokensKeyword(x):rest -> Just(x, rest)
        xs -> Nothing

oneOfKeywords :: [Keyword] -> Parser Keyword
oneOfKeywords keywords = do keyword <- parseKeyword
                            if keyword `elem` keywords then
                                return keyword
                            else empty

sinkKeyword :: Keyword -> Parser ()
sinkKeyword keyword = do oneOfKeywords [keyword]
                         return ()

parseSymbol :: Parser Symbol
parseSymbol = Parser $ 
    \tokens -> case tokens of
        TokensSymbol(x):rest -> Just(x, rest)
        xs -> Nothing

oneOfSymbols :: [Symbol] -> Parser Symbol
oneOfSymbols symbols = do symbol <- parseSymbol
                          if symbol `elem` symbols then
                              return symbol
                          else empty

sinkSymbol :: Symbol -> Parser ()
sinkSymbol = do oneOfSymbols [Symbol]
                return ()

parseIdentifier :: Parser Identifier
parseIdentifier = Parser $ 
    \tokens -> case tokens of
        TokensIdentifier(x):rest -> Just(x, rest)
        xs -> Nothing

data JackClass = JackClass JackClassName [JackClassVarDec] [JackSubroutineDec]
parseClass :: Parser JackClass
parseClass = do sinkKeyword Class
                className <- parseClassName
                sinkSymbol LeftBracket
                jackClassVarDecs <- parseClassVarDeclarations
                jackSubroutineDeclarations <- parseSubroutineDeclarations
                sinkSymbol RightBracket 
                return(JackClass className jackClassVarDecs jackSubroutineDeclarations)

jackKeyword :: Keyword -> a -> Parser a
jackKeyword keyword a = fmap (\_ -> a) sinkKeyword keyword 

data JackStaticOrField = JackStatic | JackField
parseStaticOrField :: Parser JackStaticOrField
parseStaticOrField = parseStatic <|> parseField
parseStatic :: Parser JackStaticOrField
parseStatic = Static `jackKeyword` JackStatic 
parseField :: Parser JackStaticOrField
parseField = Field `jackKeyword` JackField


data JackClassVarDec = JackClassVarDec JackStaticOrField JackType JackVarName [JackVarName]
parseClassVarDec :: Parser JackClassVarDec
parseClassVarDec = do lifetime <- parseStaticOrField
                      jackType <- parseType
                      name <- parseVarName
                      names <- parseVarNames
                      return(JackClassVarDec lifetime jackType name names)

parseClassVarDeclarations :: Parser [JackClassVarDec]
parseClassVarDeclarations = many parseClassVarDec

data JackType = JackTypeInt | JackTypeChar | JackTypeBoolean | JackTypeClassName JackClassName
parseType :: Parser JackType
parseType = parseInt <|> parseChar <|> parseBoolean <|> parseTypeClassName 
parseInt = jackKeyword KeywordInt JackTypeInt
parseChar = jackKeyword KeywordChar JackTypeChar
parseBoolean = jackKeyword Boolean JackTypeBoolean
parseTypeClassName = fmap JackTypeClassName parseClassName

data JackSubroutineKind = JackConstructor | JackFunction | JackMethod
parseSubroutineKind :: Parser JackSubroutineKind
parseSubroutineKind = parseConstructor <|> parseFunction <|> parseMethod
parseConstructor = jackKeyword Constructor JackConstructor
parseFunction = jackKeyword Function JackFunction
parseMethod = jackKeyword Method JackMethod

data JackReturnType = JackVoid | JackReturn JackType
parseReturnType :: Parser JackReturnType
parseReturnType = parseVoid <|> parseReturn
parseVoid = jackKeyword Void JackVoid
parseReturn = fmap JackReturn parseType

data JackSubroutineDec = JackSubroutineDec JackSubroutineKind JackReturnType JackSubroutineName JackParameterList JackSubroutineBody
parseSubroutineDec ::  Parser JackSubroutineDec
parseSubroutineDec = do subroutineKind <- parseSubroutineKind
                        typeKind <- parseReturnType
                        subName <- parseSubroutineName
                        parseSymbol LeftParen 
                        parameters <- parseParameterList
                        parseSymbol RightParen
                        body <- parseSubroutineBody
                        return(JackSubroutineDec subroutineKind typeKind subName parameters body)

parseSubroutineDeclarations :: Parser [JackSubroutineDec]
parseSubroutineDeclarations = many parseSubroutineDec

data JackParameterList = JackParameterList [(JackType, JackVarName)]

parseParameterList :: Parser JackParameterList
parseParameterList = parseFilledParameterList <|> return (JackParameterList [])

parseFilledParameterList :: Parser JackParameterList
parseFilledParameterList = do firstType <- parseType
                              firstVar <- parseVarName
                              rest <- parseVarTypesAndNames
                              return(JackParameterList( (firstType, firstVar):rest) )

data JackSubroutineBody = JackSubroutineBody [JackVarDec] JackStatements
parseSubroutineBody :: Parser JackSubroutineBody 
parseSubroutineBody = do sinkSymbol LeftBracket
                         variableDeclarations <- parseVariableDeclarations
                         statements <- parseStatements
                         sinkSymbol RightBracket
                         return(JackSubroutineBody variableDeclarations statements)

data JackVarDec = JackVarDec JackType JackVarName [JackVarName]
parseVariableDeclaration :: Parser JackVarDec
parseVariableDeclaration = do sinkKeyword Var
                              typeName <- parseType
                              varName <- parseVarName
                              varNames <- parseVarNames
                              return(JackVarDec typeName varName varNames) 

parseVariableDeclarations :: Parser [JackVarDec]
parseVariableDeclarations = many parseVariableDeclaration

parseCommaVarName :: Parser JackVarName
parseCommaVarName = sinkSymbol Comma >> parseVarName

parseVarNames :: Parser [JackVarName]
parseVarNames = many parseCommaVarName

parseVarTypesAndNames :: Parser [(JackType, JackVarName)]
parseVarTypesAndNames = many parseCommaTypeVarName

parseCommaTypeVarName :: Parser (JackType, JackVarName)
parseCommaTypeVarName = do sinkSymbol Comma
                           typeName <- parseType
                           varName <- parseVarName
                           return(typeName, varName)

type JackVarName = Identifier
parseVarName :: Parser JackVarName
parseVarName = parseIdentifier

type JackClassName = Identifier
parseClassName :: Parser JackClassName
parseClassName = parseIdentifier

type JackSubroutineName = Identifier
parseSubroutineName :: Parser JackSubroutineName
parseSubroutineName = parseIdentifier

data JackStatements = JackStatements [JackStatement]
data JackStatement = StateLet LetStatement | StateIf IfStatement |
    StateWhile WhileStatement |
    StateDo DoStatement |
    StateReturn ReturnStatement 

data LetStatement =  LetStatement JackVarName (Maybe JackExpression) JackExpression
parseLetStatement :: [Token] -> (JackStatement, [Token])


data IfStatement = IfStatement JackExpression JackStatements (Maybe JackStatements)
data WhileStatement =  WhileStatement JackExpression JackStatements
data DoStatement =  DoStatement JackSubroutineCall
data ReturnStatement = ReturnStatement (Maybe JackExpression) 

data JackExpression = JackExpression JackTerm [(JackOp, JackTerm)]
data JackArray = JackArray JackVarName JackExpression
data JackTerm = JackIntegerConstant Integer |
  JackStringConstant String |
  JackTermKeyword JackTermKeywordConstant |
  JackTermName JackVarName |
  JackTermArray JackArray |
  JackTermSubroutine JackSubroutineCall |
  JackTermCalled JackExpression |
  JackOperateWith JackUnaryOp JackTerm

data JackSubroutineCall = JackStaticCall JackSubroutineName JackExpressionList |
    JackClassCall JackClassName JackSubroutineName JackExpressionList |
    JackVarCall JackVarName JackSubroutineName JackExpressionList

data JackExpressionList = JackExpressionList (Maybe(JackExpression, [JackExpression]))

data JackOp = JackPlus | JackMinus | JackTimes | JackDivide | JackAnd | JackBar | JackLess | JackGreater | JackEqual
data JackUnaryOp = JackUnaryMinus | JackTilde
data JackTermKeywordConstant = JackTrue | JackFalse | JackNull | JackThis 


