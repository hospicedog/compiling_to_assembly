module Parser(Source, ParseResult, Parser, parse) where

import Control.Applicative((<|>))
import Data.Char(isSpace, isDigit, isAlphaNum, isAlpha, ord)
import Data.Bifunctor(first)
import Ast(AST(IdNode, NumNode, Not, Add, Sub, Mul, Div, Equal, NotEqual, Call, Return, Block, If, While, FnDef, VarDef, Assign))

type Source = String
type ParseResult a = (a, Source)
type Parser a = Source -> Maybe (ParseResult a)

matchChar :: (Char -> Bool) -> Parser Char
matchChar p src = case src of
    [] -> Nothing
    (s:ss) -> if p s then Just (s, ss) else Nothing

matchLiteral :: String -> Parser String
matchLiteral str src = if isPrefix str src then Just (splitAt (length str) src) else Nothing
    where isPrefix = foldr (\ x rec src -> not (null src) && x == head src && rec (tail src)) (const True)

matchPred :: Parser a -> (a -> Bool) -> Parser a
matchPred parser pred src | null src = Nothing
                          | otherwise = do { (val, src) <- parser src; if pred val then Just (val, src) else Nothing }

eof :: Parser Char
eof src = if null src then Just ('\0', []) else Nothing

choice :: Parser a -> Parser a -> Parser a
choice parser other src = parser src <|> other src

multiChoice :: [Parser a] -> Parser a
multiChoice choices src = foldr1 (\ x r -> if null (x src) then r else x) choices src

bind :: Parser a -> (a -> Parser b) -> Parser b
bind parser callback src = parser src >>= uncurry callback

both :: Parser a -> Parser b -> Parser (a,b)
both left right = bind left (\ l -> bind right (\ r src -> Just ((l, r), src)))

keepRight :: Parser a -> Parser b -> Parser b
keepRight left right src = left src >>= right . snd

keepLeft :: Parser a -> Parser b -> Parser a
keepLeft left right src = do {
    (res, src) <- left src;
    (_, src')  <- right src;
    Just (res, src')
}

keepMid :: Parser a -> Parser b -> Parser c -> Parser b
keepMid l m r = keepRight l (keepLeft m r)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore parser src = 
    if null (parser src) then
        Just ([], src)
    else do {
        (v, src')  <- parser src;
        (vs, rest) <- zeroOrMore parser src';
        Just (v:vs, rest)
    }

oneOrMore :: Parser a -> Parser [a]
oneOrMore parser src = do {
    (r, src')  <- parser src;
    (rs, rest) <- zeroOrMore parser src';
    Just (r:rs, rest)
}

prepend :: Parser a -> Parser [a] -> Parser [a]
prepend headParser tailParser = bind headParser (\ h -> mapFst (h:) tailParser)

mapFst :: (a -> b) -> Parser a -> Parser b
mapFst f = (.) $ fmap (first f)

lineComment src = do {
    (slashes, src) <- matchLiteral "//" src;
    (comment, src) <- zeroOrMore (matchPred (matchChar (const True)) (/= '\n')) src;
    (end, src) <- choice (matchChar (== '\n')) eof src;
    Just ("//" ++ comment, src)
}

multilineComment src = do {
    (start, src) <- matchLiteral "/*" src;
    (comment, src) <- zeroOrMore (matchPred (choice (matchLiteral "*/") (mapFst (:[]) $ matchChar (const True))) (/= "*/")) src;
    (end, src) <- matchLiteral "*/" src;
    Just ("/*" ++ concat comment ++ "*/", src)
}

whitespace = mapFst (:[]) $ matchChar isSpace
comment    = choice lineComment multilineComment
ignored    = mapFst concat $ zeroOrMore (choice whitespace comment)

token t = keepRight ignored (matchLiteral t)

functionToken = token "function"
ifToken       = token "if"
elseToken     = token "else"
returnToken   = token "return"
letToken      = token "let"
assignToken   = token "="
whileToken    = token "while"
notToken      = token "!"
lessToken     = token "<"
lEqToken      = token "<="
greaterToken  = token ">"
gEqToken      = token ">="
equalToken    = token "=="
notEqualToken = token "!="
plus          = token "+"
minus         = token "-"
asterisk      = token "*"
slash         = token "/"
comma         = token ","
semicolon     = token ";"
leftParen     = token "("
rightParen    = token ")"
leftBrace     = token "{"
rightBrace    = token "}"

number = mapFst (NumNode . parseDigits) digits
    where digits         = keepRight ignored (oneOrMore (matchChar isDigit))
          parseDigits ns = foldr (\ x rec n -> (ord x - ord '0') * (10 ^ n) + rec (n-1)) (const 0) ns (length ns - 1)

idToken  = keepRight ignored (mapFst (uncurry (:)) $ both (matchChar (\ c -> c == '_' || isAlpha c)) (zeroOrMore (matchChar (\ c -> c == '_' || isAlphaNum c))))
variable = mapFst IdNode idToken

args  = choice (prepend expression (zeroOrMore (keepRight comma expression))) (\ src -> Just ([], src))
call  = bind idToken (\ fName -> mapFst (Call fName) $ keepMid leftParen args rightParen)
atom  = multiChoice [call, variable, number, keepMid leftParen expression rightParen]
unary = choice (mapFst Not $ keepRight notToken atom) atom

infixOp :: Parser AST -> [(Parser Source, AST -> AST -> AST)] -> Parser AST
infixOp term ops = choice
    (bind term
        (\ leftOp -> bind (both (multiChoice $ map fst ops) (choice (infixOp term ops) term))
            (\ (op, rightOp) src -> 
                Just (foldr 
                        (\ x rec src -> if not (null (fst x src)) then snd x else rec src) 
                        (error "No infix match") 
                        ops
                        op 
                      leftOp rightOp,
                     src)
    )))
    term

multiplication = infixOp unary [(asterisk, Mul), (slash, Div)]
addition       = infixOp multiplication [(plus, Add), (minus, Sub)]
expression     = infixOp addition [(equalToken, Equal), (notEqualToken, NotEqual)]

returnStm     = mapFst Return $ keepMid returnToken expression semicolon
expressionStm = keepLeft expression semicolon
ifStm         = bind (keepRight ifToken (keepMid leftParen expression rightParen))
    (\ p -> bind (both statement (keepRight elseToken statement))
        (\ (c, a) src -> Just (If p c a, src)))
whileStm      = mapFst (uncurry While) $ both (keepMid (both whileToken leftParen) expression rightParen) statement
variableStm   = mapFst (uncurry VarDef) $ both (keepRight letToken idToken) (keepMid assignToken expression semicolon)
assignStm     = mapFst (uncurry Assign) $ both idToken (keepMid assignToken expression semicolon)
blockStm      = mapFst Block $ keepMid leftBrace (zeroOrMore statement) rightBrace
parameters    = choice (prepend idToken (zeroOrMore (keepRight comma idToken))) (\ src -> Just ([], src))
functionStm   = mapFst (\ ((name, params), body) -> FnDef name params body) $ both (both (keepRight functionToken idToken) (keepMid leftParen parameters rightParen)) blockStm
statement     = multiChoice [returnStm, ifStm, whileStm, variableStm, assignStm, blockStm, functionStm, expressionStm]

parse :: Parser AST
parse = mapFst Block $ keepMid ignored (zeroOrMore statement) ignored
