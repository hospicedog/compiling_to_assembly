module Ast(AST(IdNode, NumNode, Not, Add, Sub, Mul, Div, Equal, NotEqual, Call, Return, Block, If, While, FnDef, VarDef, Assign), newVariables) where

data AST
    = IdNode String
    | NumNode Int
    | Not AST
    | Add AST AST
    | Sub AST AST
    | Mul AST AST
    | Div AST AST
    | Equal AST AST
    | NotEqual AST AST
    | Return AST
    | Block [AST]
    | Call String [AST] 
    | If AST AST AST
    | While AST AST
    | FnDef String [String] AST
    | VarDef String AST
    | Assign String AST

instance Show AST where
    show = showAST 0 
        where 
            showAST :: Int -> AST -> String
            showAST n ast = indent n ++ case ast of
                IdNode id            -> "id: " ++ id
                NumNode num          -> "num: " ++ show num
                Not ast              -> "NOT (\n" ++ showAST (n+1) ast ++ nindent n ++ ")"
                Add l r              -> "ADD" ++ binNode n l r
                Sub l r              -> "SUB" ++ binNode n l r
                Mul l r              -> "MUL" ++ binNode n l r
                Div l r              -> "DIV" ++ binNode n l r
                Equal l r            -> "EQU" ++ binNode n l r
                NotEqual l r         -> "NEQ" ++ binNode n l r
                Return ast           -> "Return (\n" ++ showAST (n+1) ast ++ nindent n ++ ")"
                Block asts           -> "Block: [\n" ++ foldr (\ x r -> showAST (n+1) x ++ ",\n" ++ r) "" asts ++ nindent n ++ "]"
                Call name args       -> "Call " ++ name ++ ": [\n" ++ foldr (\ x r -> showAST (n+1) x ++ ",\n" ++ r) "" args ++ nindent n ++ "]"
                If p c a             -> "If (\n" ++ showAST (n+1) p ++ nindent n ++ ") THEN (\n" ++ showAST (n+1) c ++ nindent n ++ ") ELSE (\n" ++ showAST (n+1) a ++ nindent n ++ ")"
                While p b            -> "While" ++ binNode n p b
                FnDef name args body -> "Funcion: " ++ name ++ ", Args: " ++ show args ++ nindent n ++ "(\n" ++ showAST (n+1) body ++ nindent n ++ ")"
                VarDef name val      -> "let " ++ strNode n name val
                Assign name val      -> strNode n name val
                where indent        = concat . flip replicate "  "
                      nindent       = ("\n" ++) . indent
                      binNode n l r = " (\n" ++ showAST (n+1) l ++ ",\n" ++ showAST (n+1) r ++ nindent n ++ ")"
                      strNode n l r = l ++ " = {\n" ++ showAST (n+1) r ++ nindent n ++ "}"

newVariables :: AST -> [String]
newVariables ast = case ast of
    FnDef _ vs b -> vs ++ newVariables b
    VarDef v a   -> v : newVariables a
    IdNode _     -> []
    NumNode _    -> []
    Not a        -> newVariables a
    Add l r      -> concatMap newVariables [l, r]
    Sub l r      -> concatMap newVariables [l, r]
    Mul l r      -> concatMap newVariables [l, r]
    Div l r      -> concatMap newVariables [l, r]
    Equal l r    -> concatMap newVariables [l, r]
    NotEqual l r -> concatMap newVariables [l, r]
    Return a     -> newVariables a
    Block as     -> concatMap newVariables as
    Call _ as    -> concatMap newVariables as
    If p c a     -> concatMap newVariables [p, c, a]
    While p b    -> concatMap newVariables [p, b]
    Assign _ val -> newVariables val

