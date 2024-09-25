import Ast
import Parser(parse)
import Data.Map(Map, empty, insert, (!), union, size)
import Data.Maybe(fromJust)
import Data.Bifunctor(first)
import System.Environment(getArgs)

emitList :: String -> (Int, Map String Int) -> [AST] -> (String, (Int, Map String Int))
emitList text info = foldr (\ x rec -> first (++ text ++ fst rec) (emit (snd rec) x)) ("", info)

emit :: (Int, Map String Int) -> AST -> (String, (Int, Map String Int))
emit inf0@(l0, d0) ast = case ast of
    Return r    -> emit inf0 r
    Block bs    -> emitList "" inf0 bs
    Call n args -> first 
        (\ params -> 
            (if 0 < onStack && odd onStack then "sub rsp, 8\n" else "") 
            ++ params
            ++ unlines (zipWith (\ _ -> (++) "pop ") (take 6 args) fnCallRegs)
            ++ "call " ++ n ++ "\n"
            ++ if 0 < onStack then "add rsp, " ++ showAlignedBits onStack ++ "\n" else ""
        ) 
        (emitList "push rax\n" inf0 (reverse args))
        where onStack = length args - 6

    ast -> first unlines $ case ast of 
        IdNode n     -> (["mov rax, [rbp" ++ offset (d0 ! n) ++ "]"], inf0)
        NumNode n    -> (["mov rax, " ++ show n], inf0)
        Not u        -> let (ru, inf1) = emit inf0 u in
                        ( [ru ++ "cmp rax, 0", "cmove rax, 1", "cmovne rax, 0"], inf1 )
        Add a b      -> let (ra, inf1) = emit inf0 a in
                        let (rb, inf2) = emit inf1 b in
                        ( [ra ++ "push rax", rb ++ "pop rdi", "add rax, rdi"], inf2 )
        Sub a b      -> let (ra, inf1) = emit inf0 a in
                        let (rb, inf2) = emit inf1 b in
                        ( [ra ++ "push rax", rb ++ "mov rdi, rax", "pop rax", "sub rax, rdi"], inf2 )
        Mul a b      -> let (ra, inf1) = emit inf0 a in
                        let (rb, inf2) = emit inf1 b in
                        ( [ra ++ "push rax", rb ++ "pop rdi", "mul rdi"], inf2 )
        Div a b      -> let (ra, inf1) = emit inf0 a in
                        let (rb, inf2) = emit inf1 b in
                        ( [ra ++ "push rax", rb ++ "mov rdi, rax", "pop rax", "div rdi"], inf2 )
        Equal a b    -> let (ra, inf1) = emit inf0 a in
                        let (rb, inf2) = emit inf1 b in
                        ( [ra ++ "push rax", rb ++ "pop rdi", "xor rsi, rsi", "mov rcx, 1", "cmp rax, rdi", "cmove rax, rcx", "cmovne rax, rsi"], inf2 )
        NotEqual a b -> let (ra, inf1) = emit inf0 a in
                        let (rb, inf2) = emit inf1 b in
                        ( [ra ++ "push rax", rb ++ "pop rdi", "xor rsi, rsi", "mov rcx, 1", "cmp rax, rdi", "cmove rax, rsi", "cmovne rax, rcx"], inf2 )
        If p c a     -> let (rp, (l1, d1)) = emit inf0 p in
                        let (rc, (l2, d2)) = emit (l1 + 1, d1) c in
                        let (ra, inf3)     = emit (l2 + 1, d2) a in
                        ( [rp ++ "cmp rax, 0", "je " ++ label l1, rc ++ "jmp " ++ label l2, label l1 ++ ":", ra ++ label l2 ++ ":"], inf3 )
        While p b    -> let (rp, (l1, d1)) = emit inf0 p in
                        let (rb, (l2, d2)) = emit (l1 + 1, d1) b in
                        ( [label l1 ++ ":", rp ++ "cmp rax, 0", "je " ++ label l2, rb ++ "jmp " ++ label l1, label l2 ++ ":"], (l2 + 1, d2) )
        FnDef n p b  -> let (rb, (l1, _)) = emit (l0, vars) b in
                        ( [n ++ ":", "push rbp", "mov rbp, rsp", "sub rsp, " ++ showAlignedBits (length $ onRegs ++ fVars)] 
                          ++ zipWith (\ n r -> concat ["mov [rbp", show (vars ! n),"], ", r]) onRegs fnCallRegs
                          ++ [rb]
                          ++ if n == "_start" then ["mov rdi, rax", "mov rax, 60", "syscall"] else ["leave", "ret"]
                        , (l1, d0) )
                        where 
                            fVars             = newVariables b
                            (onRegs, onStack) = splitAt 6 p
                            makeDic c         = foldr (\ x rec n -> insert x (n * c) (rec (n + 1))) (const empty)
                            vars              = union (makeDic (-8) (onRegs ++ fVars) 1) (makeDic 8 onStack (1 + length onStack `mod` 2))
        VarDef n v   -> let (rv, inf1) = emit inf0 v in
                        ( [rv, "mov [rbp" ++ offset (d0 ! n) ++ "], rax"], inf1)
        Assign n v   -> let (rv, inf1) = emit inf0 v in
                        ( [rv, "mov [rbp" ++ offset (d0 ! n) ++ "], rax"], inf1)
    where
        fnCallRegs = ["rdi","rsi","rdx","rcx","r8","r9"]
        showAlignedBits b = show ((b + b `mod` 2) * 8)
        offset n = (if n >= 0 then "+" else "") ++ show n



label :: Int -> String
label = (".L" ++) . show 

compile :: String -> String
compile s = unlines ["global _start", fromJust (parse s >>= Just . fst . emit (0, empty) . fst)]

main :: IO ()
main = do {
    args <- getArgs;
    file <- readFile (head args);
    writeFile (takeWhile (/= '.') (head args) ++ ".asm") (compile file)
}

