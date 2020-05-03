module StaticCheck where

import qualified Data.Map as Map
import Data.Maybe
import Data.List

import AbsGrammar
import ErrM

import Control.Monad.Reader 
import Control.Monad.Except
import Control.Monad.Trans.Except

type Var = Ident
type Fn = Ident
type Depth = Int

data PrimitiveType = IntType | StringType | BoolType | VoidType deriving Eq

instance Show PrimitiveType where
    show t = case t of
        IntType -> "int"
        StringType -> "string"
        BoolType -> "bool"
        VoidType -> "void"

type FnType = (PrimitiveType, [PrimitiveType])
mainFnSig :: FnType
mainFnSig = (IntType, [])

showFnType :: FnType -> String
showFnType (retType, argTypes) =
    show retType ++ " (" ++ argTypesStr ++ ")" where 
        argTypesStr = intercalate "," $ map show argTypes

-- both statements and expressions have return type
-- statement return types correspond to possible returns inside function body
-- Just VoidType statement return type corresponds to return with no expression
-- Nothing statement return type corresponds to statement that does not end function call
-- expression returns are ordinary types
-- 
-- for example:
-- int test() {
--     return 5 ;
-- }
--
-- int main() {
--     test() ; // as statement it returns Nothing but as expression it returns int
--     { // as statement it returns integer because there is return in this block
--         return 0 ;
--     }
-- }
type StmtReturnType = Maybe PrimitiveType

type VTEnv = Map.Map Var (PrimitiveType, Depth)
type FTEnv = Map.Map Fn (FnType, Depth)

data SCError = UndeclaredFn
             | UndeclaredVar 
             | DoubleVarDecl Var Position
             | DoubleFnDecl Fn Position
             | MismatchedArgCount
             | WrongFnArgType
             | WrongAssignmentType
             | InterruptOutsideLoop
             | WrongWhileCondType
             | WrongIfCondType
             | NonVoidReturnInVoidFn
             | VoidReturnInNonVoidFn
             | BlockAmbiguousReturnType
             | IfAmbiguousReturnType
             | WrongReturnType
             | WrongMainFnSig FnType
             | NoMainFn
             | VoidTypeToPrint
             | NotIntUsedWithIntOp
             | NotBoolUsedWithBoolOp
             | VoidExprInNonVoidReturn

instance Show SCError where
    show (WrongMainFnSig sig) =
        "Wrong signature of the main function: " ++ showFnType sig ++ " instead of " ++ showFnType mainFnSig
    show NoMainFn = "No main function declared"
    show (DoubleVarDecl var pos) =
        "Double declaration of variable with name '" ++ showIdent var ++ "' at " ++ showPos pos 
    show (DoubleFnDecl fn pos) =
        "Double declaration of function with name '" ++ showIdent fn ++ "' at " ++ showPos pos 
    -- show (UndeclaredVar var pos) =
    --     "Reference to undeclared variable with name '" ++ showIdent var ++ "' at " ++ showPos pos 
    -- show (UndeclaredFn fn pos) =
    --     "Reference to undeclared function with name '" ++ showIdent fn ++ "' at " ++ showPos pos

data TEnv = Env { venv :: VTEnv, fenv :: FTEnv, depth :: Depth, loop :: Bool } deriving Show
emptyTEnv = Env Map.empty Map.empty 0 False

type SCResult = ExceptT SCError IO
type SC = ReaderT TEnv SCResult

readType :: Type Position -> PrimitiveType
readType (Int _) = IntType
readType (Bool _) = BoolType
readType (Str _) = StringType
readType (Void _) = VoidType
-- error handling?

readFnType :: Type Position -> FnType
readFnType (Fun _ retType argTypes) = 
    (readType retType, map readType argTypes)

updateVTEnv :: Var -> PrimitiveType -> TEnv -> TEnv
updateVTEnv v t (Env venv fenv d l) =
    Env (Map.insert v (t, d) venv) fenv d l 

updateFTEnv :: Fn -> FnType -> TEnv -> TEnv
updateFTEnv f t (Env venv fenv d l) =
    Env venv (Map.insert f (t, d) fenv) d l

incrDepth :: TEnv -> TEnv
incrDepth (Env venv fenv d l) = Env venv fenv (d+1) l

setLoopFalse :: TEnv -> TEnv
setLoopFalse (Env venv fenv d _) = Env venv fenv d False

setLoopTrue :: TEnv -> TEnv
setLoopTrue (Env venv fenv d _) = Env venv fenv d True

argToType :: Arg Position -> Type Position
argToType (ValArg _ t _) = t
argToType (RefArg _ t _) = t

argToVarDecl :: Arg Position -> Decl Position
argToVarDecl (ValArg p t i) = VarDecl p t i
argToVarDecl (RefArg p t i) = VarDecl p t i

checkVar :: Var -> SC PrimitiveType
checkVar varIdent = do
    env <- ask
    case Map.lookup varIdent (venv env) of
        Nothing -> lift $ throwError UndeclaredVar
        Just (t, _) -> return t

checkFn :: Fn -> SC FnType
checkFn fnIdent = do
    env <- ask
    case Map.lookup fnIdent (fenv env) of
        Nothing -> lift $ throwError UndeclaredFn
        Just (t, _) -> return t

checkApp :: Fn -> [Expr Position] -> SC PrimitiveType
checkApp fnIdent args = do
    (retType, argTargetTypes) <- checkFn fnIdent
    argTypes <- mapM checkExpr args
    if not (length argTypes == length argTargetTypes)
        then lift $ throwError MismatchedArgCount
        else if not (argTypes == argTargetTypes)
            then lift $ throwError WrongFnArgType
            else return retType

checkIntOp :: Expr Position -> SC ()
checkIntOp expr = do
    exprType <- checkExpr expr
    if not (exprType == IntType)
        then lift $ throwError NotIntUsedWithIntOp
        else return ()

checkBoolOp :: Expr Position -> SC ()
checkBoolOp expr = do
    exprType <- checkExpr expr
    if not (exprType == BoolType)
        then lift $ throwError NotBoolUsedWithBoolOp
        else return ()

checkExpr :: Expr Position -> SC PrimitiveType
checkExpr expr = case expr of
    EVar _ varIdent -> checkVar varIdent
    ELitInt _ _ -> return IntType
    ELitTrue _ -> return BoolType
    ELitFalse _ -> return BoolType
    EApp _ fnIdent args -> checkApp fnIdent args
    EString _ _ -> return StringType
    Neg _ expr -> checkIntOp expr >> return IntType
    Not _ expr -> checkBoolOp expr >> return BoolType
    EMul _ expr1 _ expr2 -> checkIntOp expr1 >> checkIntOp expr2 >> return IntType
    EAdd _ expr1 _ expr2 -> checkIntOp expr1 >> checkIntOp expr2 >> return IntType
    ERel _ expr1 _ expr2 -> checkIntOp expr1 >> checkIntOp expr2 >> return BoolType
    EAnd _ expr1 expr2 -> checkBoolOp expr1 >> checkBoolOp expr2 >> return BoolType
    EOr _ expr1 expr2 -> checkBoolOp expr1 >> checkBoolOp expr2 >> return BoolType

checkAss :: Var -> Expr Position -> SC ()
checkAss varIdent expr = do
    varType <- checkVar varIdent
    exprType <- checkExpr expr
    if not (varType == exprType)
        then lift $ throwError WrongAssignmentType
        else return ()

checkCond :: Expr Position -> Stmt Position -> SC StmtReturnType
checkCond expr stmt = do
    exprType <- checkExpr expr
    if not (exprType == BoolType)
        then lift $ throwError WrongIfCondType
        else checkStmt stmt

checkCondElse :: Expr Position -> Stmt Position -> Stmt Position -> SC StmtReturnType
checkCondElse expr stmt1 stmt2 = do
    exprType <- checkExpr expr
    if not (exprType == BoolType)
        then lift $ throwError WrongIfCondType
        else do
            retType1 <- checkStmt stmt1
            retType2 <- checkStmt stmt2
            case (retType1, retType2) of
                (Nothing, _) -> return retType2
                (_, Nothing) -> return retType1
                _ -> if retType1 == retType2
                    then return retType1
                    else lift $ throwError IfAmbiguousReturnType

checkWhile :: Expr Position -> Stmt Position -> SC StmtReturnType
checkWhile expr stmt = do
    exprType <- checkExpr expr
    if not (exprType == BoolType)
        then lift $ throwError WrongWhileCondType
        else do
            env <- ask    
            local (const $ setLoopTrue env) (checkStmt stmt)

checkInterrupt :: Inter Position -> SC ()
checkInterrupt _ = do
    env <- ask
    if loop env
        then return ()
        else lift $ throwError InterruptOutsideLoop

checkPrint :: Expr Position -> SC ()
checkPrint expr = do
    exprType <- checkExpr expr
    if exprType == VoidType
        then lift $ throwError VoidTypeToPrint
        else return ()

checkRet :: Expr Position -> SC StmtReturnType
checkRet expr = do
    exprType <- checkExpr expr
    if exprType == VoidType
        then lift $ throwError VoidExprInNonVoidReturn
        else return $ Just exprType

checkStmt :: Stmt Position -> SC StmtReturnType
checkStmt stmt = case stmt of
    Empty _ -> return Nothing
    BStmt _ block -> do
        env <- ask  
        local (const $ incrDepth env) (checkBlock block)
    Ass _ varIdent expr -> checkAss varIdent expr >> return Nothing
    Ret _ expr -> checkRet expr
    VRet _ -> return $ Just VoidType
    Cond _ expr stmt -> checkCond expr stmt
    CondElse _ expr stmt1 stmt2 -> checkCondElse expr stmt1 stmt2
    While _ expr stmt -> checkWhile expr stmt
    Interrupt _ inter -> checkInterrupt inter >> return Nothing
    SExp _ expr -> checkExpr expr >> return Nothing
    Print _ expr -> checkPrint expr >> return Nothing

checkBlock :: Block Position -> SC StmtReturnType
checkBlock (Blk _ decls stmts) = do
    env <- checkDecls decls
    stmtReturnsMaybe <- local (const env) (mapM checkStmt stmts)
    let stmtReturns = filter (not . isNothing) stmtReturnsMaybe
    if null stmtReturns
        then return Nothing
        else if and $ map (== head stmtReturns) (tail stmtReturns)
            then return $ head stmtReturns
            else lift $ throwError BlockAmbiguousReturnType 
    
checkRetType :: PrimitiveType -> PrimitiveType -> SC ()
checkRetType VoidType retType = 
    if retType == VoidType then return () else lift $ throwError NonVoidReturnInVoidFn

checkRetType targetRetType VoidType = lift $ throwError VoidReturnInNonVoidFn

checkRetType targetRetType retType =
    if retType == targetRetType then return () else lift $ throwError WrongReturnType

checkDecl :: Decl Position -> SC TEnv
checkDecl (VarDecl pos varType varIdent) = do
    env <- ask
    case Map.lookup varIdent (venv env) of
        Nothing -> return $ updateVTEnv varIdent (readType varType) env
        Just (t, dp) -> if dp == depth env
            then lift $ throwError $ DoubleVarDecl varIdent pos
            else return $ updateVTEnv varIdent (readType varType) env
    
checkDecl (FnDecl pos retType fnIdent args block) = do
    env <- ask
    let fnType = Fun pos retType (map argToType args)
    envWithFnName <- case Map.lookup fnIdent (fenv env) of
        Nothing -> return $ updateFTEnv fnIdent (readFnType fnType) env
        Just (sig, dp) -> if dp == depth env
            then lift $ throwError $ DoubleFnDecl fnIdent pos
            else return $ updateFTEnv fnIdent (readFnType fnType) env
    let localEnvInit = setLoopFalse $ incrDepth envWithFnName
    localEnv <- local (const localEnvInit) (checkDecls $ map argToVarDecl args)
    blockRetType <- local (const localEnv) (checkBlock block)
    checkRetType (readType retType) (fromMaybe VoidType blockRetType)
    return envWithFnName
    

checkDecls :: [Decl Position] -> SC TEnv
checkDecls [] = ask
checkDecls (decl:decls) = do
    env <- checkDecl decl 
    local (const env) (checkDecls decls)

checkProg :: Program Position -> SC ()
checkProg (Prog pos decls) = do 
    env <- checkDecls decls
    let main = Ident "main"
    case Map.lookup main (fenv env) of
        Nothing       -> lift $ throwError NoMainFn
        Just (sig, _) -> 
            if sig == mainFnSig
                then return ()
                else lift $ throwError $ WrongMainFnSig sig

sCheck :: Program Position -> SCResult ()
sCheck prog = runReaderT (checkProg prog) emptyTEnv