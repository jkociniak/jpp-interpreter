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

showListOfPrimitiveTypes :: [PrimitiveType] -> String
showListOfPrimitiveTypes types = "(" ++ typesStr ++ ")" where 
    typesStr = intercalate "," $ map show types

showFnType :: FnType -> String
showFnType (retType, argTypes) =
    show retType ++ " " ++ showListOfPrimitiveTypes argTypes

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

data SCError = UndeclaredFn Fn Position
             | UndeclaredVar Var Position
             | DoubleVarDecl Var Position
             | DoubleFnDecl Fn Position
             | MismatchedArgCount Fn Int Int Position
             | WrongFnArgType Fn [PrimitiveType] [PrimitiveType] Position
             | WrongAssignmentType Var PrimitiveType PrimitiveType Position
             | InterruptOutsideLoop Position
             | WrongWhileCondType PrimitiveType Position
             | WrongIfCondType PrimitiveType Position
             | NonVoidReturnInVoidFn Fn PrimitiveType -- TODO position printing
             | VoidReturnInNonVoidFn Fn PrimitiveType -- TODO position printing
             | BlockAmbiguousReturnType -- TODO position printing
             | IfAmbiguousReturnType -- TODO position printing
             | WrongReturnType Fn PrimitiveType PrimitiveType -- TODO position printing
             | WrongMainFnSig FnType
             | NoMainFn
             | VoidTypeToPrint Position
             | NotIntUsedWithIntOp PrimitiveType Position
             | NotBoolUsedWithBoolOp PrimitiveType Position
             | VoidExprInNonVoidReturn Position

instance Show SCError where
    show (WrongMainFnSig sig) =
        "Wrong signature of the main function: " ++ showFnType sig ++ " instead of " ++ showFnType mainFnSig
    show NoMainFn = "No main function declared"
    show (DoubleVarDecl var pos) =
        "Double declaration of variable with name '" ++ showIdent var ++ "' at " ++ showPos pos 
    show (DoubleFnDecl fn pos) =
        "Double declaration of function with name '" ++ showIdent fn ++ "' at " ++ showPos pos 
    show (UndeclaredVar var pos) =
        "Reference to undeclared variable with name '" ++ showIdent var ++ "' at " ++ showPos pos 
    show (UndeclaredFn fn pos) =
        "Reference to undeclared function with name '" ++ showIdent fn ++ "' at " ++ showPos pos
    show (MismatchedArgCount fn len targetLen pos) =
        "Mismatched argument count when calling function with name '" ++ showIdent fn 
        ++ "': got " ++ show len ++ " instead of " ++ show targetLen ++ " args at " ++ showPos pos 
    show (WrongFnArgType fn types targetTypes pos) =
        "Wrong argument types when calling function with name '" ++ showIdent fn 
        ++ "': got " ++ showListOfPrimitiveTypes types ++ " instead of " ++ showListOfPrimitiveTypes targetTypes ++ " at " ++ showPos pos
    show (WrongAssignmentType var t targetT pos) =
        "Wrong expression type in assignment to variable with name '" ++ showIdent var
        ++ "': got " ++ show t ++ " instead of " ++ show targetT ++ " at " ++ showPos pos
    show (InterruptOutsideLoop pos) = "Break or continue outside of loop at " ++ showPos pos
    show (WrongWhileCondType t pos) = "Non-boolean expression in while loop condition: got " ++ show t ++ " at " ++ showPos pos
    show (WrongIfCondType t pos) = "Non-boolean expression in if condition: got " ++ show t ++ " at " ++ showPos pos
    show (NotIntUsedWithIntOp t pos) = "Non-integer expression used with integer operator: got " ++ show t ++ " at " ++ showPos pos
    show (NotBoolUsedWithBoolOp t pos) = "Non-boolean expression used with boolean operator: got " ++ show t ++ " at " ++ showPos pos
    show (VoidTypeToPrint pos) = "Void expression used with print statement at " ++ showPos pos
    show (VoidExprInNonVoidReturn pos) = "Void expression used with non-void return statement at " ++ showPos pos
    show (NonVoidReturnInVoidFn fn t) = "Non-void return in function of type void with name '" ++ showIdent fn
        ++ "': got " ++ show t ++ " instead of void"
    show (VoidReturnInNonVoidFn fn targetT) = "Void return in function of non-void type with name '" ++ showIdent fn
        ++ "': got void instead of " ++ show targetT
    show BlockAmbiguousReturnType = "Different possible return types in block statement"
    show IfAmbiguousReturnType = "Different possible return types in if statement"
    show (WrongReturnType fn t targetT) = "Wrong return type in function with name '" ++ showIdent fn 
        ++ "': got " ++ show t ++ " instead of " ++ show targetT

data TEnv = Env { venv :: VTEnv, fenv :: FTEnv, depth :: Depth, loop :: Bool } deriving Show

emptyTEnv :: TEnv
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

checkVar :: Position -> Var -> SC PrimitiveType
checkVar pos varIdent = do
    env <- ask
    case Map.lookup varIdent (venv env) of
        Nothing -> lift $ throwError $ UndeclaredVar varIdent pos
        Just (t, _) -> return t

checkFn :: Position -> Fn -> SC FnType
checkFn pos fnIdent = do
    env <- ask
    case Map.lookup fnIdent (fenv env) of
        Nothing -> lift $ throwError $ UndeclaredFn fnIdent pos
        Just (t, _) -> return t

checkApp :: Position -> Fn -> [Expr Position] -> SC PrimitiveType
checkApp pos fnIdent args = do
    (retType, argTargetTypes) <- checkFn pos fnIdent
    argTypes <- mapM checkExpr args
    if not (length argTypes == length argTargetTypes)
        then lift $ throwError $ MismatchedArgCount fnIdent (length argTypes) (length argTargetTypes) pos
        else if not (argTypes == argTargetTypes)
            then lift $ throwError $ WrongFnArgType fnIdent argTypes argTargetTypes pos
            else return retType

checkIntOp :: Expr Position -> SC ()
checkIntOp expr = do
    exprType <- checkExpr expr
    if not (exprType == IntType)
        then lift $ throwError $ NotIntUsedWithIntOp exprType (getExprPos expr)
        else return ()

checkBoolOp :: Expr Position -> SC ()
checkBoolOp expr = do
    exprType <- checkExpr expr
    if not (exprType == BoolType)
        then lift $ throwError $ NotBoolUsedWithBoolOp exprType (getExprPos expr)
        else return ()

checkExpr :: Expr Position -> SC PrimitiveType
checkExpr expr = case expr of
    EVar pos varIdent -> checkVar pos varIdent
    ELitInt _ _ -> return IntType
    ELitTrue _ -> return BoolType
    ELitFalse _ -> return BoolType
    EApp pos fnIdent args -> checkApp pos fnIdent args
    EString _ _ -> return StringType
    Neg _ expr -> checkIntOp expr >> return IntType
    Not _ expr -> checkBoolOp expr >> return BoolType
    EMul _ expr1 _ expr2 -> checkIntOp expr1 >> checkIntOp expr2 >> return IntType
    EAdd _ expr1 _ expr2 -> checkIntOp expr1 >> checkIntOp expr2 >> return IntType
    ERel _ expr1 _ expr2 -> checkIntOp expr1 >> checkIntOp expr2 >> return BoolType
    EAnd _ expr1 expr2 -> checkBoolOp expr1 >> checkBoolOp expr2 >> return BoolType
    EOr _ expr1 expr2 -> checkBoolOp expr1 >> checkBoolOp expr2 >> return BoolType

checkAss :: Position -> Var -> Expr Position -> SC ()
checkAss pos varIdent expr = do
    varType <- checkVar pos varIdent
    exprType <- checkExpr expr
    if not (varType == exprType)
        then lift $ throwError $ WrongAssignmentType varIdent exprType varType pos
        else return ()

checkCond :: Expr Position -> Stmt Position -> SC StmtReturnType
checkCond expr stmt = do
    exprType <- checkExpr expr
    if not (exprType == BoolType)
        then lift $ throwError $ WrongIfCondType exprType (getExprPos expr)
        else checkStmt stmt

checkCondElse :: Expr Position -> Stmt Position -> Stmt Position -> SC StmtReturnType
checkCondElse expr stmt1 stmt2 = do
    exprType <- checkExpr expr
    if not (exprType == BoolType)
        then lift $ throwError $ WrongIfCondType exprType (getExprPos expr)
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
        then lift $ throwError $ WrongWhileCondType exprType (getExprPos expr)
        else do
            env <- ask    
            local (const $ setLoopTrue env) (checkStmt stmt)

checkInterrupt :: Inter Position -> SC ()
checkInterrupt inter = do
    env <- ask
    let pos = getInterPos inter
    if loop env
        then return ()
        else lift $ throwError $ InterruptOutsideLoop pos

checkPrint :: Expr Position -> SC ()
checkPrint expr = do
    exprType <- checkExpr expr
    if exprType == VoidType
        then lift $ throwError $ VoidTypeToPrint (getExprPos expr)
        else return ()

checkRet :: Expr Position -> SC StmtReturnType
checkRet expr = do
    exprType <- checkExpr expr
    if exprType == VoidType
        then lift $ throwError $ VoidExprInNonVoidReturn (getExprPos expr)
        else return $ Just exprType

checkStmt :: Stmt Position -> SC StmtReturnType
checkStmt stmt = case stmt of
    Empty _ -> return Nothing
    BStmt _ block -> do
        env <- ask  
        local (const $ incrDepth env) (checkBlock block)
    Ass pos varIdent expr -> checkAss pos varIdent expr >> return Nothing
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
    
checkRetType :: Fn -> PrimitiveType -> PrimitiveType -> SC ()
checkRetType fnIdent retType VoidType = 
    if retType == VoidType then return () else lift $ throwError $ NonVoidReturnInVoidFn fnIdent retType

checkRetType fnIdent VoidType targetRetType = lift $ throwError $ VoidReturnInNonVoidFn fnIdent targetRetType

checkRetType fnIdent retType targetRetType =
    if retType == targetRetType then return () else lift $ throwError $ WrongReturnType fnIdent retType targetRetType

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
    checkRetType fnIdent (fromMaybe VoidType blockRetType) (readType retType) 
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