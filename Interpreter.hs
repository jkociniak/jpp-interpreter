module Interpreter where

import AbsGrammar
import ErrM

import qualified Data.Map as Map
import Data.Maybe

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except

type Var = Ident
type FunId = Ident
type Loc = Int

data Value = IntVal Integer | BoolVal Bool | StringVal String | VoidVal

instance Show Value where
    show (IntVal i) = show i
    show (BoolVal b) = show b
    show (StringVal s) = s
    show VoidVal = error "void type value is not printable" -- proper type of expression should be checked by static check

defaultIntValue :: Value
defaultIntValue = IntVal 0

defaultBoolValue :: Value
defaultBoolValue = BoolVal False

defaultStringValue :: Value
defaultStringValue = StringVal ""

type StmtReturn = Maybe Value

data Store = Store { storeMap :: Map.Map Loc Value, nextLoc :: Loc }

emptyStore :: Store
emptyStore = Store Map.empty 0

type VEnv = Map.Map Var Loc

data FunArg = Val Value | Ref Loc
data FunArgType = ValType | RefType
data Func = Func [FunArgType] ([FunArg] -> Interpreter Value)
type FEnv = Map.Map FunId Func

data Env = Env { venv :: VEnv, fenv :: FEnv }

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

data RuntimeError = DivisionByZero Position
                  | NoReturn FunId Position
                  | NotAnLValue Position

instance Show RuntimeError where
    show (DivisionByZero pos) = "Expression at " ++ showPos pos ++ " evaluated to 0 while trying to divide by it"
    show (NoReturn fn pos) = "Call of non-void function with name '" ++ showIdent fn ++ 
        "' defined at " ++ showPos pos ++ " reached end with no return"
    show (NotAnLValue pos) = "Expression at " ++ showPos pos ++ " is not an lvalue and couldn't be passed by reference"

type IResult = ExceptT RuntimeError IO

type Interpreter = ReaderT Env (StateT Store IResult)

getDefaultValue :: Type a -> Value
getDefaultValue (Int _) = defaultIntValue
getDefaultValue (Bool _) = defaultBoolValue
getDefaultValue (Str _) = defaultStringValue

insertDefaultValue :: Type a -> Store -> (Store, Loc)
insertDefaultValue varType (Store storeMap nextLoc) =
    let defaultValue = getDefaultValue varType in
        (Store (Map.insert nextLoc defaultValue storeMap) (nextLoc + 1), nextLoc)

updateLoc :: Loc -> Value -> Store -> Store
updateLoc loc value (Store storeMap nextLoc) =
    Store (Map.insert loc value storeMap) nextLoc

updateVEnv :: Var -> Loc -> Env -> Env
updateVEnv varIdent loc (Env venv fenv) =
    Env (Map.insert varIdent loc venv) fenv

updateFEnv :: FunId -> Func -> Env -> Env
updateFEnv fnIdent fun (Env venv fenv) =
    Env venv (Map.insert fnIdent fun fenv)

argToFunArgType :: Arg Position -> FunArgType
argToFunArgType arg = case arg of
    ValArg _ _ _ -> ValType
    RefArg _ _ _ -> RefType

exprToFunArg :: (Expr Position, FunArgType) -> Interpreter FunArg
exprToFunArg (expr, ValType) = do
    exprVal <- evalExpr expr
    return $ Val exprVal

exprToFunArg (EVar _ varIdent, RefType) = do
    env <- ask
    let loc = fromJust $ Map.lookup varIdent (venv env) -- existence of variable should checked by static check
    return $ Ref loc

exprToFunArg (expr, RefType) = throwError $ NotAnLValue (getExprPos expr) -- static check should check if passed expr is an lvalue; not implemented yet so its runtime error

evalFun :: FunId -> [Expr Position] -> Interpreter Value
evalFun fnIdent exprs = do
    env <- ask
    let Func argTypes funToEval = fromJust $ Map.lookup fnIdent (fenv env) -- existence of function should checked by static check
    args <- mapM exprToFunArg (zip exprs argTypes)
    funToEval args

evalVar :: Var -> Interpreter Value
evalVar varIdent = do
    env <- ask
    store <- get
    let loc = fromJust $ Map.lookup varIdent (venv env) -- existence of variable should checked by static check
    let val = fromJust $ Map.lookup loc (storeMap store) 
    return val

negateExpr :: Expr Position -> Interpreter Value
negateExpr expr = do
    exprVal <- evalExpr expr
    case exprVal of
        IntVal i -> return $ IntVal (-i)
        BoolVal b -> return $ BoolVal (not b)
        _ -> error "Type is not suitable for negation!" -- proper type of expressions should be checked by static check

multiplyExprs :: Expr Position -> Expr Position -> MulOp Position -> Interpreter Value
multiplyExprs expr1 expr2 op = do
    exprVal1 <- evalExpr expr1
    exprVal2 <- evalExpr expr2
    case (exprVal1, exprVal2) of
        (IntVal i1, IntVal i2) -> case op of
            Times _ -> return $ IntVal (i1 * i2)
            Div _ -> if i2 == 0
                then throwError $ DivisionByZero (getExprPos expr2)
                else return $ IntVal (i1 `div` i2)
            Mod _ -> return $ IntVal (i1 `mod` i2)
        _ -> error "expressions are not suitable for integer operations" -- proper type of expressions should be checked by static check

addExprs :: Expr Position -> Expr Position -> AddOp Position -> Interpreter Value
addExprs expr1 expr2 op = do
    exprVal1 <- evalExpr expr1
    exprVal2 <- evalExpr expr2
    case (exprVal1, exprVal2) of
        (IntVal i1, IntVal i2) -> case op of
            Plus _ -> return $ IntVal (i1 + i2)
            Minus _ -> return $ IntVal (i1 - i2)
        _ -> error "expressions are not suitable for integer operations" -- proper type of expressions should be checked by static check

compareExprs :: Expr Position -> Expr Position -> RelOp Position -> Interpreter Value
compareExprs expr1 expr2 op = do
    exprVal1 <- evalExpr expr1
    exprVal2 <- evalExpr expr2
    case (exprVal1, exprVal2) of
        (IntVal i1, IntVal i2) -> case op of
            LTH _ -> return $ BoolVal (i1 < i2)
            LE _ -> return $ BoolVal (i1 <= i2)
            GTH _ -> return $ BoolVal (i1 > i2)
            GE _ -> return $ BoolVal (i1 >= i2)
            EQU _ -> return $ BoolVal (i1 == i2)
            NE _ -> return $ BoolVal (i1 /= i2)
        _ -> error "expressions are not suitable for integer operations" -- proper type of expressions should be checked by static check
            
evalExprsAnd :: Expr Position -> Expr Position -> Interpreter Value
evalExprsAnd expr1 expr2 = do
    exprVal1 <- evalExpr expr1
    exprVal2 <- evalExpr expr2
    case (exprVal1, exprVal2) of
        (BoolVal b1, BoolVal b2) -> return $ BoolVal (b1 && b2) 
        _ -> error "expressions are not suitable for boolean operations" -- proper type of expressions should be checked by static check

evalExprsOr :: Expr Position -> Expr Position -> Interpreter Value
evalExprsOr expr1 expr2 = do
    exprVal1 <- evalExpr expr1
    exprVal2 <- evalExpr expr2
    case (exprVal1, exprVal2) of
        (BoolVal b1, BoolVal b2) -> return $ BoolVal (b1 || b2) 
        _ -> error "expressions are not suitable for boolean operations" -- proper type of expressions should be checked by static check

evalExpr :: Expr Position -> Interpreter Value
evalExpr expr = case expr of
    EVar _ varIdent -> evalVar varIdent
    ELitInt _ v -> return $ IntVal v
    ELitTrue _ -> return $ BoolVal True
    ELitFalse _ -> return $ BoolVal False
    EApp _ fnIdent exprs -> evalFun fnIdent exprs
    EString _ s -> return $ StringVal s
    Neg _ expr -> negateExpr expr
    Not _ expr -> negateExpr expr
    EMul _ expr1 op expr2 -> multiplyExprs expr1 expr2 op
    EAdd _ expr1 op expr2 -> addExprs expr1 expr2 op
    ERel _ expr1 op expr2 -> compareExprs expr1 expr2 op
    EAnd _ expr1 expr2 -> evalExprsAnd expr1 expr2
    EOr _ expr1 expr2 -> evalExprsOr expr1 expr2

interpretBlock :: Block Position -> Interpreter StmtReturn
interpretBlock (Blk _ decls stmts) = do
    env <- interpretDecls decls
    local (const env) (interpretStmts stmts)

interpretAss :: Var -> Expr Position -> Interpreter ()
interpretAss varIdent expr = do
    exprVal <- evalExpr expr
    env <- ask
    store <- get
    let loc = fromJust $ Map.lookup varIdent (venv env) -- existence of variable should checked by static check
    put $ updateLoc loc exprVal store

interpretCond :: Expr Position -> Stmt Position -> Interpreter StmtReturn
interpretCond expr stmt = do
    exprVal <- evalExpr expr
    let BoolVal b = exprVal -- proper type of expression should be checked by static check
    if b then interpretStmt stmt else return Nothing

interpretCondElse :: Expr Position -> Stmt Position -> Stmt Position -> Interpreter StmtReturn
interpretCondElse expr stmt1 stmt2 = do
    exprVal <- evalExpr expr
    let BoolVal b = exprVal -- proper type of expression should be checked by static check 
    if b then interpretStmt stmt1 else interpretStmt stmt2

interpretWhile :: Expr Position -> Stmt Position -> Interpreter StmtReturn
interpretWhile expr stmt = do
    exprVal <- evalExpr expr
    let BoolVal b = exprVal -- proper type of expression should be checked by static check
    if b then interpretStmts [stmt, While Nothing expr stmt] else return Nothing    

interpretPrint :: Expr Position -> Interpreter ()
interpretPrint expr = do
    exprVal <- evalExpr expr
    liftIO $ putStrLn $ show exprVal -- proper type of expression should be checked by static check
    return ()

interpretStmt :: Stmt Position -> Interpreter StmtReturn
interpretStmt stmt = case stmt of
    Empty _ -> return Nothing
    BStmt _ block -> do 
        env <- ask
        local (const env) (interpretBlock block)
    Ass _ varIdent expr -> interpretAss varIdent expr >> return Nothing
    Ret _ expr -> do 
        retVal <- evalExpr expr
        return $ Just retVal
    VRet _ -> return $ Just VoidVal
    Cond _ expr stmt -> interpretCond expr stmt
    CondElse _ expr stmt1 stmt2 -> interpretCondElse expr stmt1 stmt2
    While _ expr stmt -> interpretWhile expr stmt
    Interrupt _ inter -> return Nothing
    SExp _ expr -> evalExpr expr >> return Nothing
    Print _ expr -> interpretPrint expr >> return Nothing

interpretStmtAux :: Maybe Value -> Stmt Position -> Interpreter StmtReturn
interpretStmtAux (Just v) _ = return $ Just v
interpretStmtAux Nothing stmt = interpretStmt stmt

interpretStmts :: [Stmt Position] -> Interpreter StmtReturn
interpretStmts stmts = do
    foldM interpretStmtAux Nothing stmts

interpretArg :: Arg Position -> FunArg -> Interpreter Env
interpretArg (ValArg _ _ _) (Ref _) = error "passing location instead of value in fn call"
interpretArg (RefArg _ _ _) (Val _) = error "passing value instead of location in fn call"
interpretArg (ValArg _ varType varIdent) (Val v) = do
    env <- ask
    store <- get
    let (updatedStore, loc) = insertDefaultValue varType store
    let updatedStoreValueSet = updateLoc loc v updatedStore
    put updatedStoreValueSet
    return $ updateVEnv varIdent loc env

interpretArg (RefArg _ varType varIdent) (Ref l) = do
    env <- ask
    return $ updateVEnv varIdent l env

interpretArgs :: [Arg Position] -> [FunArg] -> Interpreter Env
interpretArgs [] (x:_) = error "mismatched argument count in fn call"
interpretArgs (x:_) [] = error "mismatched argument count in fn call"
interpretArgs [] [] = ask
interpretArgs (codeArg:codeArgs) (passedArg:passedArgs) = do
    env <- interpretArg codeArg passedArg
    local (const env) (interpretArgs codeArgs passedArgs)

interpretDecl :: Decl Position -> Interpreter Env
interpretDecl (VarDecl _ varType varIdent) = do
    env <- ask
    store <- get
    let (updatedStore, loc) = insertDefaultValue varType store
    put updatedStore
    return $ updateVEnv varIdent loc env

interpretDecl (FnDecl pos fnType fnIdent args block) = do
    env <- ask
    let funArgTypes = map argToFunArgType args
    let fun argList = do
        let envWithFnName = updateFEnv fnIdent (Func funArgTypes fun) env
        envWithParams <- local (const envWithFnName) $ interpretArgs args argList
        maybeRetValue <- local (const envWithParams) $ interpretBlock block
        case (fromMaybe VoidVal maybeRetValue, fnType) of
            (VoidVal, Void _) -> return VoidVal
            (VoidVal, _) -> throwError $ NoReturn fnIdent pos
            (retVal, _) -> return retVal
    return $ updateFEnv fnIdent (Func funArgTypes fun) env

interpretDecls :: [Decl Position] -> Interpreter Env
interpretDecls [] = ask
interpretDecls (d:ds) = do
    env <- interpretDecl d
    local (const env) (interpretDecls ds)

interpretProg :: Program Position -> Interpreter Value
interpretProg (Prog pos decls) = do
    env <- interpretDecls decls
    let main = Ident "main"
    local (const env) (evalFun main [])

interpretProgram :: Program Position -> IResult Value
interpretProgram prog = do
    (retVal, store) <- runStateT (runReaderT (interpretProg prog) emptyEnv) emptyStore
    return retVal

