module AbsGrammar where

type Position = Maybe (Int, Int) -- (line, col)
showPos :: Position -> String
showPos (Just (x, y)) = "line " ++ show x ++ ", col " ++ show y

newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
showIdent :: Ident -> String
showIdent (Ident i) = i

data Program a = Prog a [Decl a]
  deriving (Eq, Ord, Show, Read)

instance Functor Program where
    fmap f x = case x of
        Prog a decls -> Prog (f a) (map (fmap f) decls)


data Block a = Blk a [Decl a] [Stmt a]
  deriving (Eq, Ord, Show, Read)

instance Functor Block where
    fmap f x = case x of
        Blk a decls stmts -> Blk (f a) (map (fmap f) decls) (map (fmap f) stmts)


data Decl a
    = VarDecl a (Type a) Ident
    | FnDecl a (Type a) Ident [Arg a] (Block a)
  deriving (Eq, Ord, Show, Read)

instance Functor Decl where
    fmap f x = case x of
        VarDecl a type_ ident -> VarDecl (f a) (fmap f type_) ident
        FnDecl a type_ ident args block -> FnDecl (f a) (fmap f type_) ident (map (fmap f) args) (fmap f block)

  
data Arg a = ValArg a (Type a) Ident | RefArg a (Type a) Ident
  deriving (Eq, Ord, Show, Read)

instance Functor Arg where
    fmap f x = case x of
        ValArg a type_ ident -> ValArg (f a) (fmap f type_) ident
        RefArg a type_ ident -> RefArg (f a) (fmap f type_) ident


data Stmt a
    = Empty a
    | BStmt a (Block a)
    | Ass a Ident (Expr a)
    | Ret a (Expr a)
    | VRet a
    | Cond a (Expr a) (Stmt a)
    | CondElse a (Expr a) (Stmt a) (Stmt a)
    | While a (Expr a) (Stmt a)
    | Interrupt a (Inter a)
    | SExp a (Expr a)
    | Print a (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Stmt where
    fmap f x = case x of
        Empty a -> Empty (f a)
        BStmt a block -> BStmt (f a) (fmap f block)
        Ass a ident expr -> Ass (f a) ident (fmap f expr)
        Ret a expr -> Ret (f a) (fmap f expr)
        VRet a -> VRet (f a)
        Cond a expr stmt -> Cond (f a) (fmap f expr) (fmap f stmt)
        CondElse a expr stmt1 stmt2 -> CondElse (f a) (fmap f expr) (fmap f stmt1) (fmap f stmt2)
        While a expr stmt -> While (f a) (fmap f expr) (fmap f stmt)
        Interrupt a inter -> Interrupt (f a) (fmap f inter)
        SExp a expr -> SExp (f a) (fmap f expr)
        Print a expr -> Print (f a) (fmap f expr)


data Inter a = Break a | Continue a
  deriving (Eq, Ord, Show, Read)

instance Functor Inter where
    fmap f x = case x of
        Break a -> Break (f a)
        Continue a -> Continue (f a)


data Type a
    = Int a | Str a | Bool a | Void a | Fun a (Type a) [Type a]
  deriving (Eq, Ord, Show, Read)

instance Functor Type where
    fmap f x = case x of
        Int a -> Int (f a)
        Str a -> Str (f a)
        Bool a -> Bool (f a)
        Void a -> Void (f a)
        Fun a type_ types -> Fun (f a) (fmap f type_) (map (fmap f) types)


data Expr a
    = EVar a Ident
    | ELitInt a Integer
    | ELitTrue a
    | ELitFalse a
    | EApp a Ident [Expr a]
    | EString a String
    | Neg a (Expr a)
    | Not a (Expr a)
    | EMul a (Expr a) (MulOp a) (Expr a)
    | EAdd a (Expr a) (AddOp a) (Expr a)
    | ERel a (Expr a) (RelOp a) (Expr a)
    | EAnd a (Expr a) (Expr a)
    | EOr a (Expr a) (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Expr where
    fmap f x = case x of
        EVar a ident -> EVar (f a) ident
        ELitInt a integer -> ELitInt (f a) integer
        ELitTrue a -> ELitTrue (f a)
        ELitFalse a -> ELitFalse (f a)
        EApp a ident exprs -> EApp (f a) ident (map (fmap f) exprs)
        EString a string -> EString (f a) string
        Neg a expr -> Neg (f a) (fmap f expr)
        Not a expr -> Not (f a) (fmap f expr)
        EMul a expr1 mulop expr2 -> EMul (f a) (fmap f expr1) (fmap f mulop) (fmap f expr2)
        EAdd a expr1 addop expr2 -> EAdd (f a) (fmap f expr1) (fmap f addop) (fmap f expr2)
        ERel a expr1 relop expr2 -> ERel (f a) (fmap f expr1) (fmap f relop) (fmap f expr2)
        EAnd a expr1 expr2 -> EAnd (f a) (fmap f expr1) (fmap f expr2)
        EOr a expr1 expr2 -> EOr (f a) (fmap f expr1) (fmap f expr2)


data AddOp a = Plus a | Minus a
  deriving (Eq, Ord, Show, Read)

instance Functor AddOp where
    fmap f x = case x of
        Plus a -> Plus (f a)
        Minus a -> Minus (f a)


data MulOp a = Times a | Div a | Mod a
  deriving (Eq, Ord, Show, Read)

instance Functor MulOp where
    fmap f x = case x of
        Times a -> Times (f a)
        Div a -> Div (f a)
        Mod a -> Mod (f a)


data RelOp a = LTH a | LE a | GTH a | GE a | EQU a | NE a
  deriving (Eq, Ord, Show, Read)

instance Functor RelOp where
    fmap f x = case x of
        LTH a -> LTH (f a)
        LE a -> LE (f a)
        GTH a -> GTH (f a)
        GE a -> GE (f a)
        EQU a -> EQU (f a)
        NE a -> NE (f a)
