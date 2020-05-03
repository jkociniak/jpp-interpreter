module Interpreter where

import AbsGrammar
import ErrM
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except

type Var = Ident
type Fn = Ident
type Loc = Int
type 

data Value = Int Int | Bool Bool | String String

type IResult = ExceptT RuntimeError IO

type WrapperT = ReaderT Env (StateT Store IResult)


