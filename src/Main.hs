{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lexer
import Parser

type Env = String -> Exp

emptyEnv :: a
emptyEnv = error "Not found"

envLookup :: t1 -> (t1 -> t2) -> t2
envLookup s env = env s

envBind :: (Eq t) => t -> p -> (t -> p) -> t -> p
envBind s v env s' = if s == s' then v else env s

eval :: Exp -> Env -> Int
eval (Int v) _ = v
eval (Plus e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Minus e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Times e1 e2) env = (eval e1 env) * (eval e2 env)
eval (Div e1 e2) env = (eval e1 env) `div` (eval e2 env)
eval (Negate e) env = -(eval e env)
eval (Var s) env = eval (envLookup s env) env
eval (Let s e1 e2) env = eval e2 env'
  where
    env' = envBind s e1 env
eval _ _ = 42

run :: Exp -> Int
run e = eval e emptyEnv

main :: IO ()
main = do
  let prog = "let x = 5 in x + 3"
  let toks = scanTokens prog
  let ast = parseCalc toks
  print ast
  print (run ast)