module Machine.Compile where

import Machine.Interpret

class Compile exp id where
        compile :: exp -> [id] -> [Instr] -> [Instr]
        compileLambda :: exp -> [id] -> [Instr] -> [Instr]
        compileLambda body n c = LDF (compile body n [RTN]) : c
        compileIf :: exp -> exp -> exp -> [id] -> [Instr] -> [Instr]
        compileIf cond true false n c =
                compile cond n (SEL (compile true n [JOIN]) (compile false n [JOIN]) : c)
        compileApp :: [exp] -> [id] -> [Instr] -> [Instr]
        compileApp [] _ c = c
        compileApp (a : as) n c = compileApp as n (compile a n (CONS : c))
        compilePrim :: [exp] -> [id] -> [Instr] -> [Instr]
        compilePrim [] _ c = c
        compilePrim (a : as) n c = compilePrim as n (compile a n c)