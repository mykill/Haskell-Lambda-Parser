--Michael June Aranas   	
Lambda Parser
--
--*Note:
--	When parsing an arguments the syntax is 
--	*Main> parseL “<arg> “

import Parsing
import Data.List
type Ide = String

data Exp = Var Ide
	 | App Exp Exp
	 | Lam Ide Exp
	   deriving Show
--unparse Lamda Expression
unparse :: Exp -> String
unparse (Var v) = v
unparse (Lam v exp) = "(\\" ++ v ++ "." ++ (unparse exp) ++ ")"
unparse (App fun arg) = "(" ++ (unparse fun) ++ " " ++ (unparse arg) ++ ")"

– Free Variables
fv :: Exp -> [Ide]
fv (Var x) = [x]
fv (App e1 e2) =
     union (fv e1) (fv e2)
fv (Lam x e) =
     delete x (fv e)

--Substitution
subst :: (Ide, Exp) -> Exp -> Exp
subst s@(x,e) e'@(Var y)
	| x==y = e
	| otherwise = e'
subst s@(x,e) e'@(App e1 e2) = App (subst s e1) (subst s e2)
subst s@(x,e) e'@(Lam y e'')
	| x==y = e'
	| y `notElem` (fv e) = Lam y (subst s e'')
	| otherwise = Lam z (subst s (subst (y, Var z) e'') )
	   where z = newIde (y: (fv e))

-- generate a new Ide which does not appear in xs
newIde :: [Ide] -> Ide
newIde xs = concat xs


--Beta reduce/conversion
--returns [e] if reducible, [] otherwise
betaReduce :: Exp -> [Exp]
betaReduce (App (Lam x e1) e2)
	= [subst (x,e2) e1]
betaReduce _ = []

--eta reduce/conversion
--returns [e] if reducible, [] otherwise
etaReduce :: Exp -> [Exp]
etaReduce (Lam x (App e (Var y)))
	| (x==y) && (x `notElem` fv(e)) = [e]
	| otherwise = []
etaReduce _ = []

--Leftmost-Outermost Reduction
loReduce :: Exp -> [Exp]
loReduce (Var _) = []
loReduce (App (Lam x e1) e2) = [subst (x,e2) e1]
loReduce (App e1 e2) = 
	case loReduce e1 of
	[e1'] -> [App e1' e2]
	[] ->
		case loReduce e2 of
		[e2'] -> [App e1 e2']; [] -> []
loReduce (Lam x e'@(App e (Var y)))
	| (x==y) && (x `notElem` fv(e)) =[e]
	| otherwise =
		case loReduce e' of
		[e''] -> [Lam x e'']; [] -> []
loReduce (Lam x e) =
	case loReduce e of
	[e'] -> [Lam x e']; [] -> []

--left most innermost Reduction
liReduce :: Exp -> [Exp]
liReduce (Var _) = []
liReduce (App e@(Lam x e1) e2) =
	case liReduce e2 of
	[e2'] -> [App e e2']
	[] -> [subst (x,e2) e1]
liReduce (App e1 e2) =
	case liReduce e1 of
	[e1'] -> [App e1' e2]
	[] ->
		case liReduce e2 of
		[e2'] -> [App e1 e2']; [] -> []
liReduce (Lam x e'@(App e (Var y)))
	| (x==y)&&(x `notElem` fv(e)) = [e]
	| otherwise =
		case liReduce e of
		[e'] -> [Lam x e']; [] -> []
liReduce (Lam x e) =
	case liReduce e of
	[e'] -> [Lam x e']; [] ->[]

-- iterates reduction steps
iterateReduce :: (Exp -> [Exp]) -> Exp -> [Exp]
iterateReduce stepReduce e =
	e: case stepReduce e of
		[e'] -> iterateReduce stepReduce e'
		[] -> []
-- normal-order reduction
noReduce = iterateReduce loReduce
-- applicative order reduction
aoReduce = iterateReduce liReduce

lam :: Parser Exp
lam = (do symbol "(\\"; e <-identifier; symbol "."; f <- var; symbol ")"; return ((Lam e f))) +++ app 
 
app :: Parser Exp
app = (do symbol "("; t <- lam;e <- lam; symbol ")"; return (App t e)) +++ var 

var :: Parser Exp
var = (do t <- identifier;  return (Var t))

parseL :: String -> Exp
parseL str = case parse lam str of
                           [(ast,[])] -> ast
                           [(_,out)]  -> error ("unused input " ++ out)
                           []         -> error "invalid input"
