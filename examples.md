 ## FIXED-POINT COMBINER

``` Ocaml
prod = letrec sum : Nat -> Nat -> Nat = L n:Nat. L m:Nat. 
    if iszero n then m else succ (sum (pred n) m)
    in letrec prod : Nat -> Nat -> Nat = L m:Nat. L n:Nat.
        if iszero m then 0 else sum n (prod (pred m) n)
        in prod;;
prod 5 10;;

fact = letrec sum : Nat -> Nat -> Nat = L n:Nat. L m:Nat. 
    if iszero n then m else succ (sum (pred n) m)
    in letrec prod : Nat -> Nat -> Nat = L m:Nat. L n:Nat. 
        if iszero m then 0 else sum n (prod (pred m) n)
        in letrec fact : Nat -> Nat =  L n:Nat. 
            if iszero n then 1 else prod n (fact (pred n))
            in fact;;
fact 5;;

fib = letrec sum : Nat -> Nat -> Nat = L n:Nat. L m:Nat.
    if iszero n then m else succ (sum (pred n) m)
    in letrec fib : Nat -> Nat = L n:Nat. 
        if iszero n then 0 else 
            if iszero (pred n) then 1 else sum (fib (pred (pred n))) (fib (pred n)) 
            in fib;;
fib 10;;
```

 ## GLOBAL DEFINITION CONTEXT
``` Ocaml
x = 5;;
f = L y:Nat. x;;

f 3;;
x = 7;;
f 3;;
```

 ## STRINGS
``` Ocaml
a = "para";;
b = "sol";;
c = concat (concat a b) "es";;

letrec repeat : String -> Nat -> String = 
    L s : String. L n : Nat.
        if iszero n then "" else concat s (repeat s (pred n))
in repeat "ja" 3;;
```

 ## TUPLES
``` Ocaml
tuple = {true, {"cadena1", "cadena2"}, 3};;
tuple.2.1;;
```

 ## RECORDS
``` Ocaml
record = {a = 1, tuple = {23, 24}, y = true};;
record.tuple.1;;
```

 ## LISTS
``` Ocaml
l1 = cons[Nat] 1 cons[Nat] 2 cons[Nat] 3 empty[Nat];;
l2 = cons[Nat] 4 cons[Nat] 5 empty[Nat];;
f = L x:Nat. succ x;;

letrec len:List[Nat] -> Nat = L l:List[Nat]. 
    if isempty[Nat] l then 0 
        else succ (len tail[Nat] l)
    in len l1;;

letrec append: List[Nat] -> List[Nat] -> List[Nat] = 
    L l1:List[Nat]. L l2:List[Nat].
    if isempty[Nat] l1 then l2 
    else cons[Nat] (head[Nat] l1) (append (tail[Nat] l1) l2)
in append l1 l2;;

letrec map : List[Nat] -> (Nat -> Nat) -> List[Nat] =
  L l1:List[Nat]. L f:(Nat -> Nat).
    if isempty[Nat] l1 then empty[Nat]
    else cons[Nat] (f (head[Nat] l1)) (map (tail[Nat] l1) f)
in map l1 f;;
```

## SUBTYPING
```
(L r:{x:Nat}. r.x) {x = 1, y = 2};;
(L r:{x:{y:Nat}}. r.x.y) {x = {y = 3}, z = 4};;
(L f:{x:Nat, y:Nat} -> {x:Nat, y:Nat}. f {x = 5, y = 6}) (L x:{x:Nat, y:Nat}. {x = x.x, y = x.x});;
(L r:{}. r) {x = 1, y = 2};;
(L r:{}. 3) {x = 1, y = 2};;
```
