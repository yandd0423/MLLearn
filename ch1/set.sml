infix mem;

fun (x mem []) = false
  | (x mem (y::ys)) = x = y orelse x mem ys;

fun newmem (x, xs) = if x mem xs then xs else x::xs;

(* list -> set *)
fun setof [] = []
  | setof (x::xs) = newmem (x, setof xs);

val s1 = setof [true, false, false, true, false];

fun union ([], ys) = ys
  | union (x::xs, ys) = newmem (x, union (xs, ys));

val s2 = union ([1,2,3], [0,2,4]);

fun inter ([], ys) = []
  | inter (x::xs, ys) = if x mem ys then x :: inter (xs, ys)
                                    else inter (xs, ys);

val s3 = inter ([1,2,3], [0,2,4]);


infix subs;  (* is sub set ... *)

fun ([] subs _) = true
  | ((x::xs) subs ys) = (x mem ys) andalso (xs subs ys);


infix seq;  (* equal *)

fun ([] seq []) = true
  | ((x::xs) seq (y::ys)) = (x = y) andalso (xs seq ys)
  | (_ seq _) = false;

fun (x seq y) = (x subs y) andalso (y subs x);


infix @@

fun (x @@ []) = []
  | (x @@ (l::ls)) = (x::l) :: (x @@ ls);

(* 1 ## [[10,20], [30, 40]]; *)


fun powerset [] = [[]]
  | powerset (x::xs) = (x @@ powerset(xs)) @ powerset xs;


fun powset ([], base) = [base]
  | powset (x::xs, base) = powset (xs, base) @ powset (xs, x::base);


fun cartprod ([], _) = []
  | cartprod (_, []) = []
  | cartprod (x::xs, ys) = let val xsprod = cartprod (xs, ys)
                               fun pairx [] = xsprod
                                 | pairx (y::ytail) = (x,y) :: (pairx ytail)
                            in pairx ys end;


fun h (x, []) = []
  | h (x, (y::ys)) = (x,y) :: h (x, ys);

fun cartprod ([], _) = []
  | cartprod (_, []) = []
  | cartprod (x::xs, ys) = h (x, ys) :: cartprod (xs, ys)

val xy1 = cartprod ([], [1,2]);
val xy2 = cartprod ([1,2], []);
val xy3 = cartprod ([1,2], ["a", "b", "c"]);
                            