(* 二进制是按逆序方式表示的 *)

fun bincarry (0, ps)      = ps
  | bincarry (1, [])      = [1]
  | bincarry (1, (p :: ps)) = (1-p) :: bincarry (p, ps);

fun binsum (c, [], qs) = bincarry (c, qs)
  | binsum (c, ps, []) = bincarry (c, ps)
  | binsum (c, (p::ps), (q::qs)) = let val t = c + p + q
                                       val rem = t mod 2
                                       val quo = t div 2
                                    in rem :: binsum (quo, ps, qs) end;

(* 11 + 30 *)
val s = binsum (0, [1,1,0,1], [0,1,1,1,1]);   (* [1,0,0,1,0,1] *)

(* todo: decimal <=> binary *)

fun decimal2binary 0 = [0]
  | decimal2binary 1 = [1]
  | decimal2binary n = let val q = n div 2
                           val r = n mod 2
                       in r :: decimal2binary q end;

fun binary2decimal [] = 0
  | binary2decimal (b::bs) = b + 2 * binary2decimal bs;



fun binprod ([], _) = []
  | binprod (0::xs, ys) = 0 :: binprod (xs, ys)
  | binprod (1::xs, ys) = binsum (0, ys, 0::binprod (xs, ys));


val a = decimal2binary 11;
val b = decimal2binary 30;
val s = binsum (0, a, b);
val m = binprod (a, b);


