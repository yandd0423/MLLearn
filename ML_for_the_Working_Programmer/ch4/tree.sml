datatype 'a tree = Lf
                 | Br of 'a * 'a tree * 'a tree;

val tree2 = Br(2, Br(1, Lf, Lf), Br(3, Lf, Lf));

fun size Lf = 0
  | size (Br(_, l, r)) = 1 + size l + size r;


local
  fun max (x, y) = if x > y then x else y;
in
  fun depth Lf = 0
    | depth (Br(_, l, r)) = 1 + max(depth l, depth r);
end

fun comptree (k, 0) = Lf
  | comptree (k, n) = Br(k, comptree(2*k, n-1), comptree(2*k+1, n-1));

fun reflect Lf = Lf
  | reflect (Br(v, l, r)) = Br(v, reflect r, reflect l);

val tree7 = comptree(1, 3);
val tree7' = reflect tree7;

local
    datatype ('a, 'b) tree = Lf of 'a
                        | Br of 'b * ('a, 'b) tree * ('a, 'b) tree;

in
    val t = Br(1, Br(2, Lf "a", Lf "b"), Br(3, Lf "x", Lf "y"));
end

fun preorder Lf = []
  | preorder (Br(v, l, r)) = v :: preorder(l) @ preorder(r);

fun inorder Lf = []
  | inorder (Br(v, l, r)) = inorder(l) @ [v] @ inorder(r);

fun postorder Lf = []
  | postorder (Br(v, l, r)) = postorder(l) @ postorder(r) @ [v];



fun balpre [] = Lf
  | balpre (x::xs) = 
            let val k = length (xs) div 2
            in Br(x, balpre (List.take(xs, k)), balpre (List.drop(xs, k))) end;

fun balin [] = Lf
  | balin xs =
        let val k = length xs div 2
            val y::ys = List.drop (xs, k)
        in Br (y, balin(List.take(xs, k)), balin ys) end;

