(* 多态类型声明 *)
fun pairself x = (x, x);

fun fst (x, _) = x;
fun snd (_, y) = y;

fun silly x = fst (fst (pairself (pairself x)));

fun I x = x;

pairself 3;
pairself 3.1;
pairself (1789, "hello");