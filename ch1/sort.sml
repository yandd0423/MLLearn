local
  val a = 16807.0 and m = 2147483647.0
in
  fun nextrand seed = 
        let val t = a * seed
        in t - m * real(floor( t / m )) end
end;

fun randlist (n, seed, tail) = 
        if n = 0 then (seed, tail)
        else randlist (n-1, nextrand seed, seed :: tail);

val (seed, rs) = randlist (100000, 1.0, []);


fun ins (x, []) = [x]
  | ins (x, y::ys) = if x <= y then x::y::ys
                              else y :: ins (x, ys);



fun ins_sort [] = []
  | ins_sort (x::xs) = ins (x, ins_sort xs);


val data = [1, 5, 3, 7, 4, 6, 2];

fun qsort [] = []
  | qsort (x::xs) = 
        let fun partition (pivot, []) = ([], [])
              | partition (pivot, v::vs) = let val (left, right) = partition (pivot, vs)
                                           in if v <= pivot then (v::left, right) else (left, v::right) end
        in let val (left, right) = partition (x, xs)
           in qsort (left) @ [x] @ qsort (right) end
        end;


local
  fun split [] = ([], [])
    | split [x] = ([x], [])
    | split (x::y::zs) = let val (t, s) = split (zs) in (x::t, y::s) end;
  fun merge (xs, []) = xs
    | merge ([], ys) = ys
    | merge (x::xs, y::ys) = if x <= y then x :: merge (xs, y::ys)
                                       else y :: merge (x::xs, ys);
in
  fun merge_sort [] = []
    | merge_sort [x] = [x]
    | merge_sort xs = 
        let val (l, r) = split xs
        in merge ( merge_sort l, merge_sort r) end
end;
