val graph1 = [("a", "b"), ("a", "c"), ("a", "d"),
              ("b", "e"),
              ("c", "f"),
              ("d", "e"),
              ("e", "f"), ("e", "g")];


fun nexts (a, []) = []
  | nexts (a, (x,y)::pairs) = if a = x then y :: nexts (a, pairs)
                                       else nexts (a, pairs);

val adj_e = nexts ("e", graph1);


infix mem;

fun (_ mem []) = false
  | (x mem (y::ys)) = (x = y) orelse (x mem ys);

fun dfs ([], g, visited) = rev visited
  | dfs (x::xs, g, visited) = if (x mem visited) then dfs (xs, g, visited)
                                               else dfs (nexts(x, g) @ xs, g, x::visited);

val path = dfs (["a"], graph1, []);
val path2 = dfs(["b"], graph1, []);
val path3 = dfs(["b"], ("f", "d")::graph1, []);


(* topological_sort
has_cycle *)
