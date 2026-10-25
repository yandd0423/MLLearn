fun upto (m, n) = if m > n then []
                           else m :: upto(m+1, n);

fun prod [] = 1
  | prod (x :: xs) = x * prod xs;       


fun maxl [x] = x
  | maxl (x::y::zs) = if x > y then maxl (x::zs)
                               else maxl (y::zs);

fun null [] = true
  | null (_::_) = false;  

fun hd (x::_) = x;  
fun tl (_::xs) = xs;   


fun nlength [] = 0
  | nlength (_::xs) = 1 + nlength xs;

local
  fun addlen ([], n) = n
    | addlen (_::xs, n) = addlen(xs, n+1);
in
  fun length (lst) = addlen(lst, 0);
end  


fun take ([], i) = []
  | take (_, 0) = []
  | take ((x::xs), i) = x :: take(xs, i-1);


local 
  fun tk ([], i, taken) = taken
    | tk (_, 0, taken) = taken
    | tk ((x::xs), i, taken) = tk (xs, i-1, x :: taken);
in 
  fun rtake (l, i) = tk (l, i, []);
end;

fun drop ([], i) = []
  | drop (xs, 0) = xs
  | drop ((x::xs), i) = drop (xs, i-1);


infixr 5 @;

fun ([] @ ys) = ys
  | ((x::xs) @ ys) = x :: (xs @ ys);

fun rev [] = []
  | rev (x::xs) = rev xs @ [x];

local
  fun f ([], k) = k
    | f ((x::xs), k) = f(xs, x::k);
in
  fun rev l = f (l, []);
end;

fun concat [] = []
  | concat (l :: ls) = l @ concat ls;

concat [[1], [2,3]];

fun zip ((x::xs), (y::ys)) = (x,y) :: zip (xs, ys)
  | zip _                  = [];

fun unzip ([]) = ([], [])
  | unzip ((x,y) :: zs) = let val (us, vs)  = unzip zs
                          in (x::us, y::vs) end;

fun conspair ((x,y), (xs,ys)) = (x::xs, y::ys);

fun unzip ((x,y)::pairs) = conspair ((x,y), unzip pairs)
  | unzip _ = ([], [])

