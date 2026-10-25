fun gcd(m, n) = 
    if m = 0 then n
             else gcd(n mod m, m);

gcd(12, 21);

fun length [] = 0
  | length (x::xs) = 1 + length xs;



type vec = real * real;

fun add_vec (x1,y1) (x2,y2) : vec = (x1+x2, y1+y2);


type king = { name    : string,
              born    : int,
              crowned : int,
              died    : int,
              quote   : string };

fun lifetime (k : king) = #died k - #born k;

(*an other define *)
fun lifetime ({born, died, ...} : king) = died - born;

val henryVI = { name = "Henry VI",
                born = 1421,
                crowned = 1422,
                died = 1471,
                quote = "Weep, wretched man, I'll aid thee tear for tear"}