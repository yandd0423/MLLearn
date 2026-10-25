signature ARITH =
sig
  type t
  val zero : t
  val sum  : t * t -> t
  val diff : t * t -> t
  val prod : t * t -> t
  val quo  : t * t -> t
end;


structure Complex : ARITH =
struct
  type t = real * real;
  val zero = (0.0, 0.0);
  fun sum ((x,y), (x',y')) = (x+x', y+y') : t;
  fun diff ((x,y), (x',y')) = (x-x', y-y') : t;
  fun prod ((x,y), (x',y')) = (x*x' - y*y', x*y' + x'*y) : t;
  fun recip (x, y) = 
                        let val t = x*x + y*y
                        in (x/t, ~y/t) end;
  fun quo (z, z') = prod(z, recip z');
end;


val i = (0.0, 1.0);
val a = (0.3, 0.0);
val b = Complex.sum(i, a);
Complex.sum(b, (0.7, 0.0));