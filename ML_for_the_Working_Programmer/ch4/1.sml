datatype person = King
                | Peer of string * string * int
                | Knight of string
                | Peasant of string;


val persons = [King, Peasant "Jack Cade", Knight "Gawain"];

fun title King = "His Majesty the King"
  | title (Peer(deg, terr, _)) = "The " ^ deg ^ " of " ^ terr
  | title (Knight name) = "Sir " ^ name
  | title (Peasant name) = name;

title (Peer ("Earl", "Carlisle", 7));
title (Knight "Galahad");

title (Knight ("Galahad"));

(* 返回所以骑士的名字 *)
fun sirs []                    = []
  | sirs ((Knight name) :: ps) = name :: sirs ps
  | sirs (p::ps)               = sirs ps;

sirs persons;

fun superior (King, Peer _)        = true
  | superior (King, Knight _)      = true
  | superior (King, Peasant _)     = true
  | superior (Peer _, Knight _)    = true
  | superior (Peer _, Peasant _)   = true
  | superior (Knight _, Peasant _) = true
  | superior _                     = false;


fun rank King = 4
  | rank (Peer _) = 3
  | rank (Knight _) = 2
  | rank (Peasant _) = 1;

map rank persons;

fun superior x y = rank x > rank y;


type Point = real * real;

datatype Shape = Line of Point * Point
               | Triangle of Point * Point * Point
               | Rectangle of Point * Point
               | Circle of Point * real;

fun area (Line _) = 0.0
  | area (Triangle (x, y, z)) = 123.0
  | area (Rectangle ((x1,y1), (x2,y2))) = (x2-x1) * (y2-y1)
  | area (Circle (_, r)) = Math.pi * r * r;


area (Rectangle((0.0, 0.0), (1.0, 2.0)));
area (Circle ((0.0, 0.0), 1.1));



(* enum *)
datatype degree = Duke | Marquis | Earl | Viscount | Baron

datatype my_bool = True | False;

fun not true = false
  | not false = true;

datatype order = LESS | EQUAL | GREATER;  

(* polymorphism datatype *)

(* disjoint sum *)
datatype ('a, 'b) sum = In1 of 'a | In2 of 'b;

val a = [In1(King), In2("Scotland")];
val b = [In1("tyrant"), In2(1024)];

fun concat1 [] = ""
  | concat1 ((In1 s) :: l) = s ^ concat1 l
  | concat1 ((In2 _) :: l) =     concat1 l;

concat1 [In1 "O!", In2(1040, 1057), In1 "Scotland"];
concat1 [In1 "O!", In2(10, 20), In2(1040, 0), In1 "Scotland"];
(* concat1 [In1 "O!", In2(1040, 1057), In1 123]; *)
(* concat1 [In1 100, In2(1040, 1057), In1 123]; *)
(* concat1 [In1 "O!", In2(10, 20), In2(1040), In1 "Scotland"]; *)

exception Empty;

fun hd (x::_) = x
  | hd _ = raise Empty;

fun tl (_::xs) = xs
  | tl _ = raise Empty;


exception Change;

fun backChange (_, 0) = []
  | backChange ([], amount) = raise Change
  | backChange (c::coinvals, amount) = 
                        if amount < 0 then raise Change
                                      else c :: backChange(c::coinvals, amount - c) handle
                                        Change => backChange(coinvals, amount);

(* backChange ([10,2], 27); *)
backChange ([5,2], 16);

                