fun change (_, 0) = []
  | change ([], _) = []
  | change (x::coinvals, amount) = if x > amount then change (coinvals, amount)
                                                 else x :: change (x::coinvals, amount - x);


val gb_coins = [50, 20, 10, 5, 2, 1]
val us_coins = [25, 10, 5, 1]


fun printIntList (lst: int list) =
  let
    fun aux [] = print "\n"
      | aux (x::xs) = (
          print (Int.toString x);
          print " ";
          aux xs
        )
  in
    aux lst
  end;

val ans = change (gb_coins, 43);
printIntList(ans);

val ans = change (us_coins, 43);
printIntList(ans);

val ans = change ([5,2], 16);  (* 是有解的，可是... *)
printIntList(ans);


infix ##

(* fun (x ## []) = []
  | (x ## [[]]) = [[x]]
  | (x ## [l::ls]) = (x::l) :: (x ## [ls]); *)

  fun (x ## []) = []
  | (x ## [[]]) = [[x]]
  | (x ## (l::ls)) = (x::l) :: (x ## ls);

(* 1 ## [[10,20], [30, 40]]; *)

fun change (_, 0) = [[]]
  | change ([], _) = []
  | change (x::coinvals, amount) = if x > amount then change (coinvals, amount)
                                                 else (x ## change (x::coinvals, amount - x)) @ change (coinvals, amount);


fun change_all (_, 0, coins) = [coins]
  | change_all ([], _, coins) = []
  | change_all (x::coinvals, amount, coins) = if x > amount then change_all(coinvals, amount, coins)
                                                            else change_all(x::coinvals, amount - x, x::coins) @ change_all(coinvals, amount, coins);


fun change_all (_, 0, coins) = [coins]
  | change_all ([], _, coins) = []
  | change_all (x::coinvals, amount, coins) = if amount < 0 then []
                                                            else change_all(x::coinvals, amount - x, x::coins) @ change_all(coinvals, amount, coins);

val ans = change ([5,2], 16);
(* printIntList(ans); *)
val ans = change (gb_coins, 16);

val ans = change_all ([5,2], 16, []);
val ans = change_all (gb_coins, 16, []);

