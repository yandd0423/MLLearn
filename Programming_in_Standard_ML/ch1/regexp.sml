signature REGEXP =
sig
  datatype regexp = Zero 
                  | One
                  | Char of char
                  | Plus of regexp * regexp
                  | Times of regexp * regexp
                  | Star of regexp
    
  exception SyntaxError of string

  val parse  : string -> regexp
  val format : regexp -> string              
end

signature MATCHER =
sig
  structure RegExp : REGEXP

  val accepts : RegExp.regexp -> string -> bool
end



structure RegExp :> REGEXP = 
struct
    datatype regexp = Zero 
                    | One
                    | Char of char
                    | Plus of regexp * regexp
                    | Times of regexp * regexp
                    | Star of regexp

    datatype token = AtSign
                | Percent
                | Literal of char
                | PlusSign     (* alternation *)
                | TimesSign    (* catentation *)
                | Asterisk     (* iteration *)
                | LParen
                | RParen

    exception LexicalError;
    exception SyntaxError of string

    (* char list -> token list*)
    fun tokenize [] = []
    | tokenize (#"+" :: cs) = PlusSign  :: tokenize cs   (* alternation *)
    | tokenize (#"." :: cs) = TimesSign :: tokenize cs   (* catentation *)
    | tokenize (#"*" :: cs) = Asterisk  :: tokenize cs   (* iteration *)
    | tokenize (#"(" :: cs) = LParen    :: tokenize cs
    | tokenize (#")" :: cs) = RParen    :: tokenize cs
    | tokenize (#"@" :: cs) = AtSign    :: tokenize cs   (* "@" stands for the empty regular expression *)
    | tokenize (#"%" :: cs) = Percent   :: tokenize cs   (* "%" stands for the regular expression accepting only the null string *)
    | tokenize (#"\\" :: c :: cs) = Literal c :: tokenize cs
    | tokenize (#"\\" :: []) = raise LexicalError
    | tokenize (#" " :: cs) = tokenize cs
    | tokenize (c :: cs) = Literal c :: tokenize cs;

    fun parse_rexp ts = 
            let 
                val (r, ts') = parse_rtrm ts
            in
                case ts'
                    of (PlusSign :: ts'') =>
                        let 
                            val (r', ts''') = parse_rexp ts''
                        in
                            (Plus (r, r'), ts''')
                        end
                    | _ => (r, ts')
            end

    and parse_rtrm ts =
            let 
                val (r, ts') = parse_rfac ts
            in
                case ts'
                    of (TimesSign :: ts'') =>
                        let
                            val (r', ts''') = parse_rexp ts''
                        in 
                            (Times (r, r'), ts''')
                        end
                    | _ => (r, ts')
            end

    and parse_rfac ts =
            let
                val (r, ts') = parse_ratm ts
            in
                case ts'
                    of (Asterisk :: ts'') => (Star r, ts'')
                    | _ => (r, ts')
            end

    and parse_ratm [] = raise SyntaxError "No atom"
      | parse_ratm (AtSign :: ts) = (Zero, ts)
      | parse_ratm (Percent :: ts) = (One, ts)
      | parse_ratm ((Literal c) :: ts) = (Char c, ts)
      | parse_ratm (LParen :: ts) =
            let
                val (r, ts') = parse_rexp ts
            in
                case ts'
                    of (RParen :: ts'') => (r, ts'')
                    | _ => raise SyntaxError "No close paren"
            end

    fun parse s = 
            let 
                val (r, s') = parse_rexp (tokenize (String.explode s))
            in 
                case s' of
                nil => r
                | _   => raise SyntaxError "Bad Input."
            end
            handle
            LexicalError => raise SyntaxError "Bad Inut."

    (* fun format r = String.implode (format_exp r) *)
    fun format r = "todo"
end

structure Matcher :> MATCHER =
struct
    structure RegExp = RegExp
    open RegExp

    fun match Zero _ k = false
      | match One cs k = k cs
      | match (Char c) cs k = 
            (case cs of [] => false 
                      | c'::cs' => (c = c') andalso (k cs'))
      | match (Plus (r1, r2)) cs k = 
            match r1 cs k orelse match r2 cs k
      | match (Times (r1, r2)) cs k =
            match r1 cs (fn cs' => match r2 cs' k)
      | match (Star r) cs k =
            let
                fun mstar cs' = k cs' orelse match r cs' mstar
            in
                mstar cs
            end

    fun accepts regexp string = 
            match regexp (String.explode string) (fn [] => true | _ => false)

end


val regexp = Matcher.RegExp.parse "(a+b)*"

val matches = Matcher.accepts regexp

val ex1 = matches "aabba"
val ex2 = matches "abac"

structure M = Matcher
structure R = M.RegExp

(* val regexp = R.parse "((a + %).(b + %))*" *)
val regexp = R.parse "(a+b)*"
val matches = M.accepts regexp
val ex1 = matches "aabba"
val ex2 = matches "abac"
