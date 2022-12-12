(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s1 : string, ss : string list) =
    case ss of
	[] => NONE
      | first :: ss' => case same_string(first, s1) of
			    true => SOME ss'
			  | false => case all_except_option(s1, ss') of
					 NONE => NONE
				       | SOME list => SOME (first :: list)

fun get_substitutions1 (subs, s : string) =
    case subs of
	[] => []
      | first :: tail => case all_except_option(s, first) of
			     NONE => get_substitutions1(tail, s)
			   | SOME xs => xs @ get_substitutions1(tail, s)

fun get_substitutions2 (subs, s : string) =
    let
	fun helper (subs, s, acc) =
	    case subs of
		[] => acc
	      | first :: tail => case all_except_option(s, first) of
				     NONE => helper(tail, s, acc)
				   | SOME xs => helper(tail, s, acc @ xs) 
    in
	helper(subs, s, [])
    end

fun similar_names (subs, fullname : {first:string,middle:string,last:string}) =
    let
	fun aux (namelist, {first = x, middle = y, last = z}) =
	    case namelist of
		[] => []
	      | str :: strs => {first = str, middle = y, last = z} :: aux(strs, fullname)
	fun helper (substis, {first = x, middle = y, last = z}) =
	    aux(get_substitutions2(substis, x), fullname)
    in
	fullname :: helper(subs, fullname)
    end
		      
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
	
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
	      (* put your solutions for problem 2 here *)

fun card_color (suit, rank) =
    case suit of
	Spades => Black
      | Clubs => Black 
      | _ => Red

fun card_value (suit, rank) =
    case rank of
	Num i => i
      | Ace => 11
      | _ => 10 

fun remove_card (cs, c, e) =
    case cs of
 	[] => raise e
      | card :: cards => if card = c
			 then cards
			 else card :: remove_card (cards, c, e)
(*if then else was not illegal*)

fun all_same_color (cards) =
    case cards of
	[] => true
      | _ :: [] => true
      | head ::(neck::rest) => (card_color(head) = card_color(neck) andalso all_same_color(neck::rest))

fun sum_cards (cards) =
    let
	fun helper (cardlist, acc) =
	    case cardlist of
		[] => acc
	      | card :: listofcs => helper(listofcs, acc + card_value(card))	    
    in
	helper(cards, 0)
    end

fun score (heldcs, goal) =
    let
	val sum = sum_cards(heldcs)
	val prelim = case sum>goal of
			 true => (sum - goal) * 3
		       | false => goal - sum 
    in
	case all_same_color(heldcs) of
	    true => prelim div 2
	  | false => prelim 
    end

fun officiate (cardlist, movelist, goal) =
    let
	val heldcards = []
	fun state (cardlist, heldlist, movelist, current_score) =
	    case movelist of
		[] => current_score
	      | move :: future_moves => case move of
					    Discard card => let val new_heldlist = remove_card(heldlist, card, IllegalMove)
							    in state(cardlist, new_heldlist, future_moves, score(new_heldlist, goal))
							    end
					  | Draw => case cardlist of
							[] => current_score
						      | top_card :: left_deck => let val new_heldlist = top_card :: heldlist
										 in
										     if sum_cards(new_heldlist) > goal
										     then score(new_heldlist, goal)
										     else state(left_deck, new_heldlist, future_moves, score(new_heldlist, goal))
										 end
    in
	state(cardlist, heldcards, movelist, score(heldcards, goal))
    end
