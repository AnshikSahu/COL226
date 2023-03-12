(*Note: Lists with varriable names ending in "__" are to be read from left to
 right and the rest from right to left*)

(*Arthematic funcions---------------------------------------------------------------------------------*)

fun digit(l) = case l of
                    x :: xs => x
                  | [] => 0;
(*gives the first element for a non empty list and 0 for an empty list*)

fun headlesslist(l)= case l of
                          x::xs => xs
                        | [] => [];
(*gives the tail for a non empty list and an empty list for an empty list*)

fun add_digits(x,y,c)= (x+y+c) mod 10;
(*gives the unit digit of the sum of x,y and c*)

fun carry_in_addition(x,y,c)= (x+y+c) div 10;
(*gives the tens digit of the sum of x,y and c*)

fun add(l1,l2,c)=if length(l1) + length(l2) <> 0 then
  add_digits(digit(l1),digit(l2),c) :: add( headlesslist(l1), headlesslist(l2) ,
  carry_in_addition(digit(l1),digit(l2),c)) 
                  else if c<>0 then [c] else [] ;
(*recursively adds two numbers represented as lists by adding heads of the list and passing
 rest of the elements of the lists along with a carry to itself until both lists are empty*)

fun subtract_digits(x,y,c)= (x-y+c) mod 10;
(*gives the unit digit when subtracting y from x and also accounting for carry
 given and carry borrowed*)

fun carry_in_subtraction(x,y,c)= (x-y+c) div 10;
(*gives the carry borrowed when subtracting y from x*)

fun dirty_subtract(l1,l2,c)=if length(l1) + length(l2) <> 0 then
  subtract_digits(digit(l1),digit(l2),c) :: dirty_subtract( headlesslist(l1) ,
  headlesslist(l2), carry_in_subtraction(digit(l1),digit(l2),c)) 
                             else [] ;
(*recursively subtracts two numbers represented as lists by subtracting heads of the list and passing
 rest of the elements of the lists along with a carry to itself until both lists are empty*)

fun clean(l)=if length(l)=0 then []
             else let val r=clean(tl(l)) in if (length(r)=0) andalso (hd(l)=0) then [] 
                  else hd(l) :: r end;
(*removes unnecessary zeros from the end of the list recursively until the last element of
 the list is is non-zero*)

fun subtract(l1,l2,c)=clean(dirty_subtract(l1,l2,0));
(*subtracts two numbers represented as lists and also removes unnecessary zeros*)

fun compare_adv(l1,l2)= case l1 of 
                             x :: xs => let val i=compare_adv(tl(l1),tl(l2)) in
                               if i=2 then 
                               if hd(l1)>hd(l2) then 0
                               else if hd(l1)<hd(l2) then 1 else 2 else i end
                           | [] => 2;
(*compares two numbers represented by lists of equal length recursively by comparing the
 later digits first and if they are equal then comparing the first elements.
 0: l1>l2 , 1: l1<l2 , 2: l1=l2 *)

fun compare(l1,l2) = let val l_1=clean(l1)
  val l_2=clean(l2) in if length(l_1) > length(l_2) then true
                     else if length(l_1) < length(l_2) then false 
                          else compare_adv(l_1,l_2)=0 end;
(*compares two numbers represented as lists and gives the value of l1>l2*) 

fun multiply_digits(x,n,c)= (x*n+c) mod 10;
(*gives the unit digit when muntiplying x and y and adding the carry c*)

fun carry_in_multiplication(x,n,c) = (x*n+c) div 10;
(*gives the tens digit when muntiplying x and y and adding the carry c*)

fun multiply(l,n,c)= if length(l) <> 0 then
  multiply_digits(hd(l),n,c) ::
  multiply(tl(l),n,carry_in_multiplication(hd(l),n,c)) 
                     else if c <> 0 then [c] else [] ;
(*multiplies a number represented as a list and a single digit number
 recursively by multiplying the first element and passing rest of the digits to
 itself along with a carry*)

(*Conversion Functions--------------------------------------------------------------------------*)

fun convert_to_digit(x) = Char.ord(x)-48;
(*converts a digit in character form to its numerical value*)

fun convert_to_character(n)=chr(n+48);
(*converts a digit to character*)

fun change([])=[] | change(x::xs)=convert_to_digit(x)::change(xs);
(*changes a list of characters to a list of their corresponding digits*)

fun convert(str)=change(String.explode(str));
(*converts a string to a list of characters*)

fun change_back_and_reverse([],y)=y |
  change_back_and_reverse(x::xs,y)=change_back_and_reverse(xs,convert_to_character(x)::y);
(*changes a list of integers to a reversed list of characters and appends it to the second list*)

fun convert_back([])="0" | convert_back(l)=implode(change_back_and_reverse(l,[]));
(*converts a list of digits into its corresponding string representation*)

(*Find Digit Functions--------------------------------------------------------------------------*)

fun check_equation(digit,remainder,bulk)= not(compare(multiply(digit::bulk,digit,0),remainder));
(*gives the value of (digit*digit + bulk*digit*10) <= remainder*)

fun loop(bulk,remainder,digit)= if check_equation(digit,remainder,bulk) then digit 
                                else loop(bulk,remainder,digit-1);
(*returns the largest single digit number less than digit for which check_equation() gives true by
  recursively going through all values less than given digit in decreasing order*)

fun find_digit(bulk,remainder)= loop(bulk,remainder,9);
(*finds the digit to be merged into bulk as the new unit digit*)

(*Long Division Functions------------------------------------------------------------------------*)

fun update_varriables(bulk,remainder,answer,number__)=let val
  dig=find_digit(bulk,remainder) 
  in if length(number__)=0 then
  (convert_back(dig::answer),convert_back(subtract(remainder,multiply(dig::bulk,dig,0),0))) 
     else
       update_varriables(add(dig::bulk,[dig],0),hd(tl(number__))::hd(number__)::subtract(remainder,multiply(dig::bulk,dig,0),0),dig::answer,tl(tl(number__)))
end ;
(*this function recursively performs the long division*)

fun calculate_squareroot(number__)=let val l=length(number__) in if l=0 then ("0","0")
  else if l=1 then update_varriables([],number__,[],[]) else if l mod 2 = 0 then
  update_varriables([],[hd(tl(number__)),hd(number__)],[],tl(tl(number__))) 
                                 else
                                   update_varriables([],[hd(number__)],[],tl(number__)) end;
(*calculates the nearest integer squareroot and the remainder of a number represented as a
 list of digits*)

(*Main Function-----------------------------------------------------------------------------------*)

fun isqrtld(str)=calculate_squareroot(convert(str));
(*calculates the nearest integer squareroot and the remainder of a number represented as a string*)
