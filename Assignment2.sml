val input = TextIO.openIn "input.txt";
val l= explode(TextIO.inputAll input);

fun deciding(state : int*int*int*int*int*char*char*char) = #1 state = 0;
fun reading(state : int*int*int*int*int*char*char*char) = #1 state = 1;

fun linkactive(state : int*int*int*int*int*char*char*char) = (#5 state div 32) mod 2 = 1;
fun indentation(state : int*int*int*int*int*char*char*char) = #2 state;
fun headingactive(state : int*int*int*int*int*char*char*char) = #3 state>0;
fun listactive(state : int*int*int*int*int*char*char*char) = #4 state > 0;
fun boldactive(state : int*int*int*int*int*char*char*char) = #5 state mod 2 = 1;
fun italicactive(state : int*int*int*int*int*char*char*char) = (#5 state div 2) mod 2 = 1;
fun underlineactive(state : int*int*int*int*int*char*char*char) = (#5 state div 4) mod 2 = 1;
fun paragraphactive(state : int*int*int*int*int*char*char*char) = (#5 state div 8) mod 2 = 1;
fun tableactive(state : int*int*int*int*int*char*char*char) = (#5 state div 16) mod 2 = 1;

fun addquote(n : int, s : string) = if n = 0 then s else addquote(n - 1,  "<blockquote>" ^ s);
fun endquote(n : int, s : string)= if n = 0 then s else endquote(n - 1, s ^ "</blockquote>");
fun deacactivateindentation(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = ((1, 0, #3 state, #4 state, #5 state, #6 state, #7 state, #8 state), endquote(#2 state,""), l, lout);

fun increaseheadinglevel(state : int*int*int*int*int*char*char*char,l : char list,lout : string list) =((0, #2 state, #3 state + 1, #4 state, #5 state, #6 state, #7 state, #8 state),"", l ,lout);
fun addheading(state : int*int*int*int*int*char*char*char, l : char list,lout : string list) = ( (1,#2 state,#3 state,#4 state, #5 state, #6 state, #7 state, #8 state), "<h" ^ Int.toString(#3 state) ^ ">", l,lout);
fun deactivateheading(state : int*int*int*int*int*char*char*char, l : char list,lout : string list) = ((1, #2 state, 0, #4 state, #5 state, #6 state, #7 state, #8 state),"</h" ^ Int.toString(#3 state) ^ ">", l,lout );

fun activatelist(state : int*int*int*int*int*char*char*char, l : char list,lout : string list) = ((#1 state, #2 state, #3 state, 1, #5 state, #6 state, #7 state, #8 state),"", l,lout);
fun addorderedlist(state : int*int*int*int*int*char*char*char, l : char list,lout : string list) = ((#1 state, #2 state, #3 state, (#4 state)*2 + 1, #5 state, #6 state, #7 state, #8 state),"<ol>", l ,lout);
fun addunorderedlist(state : int*int*int*int*int*char*char*char, l : char list,lout : string list) = ((#1 state, #2 state, #3 state, (#4 state)*2, #5 state, #6 state, #7 state, #8 state),"<ul>", l,lout);
fun checkforlist(state : int*int*int*int*int*char*char*char, l : char list,lout : string list) = (state,"", l,lout);
fun endpreviouslist(state : int*int*int*int*int*char*char*char,l : char list,lout : string list) = if (#4 state div 2) mod 2 = 1 then 
if #4 state mod 2 = 1 then ((#1 state, #2 state, #3 state, 0, #5 state, #6 state, #7 state, #8 state),"</ol>", l,lout)
else ((#1 state, #2 state, #3 state, 0, #5 state, #6 state, #7 state, #8 state),"</ul>", l,lout)
else if #4 state mod 2 = 1 then ((#1 state, #2 state, #3 state, #4 state div 2, #5 state, #6 state, #7 state, #8 state),"</ol>", l,lout)
else ((#1 state, #2 state, #3 state, #4 state div 2, #5 state, #6 state, #7 state, #8 state),"</ul>", l,lout); 

fun activatebold(state : int*int*int*int*int*char*char*char,l : char list,lout : string list) = ((1, #2 state, #3 state, #4 state, #5 state + 1, #6 state, #7 state, #8 state),"<b>",l,lout);
fun deactivatebold(state : int*int*int*int*int*char*char*char,l : char list,lout : string list) = ((1, #2 state, #3 state, #4 state, #5 state - 1, #6 state, #7 state, #8 state),"</b>",l,lout);

fun activateitalic(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = ((1, #2 state, #3 state, #4 state, #5 state + 2, #6 state, #7 state, #8 state),"<i>", l,lout);
fun deactivateitalic(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = ((1, #2 state, #3 state, #4 state, #5 state - 2, #6 state, #7 state, #8 state),"</i>",l,lout);

fun activateunderline(state : int*int*int*int*int*char*char*char,l : char list, lout : string list) = ((#1 state, #2 state, #3 state, #4 state, (#5 state div 8)*8 + 4 + #5 state mod 4, #6 state, #7 state, #8 state),"<u>",l,lout);

fun createlink(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = (state,"", l, lout);
fun activatelink(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = createlink((#1 state, #2 state, #3 state, #4 state, (#5 state div 64)*64 + 32 + #5 state mod 32, #6 state, #7 state, #8 state), l,lout);

fun deactivateunderline(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 4, #6 state, #7 state, #8 state),"</u>", l,lout);

fun activateparagraph(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = ((1, #2 state, #3 state, #4 state, (#5 state div 16)*16 + 8 + #5 state mod 8,#"\n", #6 state, #7 state),"<p>", #8 state :: l,lout);
fun deactivateparagraph(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = ((1, #2 state, #3 state, #4 state, #5 state - 8, #6 state, #7 state, #8 state),"</p>", l,lout);

fun activatetable(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = if paragraphactive(state) then 
((#1 state, #2 state, #3 state, #4 state, (#5 state div 32)*32 + 16 + #5 state mod 16, #6 state, #7 state, #8 state),"<CENTER><TABLE border="^Int.toString(1)^">", l,lout)
else ((#1 state, #2 state, #3 state, #4 state, (#5 state div 32)*32 + 16 + #5 state mod 16, #6 state, #7 state, #8 state),"<p><CENTER><TABLE border="^Int.toString(1)^">", l,lout);
fun tabledeactivate(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 16, #6 state, #7 state, #8 state),"</TABLE></CENTER>", l,lout);

fun linkdeactivate(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 32, #6 state, #7 state, #8 state),"</a>", l, lout);

fun indent(state : int*int*int*int*int*char*char*char,n, l : char list, lout : string list) = case l of
[] => if n > #2 state then let val temp1 = if underlineactive(state)  then deactivateunderline(state, l,lout) else (state,"", l,lout)
val temp2 = if boldactive(#1 temp1) then deactivatebold(#1 temp1, #3 temp1,#4 temp1) else (#1 temp1,"", #3 temp1, #4 temp1)
val temp3 = if italicactive(#1 temp2) then deactivateitalic(#1 temp2, #3 temp2,#4 temp2) else (#1 temp2,"", #3 temp2, #4 temp2)
val temp4 = if paragraphactive(#1 temp3) then deactivateparagraph(#1 temp3, #3 temp3,#4 temp3) else (#1 temp3,"", #3 temp3, #4 temp3)
val temp5 = if tableactive(#1 temp4) then tabledeactivate(#1 temp4, #3 temp4, #4 temp4) else (#1 temp4,"", #3 temp4, #4 temp4)
val st= #1 temp5
val s=(1, n , #3 st,#4 st,#5 st, #6 st, #7 st, #8 st)
 in (#1 temp4, #2 temp1 ^ #2 temp2 ^ #2 temp3 ^ #2 temp4 ^ #2 temp5 ^ addquote(n- ( #2 ( #1 temp5)),"") ,l,lout) end
else ((1, #2 state, #3 state, #4 state, #5 state,#6 state, #7 state, #8 state),"",l,lout)
| c :: xs => if c = #">" then indent((0, #2 state, #3 state, #4 state, #5 state, #6 state, #7 state, #8 state),n+1,xs,lout)
else if n > #2 state then let val temp1 = if underlineactive(state)  then deactivateunderline(state, l,lout) else (state,"", l,lout)
val temp2 = if boldactive(#1 temp1) then deactivatebold(#1 temp1, #3 temp1,#4 temp1) else (#1 temp1,"", #3 temp1, #4 temp1)
val temp3 = if italicactive(#1 temp2) then deactivateitalic(#1 temp2, #3 temp2,#4 temp2) else (#1 temp2,"", #3 temp2, #4 temp2)
val temp4 = if paragraphactive(#1 temp3) then deactivateparagraph(#1 temp3, #3 temp3,#4 temp3) else (#1 temp3,"", #3 temp3, #4 temp3)
val temp5 = if tableactive(#1 temp4) then tabledeactivate(#1 temp4, #3 temp4, #4 temp4) else (#1 temp4,"", #3 temp4, #4 temp4) 
val st= #1 temp5
val s=(1, n , #3 st,#4 st,#5 st, #6 st, #7 st, #8 st)
in (s, #2 temp1 ^ #2 temp2 ^ #2 temp3 ^ #2 temp4 ^ #2 temp5 ^ addquote(n- ( #2 ( #1 temp5)),"") ,l,lout) end
else ((1, #2 state, #3 state, #4 state, #5 state,#6 state, #7 state, #8 state),"",l,lout);
fun activateindentation(state : int*int*int*int*int*char*char*char,l : char list, lout : string list) =indent((0, #2 state , #3 state, #4 state,  #5 state, #6 state, #7 state, #8 state),1,l,lout);

fun errorcheck(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = if #1 state <>1 then if linkactive(state) then let val temp=linkdeactivate(state,l,lout) in (#1 temp, "ERROR1"^ #2 temp,#3 temp ,#4 temp) end
 else (state,"ERROR2", l,lout) else (state,"", l,lout);
fun reset(state : int*int*int*int*int*char*char*char, str, l, lout : string list) = let val temp=if headingactive(state) then deactivateheading(state,l,lout) else (state,"",l,lout) in(#1 temp,str^ #2 temp ^ "\n", #3 temp,#4 temp) end;
fun completereset(state : int*int*int*int*int*char*char*char, str, l, lout : string list) = let 
val temp1 = if underlineactive(state)  then deactivateunderline(state, l,lout) else (state,"", l,lout)
val temp2 = if boldactive(#1 temp1) then deactivatebold(#1 temp1, #3 temp1,#4 temp1) else (#1 temp1,"", #3 temp1, #4 temp1)
val temp3 = if italicactive(#1 temp2) then deactivateitalic(#1 temp2, #3 temp2,#4 temp2) else (#1 temp2,"", #3 temp2, #4 temp2)
val temp4 = if paragraphactive(#1 temp3) then deactivateparagraph(#1 temp3, #3 temp3,#4 temp3) else (#1 temp3,"", #3 temp3, #4 temp3)
val temp5 = if tableactive(#1 temp4) then tabledeactivate(#1 temp4, #3 temp4, #4 temp4) else (#1 temp4,"", #3 temp4, #4 temp4)
val temp6 = if listactive(#1 temp5) then endpreviouslist(#1 temp5, #3 temp5, #4 temp5) else (#1 temp5,"", #3 temp5, #4 temp5)
val temp7 = if headingactive(#1 temp6) then deactivateheading(#1 temp6, #3 temp6,#4 temp6) else (#1 temp6,"", #3 temp6, #4 temp6)
val temp8 = if indentation(#1 temp7) > 0 then deacactivateindentation(#1 temp7, #3 temp7, #4 temp7) else (#1 temp7,"",#3 temp7, #4 temp7)
 in (#1 temp8, str ^ #2 temp1 ^ #2 temp2 ^ #2 temp3 ^ #2 temp4 ^ #2 temp5 ^ #2 temp6 ^ #2 temp7 ^ #2 temp8 ^ "\n", #3 temp8, #4 temp8 ) end;

fun matchpattern(state : int*int*int*int*int*char*char*char, l : char list, lout : string list) = if #8 state = #"\n" andalso tableactive(state) = false andalso paragraphactive(state)= false then  reset(errorcheck(state, l,lout)) else if #7 state= #"\n" andalso #8 state= #"\n" then completereset(errorcheck(state, l,lout))
else if #7 state= #"\n" andalso #8 state = #"#" then let val temp1= completereset(errorcheck(state,l,lout)) val temp2=increaseheadinglevel(#1 temp1,#3 temp1,#4 temp1) in (#1 temp2, #2 temp1 ^ #2 temp2, #3 temp2, #4 temp2) end
else if deciding(state) andalso headingactive(state) andalso #8 state = #"#" then increaseheadinglevel(state, l,lout)
else if deciding(state) andalso headingactive(state) andalso #7 state = #"#" then 
let val temp1=addheading(state,l,lout)  val temp3=matchpattern(#1 temp1, #3 temp1,#4 temp1) in (#1 temp3, #2 temp1 ^ #2temp3, #3 temp3, #4 temp3) end
else if headingactive(state) andalso reading(state) then (state, String.str (#8 state),l,lout)
else if #7 state= #"\n" andalso #8 state = #">" andalso tableactive(state)= false andalso indentation(state)=0 then let val temp1= completereset(errorcheck(state,l,lout)) val temp2=activateindentation(#1 temp1,#3 temp1,#4 temp1) in (#1 temp2, #2 temp1 ^ #2 temp2, #3 temp2, #4 temp2) end
else if indentation(state)>0 andalso #7 state = #"\n" andalso #8 state <> #">" then completereset(errorcheck((#1 state, #2 state, #3 state, #4 state, #5 state , #"\n", #6 state, #7 state),#8 state :: l,lout))
else if indentation(state)>0 andalso #8 state = #">" then activateindentation(state,l,lout)
else if (paragraphactive(state) orelse headingactive(state)) andalso reading(state) andalso #8 state= #"*" then ((0, #2 state, #3 state, #4 state, #5 state , #6 state, #7 state, #8 state) ,"",l,lout)
else if (paragraphactive(state) orelse headingactive(state)) andalso deciding(state) andalso #7 state= #"*" then
if #8 state= #"*" then if boldactive(state) then deactivatebold(state,l,lout) else activatebold(state,l,lout)
else if italicactive(state) then deactivateitalic((#1 state, #2 state, #3 state, #4 state, #5 state, #"\n", #6 state, #7 state),#8 state :: l, lout)
else activateitalic((#1 state, #2 state, #3 state, #4 state, #5 state, #"\n", #6 state, #7 state),#8 state :: l, lout)
else if underlineactive(state) andalso #8 state = #"_" then if l=[] then deactivateunderline(state,l,lout) else if hd(l)= #" " orelse hd(l)= #"\n" then deactivateunderline(state,l,lout) else (state," ",l,lout)
else if underlineactive(state) andalso #8 state = #" " then deactivateunderline((#1 state, #2 state, #3 state, #4 state, #5 state, #"\n", #6 state, #7 state), #8 state :: l,lout)
else if (paragraphactive(state) orelse headingactive(state)) andalso reading(state) andalso underlineactive(state)=false andalso #8 state = #"_"  then activateunderline(state,l,lout)
else if paragraphactive(state)=false andalso #8 state= #"-" then if l=[] then (state,"Error3",l,lout) else if hd(l)<> #"-" then (state,"Error3",l,lout) else if tl(l)=[] then (state,"Error3",l,lout) else if hd(tl(l))<> #"-" then (state,"Error3",l,lout) else ((state,"<hr>",tl(tl(l)),lout))
else if paragraphactive(state) andalso #8 state= #"-" then if l=[] then (state,"Error3",l,lout) else if hd(l)<> #"-" then (state,"Error3",l,lout) else if tl(l)=[] then (state,"Error3",l,lout) else if hd(tl(l))<> #"-" then (state,"Error3",l,lout) else let val temp=deactivateparagraph(state,tl(tl(l)),lout) in (#1 temp,#2 temp ^ "<hr>", #3 temp,#4 temp) end
else if paragraphactive(state)= false then activateparagraph(state,l,lout)
else (state,Char.toString(#8 state),l,lout);








fun append( state : int*int*int*int*int*char*char*char, sentence,lin : char list, lout : string list) = (state,lin, sentence :: lout);

fun parse( state : int*int*int*int*int*char*char*char, l : char list , lout : string list) =let val _=print(Int.toString(#1 state) ^ Int.toString(#2 state) ^ Int.toString(#3 state) ^ Int.toString(#4 state) ^ Int.toString(#5 state) ^ Char.toString(#8 state) ^ hd(lout) ^ "\n") in case l of 
 [] => append(matchpattern((#1 state, #2 state, #3 state, #4 state, #5 state, #"\n", #"\n" , #"\n" ),l, lout))  
| c :: xs => parse(append(matchpattern((#1 state, #2 state, #3 state, #4 state, #5 state, #7 state, #8 state, c),xs, lout))) end;

fun main() = parse((1,0,0,0,0, #"\n", #"\n", #"\n"),l,[""]);
fun write(lout) =let val output = TextIO.openOut "output.txt"
        fun writestrings [] = TextIO.closeOut output
          | writestrings (x::xs) = (TextIO.output (output, x ); writestrings xs) in writestrings(lout) end;
fun reverse(state : int*int*int*int*int*char*char*char, l : char list , lout : string list) = rev(lout);
val _=write(reverse(main()));
val _ = TextIO.closeIn input;