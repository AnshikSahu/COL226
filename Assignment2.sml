val input = TextIO.openIn "input.txt";
val output = TextIO.openAppend "output.txt";
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

fun addquote(n : int, s : string, l : char list,lout : char list) = if n = 0 then s else addquote(n - 1, s ^ "<blockquote>");
fun endquote(n : int, s : string, l : char list,lout : char list)= if n = 0 then s else endquote(n - 1, s ^ "</blockquote>");
fun deacactivateindentation(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = ((#1 state, 0, #3 state, #4 state, #5 state, #6 state, #7 state, #8 state), endquote(#2 state, ""), l, lout);

fun increaseheadinglevel(state : int*int*int*int*int*char*char*char,l : char list,lout : char list) =((0, #2 state, #3 state + 1, #4 state, #5 state, #6 state, #7 state, #8 state),"", l ,lout);
fun addheading(state : int*int*int*int*int*char*char*char, l : char list,lout : char list) = ( (1,#2 state,#3 state,#4 state, #5 state, #6 state, #7 state, #8 state), "<h" ^ Int.toString(#3 state) ^ ">", l,lout);
fun deactivateheading(state : int*int*int*int*int*char*char*char, l : char list,lout : char list) = ((#1 state, #2 state, 0, #4 state, #5 state, #6 state, #7 state, #8 state),"</h" ^ Int.toString(#3 state) ^ ">", l,lout );

fun activatelist(state : int*int*int*int*int*char*char*char, l : char list,lout : char list) = ((#1 state, #2 state, #3 state, 1, #5 state, #6 state, #7 state, #8 state),"", l,lout);
fun addorderedlist(state : int*int*int*int*int*char*char*char, l : char list,lout : char list) = ((#1 state, #2 state, #3 state, (#4 state)*2 + 1, #5 state, #6 state, #7 state, #8 state),"<ol>", l ,lout);
fun addunorderedlist(state : int*int*int*int*int*char*char*char, l : char list,lout : char list) = ((#1 state, #2 state, #3 state, (#4 state)*2, #5 state, #6 state, #7 state, #8 state),"<ul>", l,lout);
fun checkforlist(state : int*int*int*int*int*char*char*char, l : char list,lout : char list) = (state,"", l,lout);
fun endpreviouslist(state : int*int*int*int*int*char*char*char,l : char list,lout : char list) = if (#4 state div 2) mod 2 = 1 then 
if #4 state mod 2 = 1 then ((#1 state, #2 state, #3 state, 0, #5 state, #6 state, #7 state, #8 state),"</ol>", l,lout)
else ((#1 state, #2 state, #3 state, 0, #5 state, #6 state, #7 state, #8 state),"</ul>", l,lout)
else if #4 state mod 2 = 1 then ((#1 state, #2 state, #3 state, #4 state div 2, #5 state, #6 state, #7 state, #8 state),"</ol>", l,lout)
else ((#1 state, #2 state, #3 state, #4 state div 2, #5 state, #6 state, #7 state, #8 state),"</ul>", l,lout); 

fun activatebold(state : int*int*int*int*int*char*char*char,l : char list,lout : char list) = ((#1 state, #2 state, #3 state, #4 state, #5 state + 1, #6 state, #7 state, #8 state),"<b>",l,lout);
fun deactivatebold(state : int*int*int*int*int*char*char*char,l : char list,lout : char list) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 1, #6 state, #7 state, #8 state),"</b>",l,lout);

fun activateitalic(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = ((#1 state, #2 state, #3 state, #4 state, #5 state + 2, #6 state, #7 state, #8 state),"<i>", l,lout);
fun deactivateitalic(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 2, #6 state, #7 state, #8 state),"</i>",l,lout);

fun deactivateunderline(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 4, #6 state, #7 state, #8 state),"</u>", l,lout);

fun activateparagraph(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = ((#1 state, #2 state, #3 state, #4 state, #5 state + 8, #6 state, #7 state, #8 state),"<p>", l,lout);
fun deactivateparagraph(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 8, #6 state, #7 state, #8 state),"</p>", l,lout);

fun activatetable(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = if paragraphactive(state) then 
((#1 state, #2 state, #3 state, #4 state, #5 state + 16, #6 state, #7 state, #8 state),"<CENTER><TABLE border="^Int.toString(1)^">", l,lout)
else ((#1 state, #2 state, #3 state, #4 state, #5 state + 16, #6 state, #7 state, #8 state),"<p><CENTER><TABLE border="^Int.toString(1)^">", l,lout);
fun tabledeactivate(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 16, #6 state, #7 state, #8 state),"</TABLE></CENTER>", l,lout);

fun linkdeactivate(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 32, #6 state, #7 state, #8 state),"</a>", l, lout);

fun errorcheck(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = if #1 state <>1 then if linkactive(state) then let val temp=linkdeactivate(state,l,lout) in (#1 temp, "ERROR"^ #2 temp,#3 temp ,#4 temp) end
 else (state,"ERROR", l,lout) else (state,"", l,lout);
fun reset(state : int*int*int*int*int*char*char*char, str, l, lout : char list) = ((1,#2 state,0,#4 state,#5 state mod 32, #6 state, #7 state, #8 state),str ^ "\n", l,lout);
fun completereset(state : int*int*int*int*int*char*char*char, str, l, lout : char list) = let 
val temp1 = if indentation(state) > 0 then deacactivateindentation(state, l,lout) else (state,"", l,lout)
val temp2 = if headingactive(state) then deactivateheading(#1 temp1, #3 temp1,#4 temp1) else (#1 temp1,"", #3 temp1, #4 temp1)
val temp3 = if listactive(state) then endpreviouslist(#1 temp2, #3 temp2,#4 temp2) else (#1 temp2,"", #3 temp2, #4 temp2)
val temp4 = if boldactive(state) then deactivatebold(#1 temp3, #3 temp3,#4 temp3) else (#1 temp3,"", #3 temp3, #4 temp3)
val temp5 = if italicactive(state) then deactivateitalic(#1 temp4, #3 temp4, #4 temp4) else (#1 temp4,"", #3 temp4, #4 temp4)
val temp6 = if tableactive(state) then tabledeactivate(#1 temp5, #3 temp5, #4 temp5) else (#1 temp5,"", #3 temp5, #4 temp5)
val temp7 = if paragraphactive(state) then deactivateparagraph(#1 temp6, #3 temp6,#4 temp6) else (#1 temp6,"", #3 temp6, #4 temp6)
val temp8 = if underlineactive(state) then deactivateunderline(#1 temp7, #3 temp7, #4 temp7) else (#1 temp7,"",#3 temp7, #4 temp7)
 in (#1 temp8, str ^ #2 temp4 ^ #2 temp5 ^ #2 temp8 ^ #2 temp6 ^ #2 temp7 ^ #2 temp3 ^ #2 temp2 ^ #2 temp1 ^ "\n", #3 temp8, #4 temp8 ) end;



fun matchpattern(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = if #8 state = #"\n" then 
if #7 state = #"\n" then completereset(errorcheck(state, l,lout)) else reset(errorcheck(state, l,lout))
else if #7 state= #"\n" andalso #8 state = #"#" then increaseheadinglevel(state, l,lout) 
else if deciding(state) andalso headingactive(state) andalso #8 state = #"#" then increaseheadinglevel(state, l,lout)
else if deciding(state) andalso headingactive(state) andalso #7 state = #"#" then let val temp1=addheading(state,l,lout) val temp2= deactivateheading(#1 temp1,#3 temp1,#4 temp1) val temp3=matchpattern(#1 temp2, #3 temp2,#4 temp2) in (#1 temp3, #2 temp1 ^ #2 temp2 ^ #2temp3, #3 temp3, #4 temp3) end
else if headingactive(state) andalso then (state, String.str (#8 state),l,lout);





fun indent(state : int*int*int*int*int*char*char*char,n, l : char list, lout : char list) = case l of
[] => (state,"",l,lout)
| c :: xs => if c = #">" then indent((#1 state, #2 state + 1, #3 state, #4 state, #5 state, #6 state, #7 state, #8 state),n+1,xs,lout)
else if n > #2 state then let val temp=matchpattern((#1 state, #2 state, #3 state, #4 state, #5 state, #7 state, #8 state,c),xs,lout) in (#1 temp, addquote(n- ( #2 state),"",#3 temp,#4 temp) ^ #2 temp,#3 temp,#4 temp) end
else matchpattern((#1 state, #2 state, #3 state, #4 state, #5 state, #7 state, #8 state,c),xs,lout);
fun activateindentation(state : int*int*int*int*int*char*char*char,l : char list, lout : char list) =indent((#1 state, #2 state, #3 state, #4 state, #5 state, #6 state, #7 state, #8 state),1,l,lout);

fun underline(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = (state,"",l,lout);
fun activateunderline(state : int*int*int*int*int*char*char*char,l : char list, lout : char list) = underline((#1 state, #2 state, #3 state, #4 state, #5 state + 4, #6 state, #7 state, #8 state),l,lout);

fun createlink(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = (state,"", l, lout);
fun activatelink(state : int*int*int*int*int*char*char*char, l : char list, lout : char list) = createlink((#1 state, #2 state, #3 state, #4 state, #5 state + 32, #6 state, #7 state, #8 state), l,lout);

<<<<<<< HEAD
fun append( state : int*int*int*int*int*char*char*char, sentence) =let val
  _=print("***"^sentence^"***") val _= if
  sentence="" then print("") else TextIO.output (output, sentence) in state end;
=======
fun append( state : int*int*int*int*int*char*char*char, sentence,lin : char list, lout : char list) = (state,lin, sentence :: lout);
>>>>>>> 874ae8b89a2c103c108c3b250061b44b978d2c44

fun parse( state : int*int*int*int*int*char*char*char, l : char list , lout : char list) = case c of 
 [] => append(matchpattern((#1 state, #2 state, #3 state, #4 state, #5 state, #"\n", #"\n" , #"\n" ),l, lout))  
| c :: xs => parse(append(matchpattern((#1 state, #2 state, #3 state, #4 state, #5 state, #7 state, #8 state, x),xs, lout)));

fun main() = parse((1,0,0,0,0, #"\n", #"\n", #"\n"),l,[]);
val _ = TextIO.closeOut output;
val _ = TextIO.closeIn input;
