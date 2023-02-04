val input = TextIO.openIn "input.txt";
val output = TextIO.openOut "output.txt";

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

fun addquote(n : int, s : string) = if n = 0 then s else addquote(n - 1, s ^ "<blockquote>");
fun endquote(n : int, s : string)= if n = 0 then s else endquote(n - 1, s ^ "</blockquote>");
fun deacactivateindentation(state : int*int*int*int*int*char*char*char) = ((#1 state, 0, #3 state, #4 state, #5 state, #6 state, #7 state, #8 state), endquote(#2 state, ""));

fun increaseheadinglevel(state : int*int*int*int*int*char*char*char) =((0, #2 state, #3 state + 1, #4 state, #5 state, #6 state, #7 state, #8 state),"");
fun addheading(state : int*int*int*int*int*char*char*char) = ( (1,#2 state,#3 state,#4 state, #5 state, #6 state, #7 state, #8 state), "<h" ^ Int.toString(#3 state) ^ ">");
fun deactivateheading(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, 0, #4 state, #5 state, #6 state, #7 state, #8 state),"</h" ^ Int.toString(#3 state) ^ ">" );

fun activatelist(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, 1, #5 state, #6 state, #7 state, #8 state),"");
fun addorderedlist(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, (#4 state)*2 + 1, #5 state, #6 state, #7 state, #8 state),"<ol>");
fun addunorderedlist(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, (#4 state)*2, #5 state, #6 state, #7 state, #8 state),"<ul>");
fun checkforlist(state : int*int*int*int*int*char*char*char) = (state,"");
fun endpreviouslist(state : int*int*int*int*int*char*char*char) = if (#4 state div 2) mod 2 = 1 then 
if #4 state mod 2 = 1 then ((#1 state, #2 state, #3 state, 0, #5 state, #6 state, #7 state, #8 state),"</ol>")
else ((#1 state, #2 state, #3 state, 0, #5 state, #6 state, #7 state, #8 state),"</ul>")
else if #4 state mod 2 = 1 then ((#1 state, #2 state, #3 state, #4 state div 2, #5 state, #6 state, #7 state, #8 state),"</ol>")
else ((#1 state, #2 state, #3 state, #4 state div 2, #5 state, #6 state, #7 state, #8 state),"</ul>"); 

fun activatebold(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state + 1, #6 state, #7 state, #8 state),"<b>");
fun deactivatebold(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 1, #6 state, #7 state, #8 state),"</b>");

fun activateitalic(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state + 2, #6 state, #7 state, #8 state),"<i>");
fun deactivateitalic(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 2, #6 state, #7 state, #8 state),"</i>");

fun deactivateunderline(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 4, #6 state, #7 state, #8 state),"</u>");

fun activateparagraph(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state + 8, #6 state, #7 state, #8 state),"<p>");
fun deactivateparagraph(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 8, #6 state, #7 state, #8 state),"</p>");

fun activatetable(state : int*int*int*int*int*char*char*char) = if paragraphactive(state) then 
((#1 state, #2 state, #3 state, #4 state, #5 state + 16, #6 state, #7 state, #8 state),"<CENTER><TABLE border="^Int.toString(1)^">")
else ((#1 state, #2 state, #3 state, #4 state, #5 state + 16, #6 state, #7 state, #8 state),"<p><CENTER><TABLE border="^Int.toString(1)^">");
fun tabledeactivate(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 16, #6 state, #7 state, #8 state),"</TABLE></CENTER>");

fun linkdeactivate(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 32, #6 state, #7 state, #8 state),"</a>");

fun errorcheck(state : int*int*int*int*int*char*char*char) = if #1 state <>1 then if linkactive(state) then let val temp=linkdeactivate(state) in (#1 temp, "ERROR"^ #2 temp) end
 else (state,"ERROR") else (state,"");
fun reset(state : int*int*int*int*int*char*char*char, str) = ((1,#2 state,0,#4 state,#5 state mod 32, #6 state, #7 state, #8 state),str ^ "\n");
fun completereset(state : int*int*int*int*int*char*char*char, str) = let 
val temp1 = if indentation(state) > 0 then deacactivateindentation(state) else (state,"")
val temp2 = if headingactive(state) then deactivateheading(#1 temp1) else (#1 temp1,"")
val temp3 = if listactive(state) then endpreviouslist(#1 temp2) else (#1 temp2,"")
val temp4 = if boldactive(state) then deactivatebold(#1 temp3) else (#1 temp3,"")
val temp5 = if italicactive(state) then deactivateitalic(#1 temp4) else (#1 temp4,"")
val temp6 = if tableactive(state) then tabledeactivate(#1 temp5) else (#1 temp5,"")
val temp7 = if paragraphactive(state) then deactivateparagraph(#1 temp6) else (#1 temp6,"")
val temp8 = if underlineactive(state) then deactivateunderline(#1 temp7) else (#1 temp7,"")
 in (#1 temp8, str ^ #2 temp4 ^ #2 temp5 ^ #2 temp8 ^ #2 temp6 ^ #2 temp7 ^ #2 temp3 ^ #2 temp2 ^ #2 temp1 ^ "\n" ) end;



fun matchpattern(state : int*int*int*int*int*char*char*char) = if #8 state = #"\n" then 
if #7 state = #"\n" then completereset(errorcheck(state)) else reset(errorcheck(state))
else if #7 state= #"\n" andalso #8 state = #"#" then increaseheadinglevel(state) 
else if deciding(state) andalso headingactive(state) andalso #8 state = #"#" then increaseheadinglevel(state)
else if deciding(state) andalso headingactive(state) andalso #7 state = #"#" then let val temp1=addheading(state) val temp2=matchpattern(#1 temp1) in (#1 temp2, #2 temp1 ^ #2 temp2) end
else (state, String.str (#8 state));





fun indent(state : int*int*int*int*int*char*char*char,n) = let val SOME c= TextIO.input1 input in if c = #">" then indent((#1 state, #2 state + 1, #3 state, #4 state, #5 state, #6 state, #7 state, #8 state),n+1)
else if n > #2 state then let val temp=matchpattern((#1 state, #2 state, #3 state, #4 state, #5 state, #7 state, #8 state,c)) in (#1 temp, addquote(n- ( #2 state),"") ^ #2 temp) end
else matchpattern((#1 state, #2 state, #3 state, #4 state, #5 state, #7 state, #8 state,c)) end;
fun activateindentation(state : int*int*int*int*int*char*char*char) =indent((#1 state, #2 state, #3 state, #4 state, #5 state, #6 state, #7 state, #8 state),1);

fun underline(state : int*int*int*int*int*char*char*char) = (state,"");
fun activateunderline(state : int*int*int*int*int*char*char*char) = underline(#1 state, #2 state, #3 state, #4 state, #5 state + 4, #6 state, #7 state, #8 state);

fun createlink(state : int*int*int*int*int*char*char*char) = (state,"");
fun activatelink(state : int*int*int*int*int*char*char*char) = createlink(#1 state, #2 state, #3 state, #4 state, #5 state + 32, #6 state, #7 state, #8 state);

fun append( state : int*int*int*int*int*char*char*char, sentence) =let val _=TextIO.output (output, sentence) in state end;

fun parse( state : int*int*int*int*int*char*char*char) = let val c = TextIO.input1 input val SOME c_ = c in
 if (c = NONE) then append(matchpattern((#1 state, #2 state, #3 state, #4 state, #5 state, #"\n", #"\n" , #"\n" ))) 
 else parse(append(matchpattern((#1 state, #2 state, #3 state, #4 state, #5 state, #7 state, #8 state, c_)))) end;

fun main() = parse((1,0,0,0,0, #"\n", #"\n", #"\n"));
val _ = TextIO.closeOut output;
val _ = TextIO.closeIn input;