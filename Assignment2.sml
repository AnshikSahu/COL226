val input = TextIO.openIn "input.txt";
val output = TextIO.openOut "output.txt";

fun ignore(state : int*int*int*int*int*char*char*char) = #1 state = -1;
fun deciding(state : int*int*int*int*int*char*char*char) = #1 state = 0;
fun reading(state : int*int*int*int*int*char*char*char) = #1 state = 1;

fun indentation(state : int*int*int*int*int*char*char*char) = #2 state;
fun increaseindentation(state : int*int*int*int*int*char*char*char) =((#1 state, #2 state + 1, #3 state, #4 state, #5 state, #6 state, #7 state, #8 state), "<blockquote>");
fun endquote(n : int, s : string)= if n = 0 then s else endquote(n - 1, s ^ "</blockquote>");
fun deacactivateindentation(state : int*int*int*int*int*char*char*char) = ((#1 state, 0, #3 state, #4 state, #5 state, #6 state, #7 state, #8 state), endquote(#2 state, ""));

fun heading(state : int*int*int*int*int*char*char*char) = #3 state;
fun increaseheadinglevel(state : int*int*int*int*int*char*char*char) =((#1 state, #2 state, #3 state + 1, #4 state, #5 state, #6 state, #7 state, #8 state),"");
fun addheading(state : int*int*int*int*int*char*char*char) = ( state, "<h" ^ Int.toString(#3 state) ^ ">");
fun deactivateheading(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, 0, #4 state, #5 state, #6 state, #7 state, #8 state),"</h" ^ Int.toString(#3 state) ^ ">" );

fun listactive(state : int*int*int*int*int*char*char*char) = #4 state > 0;
fun activatelist(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, 1, #5 state, #6 state, #7 state, #8 state),"");
fun addorderedlist(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, (#4 state)*2 + 1, #5 state, #6 state, #7 state, #8 state),"<ol>");
fun addunorderedlist(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, (#4 state)*2, #5 state, #6 state, #7 state, #8 state),"<ul>");
fun checkforlist(state : int*int*int*int*int*char*char*char) = ;
fun endpreviouslist(state : int*int*int*int*int*char*char*char) = if (#4 state div 2) mod 2 = 1 then 
if #4 state mod 2 = 1 then ((#1 state, #2 state, #3 state, 0, #5 state, #6 state, #7 state, #8 state),"</ol>")
else ((#1 state, #2 state, #3 state, 0, #5 state, #6 state, #7 state, #8 state),"</ul>")
else if #4 state mod 2 = 1 then ((#1 state, #2 state, #3 state, #4 state div 2, #5 state, #6 state, #7 state, #8 state),"</ol>")
else ((#1 state, #2 state, #3 state, #4 state div 2, #5 state, #6 state, #7 state, #8 state),"</ul>"); 

fun boldactive(state : int*int*int*int*int*char*char*char) = #5 state mod 2 = 1;
fun activatebold(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state + 1, #6 state, #7 state, #8 state),"<b>");
fun deactivatebold(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 1, #6 state, #7 state, #8 state),"</b>");

fun italicactive(state : int*int*int*int*int*char*char*char) = (#5 state div 2) mod 2 = 1;
fun activateitalic(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state + 2, #6 state, #7 state, #8 state),"<i>");
fun deactivateitalic(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 2, #6 state, #7 state, #8 state),"</i>");

fun underlineactive(state : int*int*int*int*int*char*char*char) = (#5 state div 4) mod 2 = 1;
fun underline(state : int*int*int*int*int*char*char*char) = ;
fun activateunderline(state : int*int*int*int*int*char*char*char) = underline(#1 state, #2 state, #3 state, #4 state, #5 state + 4, #6 state, #7 state, #8 state);
fun deactivateunderline(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 4, #6 state, #7 state, #8 state),"</u>");

fun paragraphactive(state : int*int*int*int*int*char*char*char) = (#5 state div 8) mod 2 = 1;
fun activateparagraph(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state + 8, #6 state, #7 state, #8 state),"<p>");
fun deactivateparagraph(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 8, #6 state, #7 state, #8 state),"</p>");

fun tableactive(state : int*int*int*int*int*char*char*char) = (#5 state div 16) mod 2 = 1;
fun activatetable(state : int*int*int*int*int*char*char*char) = if paragraphactive(state) then 
((#1 state, #2 state, #3 state, #4 state, #5 state + 16, #6 state, #7 state, #8 state),"<CENTER><TABLE border="1">")
else ((#1 state, #2 state, #3 state, #4 state, #5 state + 16, #6 state, #7 state, #8 state),"<p><CENTER><TABLE border="1">");
fun tabledeactivate(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 16, #6 state, #7 state, #8 state),"</TABLE></CENTER>");

fun linkactive(state : int*int*int*int*int*char*char*char) = (#5 state div 32) mod 2 = 1;
fun createlink(state : int*int*int*int*int*char*char*char) = ;
fun activatelink(state : int*int*int*int*int*char*char*char) = createlink(#1 state, #2 state, #3 state, #4 state, #5 state + 32, #6 state, #7 state, #8 state);
fun linkdeactivate(state : int*int*int*int*int*char*char*char) = ((#1 state, #2 state, #3 state, #4 state, #5 state - 32, #6 state, #7 state, #8 state),"</a>");

fun matchpattern(state : int*int*int*int*int*char*char*char, input) = ;

fun append( state : int*int*int*int*int*char*char*char, sentence) =let val _=TextIO.output (output, sentences) in state end;

fun parse( state : int*int*int*int*int*char*char*char) = let val c = TextIO.inputChar input in
 if (c = NONE) then append(matchpattern((#1 state, #2 state, #3 state, #4 state, #5 state, # "\n", # "\n" , # "\n" ))) 
 else parse(append(matchpattern((#1 state, #2 state, #3 state, #4 state, #5 state, #7 state, #8 state, c)))) end;

fun main() = parse((0,0,0,0,0, "\n", "\n", "\n"));
val _ = TextIO.closeOut output;
val _ = TextIO.closeIn input;