fun countdigits(x:String)=case x of
x::xs=1+countdigits(xs)
""=0;
