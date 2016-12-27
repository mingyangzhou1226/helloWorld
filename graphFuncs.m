(* ::Package:: *)

(* ::Title:: *)
(*MM graphics package*)


(*Histograms*)


fHistographData[data_,range_]:= Module[
	{tmp,f1,f2},
	f1[x_]:=If[x>= range[[2]],range[[2]]-range[[3]],x];
	f2[x_]:=If[x< range[[1]],range[[1]],x];
	tmp = f1/@data ;
	tmp = f2/@tmp;
	tmp 
]


(*Pie chart*)


(*list plot*)


(*date list plot*)


(*% bar chart *)
