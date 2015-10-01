(* Mathematica Package *)

(* Created by the Wolfram Workbench Sep 30, 2015 *)

BeginPackage["SimpleSet`"]
(* Exported symbols added here with SymbolName::usage *) 

setMake::usage = "";
singletonQ::usage = "";

Begin["`Private`"]
(* Implementation of the package *)


(*set // ClearAll*)
set[ass_Association]~Format~TraditionalForm := 
 BracketingBar @@ (ass@"elems") /; ass@"type" == "immediate"
set[ass_Association]@property_String := ass@property
set /: Normal[A_set] /; A@"type" == "immediate" := A@"elems"
set /: x_ \[Element] A_set /; A@"type" == "immediate" := 
 Or @@ Thread[x == A@"elems"]

(*set /: xs_List \[Subset] A_set /; A@"type" == "immediate" := 
 And @@ (# \[Element] A & /@ xs)*)
set /: B_set \[Subset] A_set /; 
  A@"type" == "immediate" \[And] B@"type" == "immediate" := 
 B@"elems" \[Subset] A
 
set /: A_set \[Intersection] B_set /; 
  A@"type" == "immediate" \[And] B@"type" == "immediate" := 
 A@"elems" \[Intersection] B@"elems" // setMake
set /: A_set \[Union] B_set /; 
  A@"type" == "immediate" \[And] B@"type" == "immediate" := 
 A@"elems" \[Union] B@"elems" // setMake
set /: A_set~Complement~B_set /; 
  A@"type" == "immediate" \[And] B@"type" == "immediate" := 
 A@"elems"~Complement~B@"elems" // setMake
set /: Length@A_set /; A@"type" == "immediate" := Length@A@"elems"
set /: Map[f_, A_set, levelspec_: {1}] /; A@"type" == "immediate" := 
 Map[f, A@"elems", levelspec] // setMake
 
set /: Subsets[A_set,args___] /; A@"type" == "immediate" := setMake /@ Subsets@A@"elems" // setMake 
 
(*setMake // Clear*)
setMake[]:=setMake@{}
setMake@elems_List := 
 set@Association["elems" -> Union@elems, "type" -> "immediate"]
setMake[Reals] := set@Association["elems" -> Reals, "type" -> "abstruct"]
(*singletonQ // Clear*)
singletonQ@A_set /; A@"type" == "immediate" := Length@A == 1


End[]

EndPackage[]

