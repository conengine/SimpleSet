(* Mathematica Package *)

(* Created by the Wolfram Workbench Sep 30, 2015 *)

BeginPackage["SimpleSet`"]
(* Exported symbols added here with SymbolName::usage *) 

set::usage = "";
setMake::usage = "";

singletonQ::usage = "";
immediateSetQ::usage = "";
abstructSetQ::usage = "";

Begin["`Private`"]
(* Implementation of the package *)

(* TODO:Move 'abstructSets' from Module to private.*)
(* TODO:implement "comprehension" mechanism *)
(* TODO:implement all method of abstruct ver.*)
(* TODO:implement all method between  abstruct and immediate.*)

(* ::Section:: *)
(* Constructor *)

setMake // Options = {"abstructSet" -> False}; 

Module[{abstructSets = {Primes, Integers, Rationals, Reals, Complexes}},
    setMake[arg_, OptionsPattern[]] /; MemberQ[abstructSets, arg] := 
        set@Association["elems" -> arg, "type" -> "abstruct"] /; OptionValue@"abstructSet" 
]

(*abstruct set:*)
setMake[arg_, OptionsPattern[]] := 
    set@Association["elems" -> arg, "type" -> "abstruct"] /; OptionValue@"abstructSet"

setMake[{}, OptionsPattern[]] := 
    set@Association["elems" -> {}, "type" -> "abstruct"] /; OptionValue@"abstructSet"


setMake[elems_List, OptionsPattern[]]  := 
    set@Association["elems" -> Union@elems, "type" -> "immediate"] /; Not@OptionValue@"abstructSet"

setMake[x_, OptionsPattern[]] /; Not@OptionValue@"abstructSet" := x

(* ::Section:: *)
(* 'set' data type *)

set[ass_Association]~Format~TraditionalForm := 
 BracketingBar @@ (ass@"elems") /; ass@"type" == "immediate"
 
set[ass_Association]@property_String := ass@property

(* ::Section:: *)
(* Methods *)

singletonQ@A_set?immediateSetQ := Length@A == 1

immediateSetQ@A_set /; A@"type" == "immediate" = True;
immediateSetQ@_set  = False;
immediateSetQ@arg_ := (Message[immediateSetQ::notset, arg];False)
immediateSetQ::notset = "'`1`' is not a set.";

abstructSetQ@A_set /; A@"type" == "abstruct" = True;
abstructSetQ@_set  = False;
abstructSetQ@arg_ := (Message[abstructSetQ::notset, arg];False)
abstructSetQ::notset = "'`1`' is not a set.";

(* ::Section:: *)
(* Methods overload *)

omegaChainNumbers = {Primes, "naturals", Integers, Rationals, Reals, Complexes};

cpoNumbers =(*Flatten@*)
 If[Length@# > 1, 
    Append[#0@Rest@#, First@# \[DirectedEdge](*Rest@*)#(*//
     Thread*)], {}] &@omegaChainNumbers;

numberQfunc@Reals = # \[Element] Reals &;
numberQfunc@Integers = Head@SetPrecision[#, \[Infinity]] === Integer &;
numberQfunc@Primes = PrimeQ;
numberQfunc@Complexes = # \[Element] Complexes &;
numberQfunc@Rationals = # \[Element] Rationals &;
numberQfunc@"naturals" = numberQfunc[Integers]@# \[And] # >= 0 &;

set /: A_set?immediateSetQ == B_set?immediateSetQ := A@"elems" == B@"elems"
set /: Equal[A_set?immediateSetQ, B_set?immediateSetQ, Cs__set] := A == B && Equal[B, Cs]

set /: Normal[A_set?immediateSetQ] := A@"elems"

set /: (Alternatives|List)@B__ \[Element] A_set(*?immediateSetQ*) := 
    AllTrue[{B}, # \[Element] A&]
set /: b_ \[Element] A_set?immediateSetQ := MemberQ[A@"elems", b]
set /: b_ \[Element] A_set?abstructSetQ /; MemberQ[omegaChainNumbers, A@"elems"] := 
    numberQfunc[A@"elems"]@b

set /: B_set?immediateSetQ \[SubsetEqual] A_set(*?immediateSetQ*) := B@"elems" \[Element] A 

set /: A_set?immediateSetQ \[Intersection] B_set?immediateSetQ :=
 A@"elems" \[Intersection] B@"elems" // setMake
 
set /: A_set?immediateSetQ \[Union] B_set?immediateSetQ := 
    A@"elems" \[Union] B@"elems" // setMake

set /: A_set?immediateSetQ~Complement~B_set?immediateSetQ := 
    A@"elems"~Complement~B@"elems" // setMake

set /: Length@A_set?immediateSetQ := Length@A@"elems"

set /: Map[f_, A_set?immediateSetQ, levelspec_: {1}] := 
    Map[f, A@"elems", levelspec] // setMake
 
set /: Subsets[A_set?immediateSetQ, args___] := setMake /@ Subsets@A@"elems" // setMake 

Module[{funcToBypass = {Sort, Union, Part, Permutations, MemberQ}},
    set /: func_?(MemberQ[funcToBypass, #]&)[A_set?immediateSetQ, argsForFunc___] := 
        setMake@func[A@"elems", argsForFunc]
]

(*set /: Part[A_set, args__] /; A@"type" == "immediate" := Part[A@"elems", args]*)

(* ::Section:: *)
(* End *)

End[]

EndPackage[]

