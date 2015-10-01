(* Mathematica Test File *)

Test[
    setMake@{3}
    ,
    SimpleSet`Private`set@Association["elems" -> {3}, "type" -> "immediate"]
    ,
    TestID->"Test-20150930-S8X3R7"
]

Test[
    setMake@{}
    ,
    SimpleSet`Private`set@Association["elems" -> {}, "type" -> "immediate"]
    ,
    TestID->"a"
]

Test[
    setMake[]
    ,
    SimpleSet`Private`set@Association["elems" -> {}, "type" -> "immediate"]
    ,
    TestID->"no_arg"
]

Test[
	setMake@{1,3,1}
	,
	setMake@{1,3}
	,
	TestID->"Test-20150930-T7X7Q4"
]
