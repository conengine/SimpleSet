(* Mathematica Test File *)

Test[
    setMake@{3}
    ,
    set@Association["elems" -> {3}, "type" -> "immediate"]
    ,
    TestID->"Test-20150930-S8X3R7"
]

Test[
    setMake@{}
    ,
    set@Association["elems" -> {}, "type" -> "immediate"]
    ,
    TestID->"a"
]

Test[
	setMake@{1,3,1}
	,
	setMake@{1,3}
	,
	TestID->"Test-20150930-T7X7Q4"
]
