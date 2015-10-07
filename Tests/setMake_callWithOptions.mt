(* Mathematica Test File *)
Test[
	setMake["hoge", "abstructSet"->True]
	,
	set@Association["elems"->"hoge", "type"->"abstruct"]
	,
	TestID->"setMake_callWithOptions-20151007-K4E0C2"
]

Test[
	setMake[{}, "abstructSet"->True]
	,
	set@Association["elems"->{}, "type"->"abstruct"]
	,
	TestID->"abstruct empty set"
]

Test[
    setMake@{}
    ,
    setMake[{}, "type"->"immediate"]
    ,
    TestID->"immediate empty set"
]
