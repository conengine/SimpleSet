(* Mathematica Test File *)
Test[
	setMake["hoge", "abstructSet" -> True]
	,
	set@Association["elems"->"hoge", "type" -> "abstruct"]
	,
	TestID->"User-defined abstruct set"
]

