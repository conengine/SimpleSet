(* Mathematica Test File *)
Test[
	setMake["hoge", "abstructSet" -> True]
	,
	set@Association["elems"->"hoge", "type" -> "abstruct",
        "elemQFunc"->None,
        "subsetEqualQFunc"->None]
	,
	TestID->"User-defined abstruct set"
]

Test[
	setMake["strings", "abstructSet"->True, 
		 "elemQFunc"->(StringQ@# && StringLength@# == 1 &),
		 "subsetEqualQFunc"->StringQ]
	,
	set@Association["elems"->"hoge", "type" -> "abstruct",
		"elemQFunc"->(StringQ@# && StringLength@# == 1 &),
         "subsetEqualQFunc"->StringQ]
	,
	TestID->"User-defined abstruct set 2"
]

Test[
	setMake@Reals
	,
	Reals
	,
	TestID->"setMake_wrongUsage_Reals"
]

Test[
    setMake[Reals, "abstructSet"->True]
    ,
    set@Association["elems"->Reals, "type" -> "abstruct"]
    ,
    TestID->"setMake_abstruct_Reals"
]

Test[
    setMake["naturals", "abstructSet"->True]
    ,
    set@Association["elems"->"naturals", "type" -> "abstruct"]
    ,
    TestID->"setMake_abstruct_naturals"
]

Test[
    setMake["twinPrimes", "abstructSet"->True]
    ,
    "error"
    ,
    TestID->"setMake_abstruct_unknown_set"
]

