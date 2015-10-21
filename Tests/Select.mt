(* Mathematica Test File *)

A = setMake@Range@10;
Test[
	Select[A, OddQ]
	,
	setMake@{1,3,5,7,9}
	,
	TestID->"Select-simple"
]

Test[
    Select[A, 7==#&]
    ,
    setMake@{7}
    ,
    TestID->"Select-simple2"
]

Test[
    Select[A, 777==#&]
    ,
    setMake@{}
    ,
    TestID->"Select-doesnt-exist"
]


Test[
    Select[A, OddQ, 1]
    ,
    setMake@{1}
    ,
    TestID->"Select-num1"
]

Test[
    Select[A, OddQ, 2]
    ,
    setMake@{1,3}
    ,
    TestID->"Select-num2"
]

Test[
	Select[OddQ]@A
	,
	setMake@Range[1,9,2]
	,
	TestID->"Select-partial-implemention"
]
