(* Mathematica Test File *)

A = setMake@Range@10;
Test[
    SelectFirst[A, OddQ]
    ,
    1
    ,
    TestID->"SelectFirst-simple"
]

Test[
    SelectFirst[A, 7==#&]
    ,
    7
    ,
    TestID->"SelectFirst-simple2"
]

Test[
    SelectFirst[A, 777==#&]
    ,
    Missing["NotFound"]
    ,
    TestID->"SelectFirst-doesnt-exist"
]

Test[
    SelectFirst[A, 777==#&, 666]
    ,
    666
    ,
    TestID->"SelectFirst-doesnt-exist-but-default-specified"
]
\
Test[
    SelectFirst[OddQ]@A
    ,
    1
    ,
    TestID->"SelectFirst-partial-implemention"
]
