(* Mathematica Test File *)

set3 = setMake@{3}
Test[
    Element[3,set3]
    ,
    True
    ,
    TestID->"Test_Union-20150930-A0R4D6"
]

setEmpty = setMake@{}
Test[
    Element[3,setEmpty]
    ,
    False
    ,
    TestID->"Test_Union-20150930-T2D9K5"
]