Test[
    mapAtElems["f", setMake@{1,2,3}]
    ,
    setMake@{"f"@1, "f"@2, "f"@3}
    ,
    TestID->"simple_use"
]

Test[
    mapAtElems["f"]@setMake@{1,2,3}
    ,
    setMake@{"f"@1, "f"@2, "f"@3}
    ,
    TestID->"partial_application"
]

Test[
    mapAtElems["f", setMake@{"1"@1,"2"@2,"3"@3}, {2}]
    ,
    setMake@{"1"@"f"@1, "2"@"f"@2, "3"@"f"@3}
    ,
    TestID->"level_spec_2"
]
