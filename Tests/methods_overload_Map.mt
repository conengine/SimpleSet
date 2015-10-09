Test[
    Map["f", setMake@{1,2,3}]
    ,
    setMake@{"f"@1, "f"@2, "f"@3}
    ,
    TestID->"Map_simple_use"
]

Test[
    Map["f"]@setMake@{1,2,3}
    ,
    setMake@{"f"@1, "f"@2, "f"@3}
    ,
    TestID->"Map_partial_application"
]

Test[
    Map["f", setMake@{"1"@1,"2"@2,"3"@3}, {2}]
    ,
    setMake@{"1"@"f"@1, "2"@"f"@2, "3"@"f"@3}
    ,
    TestID->"Map_level_spec_2"
]
