(load "test-harness.shen")

(describe "Foldr Tests"
  (it "should fold a list of numbers, +, and 0, and be equivalent to summing the numbers"
    (should-equal
      (foldr (function +) 0 [1 2 3 4])
      10
    )
  )
  
  (it "should return the unit when the list is empty"
    (should-equal
      (foldr (function +) 0 [])
      0
    )
  )
  
  (it "should display a failed test in a nice way"
    (should-equal 1 2)
  )
  
  (it "should pend this spec because I didn't put any body inside")
  
)

(describe "An Empty Set of Specs")
