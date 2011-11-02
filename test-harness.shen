(load "helper.shen")

(defmacro should-raise-error-macro
  [should-raise-error Expression] -> [trap-error [do Expression false] 
                                                 [/. E true]]
)



(define should-equal
  X Y -> (if (= X Y) true 
             (simple-error (make-string "Expected ~A to equal~%           ~A" X Y)))
)

(define should-equal-g*
  String X Y -> (let Var (intern String)
                     Z (replace (/. X (= (head (explode X)) String)) Var X)
                     (if (= Y
                            Z)
                         true 
                         (simple-error (make-string "Expected ~A to equal~%           ~A" Y Z))))
)

(define listify
  [] -> []
  [ Head | Tail] -> [cons Head (listify Tail)]
)

(defmacro it-macro
  [it String] -> [freeze pending]
  [it String Expression] -> [freeze [do [output "it ~A: " String]
                                [let Result [trap-error Expression [/. E (listify [String [error-to-string E]])]]
                                     [do [if [= Result true] [output ".~%"] [output "F~%"]]
                                         Result]]]]
)

(defmacro describe-macro 
  [describe String] -> [do [output "********************~%Nothing in ~A to Run.~%" String] ********************] 
  [describe String | Tests] -> [let ExpandedTests (listify Tests)
                                     TestsToRun (gensymbol "tests-to-run")
                                     [do 
                                      [output "~%********************~%Starting ~A: ~%" String]
                                      [time [set TestsToRun [map [function thaw] ExpandedTests]]]
                                      [let Result [filter [/. X [and [not [= X true]]
                                                                     [not [= X pending]]]] 
                                                          [value TestsToRun]]
                                           Pending [filter [/. X [= X pending]] [value TestsToRun]] 
                                           PendingCount [length Pending]
                                           [do
                                             [output "~%Test Suite Finished (~A Tests, ~A Pending, ~A Failed). Result: "
                                                     [length [value TestsToRun]]
                                                     PendingCount
                                                     [length Result]]
                                             [if [= Result []] 
                                                 [do [print Success] [output "~%"]]
                                                 [do [output "Failure~%~%Failed Tests~%----------------~%"]
                                                     [map [/. X [output "~A~%- ~A" [head X] [head [tail X]]]] 
                                                          Result]]]
                                             [output "~%"]
                                             ********************]]]]
)
