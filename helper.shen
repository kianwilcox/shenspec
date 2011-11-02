\* symbol helpers *\

(set gensymbol-current-number 0)


(define gensymbol
  String -> (intern (make-string "~A~A" String (set gensymbol-current-number (+ 1 (value gensymbol-current-number)))))
)

\* list helpers *\
(define foldr
  _ U [] -> U
  F U List -> (F (head List) (foldr F U (tail List)))
)

(define traverse
  Merge U F [] -> U
  Merge U F [Branch | Branches] -> (Merge (traverse Merge U F Branch) (traverse Merge U F Branches))
  Merge U F Tip -> (F Tip)
)

(define walk
  F List -> (traverse (/. X Y [X | Y])
                      [] F
                      List)
)

(define replace
  Function Replacement List -> (walk (/. X (if (Function X)
                                               Replacement
                                               X))
                                     List)
)

(define repeat
  N _ -> [] where (<= N 0)
  N X -> [X | (repeat (- N 1) X)]
)

(define replace-val
  Val Replacement List -> (replace (/. X (= X Val)) Replacement List)
)

(define symbol->string
  Symbol -> (concat-strings (explode Symbol)) where (symbol? Symbol)
  _ -> (simple-error "Not a symbol")
)

(define all?
  [] -> true
  [Head | Tail] -> (and Head (all? Tail))
)

(define any?
  List -> (foldr (function or) false List)
)

(define replace-with
  _ _ [] -> []
  X Y [X | Tail] -> [Y | Tail]
  X Y [Z | Tail] -> [Z | (replace X Y Tail)]
)

(define filter
  F [] -> []
  F [El | Rest] -> [El | (filter F Rest)] where (F El)
  F [_ | Rest] -> (filter F Rest)
)

(define member?
  X [] -> false
  X [ X | _ ] -> true
  X [ _ | Y ] -> (member? X Y)
)

(define concat-strings
  [] -> ""
  [S | Rest] -> (cn S (concat-strings Rest)))
  
(define join
  [] Joiner -> (concat-strings [])
  [X] Joiner -> (concat-strings [X])
  [X | Y] Joiner -> (concat-strings [X Joiner | (join Y Joiner)])
  
)

\* string helpers *\
