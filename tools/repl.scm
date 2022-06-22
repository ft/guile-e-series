(use-modules (e-series))

(define (e-series-repl-help)
  (display "GNU Guile E-Series REPL

Examples:

  (resistor   36)         ;; Show resistor values around 36Ω in
                          ;; various E series.
  (resistor   36e3)       ;; Same but for 36kΩ.
  (resistor #e36e3)       ;; Same but with exact number notation.

  (capacitor 12 #e32e-9)  ;; Find combinations of two components
                          ;; from E12 to form a 32nF capacitor as
                          ;; closely as possible.

  ;; Find combinations of parallel circuits only in E24, that form
  ;; a 1.25μF inductor, with a maximum error of 1%.
  (inductor 24 #e1.25e-6
            #:predicate (all-of (max-error 1/100)
                                (circuit 'parallel)))

  Note that (max-error 1/100) is actually the default predicate.
  So if you change the predicate so something that does not in-
  volve max-error, you get all combinations that satisfy the new
  predicate, no matter how much the value is off.

  You can redisplay this text by typing '(e-series-repl-help)'.")
  (newline)
  (newline))

(e-series-repl-help)
