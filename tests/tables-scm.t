;; -*- scheme -*-

;; Copyright (c) 2018 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

(use-modules (srfi srfi-1)
             (test tap)
             (test setup)
             (e-series exact)
             (e-series tables))

(define (test-spec2table-length tag tab)
  (define-test (format #f "Specification length matches ~a length" tag)
    (pass-if-= (length e-tables)
               (length tab))))

(define (test-length tag tab)
  (for-each
   (lambda (e)
     (let ((s (car e)) (t (cdr e)))
       (define-test (format #f "(~a) E~a's table has ~a entries" tag s s)
         (pass-if-= s (length t)))))
   tab))

(define (diff-pred eps)
  (lambda (x)
    (let ((result (< x eps)))
      (unless result
        (format #t "# (< ~a ~a) => #f~%" x eps))
      result)))

(define (spec-to-exact-diff s)
  (case s
    ((3) (diff-pred 25))
    ((6 12 24) (diff-pred 45))
    (else (diff-pred 8))))

(with-test-bundle (guile e-series tables&exact)
  (plan (+ 1 1
           (length e-tables)
           (length exact-e-series)
           (length rounded-e-series)
           (* 2 (length e-tables))))

  (test-spec2table-length 'exact exact-e-series)
  (test-spec2table-length 'rounded rounded-e-series)

  (test-length 'spec e-tables)
  (test-length 'exact exact-e-series)
  (test-length 'rounded rounded-e-series)

  (for-each
   (lambda (exact actual)
     (let ((ex-s (car exact)) (ex-t (cdr exact))
           (ac-s (car actual)) (ac-t (cdr actual)))
       (define-test (format #f "E~a corresponds in exact and actual" ex-s)
         (pass-if-= ex-s ac-s))
       (define-test
           (format #f
                   "Differences between spec (~a) and exact in expected limits"
                   ex-s)
         (pass-if-true (every (spec-to-exact-diff ex-s)
                              (map (lambda (ex ac)
                                     (floor (* 1e3 (/ (abs (- ex ac))
                                                      (max ex ac)))))
                                   ex-t ac-t))))))
   e-tables
   exact-e-series))
