;; Copyright © 2018 Frank Terbeck <ft@bewatermyfriend.org>
;;
;; This library  is free  software; you  can redistribute  it and/or  modify it
;; under the terms of the GNU Lesser General Public License as published by the
;; Free  Software Foundation;  either version  3 of  the License,  or (at  your
;; option) any later version.
;;
;; This library is distributed in the hope  that it will be useful, but WITHOUT
;; ANY  WARRANTY;  without even  the  implied  warranty of  MERCHANTABILITY  or
;; FITNESS FOR A PARTICULAR PURPOSE. See  the GNU Lesser General Public License
;; for more details.
;;
;; You should  have received a  copy of the  GNU Lesser General  Public License
;; along with  this library;  if not,  write to  the Free  Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (e-series)
  #:use-module (srfi srfi-1)
  #:use-module (e-series adjacency)
  #:use-module (e-series combine)
  #:use-module (e-series tables)
  #:export (resistor inductor))

(define (add-circuit table entry)
  (let ((c (assq-ref entry 'combination)))
    (cons (cons 'circuit (assq-ref table c))
          entry)))

(define (r-ish-circuit e)
  (add-circuit `((direct . series)
                 (reciprocal . parallel))
               e))

(define (c-ish-circuit e)
  (add-circuit `((direct . parallel)
                 (reciprocal . series))
               e))

(define resistor*
  (case-lambda*
   ((r)
    (map (lambda (s)
           (cons s (adjacency s r)))
         (map car e-tables)))
   ((s r #:key (predicate (error-predicate 1/100)))
    (map r-ish-circuit (combine s r #:predicate predicate)))))

(define inductor* resistor*)

(define si-prefixes `(( 24  yotta Y)
                      ( 21  zetta Z)
                      ( 18    exa E)
                      ( 15   peta P)
                      ( 12   tera T)
                      (  9   giga G)
                      (  6   mega M)
                      (  3   kilo k)
                      (  2  hecto h)
                      (  1  deca da)
                      (  0  #f #f)
                      ( -1   deci d)
                      ( -2  centi c)
                      ( -3  milli m)
                      ( -6  micro µ)
                      ( -9   nano n)
                      (-12   pico p)
                      (-15  femto f)
                      (-18   atto a)
                      (-21  zepto z)
                      (-24  yocto y)))

(define (exp->prefix e)
  (let ((prefix (assq-ref si-prefixes e)))
    (if prefix
        (second prefix)
        (symbol-append 'e (string->symbol (number->string e))))))

(define (value->engexp v)
  (let* ((m&e (value->pair v))
         (exponent (inexact->exact (log10 (inexact->exact (cdr m&e)))))
         (em3 (modulo exponent 3))
         (si (- exponent em3)))
    (cons (* (car m&e) (expt 10 em3))
          si)))

(define (value->engpair v)
  (let ((p (value->engexp v)))
    (cons (car p)
          (exp->prefix (cdr p)))))

(define (pp-value v)
  (let ((p (value->engpair v)))
    (format #f "~a~6,2f~a"
            (if (cdr p) "" " ")
            (exact->inexact (car p))
            (if (cdr p) (cdr p) ""))))

(define (pp-error v)
  (if (zero? v)
      "  exact  "
      (format #f "~,3@e" v)))

(define (simtab-line)
  (format #t " ---------+-------------------------+------------+------------------------~%"))

(define (simtab-header)
  (simtab-line)
  (format #t "  Series  |          Below (Error)  |     Exact  |         Above (Error)~%")
  (simtab-line))

(define (simtab-record value record unit)
  (let* ((below (second record))
         (below-e (/ (- below value) value))
         (exact (if (> (length record) 3)
                    (string-append (pp-value (third record)) unit)
                    "        "))
         (above (last record))
         (above-e (/ (- above value) value)))
    (format #t "    E~a~a  |  ~a~a  (~,3@e)  |  ~a  |  ~a~a  (~,3@e)~%"
            (first record)
            (case (first record)
              ((3 6) "  ")
              ((12 24 48 96) " ")
              (else ""))
            (pp-value below) unit
            below-e
            exact
            (pp-value above) unit
            above-e)))

(define (comb-line)
  (format #t " ----------+------------------------+------------+------------+----------------~%"))

(define (comb-header)
  (comb-line)
  (format #t "  Desired  |        Actual (Error)  |    Part A  |    Part B  |  Circuit Type  ~%")
  (comb-line))

(define (combtab-record value record unit)
  (let* ((parts (assq-ref record 'parts))
         (part-a (pp-value (first parts)))
         (part-b (if (> (length parts) 1)
                     (string-append (pp-value (second parts)) unit)
                     "     -/-"))
         (circuit* (assq-ref record 'circuit))
         (circuit (or circuit* 'single-part)))
    (format #t " ~a~a  |  ~a~a (~a)  |  ~a~a  |  ~a  |  ~a~%"
            (pp-value value) unit
            (pp-value (assq-ref record 'value)) unit
            (pp-error (assq-ref record 'error)) unit
            part-a
            part-b
            circuit)))

(define *r-unit-string* "Ω")
(define *l-unit-string* "H")
(define (make-undefined) (if #f #f))

(define-syntax-rule (define-part-frontend type backend unit)
  (define type
    (case-lambda*
     ((value)
      (simtab-header)
      (for-each (lambda (result) (simtab-record value result unit))
                (backend value))
      (simtab-line)
      (make-undefined))
     ((s value #:key (predicate (error-predicate 1/100)))
      (comb-header)
      (for-each (lambda (result) (combtab-record value result unit))
                (backend s value #:predicate predicate))
      (comb-line)
      (make-undefined)))))

(define-part-frontend resistor resistor* *r-unit-string*)
(define-part-frontend inductor inductor* *l-unit-string*)
