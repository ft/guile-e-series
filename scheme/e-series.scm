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
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module ((e-series adjacency) #:prefix $)
  #:use-module ((e-series combine) #:prefix $)
  #:use-module (e-series pretty-print)
  #:use-module (e-series tables)
  #:export (capacitor
            inductor
            resistor
            capacitor*
            inductor*
            resistor*))

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

(define-syntax-rule (define-part-backend name c-tab)
  (define name
    (case-lambda*
     ((r)
      (map (lambda (s)
             (cons s ($adjacency s r)))
           (map car e-tables)))
     ((s r #:key (predicate ($error-predicate 1/100)))
      (map c-tab ($combine s r #:predicate predicate))))))

(define-part-backend capacitor* c-ish-circuit)
(define-part-backend resistor* r-ish-circuit)
(define inductor* resistor*)

(define (simtab-line)
  (format #t " ---------+--------------------------+-------------+--------------------------~%"))

(define (simtab-header)
  (simtab-line)
  (format #t "  Series  |           Below (Error)  |      Exact  |           Above (Error)~%")
  (simtab-line))

(define (simtab-record value record unit)
  (let* ((below (second record))
         (below-e (/ (- below value) value))
         (exact (if (> (length record) 3)
                    (string-append (pp-value (third record)) unit)
                    "         "))
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
  (format #t " ------------+-------------------------+-------------+-------------+-----------~%"))

(define (comb-header)
  (comb-line)
  (format #t "    Desired  |         Actual (Error)  |     Part A  |     Part B  |  Circuit~%")
  (comb-line))

(define (combtab-record value record unit)
  (let* ((parts (assq-ref record 'parts))
         (part-a (pp-value (first parts)))
         (part-b (if (> (length parts) 1)
                     (string-append (pp-value (second parts)) unit)
                     "     -/-"))
         (circuit* (assq-ref record 'circuit))
         (circuit (or circuit* 'single-part)))
    (format #t "  ~a~a  |  ~a~a (~a)  |  ~a~a  |  ~a  |  ~a~%"
            (pp-value value) unit
            (pp-value (assq-ref record 'value)) unit
            (pp-error (assq-ref record 'error))
            part-a unit
            part-b
            circuit)))

(define *c-unit-string* "F")
(define *l-unit-string* "H")
(define *r-unit-string* "Ω")
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
     ((s value #:key (predicate ($error-predicate 1/100)))
      (comb-header)
      (for-each (lambda (result) (combtab-record value result unit))
                (backend s value #:predicate predicate))
      (comb-line)
      (make-undefined)))))

(define-part-frontend capacitor capacitor* *c-unit-string*)
(define-part-frontend inductor inductor* *l-unit-string*)
(define-part-frontend resistor resistor* *r-unit-string*)
