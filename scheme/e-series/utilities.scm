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

(define-module (e-series utilities)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (by-100
            candidate-error
            rec
            rec+
            take-at-most
            value->pair
            value->engtriple
            value-or-first))

(define (by-100 v)
  (map (lambda (x) (/ x 100)) v))

(define (take-at-most lst n)
  (take lst (min (length lst) n)))

(define (rec x)
  (if (list? x)
      (map rec x)
      (/ 1 x)))

(define (rec+ a b)
  (rec (+ (rec a) (rec b))))

(define (candidate-error t v)
  (/ (- v t) t))

(define (value-or-first v)
  (if (list? v) (first v) v))

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

(define (base10->exponent n)
  (inexact->exact (round (log10 (inexact->exact n)))))

(define (value->pair v)
  (let* ((e (floor (log10 v)))
         (e* (inexact->exact (expt 10 (abs e))))
         (e** (if (positive? e) e* (/ 1 e*))))
    (cons (inexact->exact (/ v e**)) e**)))

(define (value->engexp v)
  (let* ((m&e (value->pair v))
         (exponent (base10->exponent (cdr m&e)))
         (em3 (modulo exponent 3))
         (si (- exponent em3)))
    (list (* (car m&e) (expt 10 em3))
          si
          em3)))

(define (value->engtriple v)
  (match (value->engexp v)
    ((n si em3) (list n (exp->prefix si) em3))))
