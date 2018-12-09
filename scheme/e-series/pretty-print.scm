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

(define-module (e-series pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module ((e-series adjacency) #:prefix $)
  #:export (pp-error
            pp-value))

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

(define (value->engexp v)
  (let* ((m&e ($value->pair v))
         (exponent (base10->exponent (cdr m&e)))
         (em3 (modulo exponent 3))
         (si (- exponent em3)))
    (list (* (car m&e) (expt 10 em3))
          si
          em3)))

(define (value->engtriple v)
  (match (value->engexp v)
    ((n si em3) (list n (exp->prefix si) em3))))

(define (pp-without-prefix n em3)
  (format #f " ~v,vf" em3 (- 5 em3) (exact->inexact n)))

(define (pp-with-prefix n em3 prefix)
  (format #f "~v,vf~a" em3 (- 5 em3) (exact->inexact n) prefix))

(define (pp-value v)
  (match (value->engtriple v)
    ((n #f em3) (pp-without-prefix n em3))
    ((n prefix em3) (pp-with-prefix n em3 prefix))))

(define (pp-error v)
  (if (zero? v)
      "  exact  "
      (format #f "~,3@e" v)))
