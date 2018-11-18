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
;;
;; Commentary:
;;
;; This module exports a function, that returns a list of values from an E-Se-
;; ries, that are in the adjacency of a given arbitrary value.

(define-module (e-series adjacency)
  #:use-module (srfi srfi-1)
  #:use-module (e-series tables)
  #:export (adjacency
            adjacency*
            pick-from-e-series
            pick-from-e-series*))

(define (value->pair v)
  (let* ((e (floor (log10 v)))
         (e* (inexact->exact (expt 10 (abs e))))
         (e** (if (positive? e) e* (/ 1 e*))))
    (cons (inexact->exact (/ v e**)) e**)))

(define (take-at-most lst n)
  (take lst (min (length lst) n)))

(define (exact-adjacency lst value)
  (let ((rest (member value lst)))
    (if rest
        (let ((above (cdr (take-at-most rest 2)))
              (below (cdr (take-at-most (member value (reverse lst)) 2))))
          (list (if (null? below) (/ (last lst) 10) (car below))
                value
                (if (null? above) (* (first lst) 10) (car above))))
        #f)))

(define (find-by-pred pred default lst value)
  (let loop ((rest lst))
    (cond ((null? rest) default)
          ((pred (car rest) value) (car rest))
          (else (loop (cdr rest))))))

(define (find-bigger lst value)
  (find-by-pred > (* (first lst) 10) lst value))

(define (find-smaller lst value)
  (find-by-pred < (/ (last lst) 10) (reverse lst) value))

(define (approximate-adjacency lst value)
  (list (find-smaller lst value)
        (find-bigger lst value)))

(define (get-adjacency lst value)
  (or (exact-adjacency lst value)
      (approximate-adjacency lst value)))

(define (adjacency s value)
  "This function returns a list of values in the vicinity of an arbitrary
‘value’, taken from an E-Series identified by the integer ‘s’.

If there is an exact match of ‘value’ in the corresponding E-series, the
function returns a list of three entries, one below the exact value, the exact
value and one above it. In case there is no exact match, the result will have
two entries: One below and one above.

Examples:

    (adjacency 12 470) => (390 470 560)  ; exact match
    (adjacency 12 430) => (390 470)      ; inexact match

The function throws an ‘invalid-e-series’ exception in case ‘s’ does not name a
valid E-series.

Note that if you're using \"123e12\" notation, consider to use the exact
variant of it (\"#e123e12\"):

    (adjacency 12 #e270e6) => (220000000 270000000 330000000)
    (adjacency 12   270e6) => (270000000 330000000)

The reason for this difference is that equality is not a valid predicate with
floating point numbers in the general case. The exact notation expands into an
arbitrarily large rational number, thus staying exact."
  (unless (is-e-series? s)
    (throw 'invalid-e-series s))
  (let* ((m&e (value->pair value))
         (m (car m&e))
         (e (cdr m&e)))
    (map (lambda (x) (* x e))
         (get-adjacency (e-series s) m))))

(define (adjacency* s value)
  "Like ‘adjacency’, but in case there is an exact match, only return that
exact value.

Examples:

    (adjacency* 12 470) => 470        ; exact match
    (adjacency* 12 430) => (390 470)  ; inexact match"
  (let ((lst (adjacency s value)))
    (if (= (length lst) 3)
        (second lst)
        lst)))

(define (pick-w/-picker picker value)
  (map (lambda (e)
         (let ((s (car e)))
           (cons s (picker s value))))
       e-tables))

(define (pick-from-e-series value)
  "Picks close values to ‘value’ from all E-series.

Example:

    (pick-from-e-series 270)
    => ((3 220 470)
        (6 220 330)
        (12 220 270 330)
        (24 240 270 300)
        (48 261 274)
        (96 267 274)
        (192 267 271))

The result is an alist, where the key is the E-series identifier and the value
is the result of ‘adjacency’."
  (pick-w/-picker adjacency value))

(define (pick-from-e-series* value)
  "Like ‘pick-from-e-series’, but uses ‘adjacency*’ instead of ‘adjacency’

Example:

    (pick-from-e-series* 270)
    => ((3 220 470)
        (6 220 330)
        (12 . 270)
        (24 . 270)
        (48 261 274)
        (96 267 274)
        (192 267 271))

The result is an alist, where the key is the E-series identifier and the value
is the result of ‘adjacency*’."
  (pick-w/-picker adjacency* value))
