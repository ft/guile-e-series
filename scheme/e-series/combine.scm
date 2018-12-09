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

(define-module (e-series combine)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (e-series adjacency)
  #:use-module (e-series predicates)
  #:use-module (e-series tables)
  #:use-module (e-series utilities)
  #:export (combine))

(define (add combination cmb predicate target lst part adj)
  (let loop ((rest (if (list? adj) adj (list adj))))
    (if (null? rest)
        lst
        (let* ((new (cmb part (car rest)))
               (item `((combination . ,combination)
                       (value . ,new)
                       (parts ,part ,(car rest))
                       (error . ,(candidate-error target new)))))
          (if (predicate item)
              (cons item (loop (cdr rest)))
              (loop (cdr rest)))))))

(define (add-initial predicate target value)
  (let ((item `(((combination . single)
                 (value . ,value)
                 (parts ,value)
                 (error . ,(candidate-error target value))))))
    (if (predicate (first item))
        item
        '())))

(define (error< a b)
  (< (abs (assq-ref a 'error))
     (abs (assq-ref b 'error))))

(define (work tag half start step fill comb+ comb-transform)
  "Return a worker function that produces combinations

Direct and reciprocal combinations require very similar iterations. The
function returned by this function implements this iteration. Its arguments
are parameters that define the results the iteration produces.

- tag: A symbol identifying the type of combination produced
- half: A function of two arguments that produces the iteration limit
- start: Another function of two arguments that produces the starting
  value for the iteration.
- step: Yet another function of two arguments; it needs to produce the
  value used in the next iteration step.
- fill: A function of three arguments, that produces a value, or a list
  of values, that can be used in combination with the current iteration
  value to produce the target value.
- comb+: A function of two values that adds these two values with respect
  to the type of combination that the iteration implements.
- comb-transform: A function of one argument that transforms the iteration
  value to the domain of the target value the combination tries to approximate.

This function is used to implement ‘direct’ and ‘reciprocal’ below."
  (lambda (p s t i)
    (let ((h (half s t)))
      (let loop ((current (start s t)) (candidates i))
        (if (< current h)
            candidates
            (loop (step s current)
                  (add tag comb+ p t candidates (comb-transform current)
                       (fill s t current))))))))

(define direct (work 'direct
                     (lambda (s t) (value-or-first (adjacency* s (/ t 2))))
                     down-e-series
                     down-e-series
                     (lambda (s t c) (adjacency* s (- t c)))
                     + identity))

(define reciprocal (work 'reciprocal
                         (lambda (s t)
                           (rec (value-or-first (adjacency* s (* t 2)))))
                         (lambda (s t) (rec (up-e-series s t)))
                         (lambda (s c) (rec (up-e-series s (rec c))))
                         (lambda (s t c) (adjacency* s (rec (- (rec t) c))))
                         rec+
                         rec))

(define* (combine s value #:key (predicate (max-error 1/100)))
  "Produce combinations of value in an E-series to approximate a target value.

This function requires an integer ‘s’ identifying an E-series and a number
‘value’ that should be approximated by two values from the requested E-series.
The function takes an optional predicate. The default predicate is to filter
combinations that approximate the target value with an error of at most 1%.

Example:

    (combine 6 #e50)  ;; Approximate a value of 50 in E6.
    => (((combination . reciprocal)
         (value . 50)
         (parts 100 100)
         (error . 0))
        ((combination . direct)
         (value . 503/10)
         (parts 47 33/10)
         (error . 3/500)))

If we were looking for a resistor of 50 Ohms in E6, that could be produced with
1% error using a parallel circuit of two 100 Ohm resistors or a series circuit
that is made up of 47 and 3.3 Ohms."
  (let* ((nearest (adjacency* s value)))
    (if (number? nearest)
        (add-initial predicate value nearest)
        (sort ((compose (lambda (lst) (reciprocal predicate s value lst))
                        (lambda (lst) (direct predicate s value lst)))
               (add-initial predicate value (second nearest)))
              error<))))
