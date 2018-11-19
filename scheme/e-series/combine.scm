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
  #:use-module (e-series tables)
  #:use-module (e-series adjacency)
  #:export (combine
            error-predicate))

(define (candidate-error t v)
  (/ (- v t) t))

(define (value-or-first v)
  (if (list? v) (first v) v))

(define (add combination predicate target lst part adj)
  (let loop ((rest (if (list? adj) adj (list adj))))
    (if (null? rest)
        lst
        (let* ((new (+ part (car rest)))
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

(define (error-predicate limit)
  (lambda (item)
    (<= (abs (assq-ref item 'error)) limit)))

(define (direct predicate s target init)
  (let ((half (value-or-first (adjacency* s (/ target 2)))))
    (let loop ((current (down-e-series s target)) (candidates* init))
      (if (< current half)
          candidates*
          (loop (down-e-series s current)
                (add 'direct predicate target candidates* current
                     (adjacency* s (- target current))))))))

(define (reciprocal predicate s target init)
  init)

(define* (combine s value #:key (predicate (error-predicate 1/100)))
  (let* ((nearest (adjacency* s value)))
    (if (number? nearest)
        (add-initial predicate value nearest)
        (sort (reciprocal predicate s value
                          (direct predicate s value
                                  (add-initial predicate value
                                               (second nearest))))
              error<))))
