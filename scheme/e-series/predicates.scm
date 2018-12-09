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

(define-module (e-series predicates)
  #:export (match-all
            bigger
            smaller
            exact
            circuit
            combination
            max-error))

(define (make-item-predicate pred key transform . argument)
  (lambda (item)
    (apply pred (cons (transform (assq-ref item key))
                      argument))))

(define (match-all)
  (lambda (_) #t))

(define (bigger)
  (make-item-predicate positive? 'error identity))

(define (smaller)
  (make-item-predicate negative? 'error identity))

(define (exact)
  (make-item-predicate zero? 'error identity))

(define (circuit type)
  (make-item-predicate eq? 'circuit identity type))

(define (combination type)
  (make-item-predicate eq? 'combination identity type))

(define (max-error limit)
  (make-item-predicate <= 'error abs limit))
