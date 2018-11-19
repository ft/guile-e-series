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
  #:use-module (srfi srfi-1)
  #:use-module (e-series tables)
  #:use-module (e-series adjacency)
  #:export (combine))

(define (candidate-error t v)
  (/ (- v t) t))

(define (value-or-first v)
  (if (list? v) (first v) v))

(define (add+ target lst part adj)
  (let loop ((rest (if (list? adj) adj (list adj))))
    (if (null? rest)
        lst
        (let ((new (+ part (car rest))))
          (cons `((combination . direct)
                  (value . ,new)
                  (parts ,part ,(car rest))
                  (error . ,(candidate-error target new)))
                (loop (cdr rest)))))))

(define (add-initial target value)
  `(((combination . single)
     (value . ,value)
     (parts ,value)
     (error . ,(candidate-error target value)))))

(define (error< a b)
  (< (abs (assq-ref a 'error))
     (abs (assq-ref b 'error))))

(define (combine s value)
  (let* ((nearest (adjacency* s value)))
    (if (number? nearest)
        (add-initial value nearest)
        (let ((half (value-or-first (adjacency* s (/ value 2)))))
          (let loop ((current (first nearest))
                     (candidates (add-initial value (second nearest))))
            (if (< current half)
                (sort candidates error<)
                (loop (down-e-series s current)
                      (add+ value candidates current
                            (adjacency* s (- value current))))))))))
