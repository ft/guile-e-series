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
;; The E series values follow along the following expression:
;;
;;                   ,-----------.
;;              n    |      (m)  |
;;         k = ,--.  |   10^
;;                 \ |
;;                  \.
;;
;; They are  however rounded to nearby  values. It's not the  natural rounding,
;; though.  This module  implements tables  that show  the exact  and naturally
;; rounded variants  of the e-series.  This is only to  show that there  are in
;; fact differences between  the mathematical definition and  the specified ta-
;; bles. The actual specification's values can be found in (e-series tables).

(define-module (e-series exact)
  #:use-module (srfi srfi-42)
  #:use-module (e-series tables)
  #:export (exact-e-series
            exact-series
            rounded-e-series
            rounded-series))

(define (by-100 v)
  (map (lambda (x) (/ x 100)) v))

(define (series-value** s n)
  (inexact->exact (round (* 100 (series-value s n)))))

(define (series-value* s n)
  (inexact->exact (* 10 (round (* 10 (series-value s n))))))

(define (series-value s n)
  (expt 10. (/ n s)))

(define (make-series f s)
  (list-ec (: n s) (f s n)))

(define (exact-series s)
  (make-series series-value s))

(define (rounded-series s)
  (make-series (if (< s 48)
                   series-value*
                   series-value**)
               s))

(define (make-table f t)
  (map (lambda (e)
         (let ((s (car e)))
           (cons s (f s))))
       t))

(define exact-e-series (make-table exact-series e-tables))
(define rounded-e-series (make-table (lambda (x) (by-100 (rounded-series x)))
                                     e-tables))
