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

(define-module (e-series tables)
  #:use-module (srfi srfi-42)
  #:use-module (e-series utilities)
  #:export (e3 e6 e12 e24 e48 e192 e-tables e-series is-e-series?))

(define e3   (by-100 (list 100 220 470)))
(define e6   (by-100 (list 100 150 220 330 470 680)))
(define e12  (by-100 (list 100 120 150 180 220 270 330 390 470 560 680 820)))
(define e24  (by-100 (list 100 110 120 130 150 160 180 200 220 240 270 300
                           330 360 390 430 470 510 560 620 680 750 820 910)))
(define e48  (by-100 (list 100 105 110 115 121 127 133 140 147 154 162 169
                           178 187 196 205 215 226 237 249 261 274 287 301
                           316 332 348 365 383 402 422 442 464 487 511 536
                           562 590 619 649 681 715 750 787 825 866 909 953)))
(define e96  (by-100 (list 100 102 105 107 110 113 115 118 121 124 127 130
                           133 137 140 143 147 150 154 158 162 165 169 174
                           178 182 187 191 196 200 205 210 215 221 226 232
                           237 243 249 255 261 267 274 280 287 294 301 309
                           316 324 332 340 348 357 365 374 383 392 402 412
                           422 432 442 453 464 475 487 499 511 523 536 549
                           562 576 590 604 619 634 649 665 681 698 715 732
                           750 768 787 806 825 845 866 887 909 931 953 976)))
(define e192 (by-100 (list 100 101 102 104 105 106 107 109 110 111 113 114
                           115 117 118 120 121 123 124 126 127 129 130 132
                           133 135 137 138 140 142 143 145 147 149 150 152
                           154 156 158 160 162 164 165 167 169 172 174 176
                           178 180 182 184 187 189 191 193 196 198 200 203
                           205 208 210 213 215 218 221 223 226 229 232 234
                           237 240 243 246 249 252 255 258 261 264 267 271
                           274 277 280 284 287 291 294 298 301 305 309 312
                           316 320 324 328 332 336 340 344 348 352 357 361
                           365 370 374 379 383 388 392 397 402 407 412 417
                           422 427 432 437 442 448 453 459 464 470 475 481
                           487 493 499 505 511 517 523 530 536 542 549 556
                           562 569 576 583 590 597 604 612 619 626 634 642
                           649 657 665 673 681 690 698 706 715 723 732 741
                           750 759 768 777 787 796 806 816 825 835 845 856
                           866 876 887 898 909 920 931 942 953 965 976 988)))

(define e-tables
  `((  3 . ,e3)
    (  6 . ,e6)
    ( 12 . ,e12)
    ( 24 . ,e24)
    ( 48 . ,e48)
    ( 96 . ,e96)
    (192 . ,e192)))

(define (is-e-series? s)
  (not (not (member s (map car e-tables)))))

(define (e-series s)
  (assq-ref e-tables s))
