;; -*- scheme -*-

;; Copyright (c) 2018 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

(use-modules (test tap)
             (test setup)
             (e-series adjacency))

(with-test-bundle (guile e-series adjacency)
  (plan 12)

  (define-test "adjacency: Invalid series throws"
    (pass-if-exception 'invalid-e-series
                       (adjacency 5 270)))
  (define-test "adjacency: Exact match"
    (pass-if-equal? (adjacency 12 270) '(220 270 330)))
  (define-test "adjacency: Exact match, lower bound"
    (pass-if-equal? (adjacency 12 100) '(82 100 120)))
  (define-test "adjacency: Exact match, upper bound"
    (pass-if-equal? (adjacency 12 820) '(680 820 1000)))
  (define-test "adjacency: Approximate match"
    (pass-if-equal? (adjacency 12 300) '(270 330)))
  (define-test "adjacency: Approximate match, boundary"
    (pass-if-equal? (adjacency 12 900) '(820 1000)))
  (define-test "adjacency*: Approximate match"
    (pass-if-equal? (adjacency* 12 300) '(270 330)))
  (define-test "adjacency*: Exact match"
    (pass-if-equal? (adjacency* 12 270) 270))
  (define-test "pick-from-e-series: Exact match in some"
    (pass-if-equal? (pick-from-e-series 270)
                    '((  3  220     470)
                      (  6  220     330)
                      ( 12  220 270 330)
                      ( 24  240 270 300)
                      ( 48  261     274)
                      ( 96  267     274)
                      (192  267     271))))
  (define-test "pick-from-e-series: Approximate match in all"
    (pass-if-equal? (pick-from-e-series 371)
                    '((  3  220     470)
                      (  6  330     470)
                      ( 12  330     390)
                      ( 24  360     390)
                      ( 48  365     383)
                      ( 96  365     374)
                      (192  370     374))))
  (define-test "pick-from-e-series*: Exact match in some"
    (pass-if-equal? (pick-from-e-series* 270)
                    '((  3  220     470)
                      (  6  220     330)
                      ( 12   .  270)
                      ( 24   .  270)
                      ( 48  261     274)
                      ( 96  267     274)
                      (192  267     271))))
  (define-test "pick-from-e-series*: Approximate match in all"
    (pass-if-equal? (pick-from-e-series* 371)
                    '((  3  220     470)
                      (  6  330     470)
                      ( 12  330     390)
                      ( 24  360     390)
                      ( 48  365     383)
                      ( 96  365     374)
                      (192  370     374)))))
