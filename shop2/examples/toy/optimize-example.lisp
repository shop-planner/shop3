(in-package :shop-user)
; This file illustrates the use of the :optimize-cost keyword argument
;  to SHOP2 in combination with variable cost operators (e.g., the
;  cost of the !pay-mortgage-interest operator below is .1 times the
;  ammount).  A variety of different values are provided to the :which
;  argument to show the different interactions between these
;  two arguments to SHOP2.

(defdomain loan (
  (:operator (!borrow-mortgage ?ammt ?acct)
	     () () ((debt ?ammt ?acct)) 15.0)
  (:operator (!borrow-credit-card ?ammt ?acct)
	     () () ((debt ?ammt ?acct)) 0.0)
  (:operator (!procrastinate)
	     () () () 0.0)
  (:operator (!pay-mortgage-interest ?acct)
	     ((debt ?ammt ?acct)) () () (* .1 ?ammt))
  (:operator (!pay-credit-card-interest ?acct)
	     ((debt ?ammt ?acct)) () () (* .2 ?ammt))

  (:method (get-money ?ammt ?acct)
	   ((have-credit-card ?acct))
	   (:ordered (!borrow-credit-card ?ammt ?acct)
		     (!pay-credit-card-interest ?acct)))

  (:method (get-money ?ammt ?acct)
	   ((have-house ?acct))
	   (:ordered (!borrow-mortgage ?ammt ?acct)
		     (!pay-mortgage-interest ?acct)))

  (:method (get-money ?ammt ?acct)
	   ((have-house ?acct))
	   (:ordered (!procrastinate)
		     (!borrow-mortgage ?ammt ?acct)
		     (!pay-mortgage-interest ?acct)))))

(defproblem p-200-nil loan
  () ((get-money 200 1001)))

(defproblem p-200-credit loan
  ((have-credit-card 1001)) ((get-money 200 1001)))

(defproblem p-200-house loan
  ((have-house 1001)) ((get-money 200 1001)))

(defproblem p-200-both loan
  ((have-credit-card 1001) (have-house 1001)) ((get-money 200 1001)))

(defproblem p-100-both loan
  ((have-credit-card 1001) (have-house 1001)) ((get-money 100 1001)))

(defproblem p-150-both loan
  ((have-credit-card 1001) (have-house 1001)) ((get-money 150 1001)))

(find-plans 'p-200-both :verbose :plans :which :all :optimize-cost t)
(find-plans 'p-100-both :verbose :plans :which :all :optimize-cost t)
(find-plans 'p-150-both :verbose :plans :which :all :optimize-cost t)
(find-plans 'p-150-both :verbose :plans :which :all-shallowest :optimize-cost t)
(find-plans 'p-150-both :verbose :plans :which :shallowest :optimize-cost t)
(find-plans 'p-150-both :verbose :plans :which :first :optimize-cost t)

(find-plans 'p-200-both :verbose :plans :which :first :optimize-cost 42)
(find-plans 'p-200-both :verbose :plans :which :first :optimize-cost 37)
(find-plans 'p-200-both :verbose :plans :which :first :optimize-cost 32)
(find-plans 'p-200-both :verbose :plans :which :shallowest :optimize-cost 37)
(find-plans 'p-200-both :verbose :plans :which :all :optimize-cost 42)
(find-plans 'p-200-both :verbose :plans :which :all :optimize-cost 37)
