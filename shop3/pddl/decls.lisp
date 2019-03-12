(in-package :shop2)

(defparameter +pddl-requirements-keywords+
  '(:strips
    :typing
    :negative-preconditions
    :disjunctive-preconditions
    :equality
    :existential-preconditions
    :universal-preconditions
    :quantified-preconditions           ; existential + universal
    :conditional-effects
    :fluents                            ; metric functions and effects
    :adl
    :durative-actions                   ; does not imply fluents
    :duration-inequalities
    :continuous-effects
    :costs))