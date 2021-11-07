#lang racket
(provide
  ==
  =/=
  symbolo
  numbero
  stringo

  define-relation
  fresh
  conde
  query
  run
  run*

  stream-take
  conj*
  disj*
  )
(require "microk-fo.rkt")
(include "mk-syntax.rkt")
