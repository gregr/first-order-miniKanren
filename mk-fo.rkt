#lang racket
(provide
  ==
  =/=
  symbolo
  numbero
  stringo
  not-symbolo
  not-numbero
  not-stringo

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
