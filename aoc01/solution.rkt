#lang racket

(define input (file->list "input"))

(println (apply * (sequence-ref (sequence-filter (lambda (x) (= (apply + x) 2020)) (in-combinations input 2)) 0)))
