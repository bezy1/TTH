#lang racket
(require racket/string)
(require racket/file)





(define (help)
  (error 'Usage "hte -t <TEMPLATE> -i <INPUT-FILE>"))
;(define read-define-block)
;(define (read-define))
(define (arguments args n)
  (if [< n 4] (help)
    (list
      (or
       (or
        (and  (string=? "-t" (vector-ref args 0))
              (and (file-exists? (vector-ref args 1)) (list "template" (vector-ref args 1))))
        (and  (string=? "-i" (vector-ref args 0))
              (and (file-exists? (vector-ref args 1)) (list "input-file" (vector-ref args 1)))))
       (error 'File (format "~a Doesn't exist" (vector-ref args 1))))
      (or
       (or
        (and  (string=? "-t" (vector-ref args 2))
              (and (file-exists? (vector-ref args 3)) (list "template" (vector-ref args 3))))
        (and  (string=? "-i" (vector-ref args 2))
              (and (file-exists? (vector-ref args 3)) (list "input-file" (vector-ref args 3)))))
       (error 'File (format "~a Doesn't exist" (vector-ref args 3))))
      )))

; If The First Element of the list is the template file return the list
; other wise flip the list and return it
(define-values (template input-file)
  (let [(var-list (let [(x (current-command-line-arguments))]
                    (let [(n (length (vector->list x)))]
                    (arguments x n))))]
    (if [string=? "template" (car (car var-list))]
        (values (car var-list) (cadr var-list))
        (values (cadr var-list) (car var-list)))))
