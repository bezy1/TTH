#lang racket
(require racket/string)
(require racket/file)





(define (help)
  (error 'Usage "hte -t <TEMPLATE> -i <INPUT-FILE>"))
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
        (values (cadr (car var-list)) (cadr (cadr var-list)))
        (values (cadr (cadr var-list)) (cadr (car var-list))))))

(define (read-input-file lines name state)
  (let [(clean-str (car (string-split (car lines))))]
    (if [= state 1]
      (if [string=? clean-str (string-append name "#")]
          ""
          (string-append (car lines) (read-input-file (cdr lines) name state)))
      (if [string=? clean-str (string-append "#" name)]
          (read-input-file (cdr lines) name 1)
          (read-input-file (cdr lines) name state)))))

(define (attributes-to-html attributes)
  (if [null? attributes]
      ""
      (string-append (string-append (car attributes) "=\"" (cadr attributes) "\""))))

(define (text-to-html text type . attributes)
 (let [(attr (and (pair? (car attributes)) (car attributes)))]
  (string-append "<" type " " (attributes-to-html attr) ">" text (string-append "</" type ">"))))

(define (read-define lines)
  (let [(clean-str (string-split (car lines)))]
    (if (string=? (car clean-str) "define#")
        (cdr lines)
        (if (< (length clean-str) 2)
            (error 'define "Wrong Variable Definition")
            (cons
             (text-to-html
              (read-input-file (file->lines input-file) (car clean-str) 0) (cadr clean-str) (cddr clean-str))
             (read-define (cdr lines)))))))



(read-define (cdr (file->lines template)))
