#lang racket
(require racket/string)
(require racket/file)
(require rebellion/collection/entry)
(require rebellion/collection/hash)
(require rebellion/streaming/reducer)



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
    (if [string=? (car clean-str) "define#"]
        (list (entry "rest" (cdr lines)))
        (if [< (length clean-str) 2]
            (error 'define "Wrong Variable Definition")
              (cons
               (entry
                (car clean-str)
                (text-to-html
                 (read-input-file (file->lines input-file) (car clean-str) 0) (cadr clean-str) (cddr clean-str)))
               (read-define (cdr lines)))))))
(define (read-dynamic lines text defines)
  (let [(clean-str (string-split (car lines)))]
    (if [>  (length clean-str) 1]
        (error 'reference "Wrong Variable Reference")
        (if [string=? (car clean-str) "dynamic#"]
            (cons text (list (cdr lines) ))
              (read-dynamic (cdr lines)
                          (string-append
                           text
                           (and
                            ((andmap (lambda (x) (not(string=? x "jasim")))  (hash-keys defines)))
                            (hash-ref defines (car clean-str))))
                          defines)))))
(define (read-static lines text)
  (if [= (length lines) 0]
      ""
      (let [(clean-str (string-split (car lines)))]
        (if [string=? (car clean-str) "#dynamic"]
            (let [(dynamic (read-dynamic (cdr lines) text defines))]
              (read-static (cadr dynamic) (car dynamic)))
            (if [string=? (car clean-str) "static#"]
                text
                (read-static (cdr lines) (string-append text (car lines))))))))



(define defines (for/reducer into-hash  ([entr (in-list (read-define (cdr (file->lines template))))]) entr))
(read-static (file->lines template) "")
