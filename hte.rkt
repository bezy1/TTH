#lang racket
(require racket/string)
(require racket/file)
(require rebellion/streaming/reducer)
(require rebellion/collection/entry)
(require rebellion/collection/hash)



(define (help)
  (error 'Usage "hte -t <TEMPLATE> -i <INPUT-FILE> -o <File-Name>"))
(define (arguments args args-number)
  (if [null? args]
      (make-hash)
      (if [< n 4]
          (if [even? args-number]
              (hash-union
               (cond  [(string=? (car args) "-t")
                       (if [file-exists? (cadr args)] (make-immutable-hash (car args) (cadr args)) (error 'File (format "~a Doesn't Exist" (cadr args))))]
                      [(string=? (car args) "-i")
                       (if [file-exists? (cadr args)] (make-immutable-hash (car args) (cadr args)) (error 'File (format "~a Doesn't Exist" (cadr args))))]
                      [(string=? (car args) "-o") (make-immutable-hash (car args) (cadr args))]))
              (arguments (cdr args-list) args-number))
          (help))))

; If The First Element of the list is the template file return the list
; other wise flip the list and return it
(define-values (template input-file output-path)
  (let [(args (vector->list (current-command-line-arguments))) (args-hash (arguments args (length args)))]
    (list (hash-ref arg-hash "-t") (hash-ref arg-hash "-i") (hash-ref arg-hash "-o"))))

(define (string-list->string string-list)
    (if [null? string-list]
        ""
        (string-append (car string-list) (string-list->string (cdr string-list)))))

(define (all-but-last l)
  (if [null? (cdr l)]
      '()
      (cons (car l) (all-but-last (cdr l)))))

(define (read-section lines name )
  (let [(clean-str (string-split (car lines)))]
    (if [string=? (string-append name "#") (car clean-str)]
        (list (cdr lines))
        (if [(and (>= (length (string->list (car clean-str))) 2) (char=? (car (string->list(car clean-str))) '#\#))]
            (let [(tag (read-tag (cdr lines) (clean-str)))]
              (string-append
               (text-to-html
                (cadr tag) (list->string (cdr (string->list (car clean-str)))) (cdr clean-str))
               (read-section (cdr tag) name)))
            (error 'Syntax (format "~a Wrong Syntax" (car clean-str)))))))
;; The Problem Now Is here
;; Car violition when lines gets empty which isn't supposed to happen
(define (read-tag lines name)
  (if [equal? (length lines) 0]
      (error 'Section "Uncompleted Section")
      (let [(clean-str (string-split (car lines)))]
        (cond [(string=? (string-append name "#") (car clean-str)) (list (cdr lines))]
              [(and (>= (length (string->list (car clean-str))) 2) (char=? '#\# (car (string->list (car clean-str)))))
               (let* [(type (list->string (cdr (string->list (car clean-str)))))
                      (tag (read-tag (cdr lines) type))]
                 (cons (string-append
                        (text-to-html (string-list->string (all-but-last tag)) type (cdr clean-str))
                        "\n")
                       (read-tag (list-ref tag (- (length tag)1)) type)))]
              [else (cons (string-append (car lines) "\n")) (read-tag (cdr lines) name)]))))

(define (read-input-file lines name)
  (let [(clean-str (string-split (car lines)))]
    (if [string=? "" name]
        (if [string=? (car (string->list (car clean-str))) '#\#]
            (read-input-file (cdr lines) (cadr clean-str)

(define (attributes-to-html attributes)
  (if [false? attributes]
      ""
      (string-append (string-append (car attributes) "=\"" (cadr attributes) "\""))))

(define (text-to-html text type . attributes)
 (let [(attr (and (pair? (car attributes)) (car attributes)))]
  (string-append "<" type " " (attributes-to-html attr) ">" text (string-append "</" type ">" "\n"))))
(define (read-dynamic lines text defines)
  (let [(clean-str (string-split (car lines)))]
    (if [>  (length clean-str) 1]
        (error 'reference "Wrong Variable Reference")
        (if [string=? (car clean-str) "dynamic#"]
            (cons text (list (cdr lines) ))
              (read-dynamic (cdr lines)
                          (string-append
                           text
                           (or
                            (and
                            (not (andmap (lambda (x) (not(string=? x (car clean-str))))  (hash-keys defines)))
                            (hash-ref defines (car clean-str)))
                            (error 'reference "Variable is not defined")))
                          defines)))))
(define (read-static lines text)
  (if [= (length lines) 0]
      ""
      (let [(clean-str (string-split (car lines)))]
        (cond ([string=? (car clean-str) "#dynamic"]
            (let [(dynamic (read-dynamic (cdr lines) text defines))]
              (read-static (cadr dynamic) (car dynamic))))
            ([string=? (car clean-str) "static#"]
                text)
            (else
                (read-static (cdr lines) (string-append text (format "~a~%" (car lines)))))))))
(define (write-all text . path)
  (if [null? path]
      (display text (open-output-file "./file.html" #:mode 'text #:exists 'replace))
      (display text (open-output-file (car path) 'text 'replace))))
(define defines (for/reducer into-hash  ([entr (in-list  entr)])))
(write-all (read-static (cdr (hash-ref defines "rest")) (or output-file "")))
