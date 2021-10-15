(define-module (lispymon user)
  #:use-module (jlib print)
  #:use-module (jlib files)
  #:export  (has-user
             make-user
             get-user-data
             set-user-data))


(define data-path "data/")
(define (ensure-datapath)
  (mkdir-if data-path))

(define (user-path uid)
  (string-append data-path uid))

(define (has-user uid) #f
  (ensure-datapath)
  (file-exists? (user-path uid)))

(define (make-user uid)
  (println "Created new user" uid)
  (set-user-data uid '()))

(define (get-user-data uid)
  (ensure-datapath)
  (with-input-from-file (user-path uid)
    (lambda ()
      (read))))

(define (set-user-data uid data)
  (ensure-datapath)
  (with-output-to-file (user-path uid)
    (lambda ()
      (write data))))
