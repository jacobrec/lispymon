(define-module (lispymon client)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 match))

(define (http-post-json loc body-str)
  (define-values (res data)
    (http-post loc #:body (string->utf8 body-str)
               #:headers '((Content-Type . "application/json"))))
  (values
   (json-string->scm (utf8->string data))
   (= 200 (response-code res))))

(define (post loc obj)
  (http-post-json loc (scm->json-string obj)))

(define (display-options response)
  (print "|")
  (map (lambda (x) (print " ") (print x) (print " |")) (vector->list (assoc-get "options" response)))
  (newline))
(define (display-response response)
  (println (assoc-get "text" response))
  (newline))

(define (confirmed-input text)
  (let ((choice "") (confirm #f))
    (while (not confirm)
      (set! choice (readline text))
      (print "\"")
      (print choice)
      (println "\"")
      (set! confirm (string=? "y" (readline "Confirm (y/n)? "))))
    choice))

(define (get-user-input response)
  (confirmed-input (assoc-get "options" response)))


(define* (get-user-selection response #:optional (err #f))
  (display-options response)
  (when err (println err))
  (define choice (readline "=> "))
  (define options (map string-downcase (vector->list (assoc-get "options" response))))
  (define possibilities (filter (lambda (x) (string-contains x choice)) options))
  (match possibilities
    ((x) x)
    (() (get-user-selection response "Could not match an option to your selection"))
    ((x y ...) (get-user-selection response "Unable to narrow your selection to a single option"))))

(define (user-reply response)
  (if (string? (assoc-get "options" response))
    (get-user-input response)
    (get-user-selection response)))


(define serverloc "http://localhost:8080")
(define (signin)
  (define name "jacob");(confirmed-input "Username: "))
  (define r (post (string-append serverloc "/signin") `((username . ,name))))
  (play r))

(define (play response)
  (display-response response)
  (define selection (user-reply response))
  (play (post "http://localhost:8080/play"
              `((token . ,(assoc-get "token" response))
                (selection . ,selection)))))

(signin)
