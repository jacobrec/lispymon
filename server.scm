(define-module (lispymon server)
  #:use-module (schingle schingle)
  #:use-module (schingle content-type)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:use-module (lispymon user)
  #:use-module (lispymon game))

;; TODO: use a system more like the below. This is for security reasons, but theres no need to do that if I haven't even implemented passwords yet. This system makes it easier for development
(define (gen-token userid)
  userid)
(define (username-from-token token)
  token)
#|
(define tokenmap (make-hash-table))
(define (gen-token userid)
  (define tok (random 18446744073709551614))
  (hash-set! tokenmap tok userid)
  tok)
(define (username-from-token token)
  (define userid (hash-ref tokenmap token))
  (hash-remove! tokenmap token)
  userid)
|#

(define (lispymon-response text options tok)
  (json `((text . ,text)
          (options . ,options)
          (token . ,tok))))

(define (get-user-prompt userid)
    (if userid
      (begin
        (let ((data (get-user-data userid))
              (tok (gen-token userid)))
          (define-values (text options) (get-display userid))
          (lispymon-response text options tok)))
      (plain "Invalid token" #:code 400)))

(define (process-response token selection)
  (define userid (username-from-token token))
  (if userid
    (begin
      (let ((data (get-user-data userid)))
        (define response-valid? (validate-response userid selection))
        (if response-valid?
            (begin
              (set-user-data userid (update data selection))
              userid)
            #f)))
    #f))

(POST /play
      (lambda (request body)
        (define uid (process-response (assoc-get "token" body) (assoc-get "selection" body)))
        (get-user-prompt uid)))

(POST /signin
      (lambda (request body)
        (define uid (assoc-get "username" body))
        (unless (has-user uid)
          (make-user uid))
        (get-user-prompt uid)))


(run-schingle #:port 8080)
