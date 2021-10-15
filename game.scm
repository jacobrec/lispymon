(define-module (lispymon game)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:use-module (lispymon user)
  #:use-module (lispymon intro)
  #:use-module (ice-9 copy-tree)
  #:export  (update
             get-display
             validate-response))


;; Ensures the selection is valid
(define (validate-response uid selection)
  (define-values (text selections) (get-display uid))
  (or (string? selections)
      (member selection (map string-downcase (vector->list selections)))))

;; Takes in data and a selection and updates the users data. Returns new data
(define (update datad selection)
  (define data (get-data-d datad))
  (define state (assoc-get 'state data))
  (case state
    ((intro) (intro-update data selection))
    (else data)))

;; Takes in user id and returns text and options
(define (get-display uid)
  (define data (get-data uid))
  (define state (assoc-get 'state data))
  (define state-data (assoc-get 'state-data data))
  (case state
    ((intro) (intro-display state-data))
    (else (values "Error. Unknown state. There is nothing you can do now." #("Okay :(")))))


;; State is one of :intro, :battle, :travel, :explore
(define default-data '((state . intro) (state-data . begin)))
(define (get-data uid)
  (get-data-d (get-user-data uid)))
(define (get-data-d data)
  (if (null? data)
    (copy-tree default-data)
    data))
