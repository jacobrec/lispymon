(define-module (lispymon intro)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:use-module (lispymon utils)
  #:use-module (lispymon lispymon)
  #:export  (intro-display
             intro-update))


(define intro-data (load-json-file "assets/intro.json"))

(define (create-starter type) ; TODO: take chosen type into account
  (define ex-type #vu8(255 0 0 255 0 0 0 255 0 0 0))
  (define ex-stat #vu8(126 0 63 126 189 255))
  (define nat (create-lispymon-nature ex-type ex-stat))
  (define nur (create-lispymon-nurture 1000 #(0 0 0 0 0 0) '()))
  (define mon (create-lispymon nat nur))
  (list mon))


(define (intro-update data selection)
  (case (assoc-get 'state-data data)
    ((begin) (acons 'name selection (assoc-set! data 'state-data 'give-mon)))
    ((give-mon) (acons 'party (create-starter selection) (assoc-set! data 'state-data 'confirm)))
    ;; TODO: make confirm go to somewhere else
    ((confirm) (alist-merge data `((state . explore) (state-data . startington))))
    (else data)))

(define (intro-display statedata)
  (case statedata
    ((begin) (values (assoc-get "name" intro-data) "Name: "))
    ((give-mon) (values (assoc-get "mon-pref" intro-data)
                        #("Fire" "Ice" "Electric")))
    ((confirm) (values (assoc-get "confirm" intro-data) #("Okay")))
    (else (values "Error. Unknown Intro state." #("Okay :(")))))
