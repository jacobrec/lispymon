(define-module (lispymon explore)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:use-module (jlib random)
  #:use-module (lispymon utils)
  #:use-module (lispymon lispymon)
  #:use-module (srfi srfi-1)
  #:export  (explore-display
             explore-update))

(define area-path "assets/areas/")

;; Gets a random value from a weighted vector
;; Example:
;; #(#(3 opt1) #(2 opt2))
;; => probably opt1 (60%) of the time
(define (random:weighted-vector vector)
  (define sum (fold (lambda (x acc) (+ acc (vector-ref x 0))) 0 (vector->list vector)))
  (define value (+ 1 (random sum)))
  (define (loop cs idx)
    (define cur (vector-ref (vector-ref vector idx) 0))
    (define cs-n (+ cs cur))
    (if (>= cs-n value)
      (vector-ref
       (vector-ref vector idx)
       1)
      (loop cs-n (+ 1 idx))))
  (loop 0 0))

(define (get-loc-from-data data)
  (define loc-name (assoc-get 'state-data data))
  (load-json-file (string-append area-path (symbol->string loc-name) ".json")))

(define (explore-encounter-dialog data loc)
  (define msg (random:vector (assoc-get '("explore" "dialog") loc)))
  (state-confirm data msg))

(define (explore-explore data)
  (define loc (get-loc-from-data data))
  (define encounters (assoc-get '("explore" "encounters") loc))
  (define encounter (random:weighted-vector encounters))
  (cond
   ((string=? "dialog" encounter) (explore-encounter-dialog data loc)))
  ;; TODO: implement
  (println "explore" encounter)
  data)

(define (state-confirm data msg)
  (alist-merge data `((state-data . ((next . ,(assoc-get 'state-data data))
                                     (type . confirm)
                                     (msg . ,msg))))))

(define (explore-rest data)
  (define loc (get-loc-from-data data))
  (define rest-info (assoc-get "rest" loc))
  (define rest-success (assoc-get "chance" rest-info))
  (define success (>= rest-success (random:uniform)))
  (if success
      (let ((msg (random:vector (assoc-get "successes" rest-info))))
        (println "here:" data)
        (state-confirm data msg))
          ;; TODO: actually heal something
      (let ((encounter "TODO"))
         data)))

(define (explore-update data selection)
  (define statedata (assoc-get 'state-data data))
  (cond
   ((and (list? statedata) (eq? 'confirm (assoc-get 'type statedata)))
    (alist-merge data `((state-data . ,(assoc-get 'next statedata)))))
   ((symbol? statedata)
    (cond
      ((string=? selection "explore") (explore-explore data))
      ((string=? selection "rest") (explore-rest data))
      (else (println "Invalid [explore] selection: " selection) data)))))

(define (explore-display statedata)

  (cond
   ((symbol? statedata)
    (let ((loc (load-json-file (string-append area-path (symbol->string statedata) ".json"))))
      (values (string-append (assoc-get "name" loc) ":\n"
                             (assoc-get "intro" loc))
             (assoc-get "options" loc))))
   ((and (list? statedata) (eq? 'confirm (assoc-get 'type statedata)))
    (values (string-append (assoc-get 'msg statedata))
            #("Okay")))
   (else (print "Unknown statedata:") (writeln statedata))))
