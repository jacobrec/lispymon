(define-module (lispymon explore)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:use-module (jlib random)
  #:use-module (lispymon utils)
  #:use-module (lispymon lispymon)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun)
  #:export  (explore-display
             explore-update))

(define area-path "assets/areas/")

;;;;
;;; Utilities
;;;;

(define (state-confirm data msg)
  (alist-merge data `((state-data . ((next . ,(assoc-get 'state-data data))
                                     (type . confirm)
                                     (msg . ,msg))))))

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

(define (get-loc-string loc-name)
  (load-json-file (string-append area-path loc-name)))
(define (get-loc loc-name)
  (get-loc-string (string-append (symbol->string loc-name) ".json")))
(define (get-loc-from-data data)
  (define loc-name (assoc-get 'state-data data))
  (get-loc loc-name))

(define (area-symbol-from-name name)
  (define files
    (filter (lambda (x) (not (or (string=? ".." x) (string=? "." x))))
            (scandir area-path)))
  (define match
    (filter (lambda (x)
              (define xname (assoc-get "name" (get-loc-string x)))
              (string=? (string-downcase xname) (string-downcase name)))
            files))
  (string->symbol
    (string-replace-substring (car match) ".json" "")))

(define (area-symbols-to-names areas)
  (map (lambda (x) (assoc-get "name" (get-loc-string (string-append x ".json"))))
       areas))
;;;;
;;; Explore encounters
;;;;
(define (explore-encounter-dialog data loc)
  (define msg (random:vector (assoc-get '("explore" "dialog") loc)))
  (state-confirm data msg))

;;;;
;;; Top level options
;;;;
(define (explore-explore data)
  (define loc (get-loc-from-data data))
  (define encounters (assoc-get '("explore" "encounters") loc))
  (define encounter (random:weighted-vector encounters))
  (cond
   ((string=? "dialog" encounter) (explore-encounter-dialog data loc)))
  data)

(define (explore-rest data)
  (define loc (get-loc-from-data data))
  (define rest-info (assoc-get "rest" loc))
  (define rest-success (assoc-get "chance" rest-info))
  (define success (>= rest-success (random:uniform)))
  (if success
      (let ((msg (random:vector (assoc-get "successes" rest-info))))
        (state-confirm data msg))
          ;; TODO: actually heal something
      (let ((encounter "TODO"))
         data)))

(define (explore-travel data)
  (define loc (get-loc-from-data data))
  (define travel-args (assoc-get '("travel" "locations") loc))
  (define dests (append (area-symbols-to-names (vector->list travel-args)) (list "Cancel")))
  (alist-merge data `((state-data . ((from . ,(assoc-get 'state-data data))
                                     (type . travel)
                                     (options . ,(list->vector dests)))))))
(define (explore-travel2 data selection)
  (define statedata (assoc-get 'state-data data))
  (if (string=? "cancel" selection)
    (alist-merge data `((state-data . ,(assoc-get 'from statedata))))
    (alist-merge data `((state-data . ,(area-symbol-from-name selection))))))
;;;;
;;; Explore
;;;;
(define (explore-update data selection)
  (define statedata (assoc-get 'state-data data))
  (cond
   ((and (list? statedata) (eq? 'confirm (assoc-get 'type statedata)))
    (alist-merge data `((state-data . ,(assoc-get 'next statedata)))))
   ((and (list? statedata) (eq? 'travel (assoc-get 'type statedata)))
    (explore-travel2 data selection))
   ((symbol? statedata)
    (cond
      ((string=? selection "explore") (explore-explore data))
      ((string=? selection "rest") (explore-rest data))
      ((string=? selection "travel") (explore-travel data))
      (else (println "Invalid [explore] selection: " selection) data)))
   (else (println "Invalid [explore] selection2: " selection) data)))

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
   ((and (list? statedata) (eq? 'travel (assoc-get 'type statedata)))
    (values (string-append "Travelling from "
                           (car (area-symbols-to-names (list (symbol->string (assoc-get 'from statedata)))))
                           ". Where would you like to go?")
            (assoc-get 'options statedata)))
   (else (print "Unknown statedata:") (writeln statedata))))
