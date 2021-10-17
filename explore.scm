(define-module (lispymon explore)
  #:use-module (jlib print)
  #:use-module (jlib lists)
  #:use-module (lispymon utils)
  #:use-module (lispymon lispymon)
  #:export  (explore-display
             explore-update))

(define (random:list l)
  (list-ref l (random (length l))))
(define (random:vector l)
  (random:list (vector->list l)))

(define area-path "assets/areas/")

(define (get-loc-from-data data)
  (define loc-name (assoc-get 'state-data data))
  (define loc (load-json-file
                (string-append area-path (symbol->string loc-name) ".json")))
  loc)
(define (explore-explore data)
  (define loc (get-loc-from-data data))
  ;; TODO: implement
  (println "explore" loc)
  data)

(define (explore-rest data)
  (define loc (get-loc-from-data data))
  (define rest-info (assoc-get "rest" loc))
  (define rest-success (assoc-get "chance" rest-info))
  (define success (>= rest-success (random:uniform)))
  (if success
      (let ((msg (random:vector (assoc-get "successes" rest-info))))
        (println "here:" data)
        (dbg
         (alist-merge data `((state-data . ((next . ,(assoc-get 'state-data data))
                                            (type . confirm)
                                            (msg . ,msg)))))))
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
