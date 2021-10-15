(define-module (lispymon utils)
  #:use-module (json)
  #:export (load-json-file))

(define (load-json-file filename)
  (json->scm (open-input-file filename)))
