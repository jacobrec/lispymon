(define-module (lispymon gene)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-60)
  #:export (genome-length
            random-individual
            individual-expression
            get-genetic-properties
            breed))

;; a genepool takes an alist of the form '((name . bitlength) ...)
;; a chromosome/gamete is a bigint contining genetic information
;; an individual is a cons cell of two chromosomes

(define (derive-dominance-vector genepool)
  (if (null? genepool)
      0
      (logior (hash (car genepool) (ash 1 (cdar genepool)))
              (ash (derive-dominance-vector (cdr genepool)) (cdar genepool)))))

(define (genome-length genepool)
  (fold + 0 (map cdr genepool)))

(define (random-individual genepool)
  (define max-value (ash 1 (genome-length genepool)))
  (cons (random max-value)
        (random max-value)))

(define (individual-expression genepool individual)
  (define dv (derive-dominance-vector genepool))
  (logior (logand (car individual) (cdr individual))
          (logand dv (logior (car individual) (cdr individual)))))

(define (get-genetic-properties genepool individual)
  (define (inner genepool expression)
    (if (null? genepool)
        '()
        (acons (caar genepool) (logand (1- (ash 1 (cdar genepool))) expression)
               (inner (cdr genepool) (ash expression (- (cdar genepool)))))))
  (inner genepool
         (individual-expression genepool individual)))

(define (create-gamete individual)
  (define mask (random (ash 1 (max (integer-length (car individual))
                                   (integer-length (cdr individual))))))
  (bitwise-merge mask (car individual) (cdr individual)))

(define (breed individual1 individual2)
  (cons (create-gamete individual1)
        (create-gamete individual2)))
