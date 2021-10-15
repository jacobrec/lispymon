(define-module (lispymon lispymon)
  #:use-module (jlib print)
  #:use-module (jlib bytes)
  #:use-module (jlib lists)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:export (create-lispymon
            create-lispymon-nature
            create-lispymon-nurture))

;;;;
;; Lispymon
;;;;
;; Type Info:
;; Each move and monster has a typing composed of 4 parts.
;; Element - Fire, Water, Electric, Earth, Air, Ice
;; Type - Plant <- Animal <- Mineral
;; Affinity - Light to Dark
;; Solidity - Physical to Spectral
;;;
;; Each monster has a collection of other attributes.
;; Weight-mod, Height-mod, base-stats
;;;
;; Monsters also have nurture values:
;; Exp, stat values(DVs), moves
;;;
;; Finally, there are derived values:
;; Level, stats, weight, height, body shape, skin material
;;;
;; TODO: eventually, instead of one byte vector representing the nature data there will be 3, for proper genetics

;; TODO: make a good description that takes into account the types, size, height, weight, and all kinfs of stuff
(define (lispymon-describe lispymon)
  (define bt (lispymon-bodytype lispymon))
  (define st (lispymon-skintype lispymon))
  (define stats (lispymon-stats lispymon))
  (string-append "This lispymon is a " bt " creature with a " st " skin."))

(define (create-lispymon nature nurture)
  `((genes . ,nature)
    (nurture . ,nurture)))
(define (create-lispymon-nurture exp dvs moves)
  `((exp . ,exp)
    (dvs . ,dvs)
    (moves . ,moves)))
(define (create-lispymon-nature type stats)
  (define height-weight-mod (u8-list->bytevector (list (random 256) (random 256))))
  (bytevector-append type stats height-weight-mod))
;; TODO: this should only return the expressed alleles
(define (lispymon-genes lispymon)
  (assoc-get 'genes lispymon))

;; exp = level ^ 3
(define (lispymon-level lispymon)
  (define exp (lispymon-exp lispymon))
  (floor (expt exp 1/3)))
(define (lispymon-exp lispymon)
  (+ 1 (assoc-get 'exp
                  (assoc-get 'nurture lispymon))))

(define (lispymon-type lispymon)
  (bytevector-subvector (lispymon-genes lispymon) 0 12))
(define (lispymon-basestats lispymon)
  (bytevector-subvector (lispymon-genes lispymon) 11 6))
;; A value between 0-2048 indicating relative size
(define (lispymon-size lispymon)
  (define bv (lispymon-genes lispymon))
  (min 1
       (+ (at bv 7)
          (at bv 8)
          (at bv 9)
          (at bv 10)
          (at bv 11)
          (at bv 13)
          (at bv 17)
          (at bv 18))))
(define (lispymon-height lispymon) ; in Kg
  (define bv (lispymon-genes lispymon))
  (define type (lispymon-type lispymon))
  (exact->inexact
    (/ (+ (at bv 17)
          (lispymon-size lispymon))
       100)))
(define (lispymon-weight lispymon) ; in m
  (define bv (lispymon-genes lispymon))
  (define type (lispymon-type lispymon))
  (* (+ (* 1 (type-plant type))
        (* 2 (type-animal type))
        (* 10 (type-mineral type)))
     (+ 0.001 (type-solidity type))
     (+ (at bv 17)
        (lispymon-size lispymon))))


(define (lispymon-stats lispymon)
  (define base (lispymon-basestats lispymon))
  (define level (lispymon-level lispymon))
  (define dv (assoc-get 'dvs (assoc-get 'nurture lispymon)))
  (println base level dv)
  (list->vector (map (lambda (x)
                       (define b (stat-scale (at base x)))
                       (define d (vector-ref dv x))
                       (round (+ (if (= 0 x) (+ 10 level) 5) (* level b) (sqrt d))))
                     (iota 6))))
;; TODO: add more body types and body shapes. Make them random, but influences by type
(define (lispymon-bodytype lispymon)
  "Snakelike")
(define (lispymon-skintype lispymon)
  "Scaley")

;; since stats are stored as a vu8 array they become scaled
(define (stat-scale bv)
  (define lower 0.5)
  (define upper 2.5)
  (+ lower (* (- upper lower) (/ bv 255))))
(define (stat-health stat)
  (stat-scale (vector-ref stat 0)))
(define (stat-strength stat)
  (stat-scale (vector-ref stat 1)))
(define (stat-magic stat)
  (stat-scale (vector-ref stat 2)))
(define (stat-defense stat)
  (stat-scale (vector-ref stat 3)))
(define (stat-resistance stat)
  (stat-scale (vector-ref stat 4)))
(define (stat-speed stat)
  (stat-scale (vector-ref stat 5)))

;;; Each of the type getters returns a number from 0-1 indicating a percent
;; 0% solidity is entirely spectral, 100% is entirely physical
(define (type-solidity type)
  (/ (at type 0) 255))
;; 0% affinity is entirely dark, 100% is entirely light
(define (type-affinity type)
  (/ (at type 1) 255))

;; plant animal and mineral are connected the percent between these 3 must equal 100
(define (type-plant type)
  (define sum (+ (at type 2) (at type 3) (at type 4)))
  (/ (at type 2) sum))
(define (type-animal type)
  (define sum (+ (at type 2) (at type 3) (at type 4)))
  (/ (at type 3) sum))
(define (type-mineral type)
  (define sum (+ (at type 2) (at type 3) (at type 4)))
  (/ (at type 4) sum))

;; elemental types also must sum to 100%
(define (type-fire type)
  (define sum (fold + 0 (map (lambda (x) (at type (+ 5 x))) (iota 6))))
  (/ (at type 5) sum))
(define (type-electric type)
  (define sum (fold + 0 (map (lambda (x) (at type (+ 5 x))) (iota 6))))
  (/ (at type 6) sum))
(define (type-ice type)
  (define sum (fold + 0 (map (lambda (x) (at type (+ 5 x))) (iota 6))))
  (/ (at type 7) sum))
(define (type-water type)
  (define sum (fold + 0 (map (lambda (x) (at type (+ 5 x))) (iota 6))))
  (/ (at type 8) sum))
(define (type-earth type)
  (define sum (fold + 0 (map (lambda (x) (at type (+ 5 x))) (iota 6))))
  (/ (at type 9) sum))
(define (type-air type)
  (define sum (fold + 0 (map (lambda (x) (at type (+ 5 x))) (iota 6))))
  (/ (at type 10) sum))


(define (at bv i)
  (bytevector-u8-ref bv i))


;; TODO: extract type effectiveness to json file
(define elemental-types (list type-fire type-ice type-air type-earth type-electric type-water))
(define super-effective `((,type-fire . ,type-ice)
                          (,type-ice . ,type-air)
                          (,type-air . ,type-earth)
                          (,type-earth . ,type-electric)
                          (,type-electric . ,type-water)
                          (,type-water . ,type-fire)))
(define not-very-effective `((,type-fire . ,type-water)
                             (,type-ice . ,type-fire)
                             (,type-air . ,type-ice)
                             (,type-earth . ,type-air)
                             (,type-electric . ,type-earth)
                             (,type-water . ,type-electric)))
(define type-super-effective `((,type-plant . ,type-mineral)
                               (,type-animal . ,type-plant)
                               (,type-mineral . ,type-animal)))
(define type-not-very-effective `((,type-plant . ,type-animal)
                                  (,type-animal . ,type-mineral)
                                  (,type-mineral . ,type-plant)))

;; Type 1 attacks type 2
(define (calculate-effective-modifier type1 type2)
  (define solid (- 1 (abs (- (type-solidity type1) (type-solidity type2)))))
  (define affinity (+ 1 (abs (- (type-affinity type1) (type-affinity type2)))))
  (define (type-circle effective not-effective types)
    (fold + 0
      (map (lambda (x) (+ (* 2 (x type1) ((assoc-get x effective) type2))
                          (/ (* (x type1) ((assoc-get x not-effective) type2)) 2)))
           types)))
  (define pam-type (type-circle type-super-effective type-not-very-effective
                                (list type-plant type-animal type-mineral)))
  (define elemental (type-circle super-effective not-very-effective elemental-types))
  (exact->inexact (* solid affinity pam-type elemental)))

;; Type 1 uses a move of type 2
(define (calculate-stab-modifier type1 type2)
  (define (matches type upper lower)
    (+ lower (* (- upper lower) (- 1 (abs (- (type type1) (type type2)))))))
  (define affinity (matches type-affinity 1.5 1))
  (define (type-circle types)
    (fold + 1 (map (lambda (x) (* (matches x 0.5 0) (x type1))) types)))
  (define pam-type (type-circle (list type-plant type-animal type-mineral)))
  (define elemental (type-circle elemental-types))
  (* affinity pam-type elemental))

(define (print-type-table type)
  (define (percent num)
    (string-append (number->string (round (* 100 num))) "%"))
  (print "Solidity:" (percent (type-solidity type)))
  (println "  Affinty:" (percent (type-affinity type)))
  (print "Plant:" (percent (type-plant type)))
  (print "  Animal:" (percent (type-animal type)))
  (println "  Mineral:" (percent (type-mineral type)))
  (print "Fire:" (percent (type-fire type)))
  (print "  Ice:" (percent (type-ice type)))
  (println "  Electric:" (percent (type-electric type)))
  (print "Water:" (percent (type-water type)))
  (print "  Earth:" (percent (type-earth type)))
  (println "  Air:" (percent (type-air type)))
  (println))


#|
(define example-type-solid-light-plant-fire #vu8(255 255 255 0 0 255 0 0 0 0 0))
(define example-type-solid-dark-mineral-ice #vu8(255 0 0 0 255 0 0 255 0 0 0))
|#


