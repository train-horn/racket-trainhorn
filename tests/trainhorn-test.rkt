#lang racket/base
;; rackunit port of the trainhorn-calc core test vectors.
;; Run: raco test tests   (or raco test -p trainhorn once installed)

(require rackunit
         racket/list
         trainhorn)

(define tol 1e-9)
(define (log10 x) (/ (log x) (log 10.0)))

(define-syntax-rule (check-input-error expr)
  (check-exn exn:fail:contract? (lambda () expr)))

;; ---------------------------------------------------------------- acoustics
(test-case "150 dB at 10 ft gives 130 dB at 100 ft"
  (check-= (spl-at-distance 150 100) 130.0 tol))

(test-case "same distance returns the source level"
  (check-= (spl-at-distance 118 10) 118.0 tol))

(test-case "closer than the reference is louder"
  (check-= (spl-at-distance 150 5) (+ 150 (* 20 (log10 2))) tol))

(test-case "metres and a custom reference: 120 dB at 1 m -> 100 dB at 10 m"
  (check-= (spl-at-distance 120 10 #:ref-distance 1 #:unit 'm) 100.0 tol))

(test-case "unit does not matter without absorption"
  (check-= (spl-at-distance 140 300 #:unit 'ft) (spl-at-distance 140 300 #:unit 'm) tol))

(test-case "absorption: 1000 m past a 10 m reference at 2 dB/km costs 2 dB"
  (define plain (spl-at-distance 150 1010 #:ref-distance 10 #:unit 'm))
  (define absorbed (spl-at-distance 150 1010 #:ref-distance 10 #:unit 'm #:absorption-db/km 2.0))
  (check-= (- plain absorbed) 2.0 tol))

(test-case "absorption converts feet to metres"
  (define plain (spl-at-distance 150 3290.84 #:unit 'ft))
  (define absorbed (spl-at-distance 150 3290.84 #:unit 'ft #:absorption-db/km 10.0))
  (check-= (- plain absorbed) (/ (* 10.0 (- 3290.84 10) 0.3048) 1000) tol))

(test-case "distance-for-spl known values"
  (check-= (distance-for-spl 150 130) 100.0 tol)
  (check-= (distance-for-spl 150 90) 10000.0 1e-6))

(test-case "round trip without absorption"
  (for ([pair (in-list '((150 37.5) (118 2.0) (153.2 2500) (100 10)))])
    (define src (car pair))
    (define d (cadr pair))
    (check-= (distance-for-spl src (spl-at-distance src d)) d 1e-6)))

(test-case "round trip with absorption (bisection)"
  (for ([d (in-list '(3.0 50.0 800.0 5000.0))])
    (define target (spl-at-distance 150 d #:unit 'm #:absorption-db/km 3.7))
    (define back (distance-for-spl 150 target #:unit 'm #:absorption-db/km 3.7))
    (check-= back d (* d 1e-9))))

(test-case "absorption shortens the distance"
  (check < (distance-for-spl 150 90 #:absorption-db/km 1.9) (distance-for-spl 150 90)))

(test-case "doubling loss and distance ratio"
  (check-= (db-doubling-loss) 6.020599913279624 tol)
  (check-= (distance-ratio-loss 10) 20.0 tol))

(test-case "combine-levels"
  (check-= (combine-levels '(100 100)) (+ 100 (* 10 (log10 2))) tol)
  (check-= (combine-levels '(150)) 150.0 tol)
  (check-= (combine-levels '(120 90)) 120.0 0.005)
  (check-= (combine-levels '(1000.0 1000.0)) (+ 1000 (* 10 (log10 2))) 1e-9))

(test-case "absorption table"
  (check-= (hash-ref air-absorption-db/km 500) 1.9 tol)
  (check-equal? (hash-count air-absorption-db/km) 8))

(test-case "invalid acoustics inputs"
  (check-input-error (spl-at-distance 150 0))
  (check-input-error (spl-at-distance 150 -5))
  (check-input-error (spl-at-distance 150 10 #:ref-distance 0))
  (check-input-error (spl-at-distance 150 10 #:unit 'yd))
  (check-input-error (spl-at-distance "loud" 10))
  (check-input-error (spl-at-distance +nan.0 10))
  (check-input-error (spl-at-distance +inf.0 10))
  (check-input-error (spl-at-distance 150 10 #:absorption-db/km -1))
  (check-input-error (spl-at-distance #t 10))
  (check-input-error (combine-levels '()))
  (check-input-error (distance-ratio-loss 0))
  (check-input-error (distance-for-spl 1e300 -1e300)))

(test-case "range errors are exn:fail:trainhorn"
  (check-exn exn:fail:trainhorn? (lambda () (spl-at-distance 150 0)))
  (check-exn #rx"distance must be greater than 0" (lambda () (spl-at-distance 150 0))))

;; ---------------------------------------------------------------- air system
(test-case "1 gallon from 14.696 psig to 0 releases 1 gallon of free air"
  (check-= (free-air-volume-cuft 1 14.696 0) (/ 231.0 1728) tol))

(test-case "tank runtime known value"
  (define expected (* (/ (* 5 (/ 231.0 1728) 40) 14.696 10) 60))
  (check-= (tank-runtime-seconds 5 150 110 10) expected tol)
  (check-= expected 10.916 0.0005))

(test-case "runtime scales linearly"
  (define base (tank-runtime-seconds 2 150 120 5))
  (check-= (tank-runtime-seconds 4 150 120 5) (* 2 base) tol)
  (check-= (tank-runtime-seconds 2 150 120 10) (/ base 2) tol)
  (check-= (tank-runtime-seconds 2 150 90 5) (* 2 base) tol))

(test-case "altitude gives more free air"
  (check > (tank-runtime-seconds 5 150 110 10 #:atmospheric-psi 12.2)
         (tank-runtime-seconds 5 150 110 10)))

(test-case "blast-count"
  (check-equal? (blast-count 10.0 2.0) 5)
  (check-equal? (blast-count 10.9 2.0) 5)
  (check-equal? (blast-count 0 1.5) 0)
  (check-equal? (blast-count (* 0.3 3) 0.3) 3))

(test-case "compressor recovery, constant flow"
  (define expected (* (/ (* 2 (/ 231.0 1728) 40) 14.696 1.0) 60))
  (check-= (compressor-recovery-seconds 2 110 150 1.0) expected tol)
  (check-= (compressor-recovery-seconds 2 110 150 1.0) 43.66 0.005))

(test-case "recovery equals runtime at the same CFM"
  (check-= (compressor-recovery-seconds 3 120 150 2.5) (tank-runtime-seconds 3 150 120 2.5) tol))

(test-case "compressor recovery, linear flow model"
  (define flat (compressor-recovery-seconds 2 110 150 1.0 #:cfm-at-zero-psi 1.0 #:rated-psi 150))
  (check-= flat (compressor-recovery-seconds 2 110 150 1.0) tol)
  (define t (compressor-recovery-seconds 2 110 150 1.0 #:cfm-at-zero-psi 2.0 #:rated-psi 150))
  (define v (/ (* 2 (/ 231.0 1728)) 14.696))
  (define steps 20000)
  (define dp (/ 40.0 steps))
  (define numeric
    (* 60 (for/sum ([i (in-range steps)])
            (/ (* v dp) (- 2.0 (/ (+ 110 (* (+ i 0.5) dp)) 150))))))
  (check-= t numeric 5e-5)
  (check < t (compressor-recovery-seconds 2 110 150 1.0)))

(test-case "invalid air-system inputs"
  (check-input-error (tank-runtime-seconds 0 150 110 10))
  (check-input-error (tank-runtime-seconds 5 110 150 10))
  (check-input-error (tank-runtime-seconds 5 150 150 10))
  (check-input-error (tank-runtime-seconds 5 150 -1 10))
  (check-input-error (tank-runtime-seconds 5 150 110 0))
  (check-input-error (compressor-recovery-seconds 2 110 150 1.0 #:cfm-at-zero-psi 2.0))
  (check-input-error (compressor-recovery-seconds 2 110 150 1.0 #:cfm-at-zero-psi 0.5 #:rated-psi 150))
  (check-input-error (compressor-recovery-seconds 2 110 150 0.1 #:cfm-at-zero-psi 5.0 #:rated-psi 100))
  (check-input-error (blast-count 10 0))
  (check-input-error (blast-count -1 1)))

;; ---------------------------------------------------------------- electrical
(test-case "resistance matches published solid-copper tables within 1 %"
  (for ([pair (in-list '(("14" 2.525) ("12" 1.588) ("10" 0.9989) ("8" 0.6282)
                         ("4" 0.2485) ("1/0" 0.09827)))])
    (define ohms (cadr pair))
    (check-= (awg-resistance-ohm/1000ft (car pair)) ohms (* ohms 0.01))))

(test-case "three gauges double the area"
  (check-= (/ (awg-area-mm2 7) (awg-area-mm2 10)) 2.0 0.01))

(test-case "AWG label forms"
  (check-= (awg-diameter-mm "1/0") (awg-diameter-mm "0") 0.0)
  (check-= (awg-diameter-mm "4/0") (awg-diameter-mm "0000") 0.0)
  (check-= (awg-diameter-mm 10) (awg-diameter-mm "10 AWG") 0.0)
  (check-= (awg-diameter-mm "4/0") 11.684 0.005))

(test-case "temperature raises resistance"
  (define r20 (awg-resistance-ohm/1000ft 10))
  (check-= (awg-resistance-ohm/1000ft 10 70) (* r20 (+ 1 (* 0.00393 50))) tol))

(test-case "voltage drop known value"
  (define drop (voltage-drop 20 10 10))
  (check-= drop (* 20 (awg-resistance-ohm/1000ft 10) 0.02) tol)
  (check-= drop 0.3995 0.0005))

(test-case "voltage drop in metres"
  (check-= (voltage-drop 20 3.048 10 #:unit 'm) (voltage-drop 20 10 10) tol))

(test-case "suggest-awg known values"
  (check-equal? (wire-suggestion-awg (suggest-awg 30 10)) "6")
  (check-equal? (wire-suggestion-awg (suggest-awg 30 10 #:system-voltage 24)) "10")
  (check-equal? (wire-suggestion-awg (suggest-awg 5 3)) "18")
  (check-equal? (wire-suggestion-awg (suggest-awg 30 10 #:system-voltage 24 #:max-drop-pct 2)) "8"))

(test-case "suggestion is minimal and within budget"
  (for ([c (in-list '((30 10 12) (15 20 12) (40 6 24) (100 15 12)))])
    (define-values (amps len volts) (apply values c))
    (define s (suggest-awg amps len #:system-voltage volts))
    (check <= (wire-suggestion-voltage-drop-pct s) 3.0)
    (define idx (index-of awg-sizes (wire-suggestion-awg s)))
    (when (> idx 0)
      (define thinner (voltage-drop amps len (list-ref awg-sizes (sub1 idx))))
      (check > (* (/ thinner volts) 100) 3.0))))

(test-case "suggestion fields"
  (define s (suggest-awg 30 10))
  (check-equal? (wire-suggestion-unit s) 'ft)
  (check-= (wire-suggestion-allowed-drop-pct s) 3.0 tol))

(test-case "impossible load raises"
  (check-input-error (suggest-awg 1000 200)))

(test-case "invalid electrical inputs"
  (for ([bad (in-list '("5/0" "abc" -1 41))])
    (check-input-error (awg-diameter-mm bad)))
  (check-input-error (awg-diameter-mm #t))
  (check-input-error (suggest-awg 0 10))
  (check-input-error (suggest-awg 10 10 #:max-drop-pct 0))
  (check-input-error (suggest-awg 10 10 #:max-drop-pct 60))
  (check-input-error (suggest-awg 10 10 #:system-voltage -12))
  (check-input-error (awg-resistance-ohm/1000ft 10 500)))
