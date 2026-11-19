#lang racket/base
;; trainhorn: train horn calculators (sound level vs distance, air tank
;; runtime, compressor recovery, wire gauge by voltage drop).
;;
;; A port of the tested trainhorn-calc core
;; (https://github.com/train-horn/trainhorn-calc). Results are estimates
;; from textbook formulas, not measurements and not legal advice.

(require racket/contract/base
         racket/math
         racket/string)

(define finite-real/c
  (flat-named-contract 'finite-real?
                       (lambda (x) (and (real? x) (not (nan? x)) (not (infinite? x))))))
(define length-unit/c (or/c 'ft 'm))
(define awg/c (or/c exact-integer? string?))

(provide
 finite-real/c length-unit/c awg/c
 (struct-out exn:fail:trainhorn)
 (struct-out wire-suggestion)
 (contract-out
  ;; constants
  [air-absorption-db/km (hash/c exact-positive-integer? (and/c real? (>=/c 0)) #:immutable #t)]
  [atmospheric-psi real?]
  [cubic-feet-per-gallon real?]
  [copper-resistivity-ohm-m real?]
  [copper-temp-coeff-per-c real?]
  [awg-sizes (listof string?)]
  ;; acoustics
  [spl-at-distance (->* (finite-real/c finite-real/c)
                        (#:ref-distance finite-real/c
                         #:unit length-unit/c
                         #:absorption-db/km finite-real/c)
                        flonum?)]
  [distance-for-spl (->* (finite-real/c finite-real/c)
                         (#:ref-distance finite-real/c
                          #:unit length-unit/c
                          #:absorption-db/km finite-real/c)
                         flonum?)]
  [distance-ratio-loss (-> finite-real/c flonum?)]
  [db-doubling-loss (-> flonum?)]
  [combine-levels (-> (listof finite-real/c) flonum?)]
  ;; air system
  [free-air-volume-cuft (->* (finite-real/c finite-real/c finite-real/c)
                             (#:atmospheric-psi finite-real/c)
                             flonum?)]
  [tank-runtime-seconds (->* (finite-real/c finite-real/c finite-real/c finite-real/c)
                             (#:atmospheric-psi finite-real/c)
                             flonum?)]
  [blast-count (-> finite-real/c finite-real/c exact-nonnegative-integer?)]
  [compressor-recovery-seconds
   (->* (finite-real/c finite-real/c finite-real/c finite-real/c)
        (#:cfm-at-zero-psi (or/c #f finite-real/c)
         #:rated-psi (or/c #f finite-real/c)
         #:atmospheric-psi finite-real/c)
        flonum?)]
  ;; electrical
  [awg-diameter-mm (-> awg/c flonum?)]
  [awg-area-mm2 (-> awg/c flonum?)]
  [awg-resistance-ohm/1000ft (->* (awg/c) (finite-real/c) flonum?)]
  [voltage-drop (->* (finite-real/c finite-real/c awg/c)
                     (#:unit length-unit/c #:temperature-c finite-real/c)
                     flonum?)]
  [suggest-awg (->* (finite-real/c finite-real/c)
                    (#:system-voltage finite-real/c
                     #:max-drop-pct finite-real/c
                     #:unit length-unit/c
                     #:temperature-c finite-real/c)
                    wire-suggestion?)]))

;; ---------------------------------------------------------------------------
;; Errors and validation

;; Raised for inputs that pass the type contracts but are out of range
;; (zero distance, start pressure below cut-off, ...). It is a subtype of
;; exn:fail:contract, so (exn:fail:contract? e) catches every input error.
(struct exn:fail:trainhorn exn:fail:contract () #:transparent)

(define (fail fmt . args)
  (raise (exn:fail:trainhorn (apply format fmt args) (current-continuation-marks))))

(define (positive who name v)
  (unless (> v 0) (fail "~a: ~a must be greater than 0, got ~a" who name v))
  (exact->inexact v))

(define (non-negative who name v)
  (unless (>= v 0) (fail "~a: ~a must be 0 or greater, got ~a" who name v))
  (exact->inexact v))

(define meters-per-foot 0.3048)
(define (to-meters v unit) (if (eq? unit 'ft) (* v meters-per-foot) v))

(define (log10 x) (/ (log x) (log 10.0)))

;; ---------------------------------------------------------------------------
;; Acoustics: free-field point source, inverse-square law

;; Approximate atmospheric absorption in dB/km at 10 degC, 70 % RH
;; (ISO 9613-1 / 9613-2 condition), keyed by octave band centre in Hz.
(define air-absorption-db/km
  (hash 63 0.1 125 0.4 250 1.0 500 1.9 1000 3.7 2000 9.7 4000 32.8 8000 117.0))

(define (spl-at-distance source-db distance
                         #:ref-distance [ref-distance 10]
                         #:unit [unit 'ft]
                         #:absorption-db/km [absorption 0])
  (define who 'spl-at-distance)
  (define src (exact->inexact source-db))
  (define d (positive who "distance" distance))
  (define dref (positive who "ref-distance" ref-distance))
  (define alpha (non-negative who "absorption-db/km" absorption))
  (define level (- src (* 20.0 (log10 (/ d dref)))))
  (if (zero? alpha)
      level
      (- level (/ (* alpha (- (to-meters d unit) (to-meters dref unit))) 1000.0))))

(define (distance-for-spl source-db target-db
                          #:ref-distance [ref-distance 10]
                          #:unit [unit 'ft]
                          #:absorption-db/km [absorption 0])
  (define who 'distance-for-spl)
  (define src (exact->inexact source-db))
  (define target (exact->inexact target-db))
  (define dref (positive who "ref-distance" ref-distance))
  (define alpha (non-negative who "absorption-db/km" absorption))
  (define free-field (* dref (expt 10.0 (/ (- src target) 20.0))))
  (when (or (zero? free-field) (infinite? free-field))
    (fail "~a: source-db and target-db are too far apart to give a usable distance" who))
  (cond
    [(or (zero? alpha) (= free-field dref)) free-field]
    [else
     ;; The level is strictly decreasing in distance, and the root lies
     ;; between the free-field answer and the reference distance.
     (define (excess d)
       (- (spl-at-distance src d #:ref-distance dref #:unit unit #:absorption-db/km alpha)
          target))
     (let loop ([lo (min free-field dref)] [hi (max free-field dref)] [i 0])
       (define mid (* 0.5 (+ lo hi)))
       (define-values (lo* hi*) (if (> (excess mid) 0) (values mid hi) (values lo mid)))
       (if (or (>= i 199) (<= (- hi* lo*) (* 1e-12 hi*)))
           (* 0.5 (+ lo* hi*))
           (loop lo* hi* (add1 i))))]))

(define (distance-ratio-loss ratio)
  (* 20.0 (log10 (positive 'distance-ratio-loss "ratio" ratio))))

(define (db-doubling-loss) (distance-ratio-loss 2))

(define (combine-levels levels)
  (when (null? levels) (fail "combine-levels: needs at least one level"))
  (define vals (map exact->inexact levels))
  (define peak (apply max vals))
  ;; Factor out the peak so very large dB values do not overflow.
  (+ peak (* 10.0 (log10 (for/sum ([v (in-list vals)]) (expt 10.0 (/ (- v peak) 10.0)))))))

;; ---------------------------------------------------------------------------
;; Air system: Boyle's law, free air in CFM

(define atmospheric-psi 14.696)
(define cubic-feet-per-gallon (/ 231.0 1728.0))

(define (pressures who high-name high low-name low)
  (define h (non-negative who high-name high))
  (define l (non-negative who low-name low))
  (unless (> h l)
    (fail "~a: ~a (~a psi) must be greater than ~a (~a psi)" who high-name h low-name l))
  (values h l))

(define (free-air who tank-gallons high-name high low-name low patm)
  (define v (positive who "tank-gallons" tank-gallons))
  (define-values (h l) (pressures who high-name high low-name low))
  (define p (positive who "atmospheric-psi" patm))
  (/ (* v cubic-feet-per-gallon (- h l)) p))

(define (free-air-volume-cuft tank-gallons start-psi end-psi
                              #:atmospheric-psi [patm atmospheric-psi])
  (free-air 'free-air-volume-cuft tank-gallons "start-psi" start-psi "end-psi" end-psi patm))

(define (tank-runtime-seconds tank-gallons start-psi cutoff-psi horn-cfm
                              #:atmospheric-psi [patm atmospheric-psi])
  (define who 'tank-runtime-seconds)
  (define cfm (positive who "horn-cfm" horn-cfm))
  (* (/ (free-air who tank-gallons "start-psi" start-psi "cutoff-psi" cutoff-psi patm) cfm) 60.0))

(define (blast-count runtime-seconds blast-seconds)
  (define rt (non-negative 'blast-count "runtime-seconds" runtime-seconds))
  (define bs (positive 'blast-count "blast-seconds" blast-seconds))
  ;; Small epsilon so exact divisions such as 10.0 / 2.0 are not lost to rounding.
  (inexact->exact (floor (+ (/ rt bs) 1e-9))))

(define (compressor-recovery-seconds tank-gallons cut-in-psi cut-out-psi compressor-cfm
                                     #:cfm-at-zero-psi [q0-in #f]
                                     #:rated-psi [rated-in #f]
                                     #:atmospheric-psi [patm atmospheric-psi])
  (define who 'compressor-recovery-seconds)
  (define cfm (positive who "compressor-cfm" compressor-cfm))
  (define vfree (free-air who tank-gallons "cut-out-psi" cut-out-psi "cut-in-psi" cut-in-psi patm))
  (cond
    [(and (not q0-in) (not rated-in)) (* (/ vfree cfm) 60.0)]
    [(or (not q0-in) (not rated-in))
     (fail "~a: #:cfm-at-zero-psi and #:rated-psi must be given together" who)]
    [else
     (define q0 (positive who "cfm-at-zero-psi" q0-in))
     (define p-rated (positive who "rated-psi" rated-in))
     (when (< q0 cfm)
       (fail "~a: cfm-at-zero-psi must be at least compressor-cfm (output falls with pressure)" who))
     (define slope (/ (- cfm q0) p-rated)) ; cfm per psi, <= 0
     (define q-in (+ q0 (* slope cut-in-psi)))
     (define q-out (+ q0 (* slope cut-out-psi)))
     (when (<= q-out 0)
       (fail "~a: the linear flow model gives no output at ~a psi" who cut-out-psi))
     (if (zero? slope)
         (* (/ vfree q0) 60.0)
         (* (/ (* tank-gallons cubic-feet-per-gallon) (exact->inexact patm))
            (/ (log (/ q-out q-in)) slope)
            60.0))]))

;; ---------------------------------------------------------------------------
;; Electrical: AWG from the ASTM B258 formula, copper at 100 % IACS

(define copper-resistivity-ohm-m 1.7241e-8)
(define copper-temp-coeff-per-c 0.00393)

(define awg-sizes
  '("18" "16" "14" "12" "10" "8" "6" "4" "2" "1" "1/0" "2/0" "3/0" "4/0"))

(define aught-sizes
  (hash "1/0" 0 "0" 0 "2/0" -1 "00" -1 "3/0" -2 "000" -2 "4/0" -3 "0000" -3))

;; AWG label -> formula index (1/0 -> 0, 4/0 -> -3).
(define (awg-number awg)
  (define n
    (cond
      [(exact-integer? awg) awg]
      [else
       (define text (string-trim (string-replace (string-upcase awg) "AWG" "")))
       (or (hash-ref aught-sizes text #f)
           (let ([n (and (regexp-match? #px"^[0-9]+$" text) (string->number text))])
             (or n (fail "unknown AWG size ~s; use 1-40 or 1/0..4/0" awg))))]))
  (cond
    [(and (or (string? awg) (eqv? awg 0)) (<= -3 n 0)) n]
    [(<= 1 n 40) n]
    [else (fail "AWG size must be between 4/0 and 40, got ~s" awg)]))

(define (awg-diameter-mm awg)
  (* 0.127 (expt 92.0 (/ (- 36 (awg-number awg)) 39.0))))

(define (awg-area-mm2 awg)
  (define d (awg-diameter-mm awg))
  (/ (* pi d d) 4.0))

(define (awg-resistance-ohm/1000ft awg [temperature-c 20])
  (unless (<= -60 temperature-c 200)
    (fail "awg-resistance-ohm/1000ft: temperature-c must be between -60 and 200 degC"))
  (define area-m2 (* (awg-area-mm2 awg) 1e-6))
  (define r20 (/ (* copper-resistivity-ohm-m 304.8) area-m2))
  (* r20 (+ 1.0 (* copper-temp-coeff-per-c (- temperature-c 20.0)))))

(define (voltage-drop amps one-way-length awg #:unit [unit 'ft] #:temperature-c [temp 20])
  (define i (positive 'voltage-drop "amps" amps))
  (define len (positive 'voltage-drop "one-way-length" one-way-length))
  (define feet (/ (to-meters len unit) meters-per-foot))
  (* i (/ (* (awg-resistance-ohm/1000ft awg temp) 2.0 feet) 1000.0)))

(struct wire-suggestion
  (awg voltage-drop-v voltage-drop-pct allowed-drop-pct system-voltage amps one-way-length unit)
  #:transparent)

(define (suggest-awg amps one-way-length
                     #:system-voltage [system-voltage 12]
                     #:max-drop-pct [max-drop-pct 3]
                     #:unit [unit 'ft]
                     #:temperature-c [temp 20])
  (define who 'suggest-awg)
  (define i (positive who "amps" amps))
  (define len (positive who "one-way-length" one-way-length))
  (define volts (positive who "system-voltage" system-voltage))
  (define pct (positive who "max-drop-pct" max-drop-pct))
  (when (> pct 50) (fail "~a: max-drop-pct must be 50 or less" who))
  (define allowed (/ (* volts pct) 100.0))
  (or (for/first ([g (in-list awg-sizes)]
                  #:when (<= (voltage-drop i len g #:unit unit #:temperature-c temp) allowed))
        (define drop (voltage-drop i len g #:unit unit #:temperature-c temp))
        (wire-suggestion g drop (* (/ drop volts) 100.0) pct volts i len unit))
      (fail "~a: no gauge up to 4/0 keeps the drop within ~a% (~a A over ~a ~a)"
            who pct i len unit)))
