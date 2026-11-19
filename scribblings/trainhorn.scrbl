#lang scribble/manual
@(require (for-label racket/base racket/contract/base trainhorn)
          scribble/example)

@(define ev (make-base-eval))
@(ev '(require trainhorn))

@title{trainhorn: Train Horn Calculators}
@author{Train Horn}

@defmodule[trainhorn]

Small, dependency-free calculators for vehicle and train horn setups:
how loud a horn is at a given distance, how long an onboard air tank lasts,
how long a compressor needs to refill it, and which copper wire gauge keeps
the voltage drop in budget. It is a faithful port of the tested
@hyperlink["https://github.com/train-horn/trainhorn-calc"]{trainhorn-calc}
core, with the same formulas and test vectors.
Train Horn also hosts
@hyperlink["https://train-horn.com/tools/"]{interactive versions of these calculators}.

All results are estimates from textbook formulas. They are not
measurements, and nothing here is a legal noise assessment.

Every export has a contract. Arguments of the wrong type raise a contract
error; values of the right type but out of range (a zero distance, a start
pressure below the cut-off, an unknown wire gauge) raise
@racket[exn:fail:trainhorn], a subtype of @racket[exn:fail:contract], so
@racket[exn:fail:contract?] catches both.

@defstruct*[(exn:fail:trainhorn exn:fail:contract) ()]{
Raised for out-of-range input. The message names the function and argument.}

@defthing[finite-real/c flat-contract?]{Accepts real numbers that are not NaN or infinite.}
@defthing[length-unit/c flat-contract?]{Either @racket['ft] or @racket['m].}
@defthing[awg/c flat-contract?]{An exact integer (1 to 40, or 0 for 1/0) or a label such as @racket["10"], @racket["10 AWG"], @racket["1/0"] or @racket["0000"].}

@section{Sound level versus distance}

A horn is treated as a point source in a free field, so the level falls by
@tt{20 log10(d2/d1)} dB, plus optional air absorption over the extra path:

@centered{@tt{L(d) = L_ref - 20 log10(d / d_ref) - alpha (d - d_ref) / 1000}}

with distances in metres for the absorption term. Ground reflection,
barriers, wind and temperature gradients are ignored, and the model does
not hold within a horn length or two of the source.

@defproc[(spl-at-distance [source-db finite-real/c]
                          [distance finite-real/c]
                          [#:ref-distance ref-distance finite-real/c 10]
                          [#:unit unit length-unit/c 'ft]
                          [#:absorption-db/km absorption finite-real/c 0])
         flonum?]{
Level at @racket[distance] from a level @racket[source-db] known at
@racket[ref-distance]. Both distances must be positive and use @racket[unit].

@examples[#:eval ev
(spl-at-distance 150 100)
(spl-at-distance 120 10 #:ref-distance 1 #:unit 'm)
(spl-at-distance 150 1000 #:unit 'm #:absorption-db/km 1.9)]}

@defproc[(distance-for-spl [source-db finite-real/c]
                           [target-db finite-real/c]
                           [#:ref-distance ref-distance finite-real/c 10]
                           [#:unit unit length-unit/c 'ft]
                           [#:absorption-db/km absorption finite-real/c 0])
         flonum?]{
Inverse of @racket[spl-at-distance]: the distance (in @racket[unit]) at which
the level falls to @racket[target-db]. Closed form without absorption,
bisection with it.

@examples[#:eval ev
(distance-for-spl 150 130)
(distance-for-spl 150 90 #:absorption-db/km 1.9)]}

@defproc[(distance-ratio-loss [ratio finite-real/c]) flonum?]{
Level change @tt{20 log10(ratio)} when the distance is multiplied by @racket[ratio] (> 0).}

@defproc[(db-doubling-loss) flonum?]{
Loss per doubling of distance, about 6.02 dB.
@examples[#:eval ev (db-doubling-loss)]}

@defproc[(combine-levels [levels (listof finite-real/c)]) flonum?]{
Combined level of incoherent sources: @tt{10 log10(sum 10^(L_i/10))}. Two
equal horns add about 3 dB. The list must not be empty.
@examples[#:eval ev (combine-levels '(100 100))]}

@defthing[air-absorption-db/km (hash/c exact-positive-integer? real?)]{
Rounded atmospheric absorption in dB/km at 10 °C and 70 % relative humidity
(an ISO 9613-2 table condition), keyed by octave-band centre frequency in Hz
(63 to 8000). A rough guide only.}

@section{Air tank runtime and compressor recovery}

By Boyle's law, a tank of @tt{V} gallons dropping from gauge pressure
@tt{P_start} to @tt{P_end} releases
@tt{V_free = V × 0.13368 × (P_start - P_end) / P_atm} cubic feet of free air.
Horn consumption and compressor output are rated in CFM of free air. The
expansion is taken as isothermal, so treat the runtime as an upper estimate.

@defthing[atmospheric-psi real?]{Sea-level atmosphere, 14.696 psi.}
@defthing[cubic-feet-per-gallon real?]{231/1728 cubic feet per US gallon.}

@defproc[(free-air-volume-cuft [tank-gallons finite-real/c]
                               [start-psi finite-real/c]
                               [end-psi finite-real/c]
                               [#:atmospheric-psi patm finite-real/c atmospheric-psi])
         flonum?]{
Free air released between two gauge pressures. @racket[start-psi] must be
greater than @racket[end-psi] (>= 0).}

@defproc[(tank-runtime-seconds [tank-gallons finite-real/c]
                               [start-psi finite-real/c]
                               [cutoff-psi finite-real/c]
                               [horn-cfm finite-real/c]
                               [#:atmospheric-psi patm finite-real/c atmospheric-psi])
         flonum?]{
Seconds of continuous horn time from @racket[start-psi] down to @racket[cutoff-psi].
@examples[#:eval ev (tank-runtime-seconds 5 150 110 10)]}

@defproc[(blast-count [runtime-seconds finite-real/c] [blast-seconds finite-real/c])
         exact-nonnegative-integer?]{
Whole blasts of @racket[blast-seconds] that fit in @racket[runtime-seconds].
@examples[#:eval ev (blast-count (tank-runtime-seconds 5 150 110 10) 2)]}

@defproc[(compressor-recovery-seconds [tank-gallons finite-real/c]
                                      [cut-in-psi finite-real/c]
                                      [cut-out-psi finite-real/c]
                                      [compressor-cfm finite-real/c]
                                      [#:cfm-at-zero-psi q0 (or/c #f finite-real/c) #f]
                                      [#:rated-psi rated (or/c #f finite-real/c) #f]
                                      [#:atmospheric-psi patm finite-real/c atmospheric-psi])
         flonum?]{
Seconds to refill from @racket[cut-in-psi] to @racket[cut-out-psi]. By default
the compressor delivers @racket[compressor-cfm] over the whole band; use the
CFM rated at your cut-out pressure. If both @racket[q0] and @racket[rated]
are given, output falls linearly from @racket[q0] at 0 psi to
@racket[compressor-cfm] at @racket[rated] psi, and the time is the closed-form
integral @tt{V/P_atm × ln(q_out/q_in) / slope}.

@examples[#:eval ev
(compressor-recovery-seconds 2 110 150 1.0)
(compressor-recovery-seconds 2 110 150 1.0 #:cfm-at-zero-psi 2.0 #:rated-psi 150)]}

@section{Wire gauge by voltage drop}

Resistances are computed from the AWG definition (ASTM B258 diameter,
annealed copper at 100 % IACS, @tt{rho = 1.7241e-8} ohm·m) and agree with
published solid-copper tables to about 1 %. The round trip is twice the
one-way run. This sizes for voltage drop only: it does not check ampacity
or fusing, so always fuse to protect the wire you choose.

@defthing[awg-sizes (listof string?)]{Gauges tried by @racket[suggest-awg], thinnest first: 18 to 4/0.}
@defthing[copper-resistivity-ohm-m real?]{1.7241e-8.}
@defthing[copper-temp-coeff-per-c real?]{0.00393 per °C.}

@defproc[(awg-diameter-mm [awg awg/c]) flonum?]{Bare conductor diameter in mm.}
@defproc[(awg-area-mm2 [awg awg/c]) flonum?]{Conductor cross-section in mm².}
@defproc[(awg-resistance-ohm/1000ft [awg awg/c] [temperature-c finite-real/c 20]) flonum?]{
Copper resistance in ohms per 1000 ft; @racket[temperature-c] must be between -60 and 200.
@examples[#:eval ev (awg-resistance-ohm/1000ft "10")]}

@defproc[(voltage-drop [amps finite-real/c] [one-way-length finite-real/c] [awg awg/c]
                       [#:unit unit length-unit/c 'ft]
                       [#:temperature-c temperature-c finite-real/c 20])
         flonum?]{
Volts lost over the out-and-back run: @tt{I × R × 2 × length}.
@examples[#:eval ev (voltage-drop 20 10 10)]}

@defstruct*[wire-suggestion ([awg string?]
                             [voltage-drop-v flonum?]
                             [voltage-drop-pct flonum?]
                             [allowed-drop-pct flonum?]
                             [system-voltage flonum?]
                             [amps flonum?]
                             [one-way-length flonum?]
                             [unit length-unit/c])
            #:transparent]{Result of @racket[suggest-awg].}

@defproc[(suggest-awg [amps finite-real/c] [one-way-length finite-real/c]
                      [#:system-voltage system-voltage finite-real/c 12]
                      [#:max-drop-pct max-drop-pct finite-real/c 3]
                      [#:unit unit length-unit/c 'ft]
                      [#:temperature-c temperature-c finite-real/c 20])
         wire-suggestion?]{
Smallest gauge in @racket[awg-sizes] whose drop stays within
@racket[max-drop-pct] (0 < x <= 50) of @racket[system-voltage]. Raises
@racket[exn:fail:trainhorn] if even 4/0 is not enough.

@examples[#:eval ev
(suggest-awg 30 10)
(wire-suggestion-awg (suggest-awg 30 10 #:system-voltage 24))]}
