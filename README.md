# trainhorn (Racket)

Train horn calculators for Racket: sound level versus distance, air tank
runtime, compressor recovery and copper wire gauge by voltage drop.

A faithful port of the tested [trainhorn-calc](https://github.com/train-horn/trainhorn-calc)
core (Python), with the same formulas and the same test vectors (within 1e-9).
Interactive versions of these calculators are on
[train-horn.com/tools](https://train-horn.com/tools/).

## Install

```sh
raco pkg install trainhorn
# or from source
raco pkg install --link --name trainhorn .
```

## Use

```racket
#lang racket/base
(require trainhorn)

(spl-at-distance 150 100)                      ; 150 dB at 10 ft -> 130.0 dB at 100 ft
(distance-for-spl 150 90 #:absorption-db/km 1.9) ; with air absorption
(db-doubling-loss)                             ; 6.0206 dB per doubling
(combine-levels '(100 100))                    ; 103.01 dB

(tank-runtime-seconds 5 150 110 10)            ; 5 gal, 150 -> 110 psi, 10 CFM horn: 10.92 s
(blast-count (tank-runtime-seconds 5 150 110 10) 2) ; 5 two-second blasts
(compressor-recovery-seconds 2 110 150 1.0)    ; 43.66 s
(compressor-recovery-seconds 2 110 150 1.0 #:cfm-at-zero-psi 2.0 #:rated-psi 150)

(voltage-drop 20 10 10)                        ; 20 A, 10 ft one way, 10 AWG: 0.3995 V
(wire-suggestion-awg (suggest-awg 30 10))      ; "6" at 12 V, 3 % budget
```

| Area | Functions |
| --- | --- |
| Acoustics | `spl-at-distance`, `distance-for-spl`, `distance-ratio-loss`, `db-doubling-loss`, `combine-levels`, `air-absorption-db/km` |
| Air system | `free-air-volume-cuft`, `tank-runtime-seconds`, `blast-count`, `compressor-recovery-seconds` |
| Wiring | `awg-diameter-mm`, `awg-area-mm2`, `awg-resistance-ohm/1000ft`, `voltage-drop`, `suggest-awg` |

Every export has a contract. Wrong types raise a contract error; out-of-range
values raise `exn:fail:trainhorn` (a subtype of `exn:fail:contract`).

## Models and limits

- **Sound:** free-field point source (inverse-square law) plus optional air
  absorption. Ground, barriers and weather are ignored. Estimates only, not
  measurements and not a legal noise assessment.
- **Air:** Boyle's law, isothermal; horn and compressor ratings in CFM of free
  air. Treat runtimes as an upper estimate.
- **Wire:** AWG resistance from ASTM B258 diameters and 100 % IACS copper
  (within about 1 % of published tables). Voltage drop only: check ampacity
  and fuse the wire.

## Develop

```sh
raco test tests            # rackunit suite
raco scribble --html --dest doc scribblings/trainhorn.scrbl
```

## License

MIT. See [LICENSE](LICENSE).
