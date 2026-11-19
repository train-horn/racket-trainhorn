#lang info
(define collection "trainhorn")
(define pkg-desc "Train horn calculators: SPL vs distance (inverse-square, air absorption), combined levels, Boyle's-law air tank runtime, compressor recovery, and copper AWG by voltage drop.")
(define version "1.0")
(define pkg-authors '("Train Horn (train-horn.com)"))
(define license 'MIT)
(define deps '("base"))
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/trainhorn.scrbl" () (library) "trainhorn")))
