
"Copyright (c) 2015, Mark Tarver

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of Mark Tarver may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY Mark Tarver ''AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Mark Tarver BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."

(DEFUN shen.typecheck (V3698 V3699)
  (LET ((Curry (shen.curry V3698)))
    (LET ((ProcessN (shen.start-new-prolog-process)))
      (LET ((Type
             (shen.insert-prolog-variables
              (shen.demodulate (shen.curry-type V3699)) ProcessN)))
        (LET ((Continuation (freeze (return Type ProcessN 'shen.void))))
          (shen.t* (CONS Curry (CONS '|:| (CONS Type NIL))) NIL ProcessN
           Continuation))))))

(DEFUN shen.curry (V3701)
  (COND
   ((AND (CONSP V3701) (shen-cl.true? (shen.special? (CAR V3701))))
    (CONS (CAR V3701) (map #'(LAMBDA (Y) (shen.curry Y)) (CDR V3701))))
   ((AND (CONSP V3701)
         (AND (CONSP (CDR V3701))
              (shen-cl.true? (shen.extraspecial? (CAR V3701)))))
    V3701)
   ((AND (CONSP V3701)
         (AND (EQ 'type (CAR V3701))
              (AND (CONSP (CDR V3701))
                   (AND (CONSP (CDR (CDR V3701)))
                        (NULL (CDR (CDR (CDR V3701))))))))
    (CONS 'type (CONS (shen.curry (CAR (CDR V3701))) (CDR (CDR V3701)))))
   ((AND (CONSP V3701) (AND (CONSP (CDR V3701)) (CONSP (CDR (CDR V3701)))))
    (shen.curry
     (CONS (CONS (CAR V3701) (CONS (CAR (CDR V3701)) NIL)) (CDR (CDR V3701)))))
   ((AND (CONSP V3701) (AND (CONSP (CDR V3701)) (NULL (CDR (CDR V3701)))))
    (CONS (shen.curry (CAR V3701)) (CONS (shen.curry (CAR (CDR V3701))) NIL)))
   (T V3701)))

(DEFUN shen.special? (V3703) (element? V3703 shen.*special*))

(DEFUN shen.extraspecial? (V3705) (element? V3705 shen.*extraspecial*))

(DEFUN shen.t* (V3710 V3711 V3712 V3713)
  (LET ((Throwcontrol (shen.catchpoint)))
    (shen.cutpoint Throwcontrol
                   (LET ((Case
                          (LET ((Error (shen.newpv V3712)))
                            (do (shen.incinfs)
                                (fwhen (shen.maxinfexceeded?) V3712
                                       (freeze
                                        (bind Error (shen.errormaxinfs) V3712
                                              V3713)))))))
                     (IF (EQ Case 'false)
                         (LET ((Case
                                (LET ((V3690 (shen.lazyderef V3710 V3712)))
                                  (IF (EQ 'fail V3690)
                                      (do (shen.incinfs)
                                          (cut Throwcontrol V3712
                                               (freeze
                                                (shen.prolog-failure V3712
                                                 V3713))))
                                      'false))))
                           (IF (EQ Case 'false)
                               (LET ((Case
                                      (LET ((V3691
                                             (shen.lazyderef V3710 V3712)))
                                        (IF (CONSP V3691)
                                            (LET ((X (CAR V3691)))
                                              (LET ((V3692
                                                     (shen.lazyderef
                                                      (CDR V3691) V3712)))
                                                (IF (CONSP V3692)
                                                    (LET ((V3693
                                                           (shen.lazyderef
                                                            (CAR V3692) V3712)))
                                                      (IF (EQ '|:| V3693)
                                                          (LET ((V3694
                                                                 (shen.lazyderef
                                                                  (CDR V3692)
                                                                  V3712)))
                                                            (IF (CONSP V3694)
                                                                (LET ((A
                                                                       (CAR
                                                                        V3694)))
                                                                  (LET ((V3695
                                                                         (shen.lazyderef
                                                                          (CDR
                                                                           V3694)
                                                                          V3712)))
                                                                    (IF (NULL
                                                                         V3695)
                                                                        (do
                                                                         (shen.incinfs)
                                                                         (fwhen
                                                                          (shen.type-theory-enabled?)
                                                                          V3712
                                                                          (freeze
                                                                           (cut
                                                                            Throwcontrol
                                                                            V3712
                                                                            (freeze
                                                                             (shen.th*
                                                                              X
                                                                              A
                                                                              V3711
                                                                              V3712
                                                                              V3713))))))
                                                                        'false)))
                                                                'false))
                                                          'false))
                                                    'false)))
                                            'false))))
                                 (IF (EQ Case 'false)
                                     (LET ((Datatypes (shen.newpv V3712)))
                                       (do (shen.incinfs)
                                           (shen.show V3710 V3711 V3712
                                            (freeze
                                             (bind Datatypes shen.*datatypes*
                                                   V3712
                                                   (freeze
                                                    (shen.udefs* V3710 V3711
                                                     Datatypes V3712
                                                     V3713)))))))
                                     Case))
                               Case))
                         Case)))))

(DEFUN shen.type-theory-enabled? () shen.*shen-type-theory-enabled?*)

(DEFUN enable-type-theory (V3719)
  (COND ((EQ '+ V3719) (set 'shen.*shen-type-theory-enabled?* 'true))
        ((EQ '- V3719) (set 'shen.*shen-type-theory-enabled?* 'false))
        (T
         (simple-error "enable-type-theory expects a + or a -
"))))

(DEFUN shen.prolog-failure (V3730 V3731) 'false)

(DEFUN shen.maxinfexceeded? ()
  (shen.greater? (inferences) shen.*maxinferences*))

(DEFUN shen.errormaxinfs () (simple-error "maximum inferences exceeded~%"))

(DEFUN shen.udefs* (V3737 V3738 V3739 V3740 V3741)
  (LET ((Case
         (LET ((V3686 (shen.lazyderef V3739 V3740)))
           (IF (CONSP V3686)
               (LET ((D (CAR V3686)))
                 (do (shen.incinfs)
                     (call (CONS D (CONS V3737 (CONS V3738 NIL))) V3740
                           V3741)))
               'false))))
    (IF (EQ Case 'false)
        (LET ((V3687 (shen.lazyderef V3739 V3740)))
          (IF (CONSP V3687)
              (LET ((Ds (CDR V3687)))
                (do (shen.incinfs) (shen.udefs* V3737 V3738 Ds V3740 V3741)))
              'false))
        Case)))

(DEFUN shen.th* (V3747 V3748 V3749 V3750 V3751)
  (LET ((Throwcontrol (shen.catchpoint)))
    (shen.cutpoint Throwcontrol
                   (LET ((Case
                          (do (shen.incinfs)
                              (shen.show
                               (CONS V3747 (CONS '|:| (CONS V3748 NIL))) V3749
                               V3750 (freeze (fwhen 'false V3750 V3751))))))
                     (IF (EQ Case 'false)
                         (LET ((Case
                                (LET ((F (shen.newpv V3750)))
                                  (do (shen.incinfs)
                                      (fwhen
                                       (shen.typedf?
                                        (shen.lazyderef V3747 V3750))
                                       V3750
                                       (freeze
                                        (bind F
                                              (shen.sigf
                                               (shen.lazyderef V3747 V3750))
                                              V3750
                                              (freeze
                                               (call (CONS F (CONS V3748 NIL))
                                                     V3750 V3751)))))))))
                           (IF (EQ Case 'false)
                               (LET ((Case
                                      (do (shen.incinfs)
                                          (shen.base V3747 V3748 V3750 V3751))))
                                 (IF (EQ Case 'false)
                                     (LET ((Case
                                            (do (shen.incinfs)
                                                (shen.by_hypothesis V3747 V3748
                                                 V3749 V3750 V3751))))
                                       (IF (EQ Case 'false)
                                           (LET ((Case
                                                  (LET ((V3582
                                                         (shen.lazyderef V3747
                                                                         V3750)))
                                                    (IF (CONSP V3582)
                                                        (LET ((F (CAR V3582)))
                                                          (LET ((V3583
                                                                 (shen.lazyderef
                                                                  (CDR V3582)
                                                                  V3750)))
                                                            (IF (NULL V3583)
                                                                (do
                                                                 (shen.incinfs)
                                                                 (shen.th* F
                                                                  (CONS '-->
                                                                        (CONS
                                                                         V3748
                                                                         NIL))
                                                                  V3749 V3750
                                                                  V3751))
                                                                'false)))
                                                        'false))))
                                             (IF (EQ Case 'false)
                                                 (LET ((Case
                                                        (LET ((V3584
                                                               (shen.lazyderef
                                                                V3747 V3750)))
                                                          (IF (CONSP V3584)
                                                              (LET ((F
                                                                     (CAR
                                                                      V3584)))
                                                                (LET ((V3585
                                                                       (shen.lazyderef
                                                                        (CDR
                                                                         V3584)
                                                                        V3750)))
                                                                  (IF (CONSP
                                                                       V3585)
                                                                      (LET ((X
                                                                             (CAR
                                                                              V3585)))
                                                                        (LET ((V3586
                                                                               (shen.lazyderef
                                                                                (CDR
                                                                                 V3585)
                                                                                V3750)))
                                                                          (IF (NULL
                                                                               V3586)
                                                                              (LET ((B
                                                                                     (shen.newpv
                                                                                      V3750)))
                                                                                (do
                                                                                 (shen.incinfs)
                                                                                 (shen.th*
                                                                                  F
                                                                                  (CONS
                                                                                   B
                                                                                   (CONS
                                                                                    '-->
                                                                                    (CONS
                                                                                     V3748
                                                                                     NIL)))
                                                                                  V3749
                                                                                  V3750
                                                                                  (freeze
                                                                                   (shen.th*
                                                                                    X
                                                                                    B
                                                                                    V3749
                                                                                    V3750
                                                                                    V3751)))))
                                                                              'false)))
                                                                      'false)))
                                                              'false))))
                                                   (IF (EQ Case 'false)
                                                       (LET ((Case
                                                              (LET ((V3587
                                                                     (shen.lazyderef
                                                                      V3747
                                                                      V3750)))
                                                                (IF (CONSP
                                                                     V3587)
                                                                    (LET ((V3588
                                                                           (shen.lazyderef
                                                                            (CAR
                                                                             V3587)
                                                                            V3750)))
                                                                      (IF (EQ
                                                                           'cons
                                                                           V3588)
                                                                          (LET ((V3589
                                                                                 (shen.lazyderef
                                                                                  (CDR
                                                                                   V3587)
                                                                                  V3750)))
                                                                            (IF (CONSP
                                                                                 V3589)
                                                                                (LET ((X
                                                                                       (CAR
                                                                                        V3589)))
                                                                                  (LET ((V3590
                                                                                         (shen.lazyderef
                                                                                          (CDR
                                                                                           V3589)
                                                                                          V3750)))
                                                                                    (IF (CONSP
                                                                                         V3590)
                                                                                        (LET ((Y
                                                                                               (CAR
                                                                                                V3590)))
                                                                                          (LET ((V3591
                                                                                                 (shen.lazyderef
                                                                                                  (CDR
                                                                                                   V3590)
                                                                                                  V3750)))
                                                                                            (IF (NULL
                                                                                                 V3591)
                                                                                                (LET ((V3592
                                                                                                       (shen.lazyderef
                                                                                                        V3748
                                                                                                        V3750)))
                                                                                                  (IF (CONSP
                                                                                                       V3592)
                                                                                                      (LET ((V3593
                                                                                                             (shen.lazyderef
                                                                                                              (CAR
                                                                                                               V3592)
                                                                                                              V3750)))
                                                                                                        (IF (EQ
                                                                                                             'list
                                                                                                             V3593)
                                                                                                            (LET ((V3594
                                                                                                                   (shen.lazyderef
                                                                                                                    (CDR
                                                                                                                     V3592)
                                                                                                                    V3750)))
                                                                                                              (IF (CONSP
                                                                                                                   V3594)
                                                                                                                  (LET ((A
                                                                                                                         (CAR
                                                                                                                          V3594)))
                                                                                                                    (LET ((V3595
                                                                                                                           (shen.lazyderef
                                                                                                                            (CDR
                                                                                                                             V3594)
                                                                                                                            V3750)))
                                                                                                                      (IF (NULL
                                                                                                                           V3595)
                                                                                                                          (do
                                                                                                                           (shen.incinfs)
                                                                                                                           (shen.th*
                                                                                                                            X
                                                                                                                            A
                                                                                                                            V3749
                                                                                                                            V3750
                                                                                                                            (freeze
                                                                                                                             (shen.th*
                                                                                                                              Y
                                                                                                                              (CONS
                                                                                                                               'list
                                                                                                                               (CONS
                                                                                                                                A
                                                                                                                                NIL))
                                                                                                                              V3749
                                                                                                                              V3750
                                                                                                                              V3751))))
                                                                                                                          (IF (shen-cl.true?
                                                                                                                               (shen.pvar?
                                                                                                                                V3595))
                                                                                                                              (do
                                                                                                                               (shen.bindv
                                                                                                                                V3595
                                                                                                                                NIL
                                                                                                                                V3750)
                                                                                                                               (LET ((Result
                                                                                                                                      (do
                                                                                                                                       (shen.incinfs)
                                                                                                                                       (shen.th*
                                                                                                                                        X
                                                                                                                                        A
                                                                                                                                        V3749
                                                                                                                                        V3750
                                                                                                                                        (freeze
                                                                                                                                         (shen.th*
                                                                                                                                          Y
                                                                                                                                          (CONS
                                                                                                                                           'list
                                                                                                                                           (CONS
                                                                                                                                            A
                                                                                                                                            NIL))
                                                                                                                                          V3749
                                                                                                                                          V3750
                                                                                                                                          V3751))))))
                                                                                                                                 (do
                                                                                                                                  (shen.unbindv
                                                                                                                                   V3595
                                                                                                                                   V3750)
                                                                                                                                  Result)))
                                                                                                                              'false))))
                                                                                                                  (IF (shen-cl.true?
                                                                                                                       (shen.pvar?
                                                                                                                        V3594))
                                                                                                                      (LET ((A
                                                                                                                             (shen.newpv
                                                                                                                              V3750)))
                                                                                                                        (do
                                                                                                                         (shen.bindv
                                                                                                                          V3594
                                                                                                                          (CONS
                                                                                                                           A
                                                                                                                           NIL)
                                                                                                                          V3750)
                                                                                                                         (LET ((Result
                                                                                                                                (do
                                                                                                                                 (shen.incinfs)
                                                                                                                                 (shen.th*
                                                                                                                                  X
                                                                                                                                  A
                                                                                                                                  V3749
                                                                                                                                  V3750
                                                                                                                                  (freeze
                                                                                                                                   (shen.th*
                                                                                                                                    Y
                                                                                                                                    (CONS
                                                                                                                                     'list
                                                                                                                                     (CONS
                                                                                                                                      A
                                                                                                                                      NIL))
                                                                                                                                    V3749
                                                                                                                                    V3750
                                                                                                                                    V3751))))))
                                                                                                                           (do
                                                                                                                            (shen.unbindv
                                                                                                                             V3594
                                                                                                                             V3750)
                                                                                                                            Result))))
                                                                                                                      'false)))
                                                                                                            (IF (shen-cl.true?
                                                                                                                 (shen.pvar?
                                                                                                                  V3593))
                                                                                                                (do
                                                                                                                 (shen.bindv
                                                                                                                  V3593
                                                                                                                  'list
                                                                                                                  V3750)
                                                                                                                 (LET ((Result
                                                                                                                        (LET ((V3596
                                                                                                                               (shen.lazyderef
                                                                                                                                (CDR
                                                                                                                                 V3592)
                                                                                                                                V3750)))
                                                                                                                          (IF (CONSP
                                                                                                                               V3596)
                                                                                                                              (LET ((A
                                                                                                                                     (CAR
                                                                                                                                      V3596)))
                                                                                                                                (LET ((V3597
                                                                                                                                       (shen.lazyderef
                                                                                                                                        (CDR
                                                                                                                                         V3596)
                                                                                                                                        V3750)))
                                                                                                                                  (IF (NULL
                                                                                                                                       V3597)
                                                                                                                                      (do
                                                                                                                                       (shen.incinfs)
                                                                                                                                       (shen.th*
                                                                                                                                        X
                                                                                                                                        A
                                                                                                                                        V3749
                                                                                                                                        V3750
                                                                                                                                        (freeze
                                                                                                                                         (shen.th*
                                                                                                                                          Y
                                                                                                                                          (CONS
                                                                                                                                           'list
                                                                                                                                           (CONS
                                                                                                                                            A
                                                                                                                                            NIL))
                                                                                                                                          V3749
                                                                                                                                          V3750
                                                                                                                                          V3751))))
                                                                                                                                      (IF (shen-cl.true?
                                                                                                                                           (shen.pvar?
                                                                                                                                            V3597))
                                                                                                                                          (do
                                                                                                                                           (shen.bindv
                                                                                                                                            V3597
                                                                                                                                            NIL
                                                                                                                                            V3750)
                                                                                                                                           (LET ((Result
                                                                                                                                                  (do
                                                                                                                                                   (shen.incinfs)
                                                                                                                                                   (shen.th*
                                                                                                                                                    X
                                                                                                                                                    A
                                                                                                                                                    V3749
                                                                                                                                                    V3750
                                                                                                                                                    (freeze
                                                                                                                                                     (shen.th*
                                                                                                                                                      Y
                                                                                                                                                      (CONS
                                                                                                                                                       'list
                                                                                                                                                       (CONS
                                                                                                                                                        A
                                                                                                                                                        NIL))
                                                                                                                                                      V3749
                                                                                                                                                      V3750
                                                                                                                                                      V3751))))))
                                                                                                                                             (do
                                                                                                                                              (shen.unbindv
                                                                                                                                               V3597
                                                                                                                                               V3750)
                                                                                                                                              Result)))
                                                                                                                                          'false))))
                                                                                                                              (IF (shen-cl.true?
                                                                                                                                   (shen.pvar?
                                                                                                                                    V3596))
                                                                                                                                  (LET ((A
                                                                                                                                         (shen.newpv
                                                                                                                                          V3750)))
                                                                                                                                    (do
                                                                                                                                     (shen.bindv
                                                                                                                                      V3596
                                                                                                                                      (CONS
                                                                                                                                       A
                                                                                                                                       NIL)
                                                                                                                                      V3750)
                                                                                                                                     (LET ((Result
                                                                                                                                            (do
                                                                                                                                             (shen.incinfs)
                                                                                                                                             (shen.th*
                                                                                                                                              X
                                                                                                                                              A
                                                                                                                                              V3749
                                                                                                                                              V3750
                                                                                                                                              (freeze
                                                                                                                                               (shen.th*
                                                                                                                                                Y
                                                                                                                                                (CONS
                                                                                                                                                 'list
                                                                                                                                                 (CONS
                                                                                                                                                  A
                                                                                                                                                  NIL))
                                                                                                                                                V3749
                                                                                                                                                V3750
                                                                                                                                                V3751))))))
                                                                                                                                       (do
                                                                                                                                        (shen.unbindv
                                                                                                                                         V3596
                                                                                                                                         V3750)
                                                                                                                                        Result))))
                                                                                                                                  'false)))))
                                                                                                                   (do
                                                                                                                    (shen.unbindv
                                                                                                                     V3593
                                                                                                                     V3750)
                                                                                                                    Result)))
                                                                                                                'false)))
                                                                                                      (IF (shen-cl.true?
                                                                                                           (shen.pvar?
                                                                                                            V3592))
                                                                                                          (LET ((A
                                                                                                                 (shen.newpv
                                                                                                                  V3750)))
                                                                                                            (do
                                                                                                             (shen.bindv
                                                                                                              V3592
                                                                                                              (CONS
                                                                                                               'list
                                                                                                               (CONS
                                                                                                                A
                                                                                                                NIL))
                                                                                                              V3750)
                                                                                                             (LET ((Result
                                                                                                                    (do
                                                                                                                     (shen.incinfs)
                                                                                                                     (shen.th*
                                                                                                                      X
                                                                                                                      A
                                                                                                                      V3749
                                                                                                                      V3750
                                                                                                                      (freeze
                                                                                                                       (shen.th*
                                                                                                                        Y
                                                                                                                        (CONS
                                                                                                                         'list
                                                                                                                         (CONS
                                                                                                                          A
                                                                                                                          NIL))
                                                                                                                        V3749
                                                                                                                        V3750
                                                                                                                        V3751))))))
                                                                                                               (do
                                                                                                                (shen.unbindv
                                                                                                                 V3592
                                                                                                                 V3750)
                                                                                                                Result))))
                                                                                                          'false)))
                                                                                                'false)))
                                                                                        'false)))
                                                                                'false))
                                                                          'false))
                                                                    'false))))
                                                         (IF (EQ Case 'false)
                                                             (LET ((Case
                                                                    (LET ((V3598
                                                                           (shen.lazyderef
                                                                            V3747
                                                                            V3750)))
                                                                      (IF (CONSP
                                                                           V3598)
                                                                          (LET ((V3599
                                                                                 (shen.lazyderef
                                                                                  (CAR
                                                                                   V3598)
                                                                                  V3750)))
                                                                            (IF (EQ
                                                                                 '@p
                                                                                 V3599)
                                                                                (LET ((V3600
                                                                                       (shen.lazyderef
                                                                                        (CDR
                                                                                         V3598)
                                                                                        V3750)))
                                                                                  (IF (CONSP
                                                                                       V3600)
                                                                                      (LET ((X
                                                                                             (CAR
                                                                                              V3600)))
                                                                                        (LET ((V3601
                                                                                               (shen.lazyderef
                                                                                                (CDR
                                                                                                 V3600)
                                                                                                V3750)))
                                                                                          (IF (CONSP
                                                                                               V3601)
                                                                                              (LET ((Y
                                                                                                     (CAR
                                                                                                      V3601)))
                                                                                                (LET ((V3602
                                                                                                       (shen.lazyderef
                                                                                                        (CDR
                                                                                                         V3601)
                                                                                                        V3750)))
                                                                                                  (IF (NULL
                                                                                                       V3602)
                                                                                                      (LET ((V3603
                                                                                                             (shen.lazyderef
                                                                                                              V3748
                                                                                                              V3750)))
                                                                                                        (IF (CONSP
                                                                                                             V3603)
                                                                                                            (LET ((A
                                                                                                                   (CAR
                                                                                                                    V3603)))
                                                                                                              (LET ((V3604
                                                                                                                     (shen.lazyderef
                                                                                                                      (CDR
                                                                                                                       V3603)
                                                                                                                      V3750)))
                                                                                                                (IF (CONSP
                                                                                                                     V3604)
                                                                                                                    (LET ((V3605
                                                                                                                           (shen.lazyderef
                                                                                                                            (CAR
                                                                                                                             V3604)
                                                                                                                            V3750)))
                                                                                                                      (IF (EQ
                                                                                                                           '*
                                                                                                                           V3605)
                                                                                                                          (LET ((V3606
                                                                                                                                 (shen.lazyderef
                                                                                                                                  (CDR
                                                                                                                                   V3604)
                                                                                                                                  V3750)))
                                                                                                                            (IF (CONSP
                                                                                                                                 V3606)
                                                                                                                                (LET ((B
                                                                                                                                       (CAR
                                                                                                                                        V3606)))
                                                                                                                                  (LET ((V3607
                                                                                                                                         (shen.lazyderef
                                                                                                                                          (CDR
                                                                                                                                           V3606)
                                                                                                                                          V3750)))
                                                                                                                                    (IF (NULL
                                                                                                                                         V3607)
                                                                                                                                        (do
                                                                                                                                         (shen.incinfs)
                                                                                                                                         (shen.th*
                                                                                                                                          X
                                                                                                                                          A
                                                                                                                                          V3749
                                                                                                                                          V3750
                                                                                                                                          (freeze
                                                                                                                                           (shen.th*
                                                                                                                                            Y
                                                                                                                                            B
                                                                                                                                            V3749
                                                                                                                                            V3750
                                                                                                                                            V3751))))
                                                                                                                                        (IF (shen-cl.true?
                                                                                                                                             (shen.pvar?
                                                                                                                                              V3607))
                                                                                                                                            (do
                                                                                                                                             (shen.bindv
                                                                                                                                              V3607
                                                                                                                                              NIL
                                                                                                                                              V3750)
                                                                                                                                             (LET ((Result
                                                                                                                                                    (do
                                                                                                                                                     (shen.incinfs)
                                                                                                                                                     (shen.th*
                                                                                                                                                      X
                                                                                                                                                      A
                                                                                                                                                      V3749
                                                                                                                                                      V3750
                                                                                                                                                      (freeze
                                                                                                                                                       (shen.th*
                                                                                                                                                        Y
                                                                                                                                                        B
                                                                                                                                                        V3749
                                                                                                                                                        V3750
                                                                                                                                                        V3751))))))
                                                                                                                                               (do
                                                                                                                                                (shen.unbindv
                                                                                                                                                 V3607
                                                                                                                                                 V3750)
                                                                                                                                                Result)))
                                                                                                                                            'false))))
                                                                                                                                (IF (shen-cl.true?
                                                                                                                                     (shen.pvar?
                                                                                                                                      V3606))
                                                                                                                                    (LET ((B
                                                                                                                                           (shen.newpv
                                                                                                                                            V3750)))
                                                                                                                                      (do
                                                                                                                                       (shen.bindv
                                                                                                                                        V3606
                                                                                                                                        (CONS
                                                                                                                                         B
                                                                                                                                         NIL)
                                                                                                                                        V3750)
                                                                                                                                       (LET ((Result
                                                                                                                                              (do
                                                                                                                                               (shen.incinfs)
                                                                                                                                               (shen.th*
                                                                                                                                                X
                                                                                                                                                A
                                                                                                                                                V3749
                                                                                                                                                V3750
                                                                                                                                                (freeze
                                                                                                                                                 (shen.th*
                                                                                                                                                  Y
                                                                                                                                                  B
                                                                                                                                                  V3749
                                                                                                                                                  V3750
                                                                                                                                                  V3751))))))
                                                                                                                                         (do
                                                                                                                                          (shen.unbindv
                                                                                                                                           V3606
                                                                                                                                           V3750)
                                                                                                                                          Result))))
                                                                                                                                    'false)))
                                                                                                                          (IF (shen-cl.true?
                                                                                                                               (shen.pvar?
                                                                                                                                V3605))
                                                                                                                              (do
                                                                                                                               (shen.bindv
                                                                                                                                V3605
                                                                                                                                '*
                                                                                                                                V3750)
                                                                                                                               (LET ((Result
                                                                                                                                      (LET ((V3608
                                                                                                                                             (shen.lazyderef
                                                                                                                                              (CDR
                                                                                                                                               V3604)
                                                                                                                                              V3750)))
                                                                                                                                        (IF (CONSP
                                                                                                                                             V3608)
                                                                                                                                            (LET ((B
                                                                                                                                                   (CAR
                                                                                                                                                    V3608)))
                                                                                                                                              (LET ((V3609
                                                                                                                                                     (shen.lazyderef
                                                                                                                                                      (CDR
                                                                                                                                                       V3608)
                                                                                                                                                      V3750)))
                                                                                                                                                (IF (NULL
                                                                                                                                                     V3609)
                                                                                                                                                    (do
                                                                                                                                                     (shen.incinfs)
                                                                                                                                                     (shen.th*
                                                                                                                                                      X
                                                                                                                                                      A
                                                                                                                                                      V3749
                                                                                                                                                      V3750
                                                                                                                                                      (freeze
                                                                                                                                                       (shen.th*
                                                                                                                                                        Y
                                                                                                                                                        B
                                                                                                                                                        V3749
                                                                                                                                                        V3750
                                                                                                                                                        V3751))))
                                                                                                                                                    (IF (shen-cl.true?
                                                                                                                                                         (shen.pvar?
                                                                                                                                                          V3609))
                                                                                                                                                        (do
                                                                                                                                                         (shen.bindv
                                                                                                                                                          V3609
                                                                                                                                                          NIL
                                                                                                                                                          V3750)
                                                                                                                                                         (LET ((Result
                                                                                                                                                                (do
                                                                                                                                                                 (shen.incinfs)
                                                                                                                                                                 (shen.th*
                                                                                                                                                                  X
                                                                                                                                                                  A
                                                                                                                                                                  V3749
                                                                                                                                                                  V3750
                                                                                                                                                                  (freeze
                                                                                                                                                                   (shen.th*
                                                                                                                                                                    Y
                                                                                                                                                                    B
                                                                                                                                                                    V3749
                                                                                                                                                                    V3750
                                                                                                                                                                    V3751))))))
                                                                                                                                                           (do
                                                                                                                                                            (shen.unbindv
                                                                                                                                                             V3609
                                                                                                                                                             V3750)
                                                                                                                                                            Result)))
                                                                                                                                                        'false))))
                                                                                                                                            (IF (shen-cl.true?
                                                                                                                                                 (shen.pvar?
                                                                                                                                                  V3608))
                                                                                                                                                (LET ((B
                                                                                                                                                       (shen.newpv
                                                                                                                                                        V3750)))
                                                                                                                                                  (do
                                                                                                                                                   (shen.bindv
                                                                                                                                                    V3608
                                                                                                                                                    (CONS
                                                                                                                                                     B
                                                                                                                                                     NIL)
                                                                                                                                                    V3750)
                                                                                                                                                   (LET ((Result
                                                                                                                                                          (do
                                                                                                                                                           (shen.incinfs)
                                                                                                                                                           (shen.th*
                                                                                                                                                            X
                                                                                                                                                            A
                                                                                                                                                            V3749
                                                                                                                                                            V3750
                                                                                                                                                            (freeze
                                                                                                                                                             (shen.th*
                                                                                                                                                              Y
                                                                                                                                                              B
                                                                                                                                                              V3749
                                                                                                                                                              V3750
                                                                                                                                                              V3751))))))
                                                                                                                                                     (do
                                                                                                                                                      (shen.unbindv
                                                                                                                                                       V3608
                                                                                                                                                       V3750)
                                                                                                                                                      Result))))
                                                                                                                                                'false)))))
                                                                                                                                 (do
                                                                                                                                  (shen.unbindv
                                                                                                                                   V3605
                                                                                                                                   V3750)
                                                                                                                                  Result)))
                                                                                                                              'false)))
                                                                                                                    (IF (shen-cl.true?
                                                                                                                         (shen.pvar?
                                                                                                                          V3604))
                                                                                                                        (LET ((B
                                                                                                                               (shen.newpv
                                                                                                                                V3750)))
                                                                                                                          (do
                                                                                                                           (shen.bindv
                                                                                                                            V3604
                                                                                                                            (CONS
                                                                                                                             '*
                                                                                                                             (CONS
                                                                                                                              B
                                                                                                                              NIL))
                                                                                                                            V3750)
                                                                                                                           (LET ((Result
                                                                                                                                  (do
                                                                                                                                   (shen.incinfs)
                                                                                                                                   (shen.th*
                                                                                                                                    X
                                                                                                                                    A
                                                                                                                                    V3749
                                                                                                                                    V3750
                                                                                                                                    (freeze
                                                                                                                                     (shen.th*
                                                                                                                                      Y
                                                                                                                                      B
                                                                                                                                      V3749
                                                                                                                                      V3750
                                                                                                                                      V3751))))))
                                                                                                                             (do
                                                                                                                              (shen.unbindv
                                                                                                                               V3604
                                                                                                                               V3750)
                                                                                                                              Result))))
                                                                                                                        'false))))
                                                                                                            (IF (shen-cl.true?
                                                                                                                 (shen.pvar?
                                                                                                                  V3603))
                                                                                                                (LET ((A
                                                                                                                       (shen.newpv
                                                                                                                        V3750)))
                                                                                                                  (LET ((B
                                                                                                                         (shen.newpv
                                                                                                                          V3750)))
                                                                                                                    (do
                                                                                                                     (shen.bindv
                                                                                                                      V3603
                                                                                                                      (CONS
                                                                                                                       A
                                                                                                                       (CONS
                                                                                                                        '*
                                                                                                                        (CONS
                                                                                                                         B
                                                                                                                         NIL)))
                                                                                                                      V3750)
                                                                                                                     (LET ((Result
                                                                                                                            (do
                                                                                                                             (shen.incinfs)
                                                                                                                             (shen.th*
                                                                                                                              X
                                                                                                                              A
                                                                                                                              V3749
                                                                                                                              V3750
                                                                                                                              (freeze
                                                                                                                               (shen.th*
                                                                                                                                Y
                                                                                                                                B
                                                                                                                                V3749
                                                                                                                                V3750
                                                                                                                                V3751))))))
                                                                                                                       (do
                                                                                                                        (shen.unbindv
                                                                                                                         V3603
                                                                                                                         V3750)
                                                                                                                        Result)))))
                                                                                                                'false)))
                                                                                                      'false)))
                                                                                              'false)))
                                                                                      'false))
                                                                                'false))
                                                                          'false))))
                                                               (IF (EQ Case
                                                                       'false)
                                                                   (LET ((Case
                                                                          (LET ((V3610
                                                                                 (shen.lazyderef
                                                                                  V3747
                                                                                  V3750)))
                                                                            (IF (CONSP
                                                                                 V3610)
                                                                                (LET ((V3611
                                                                                       (shen.lazyderef
                                                                                        (CAR
                                                                                         V3610)
                                                                                        V3750)))
                                                                                  (IF (EQ
                                                                                       '@v
                                                                                       V3611)
                                                                                      (LET ((V3612
                                                                                             (shen.lazyderef
                                                                                              (CDR
                                                                                               V3610)
                                                                                              V3750)))
                                                                                        (IF (CONSP
                                                                                             V3612)
                                                                                            (LET ((X
                                                                                                   (CAR
                                                                                                    V3612)))
                                                                                              (LET ((V3613
                                                                                                     (shen.lazyderef
                                                                                                      (CDR
                                                                                                       V3612)
                                                                                                      V3750)))
                                                                                                (IF (CONSP
                                                                                                     V3613)
                                                                                                    (LET ((Y
                                                                                                           (CAR
                                                                                                            V3613)))
                                                                                                      (LET ((V3614
                                                                                                             (shen.lazyderef
                                                                                                              (CDR
                                                                                                               V3613)
                                                                                                              V3750)))
                                                                                                        (IF (NULL
                                                                                                             V3614)
                                                                                                            (LET ((V3615
                                                                                                                   (shen.lazyderef
                                                                                                                    V3748
                                                                                                                    V3750)))
                                                                                                              (IF (CONSP
                                                                                                                   V3615)
                                                                                                                  (LET ((V3616
                                                                                                                         (shen.lazyderef
                                                                                                                          (CAR
                                                                                                                           V3615)
                                                                                                                          V3750)))
                                                                                                                    (IF (EQ
                                                                                                                         'vector
                                                                                                                         V3616)
                                                                                                                        (LET ((V3617
                                                                                                                               (shen.lazyderef
                                                                                                                                (CDR
                                                                                                                                 V3615)
                                                                                                                                V3750)))
                                                                                                                          (IF (CONSP
                                                                                                                               V3617)
                                                                                                                              (LET ((A
                                                                                                                                     (CAR
                                                                                                                                      V3617)))
                                                                                                                                (LET ((V3618
                                                                                                                                       (shen.lazyderef
                                                                                                                                        (CDR
                                                                                                                                         V3617)
                                                                                                                                        V3750)))
                                                                                                                                  (IF (NULL
                                                                                                                                       V3618)
                                                                                                                                      (do
                                                                                                                                       (shen.incinfs)
                                                                                                                                       (shen.th*
                                                                                                                                        X
                                                                                                                                        A
                                                                                                                                        V3749
                                                                                                                                        V3750
                                                                                                                                        (freeze
                                                                                                                                         (shen.th*
                                                                                                                                          Y
                                                                                                                                          (CONS
                                                                                                                                           'vector
                                                                                                                                           (CONS
                                                                                                                                            A
                                                                                                                                            NIL))
                                                                                                                                          V3749
                                                                                                                                          V3750
                                                                                                                                          V3751))))
                                                                                                                                      (IF (shen-cl.true?
                                                                                                                                           (shen.pvar?
                                                                                                                                            V3618))
                                                                                                                                          (do
                                                                                                                                           (shen.bindv
                                                                                                                                            V3618
                                                                                                                                            NIL
                                                                                                                                            V3750)
                                                                                                                                           (LET ((Result
                                                                                                                                                  (do
                                                                                                                                                   (shen.incinfs)
                                                                                                                                                   (shen.th*
                                                                                                                                                    X
                                                                                                                                                    A
                                                                                                                                                    V3749
                                                                                                                                                    V3750
                                                                                                                                                    (freeze
                                                                                                                                                     (shen.th*
                                                                                                                                                      Y
                                                                                                                                                      (CONS
                                                                                                                                                       'vector
                                                                                                                                                       (CONS
                                                                                                                                                        A
                                                                                                                                                        NIL))
                                                                                                                                                      V3749
                                                                                                                                                      V3750
                                                                                                                                                      V3751))))))
                                                                                                                                             (do
                                                                                                                                              (shen.unbindv
                                                                                                                                               V3618
                                                                                                                                               V3750)
                                                                                                                                              Result)))
                                                                                                                                          'false))))
                                                                                                                              (IF (shen-cl.true?
                                                                                                                                   (shen.pvar?
                                                                                                                                    V3617))
                                                                                                                                  (LET ((A
                                                                                                                                         (shen.newpv
                                                                                                                                          V3750)))
                                                                                                                                    (do
                                                                                                                                     (shen.bindv
                                                                                                                                      V3617
                                                                                                                                      (CONS
                                                                                                                                       A
                                                                                                                                       NIL)
                                                                                                                                      V3750)
                                                                                                                                     (LET ((Result
                                                                                                                                            (do
                                                                                                                                             (shen.incinfs)
                                                                                                                                             (shen.th*
                                                                                                                                              X
                                                                                                                                              A
                                                                                                                                              V3749
                                                                                                                                              V3750
                                                                                                                                              (freeze
                                                                                                                                               (shen.th*
                                                                                                                                                Y
                                                                                                                                                (CONS
                                                                                                                                                 'vector
                                                                                                                                                 (CONS
                                                                                                                                                  A
                                                                                                                                                  NIL))
                                                                                                                                                V3749
                                                                                                                                                V3750
                                                                                                                                                V3751))))))
                                                                                                                                       (do
                                                                                                                                        (shen.unbindv
                                                                                                                                         V3617
                                                                                                                                         V3750)
                                                                                                                                        Result))))
                                                                                                                                  'false)))
                                                                                                                        (IF (shen-cl.true?
                                                                                                                             (shen.pvar?
                                                                                                                              V3616))
                                                                                                                            (do
                                                                                                                             (shen.bindv
                                                                                                                              V3616
                                                                                                                              'vector
                                                                                                                              V3750)
                                                                                                                             (LET ((Result
                                                                                                                                    (LET ((V3619
                                                                                                                                           (shen.lazyderef
                                                                                                                                            (CDR
                                                                                                                                             V3615)
                                                                                                                                            V3750)))
                                                                                                                                      (IF (CONSP
                                                                                                                                           V3619)
                                                                                                                                          (LET ((A
                                                                                                                                                 (CAR
                                                                                                                                                  V3619)))
                                                                                                                                            (LET ((V3620
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    (CDR
                                                                                                                                                     V3619)
                                                                                                                                                    V3750)))
                                                                                                                                              (IF (NULL
                                                                                                                                                   V3620)
                                                                                                                                                  (do
                                                                                                                                                   (shen.incinfs)
                                                                                                                                                   (shen.th*
                                                                                                                                                    X
                                                                                                                                                    A
                                                                                                                                                    V3749
                                                                                                                                                    V3750
                                                                                                                                                    (freeze
                                                                                                                                                     (shen.th*
                                                                                                                                                      Y
                                                                                                                                                      (CONS
                                                                                                                                                       'vector
                                                                                                                                                       (CONS
                                                                                                                                                        A
                                                                                                                                                        NIL))
                                                                                                                                                      V3749
                                                                                                                                                      V3750
                                                                                                                                                      V3751))))
                                                                                                                                                  (IF (shen-cl.true?
                                                                                                                                                       (shen.pvar?
                                                                                                                                                        V3620))
                                                                                                                                                      (do
                                                                                                                                                       (shen.bindv
                                                                                                                                                        V3620
                                                                                                                                                        NIL
                                                                                                                                                        V3750)
                                                                                                                                                       (LET ((Result
                                                                                                                                                              (do
                                                                                                                                                               (shen.incinfs)
                                                                                                                                                               (shen.th*
                                                                                                                                                                X
                                                                                                                                                                A
                                                                                                                                                                V3749
                                                                                                                                                                V3750
                                                                                                                                                                (freeze
                                                                                                                                                                 (shen.th*
                                                                                                                                                                  Y
                                                                                                                                                                  (CONS
                                                                                                                                                                   'vector
                                                                                                                                                                   (CONS
                                                                                                                                                                    A
                                                                                                                                                                    NIL))
                                                                                                                                                                  V3749
                                                                                                                                                                  V3750
                                                                                                                                                                  V3751))))))
                                                                                                                                                         (do
                                                                                                                                                          (shen.unbindv
                                                                                                                                                           V3620
                                                                                                                                                           V3750)
                                                                                                                                                          Result)))
                                                                                                                                                      'false))))
                                                                                                                                          (IF (shen-cl.true?
                                                                                                                                               (shen.pvar?
                                                                                                                                                V3619))
                                                                                                                                              (LET ((A
                                                                                                                                                     (shen.newpv
                                                                                                                                                      V3750)))
                                                                                                                                                (do
                                                                                                                                                 (shen.bindv
                                                                                                                                                  V3619
                                                                                                                                                  (CONS
                                                                                                                                                   A
                                                                                                                                                   NIL)
                                                                                                                                                  V3750)
                                                                                                                                                 (LET ((Result
                                                                                                                                                        (do
                                                                                                                                                         (shen.incinfs)
                                                                                                                                                         (shen.th*
                                                                                                                                                          X
                                                                                                                                                          A
                                                                                                                                                          V3749
                                                                                                                                                          V3750
                                                                                                                                                          (freeze
                                                                                                                                                           (shen.th*
                                                                                                                                                            Y
                                                                                                                                                            (CONS
                                                                                                                                                             'vector
                                                                                                                                                             (CONS
                                                                                                                                                              A
                                                                                                                                                              NIL))
                                                                                                                                                            V3749
                                                                                                                                                            V3750
                                                                                                                                                            V3751))))))
                                                                                                                                                   (do
                                                                                                                                                    (shen.unbindv
                                                                                                                                                     V3619
                                                                                                                                                     V3750)
                                                                                                                                                    Result))))
                                                                                                                                              'false)))))
                                                                                                                               (do
                                                                                                                                (shen.unbindv
                                                                                                                                 V3616
                                                                                                                                 V3750)
                                                                                                                                Result)))
                                                                                                                            'false)))
                                                                                                                  (IF (shen-cl.true?
                                                                                                                       (shen.pvar?
                                                                                                                        V3615))
                                                                                                                      (LET ((A
                                                                                                                             (shen.newpv
                                                                                                                              V3750)))
                                                                                                                        (do
                                                                                                                         (shen.bindv
                                                                                                                          V3615
                                                                                                                          (CONS
                                                                                                                           'vector
                                                                                                                           (CONS
                                                                                                                            A
                                                                                                                            NIL))
                                                                                                                          V3750)
                                                                                                                         (LET ((Result
                                                                                                                                (do
                                                                                                                                 (shen.incinfs)
                                                                                                                                 (shen.th*
                                                                                                                                  X
                                                                                                                                  A
                                                                                                                                  V3749
                                                                                                                                  V3750
                                                                                                                                  (freeze
                                                                                                                                   (shen.th*
                                                                                                                                    Y
                                                                                                                                    (CONS
                                                                                                                                     'vector
                                                                                                                                     (CONS
                                                                                                                                      A
                                                                                                                                      NIL))
                                                                                                                                    V3749
                                                                                                                                    V3750
                                                                                                                                    V3751))))))
                                                                                                                           (do
                                                                                                                            (shen.unbindv
                                                                                                                             V3615
                                                                                                                             V3750)
                                                                                                                            Result))))
                                                                                                                      'false)))
                                                                                                            'false)))
                                                                                                    'false)))
                                                                                            'false))
                                                                                      'false))
                                                                                'false))))
                                                                     (IF (EQ
                                                                          Case
                                                                          'false)
                                                                         (LET ((Case
                                                                                (LET ((V3621
                                                                                       (shen.lazyderef
                                                                                        V3747
                                                                                        V3750)))
                                                                                  (IF (CONSP
                                                                                       V3621)
                                                                                      (LET ((V3622
                                                                                             (shen.lazyderef
                                                                                              (CAR
                                                                                               V3621)
                                                                                              V3750)))
                                                                                        (IF (EQ
                                                                                             '@s
                                                                                             V3622)
                                                                                            (LET ((V3623
                                                                                                   (shen.lazyderef
                                                                                                    (CDR
                                                                                                     V3621)
                                                                                                    V3750)))
                                                                                              (IF (CONSP
                                                                                                   V3623)
                                                                                                  (LET ((X
                                                                                                         (CAR
                                                                                                          V3623)))
                                                                                                    (LET ((V3624
                                                                                                           (shen.lazyderef
                                                                                                            (CDR
                                                                                                             V3623)
                                                                                                            V3750)))
                                                                                                      (IF (CONSP
                                                                                                           V3624)
                                                                                                          (LET ((Y
                                                                                                                 (CAR
                                                                                                                  V3624)))
                                                                                                            (LET ((V3625
                                                                                                                   (shen.lazyderef
                                                                                                                    (CDR
                                                                                                                     V3624)
                                                                                                                    V3750)))
                                                                                                              (IF (NULL
                                                                                                                   V3625)
                                                                                                                  (LET ((V3626
                                                                                                                         (shen.lazyderef
                                                                                                                          V3748
                                                                                                                          V3750)))
                                                                                                                    (IF (EQ
                                                                                                                         'string
                                                                                                                         V3626)
                                                                                                                        (do
                                                                                                                         (shen.incinfs)
                                                                                                                         (shen.th*
                                                                                                                          X
                                                                                                                          'string
                                                                                                                          V3749
                                                                                                                          V3750
                                                                                                                          (freeze
                                                                                                                           (shen.th*
                                                                                                                            Y
                                                                                                                            'string
                                                                                                                            V3749
                                                                                                                            V3750
                                                                                                                            V3751))))
                                                                                                                        (IF (shen-cl.true?
                                                                                                                             (shen.pvar?
                                                                                                                              V3626))
                                                                                                                            (do
                                                                                                                             (shen.bindv
                                                                                                                              V3626
                                                                                                                              'string
                                                                                                                              V3750)
                                                                                                                             (LET ((Result
                                                                                                                                    (do
                                                                                                                                     (shen.incinfs)
                                                                                                                                     (shen.th*
                                                                                                                                      X
                                                                                                                                      'string
                                                                                                                                      V3749
                                                                                                                                      V3750
                                                                                                                                      (freeze
                                                                                                                                       (shen.th*
                                                                                                                                        Y
                                                                                                                                        'string
                                                                                                                                        V3749
                                                                                                                                        V3750
                                                                                                                                        V3751))))))
                                                                                                                               (do
                                                                                                                                (shen.unbindv
                                                                                                                                 V3626
                                                                                                                                 V3750)
                                                                                                                                Result)))
                                                                                                                            'false)))
                                                                                                                  'false)))
                                                                                                          'false)))
                                                                                                  'false))
                                                                                            'false))
                                                                                      'false))))
                                                                           (IF (EQ
                                                                                Case
                                                                                'false)
                                                                               (LET ((Case
                                                                                      (LET ((V3627
                                                                                             (shen.lazyderef
                                                                                              V3747
                                                                                              V3750)))
                                                                                        (IF (CONSP
                                                                                             V3627)
                                                                                            (LET ((V3628
                                                                                                   (shen.lazyderef
                                                                                                    (CAR
                                                                                                     V3627)
                                                                                                    V3750)))
                                                                                              (IF (EQ
                                                                                                   'lambda
                                                                                                   V3628)
                                                                                                  (LET ((V3629
                                                                                                         (shen.lazyderef
                                                                                                          (CDR
                                                                                                           V3627)
                                                                                                          V3750)))
                                                                                                    (IF (CONSP
                                                                                                         V3629)
                                                                                                        (LET ((X
                                                                                                               (CAR
                                                                                                                V3629)))
                                                                                                          (LET ((V3630
                                                                                                                 (shen.lazyderef
                                                                                                                  (CDR
                                                                                                                   V3629)
                                                                                                                  V3750)))
                                                                                                            (IF (CONSP
                                                                                                                 V3630)
                                                                                                                (LET ((Y
                                                                                                                       (CAR
                                                                                                                        V3630)))
                                                                                                                  (LET ((V3631
                                                                                                                         (shen.lazyderef
                                                                                                                          (CDR
                                                                                                                           V3630)
                                                                                                                          V3750)))
                                                                                                                    (IF (NULL
                                                                                                                         V3631)
                                                                                                                        (LET ((V3632
                                                                                                                               (shen.lazyderef
                                                                                                                                V3748
                                                                                                                                V3750)))
                                                                                                                          (IF (CONSP
                                                                                                                               V3632)
                                                                                                                              (LET ((A
                                                                                                                                     (CAR
                                                                                                                                      V3632)))
                                                                                                                                (LET ((V3633
                                                                                                                                       (shen.lazyderef
                                                                                                                                        (CDR
                                                                                                                                         V3632)
                                                                                                                                        V3750)))
                                                                                                                                  (IF (CONSP
                                                                                                                                       V3633)
                                                                                                                                      (LET ((V3634
                                                                                                                                             (shen.lazyderef
                                                                                                                                              (CAR
                                                                                                                                               V3633)
                                                                                                                                              V3750)))
                                                                                                                                        (IF (EQ
                                                                                                                                             '-->
                                                                                                                                             V3634)
                                                                                                                                            (LET ((V3635
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    (CDR
                                                                                                                                                     V3633)
                                                                                                                                                    V3750)))
                                                                                                                                              (IF (CONSP
                                                                                                                                                   V3635)
                                                                                                                                                  (LET ((B
                                                                                                                                                         (CAR
                                                                                                                                                          V3635)))
                                                                                                                                                    (LET ((V3636
                                                                                                                                                           (shen.lazyderef
                                                                                                                                                            (CDR
                                                                                                                                                             V3635)
                                                                                                                                                            V3750)))
                                                                                                                                                      (IF (NULL
                                                                                                                                                           V3636)
                                                                                                                                                          (LET ((Z
                                                                                                                                                                 (shen.newpv
                                                                                                                                                                  V3750)))
                                                                                                                                                            (LET ((X&&
                                                                                                                                                                   (shen.newpv
                                                                                                                                                                    V3750)))
                                                                                                                                                              (do
                                                                                                                                                               (shen.incinfs)
                                                                                                                                                               (cut
                                                                                                                                                                Throwcontrol
                                                                                                                                                                V3750
                                                                                                                                                                (freeze
                                                                                                                                                                 (bind
                                                                                                                                                                  X&&
                                                                                                                                                                  (shen.placeholder)
                                                                                                                                                                  V3750
                                                                                                                                                                  (freeze
                                                                                                                                                                   (bind
                                                                                                                                                                    Z
                                                                                                                                                                    (shen.ebr
                                                                                                                                                                     (shen.lazyderef
                                                                                                                                                                      X&&
                                                                                                                                                                      V3750)
                                                                                                                                                                     (shen.lazyderef
                                                                                                                                                                      X
                                                                                                                                                                      V3750)
                                                                                                                                                                     (shen.lazyderef
                                                                                                                                                                      Y
                                                                                                                                                                      V3750))
                                                                                                                                                                    V3750
                                                                                                                                                                    (freeze
                                                                                                                                                                     (shen.th*
                                                                                                                                                                      Z
                                                                                                                                                                      B
                                                                                                                                                                      (CONS
                                                                                                                                                                       (CONS
                                                                                                                                                                        X&&
                                                                                                                                                                        (CONS
                                                                                                                                                                         '|:|
                                                                                                                                                                         (CONS
                                                                                                                                                                          A
                                                                                                                                                                          NIL)))
                                                                                                                                                                       V3749)
                                                                                                                                                                      V3750
                                                                                                                                                                      V3751))))))))))
                                                                                                                                                          (IF (shen-cl.true?
                                                                                                                                                               (shen.pvar?
                                                                                                                                                                V3636))
                                                                                                                                                              (do
                                                                                                                                                               (shen.bindv
                                                                                                                                                                V3636
                                                                                                                                                                NIL
                                                                                                                                                                V3750)
                                                                                                                                                               (LET ((Result
                                                                                                                                                                      (LET ((Z
                                                                                                                                                                             (shen.newpv
                                                                                                                                                                              V3750)))
                                                                                                                                                                        (LET ((X&&
                                                                                                                                                                               (shen.newpv
                                                                                                                                                                                V3750)))
                                                                                                                                                                          (do
                                                                                                                                                                           (shen.incinfs)
                                                                                                                                                                           (cut
                                                                                                                                                                            Throwcontrol
                                                                                                                                                                            V3750
                                                                                                                                                                            (freeze
                                                                                                                                                                             (bind
                                                                                                                                                                              X&&
                                                                                                                                                                              (shen.placeholder)
                                                                                                                                                                              V3750
                                                                                                                                                                              (freeze
                                                                                                                                                                               (bind
                                                                                                                                                                                Z
                                                                                                                                                                                (shen.ebr
                                                                                                                                                                                 (shen.lazyderef
                                                                                                                                                                                  X&&
                                                                                                                                                                                  V3750)
                                                                                                                                                                                 (shen.lazyderef
                                                                                                                                                                                  X
                                                                                                                                                                                  V3750)
                                                                                                                                                                                 (shen.lazyderef
                                                                                                                                                                                  Y
                                                                                                                                                                                  V3750))
                                                                                                                                                                                V3750
                                                                                                                                                                                (freeze
                                                                                                                                                                                 (shen.th*
                                                                                                                                                                                  Z
                                                                                                                                                                                  B
                                                                                                                                                                                  (CONS
                                                                                                                                                                                   (CONS
                                                                                                                                                                                    X&&
                                                                                                                                                                                    (CONS
                                                                                                                                                                                     '|:|
                                                                                                                                                                                     (CONS
                                                                                                                                                                                      A
                                                                                                                                                                                      NIL)))
                                                                                                                                                                                   V3749)
                                                                                                                                                                                  V3750
                                                                                                                                                                                  V3751))))))))))))
                                                                                                                                                                 (do
                                                                                                                                                                  (shen.unbindv
                                                                                                                                                                   V3636
                                                                                                                                                                   V3750)
                                                                                                                                                                  Result)))
                                                                                                                                                              'false))))
                                                                                                                                                  (IF (shen-cl.true?
                                                                                                                                                       (shen.pvar?
                                                                                                                                                        V3635))
                                                                                                                                                      (LET ((B
                                                                                                                                                             (shen.newpv
                                                                                                                                                              V3750)))
                                                                                                                                                        (do
                                                                                                                                                         (shen.bindv
                                                                                                                                                          V3635
                                                                                                                                                          (CONS
                                                                                                                                                           B
                                                                                                                                                           NIL)
                                                                                                                                                          V3750)
                                                                                                                                                         (LET ((Result
                                                                                                                                                                (LET ((Z
                                                                                                                                                                       (shen.newpv
                                                                                                                                                                        V3750)))
                                                                                                                                                                  (LET ((X&&
                                                                                                                                                                         (shen.newpv
                                                                                                                                                                          V3750)))
                                                                                                                                                                    (do
                                                                                                                                                                     (shen.incinfs)
                                                                                                                                                                     (cut
                                                                                                                                                                      Throwcontrol
                                                                                                                                                                      V3750
                                                                                                                                                                      (freeze
                                                                                                                                                                       (bind
                                                                                                                                                                        X&&
                                                                                                                                                                        (shen.placeholder)
                                                                                                                                                                        V3750
                                                                                                                                                                        (freeze
                                                                                                                                                                         (bind
                                                                                                                                                                          Z
                                                                                                                                                                          (shen.ebr
                                                                                                                                                                           (shen.lazyderef
                                                                                                                                                                            X&&
                                                                                                                                                                            V3750)
                                                                                                                                                                           (shen.lazyderef
                                                                                                                                                                            X
                                                                                                                                                                            V3750)
                                                                                                                                                                           (shen.lazyderef
                                                                                                                                                                            Y
                                                                                                                                                                            V3750))
                                                                                                                                                                          V3750
                                                                                                                                                                          (freeze
                                                                                                                                                                           (shen.th*
                                                                                                                                                                            Z
                                                                                                                                                                            B
                                                                                                                                                                            (CONS
                                                                                                                                                                             (CONS
                                                                                                                                                                              X&&
                                                                                                                                                                              (CONS
                                                                                                                                                                               '|:|
                                                                                                                                                                               (CONS
                                                                                                                                                                                A
                                                                                                                                                                                NIL)))
                                                                                                                                                                             V3749)
                                                                                                                                                                            V3750
                                                                                                                                                                            V3751))))))))))))
                                                                                                                                                           (do
                                                                                                                                                            (shen.unbindv
                                                                                                                                                             V3635
                                                                                                                                                             V3750)
                                                                                                                                                            Result))))
                                                                                                                                                      'false)))
                                                                                                                                            (IF (shen-cl.true?
                                                                                                                                                 (shen.pvar?
                                                                                                                                                  V3634))
                                                                                                                                                (do
                                                                                                                                                 (shen.bindv
                                                                                                                                                  V3634
                                                                                                                                                  '-->
                                                                                                                                                  V3750)
                                                                                                                                                 (LET ((Result
                                                                                                                                                        (LET ((V3637
                                                                                                                                                               (shen.lazyderef
                                                                                                                                                                (CDR
                                                                                                                                                                 V3633)
                                                                                                                                                                V3750)))
                                                                                                                                                          (IF (CONSP
                                                                                                                                                               V3637)
                                                                                                                                                              (LET ((B
                                                                                                                                                                     (CAR
                                                                                                                                                                      V3637)))
                                                                                                                                                                (LET ((V3638
                                                                                                                                                                       (shen.lazyderef
                                                                                                                                                                        (CDR
                                                                                                                                                                         V3637)
                                                                                                                                                                        V3750)))
                                                                                                                                                                  (IF (NULL
                                                                                                                                                                       V3638)
                                                                                                                                                                      (LET ((Z
                                                                                                                                                                             (shen.newpv
                                                                                                                                                                              V3750)))
                                                                                                                                                                        (LET ((X&&
                                                                                                                                                                               (shen.newpv
                                                                                                                                                                                V3750)))
                                                                                                                                                                          (do
                                                                                                                                                                           (shen.incinfs)
                                                                                                                                                                           (cut
                                                                                                                                                                            Throwcontrol
                                                                                                                                                                            V3750
                                                                                                                                                                            (freeze
                                                                                                                                                                             (bind
                                                                                                                                                                              X&&
                                                                                                                                                                              (shen.placeholder)
                                                                                                                                                                              V3750
                                                                                                                                                                              (freeze
                                                                                                                                                                               (bind
                                                                                                                                                                                Z
                                                                                                                                                                                (shen.ebr
                                                                                                                                                                                 (shen.lazyderef
                                                                                                                                                                                  X&&
                                                                                                                                                                                  V3750)
                                                                                                                                                                                 (shen.lazyderef
                                                                                                                                                                                  X
                                                                                                                                                                                  V3750)
                                                                                                                                                                                 (shen.lazyderef
                                                                                                                                                                                  Y
                                                                                                                                                                                  V3750))
                                                                                                                                                                                V3750
                                                                                                                                                                                (freeze
                                                                                                                                                                                 (shen.th*
                                                                                                                                                                                  Z
                                                                                                                                                                                  B
                                                                                                                                                                                  (CONS
                                                                                                                                                                                   (CONS
                                                                                                                                                                                    X&&
                                                                                                                                                                                    (CONS
                                                                                                                                                                                     '|:|
                                                                                                                                                                                     (CONS
                                                                                                                                                                                      A
                                                                                                                                                                                      NIL)))
                                                                                                                                                                                   V3749)
                                                                                                                                                                                  V3750
                                                                                                                                                                                  V3751))))))))))
                                                                                                                                                                      (IF (shen-cl.true?
                                                                                                                                                                           (shen.pvar?
                                                                                                                                                                            V3638))
                                                                                                                                                                          (do
                                                                                                                                                                           (shen.bindv
                                                                                                                                                                            V3638
                                                                                                                                                                            NIL
                                                                                                                                                                            V3750)
                                                                                                                                                                           (LET ((Result
                                                                                                                                                                                  (LET ((Z
                                                                                                                                                                                         (shen.newpv
                                                                                                                                                                                          V3750)))
                                                                                                                                                                                    (LET ((X&&
                                                                                                                                                                                           (shen.newpv
                                                                                                                                                                                            V3750)))
                                                                                                                                                                                      (do
                                                                                                                                                                                       (shen.incinfs)
                                                                                                                                                                                       (cut
                                                                                                                                                                                        Throwcontrol
                                                                                                                                                                                        V3750
                                                                                                                                                                                        (freeze
                                                                                                                                                                                         (bind
                                                                                                                                                                                          X&&
                                                                                                                                                                                          (shen.placeholder)
                                                                                                                                                                                          V3750
                                                                                                                                                                                          (freeze
                                                                                                                                                                                           (bind
                                                                                                                                                                                            Z
                                                                                                                                                                                            (shen.ebr
                                                                                                                                                                                             (shen.lazyderef
                                                                                                                                                                                              X&&
                                                                                                                                                                                              V3750)
                                                                                                                                                                                             (shen.lazyderef
                                                                                                                                                                                              X
                                                                                                                                                                                              V3750)
                                                                                                                                                                                             (shen.lazyderef
                                                                                                                                                                                              Y
                                                                                                                                                                                              V3750))
                                                                                                                                                                                            V3750
                                                                                                                                                                                            (freeze
                                                                                                                                                                                             (shen.th*
                                                                                                                                                                                              Z
                                                                                                                                                                                              B
                                                                                                                                                                                              (CONS
                                                                                                                                                                                               (CONS
                                                                                                                                                                                                X&&
                                                                                                                                                                                                (CONS
                                                                                                                                                                                                 '|:|
                                                                                                                                                                                                 (CONS
                                                                                                                                                                                                  A
                                                                                                                                                                                                  NIL)))
                                                                                                                                                                                               V3749)
                                                                                                                                                                                              V3750
                                                                                                                                                                                              V3751))))))))))))
                                                                                                                                                                             (do
                                                                                                                                                                              (shen.unbindv
                                                                                                                                                                               V3638
                                                                                                                                                                               V3750)
                                                                                                                                                                              Result)))
                                                                                                                                                                          'false))))
                                                                                                                                                              (IF (shen-cl.true?
                                                                                                                                                                   (shen.pvar?
                                                                                                                                                                    V3637))
                                                                                                                                                                  (LET ((B
                                                                                                                                                                         (shen.newpv
                                                                                                                                                                          V3750)))
                                                                                                                                                                    (do
                                                                                                                                                                     (shen.bindv
                                                                                                                                                                      V3637
                                                                                                                                                                      (CONS
                                                                                                                                                                       B
                                                                                                                                                                       NIL)
                                                                                                                                                                      V3750)
                                                                                                                                                                     (LET ((Result
                                                                                                                                                                            (LET ((Z
                                                                                                                                                                                   (shen.newpv
                                                                                                                                                                                    V3750)))
                                                                                                                                                                              (LET ((X&&
                                                                                                                                                                                     (shen.newpv
                                                                                                                                                                                      V3750)))
                                                                                                                                                                                (do
                                                                                                                                                                                 (shen.incinfs)
                                                                                                                                                                                 (cut
                                                                                                                                                                                  Throwcontrol
                                                                                                                                                                                  V3750
                                                                                                                                                                                  (freeze
                                                                                                                                                                                   (bind
                                                                                                                                                                                    X&&
                                                                                                                                                                                    (shen.placeholder)
                                                                                                                                                                                    V3750
                                                                                                                                                                                    (freeze
                                                                                                                                                                                     (bind
                                                                                                                                                                                      Z
                                                                                                                                                                                      (shen.ebr
                                                                                                                                                                                       (shen.lazyderef
                                                                                                                                                                                        X&&
                                                                                                                                                                                        V3750)
                                                                                                                                                                                       (shen.lazyderef
                                                                                                                                                                                        X
                                                                                                                                                                                        V3750)
                                                                                                                                                                                       (shen.lazyderef
                                                                                                                                                                                        Y
                                                                                                                                                                                        V3750))
                                                                                                                                                                                      V3750
                                                                                                                                                                                      (freeze
                                                                                                                                                                                       (shen.th*
                                                                                                                                                                                        Z
                                                                                                                                                                                        B
                                                                                                                                                                                        (CONS
                                                                                                                                                                                         (CONS
                                                                                                                                                                                          X&&
                                                                                                                                                                                          (CONS
                                                                                                                                                                                           '|:|
                                                                                                                                                                                           (CONS
                                                                                                                                                                                            A
                                                                                                                                                                                            NIL)))
                                                                                                                                                                                         V3749)
                                                                                                                                                                                        V3750
                                                                                                                                                                                        V3751))))))))))))
                                                                                                                                                                       (do
                                                                                                                                                                        (shen.unbindv
                                                                                                                                                                         V3637
                                                                                                                                                                         V3750)
                                                                                                                                                                        Result))))
                                                                                                                                                                  'false)))))
                                                                                                                                                   (do
                                                                                                                                                    (shen.unbindv
                                                                                                                                                     V3634
                                                                                                                                                     V3750)
                                                                                                                                                    Result)))
                                                                                                                                                'false)))
                                                                                                                                      (IF (shen-cl.true?
                                                                                                                                           (shen.pvar?
                                                                                                                                            V3633))
                                                                                                                                          (LET ((B
                                                                                                                                                 (shen.newpv
                                                                                                                                                  V3750)))
                                                                                                                                            (do
                                                                                                                                             (shen.bindv
                                                                                                                                              V3633
                                                                                                                                              (CONS
                                                                                                                                               '-->
                                                                                                                                               (CONS
                                                                                                                                                B
                                                                                                                                                NIL))
                                                                                                                                              V3750)
                                                                                                                                             (LET ((Result
                                                                                                                                                    (LET ((Z
                                                                                                                                                           (shen.newpv
                                                                                                                                                            V3750)))
                                                                                                                                                      (LET ((X&&
                                                                                                                                                             (shen.newpv
                                                                                                                                                              V3750)))
                                                                                                                                                        (do
                                                                                                                                                         (shen.incinfs)
                                                                                                                                                         (cut
                                                                                                                                                          Throwcontrol
                                                                                                                                                          V3750
                                                                                                                                                          (freeze
                                                                                                                                                           (bind
                                                                                                                                                            X&&
                                                                                                                                                            (shen.placeholder)
                                                                                                                                                            V3750
                                                                                                                                                            (freeze
                                                                                                                                                             (bind
                                                                                                                                                              Z
                                                                                                                                                              (shen.ebr
                                                                                                                                                               (shen.lazyderef
                                                                                                                                                                X&&
                                                                                                                                                                V3750)
                                                                                                                                                               (shen.lazyderef
                                                                                                                                                                X
                                                                                                                                                                V3750)
                                                                                                                                                               (shen.lazyderef
                                                                                                                                                                Y
                                                                                                                                                                V3750))
                                                                                                                                                              V3750
                                                                                                                                                              (freeze
                                                                                                                                                               (shen.th*
                                                                                                                                                                Z
                                                                                                                                                                B
                                                                                                                                                                (CONS
                                                                                                                                                                 (CONS
                                                                                                                                                                  X&&
                                                                                                                                                                  (CONS
                                                                                                                                                                   '|:|
                                                                                                                                                                   (CONS
                                                                                                                                                                    A
                                                                                                                                                                    NIL)))
                                                                                                                                                                 V3749)
                                                                                                                                                                V3750
                                                                                                                                                                V3751))))))))))))
                                                                                                                                               (do
                                                                                                                                                (shen.unbindv
                                                                                                                                                 V3633
                                                                                                                                                 V3750)
                                                                                                                                                Result))))
                                                                                                                                          'false))))
                                                                                                                              (IF (shen-cl.true?
                                                                                                                                   (shen.pvar?
                                                                                                                                    V3632))
                                                                                                                                  (LET ((A
                                                                                                                                         (shen.newpv
                                                                                                                                          V3750)))
                                                                                                                                    (LET ((B
                                                                                                                                           (shen.newpv
                                                                                                                                            V3750)))
                                                                                                                                      (do
                                                                                                                                       (shen.bindv
                                                                                                                                        V3632
                                                                                                                                        (CONS
                                                                                                                                         A
                                                                                                                                         (CONS
                                                                                                                                          '-->
                                                                                                                                          (CONS
                                                                                                                                           B
                                                                                                                                           NIL)))
                                                                                                                                        V3750)
                                                                                                                                       (LET ((Result
                                                                                                                                              (LET ((Z
                                                                                                                                                     (shen.newpv
                                                                                                                                                      V3750)))
                                                                                                                                                (LET ((X&&
                                                                                                                                                       (shen.newpv
                                                                                                                                                        V3750)))
                                                                                                                                                  (do
                                                                                                                                                   (shen.incinfs)
                                                                                                                                                   (cut
                                                                                                                                                    Throwcontrol
                                                                                                                                                    V3750
                                                                                                                                                    (freeze
                                                                                                                                                     (bind
                                                                                                                                                      X&&
                                                                                                                                                      (shen.placeholder)
                                                                                                                                                      V3750
                                                                                                                                                      (freeze
                                                                                                                                                       (bind
                                                                                                                                                        Z
                                                                                                                                                        (shen.ebr
                                                                                                                                                         (shen.lazyderef
                                                                                                                                                          X&&
                                                                                                                                                          V3750)
                                                                                                                                                         (shen.lazyderef
                                                                                                                                                          X
                                                                                                                                                          V3750)
                                                                                                                                                         (shen.lazyderef
                                                                                                                                                          Y
                                                                                                                                                          V3750))
                                                                                                                                                        V3750
                                                                                                                                                        (freeze
                                                                                                                                                         (shen.th*
                                                                                                                                                          Z
                                                                                                                                                          B
                                                                                                                                                          (CONS
                                                                                                                                                           (CONS
                                                                                                                                                            X&&
                                                                                                                                                            (CONS
                                                                                                                                                             '|:|
                                                                                                                                                             (CONS
                                                                                                                                                              A
                                                                                                                                                              NIL)))
                                                                                                                                                           V3749)
                                                                                                                                                          V3750
                                                                                                                                                          V3751))))))))))))
                                                                                                                                         (do
                                                                                                                                          (shen.unbindv
                                                                                                                                           V3632
                                                                                                                                           V3750)
                                                                                                                                          Result)))))
                                                                                                                                  'false)))
                                                                                                                        'false)))
                                                                                                                'false)))
                                                                                                        'false))
                                                                                                  'false))
                                                                                            'false))))
                                                                                 (IF (EQ
                                                                                      Case
                                                                                      'false)
                                                                                     (LET ((Case
                                                                                            (LET ((V3639
                                                                                                   (shen.lazyderef
                                                                                                    V3747
                                                                                                    V3750)))
                                                                                              (IF (CONSP
                                                                                                   V3639)
                                                                                                  (LET ((V3640
                                                                                                         (shen.lazyderef
                                                                                                          (CAR
                                                                                                           V3639)
                                                                                                          V3750)))
                                                                                                    (IF (EQ
                                                                                                         'let
                                                                                                         V3640)
                                                                                                        (LET ((V3641
                                                                                                               (shen.lazyderef
                                                                                                                (CDR
                                                                                                                 V3639)
                                                                                                                V3750)))
                                                                                                          (IF (CONSP
                                                                                                               V3641)
                                                                                                              (LET ((X
                                                                                                                     (CAR
                                                                                                                      V3641)))
                                                                                                                (LET ((V3642
                                                                                                                       (shen.lazyderef
                                                                                                                        (CDR
                                                                                                                         V3641)
                                                                                                                        V3750)))
                                                                                                                  (IF (CONSP
                                                                                                                       V3642)
                                                                                                                      (LET ((Y
                                                                                                                             (CAR
                                                                                                                              V3642)))
                                                                                                                        (LET ((V3643
                                                                                                                               (shen.lazyderef
                                                                                                                                (CDR
                                                                                                                                 V3642)
                                                                                                                                V3750)))
                                                                                                                          (IF (CONSP
                                                                                                                               V3643)
                                                                                                                              (LET ((Z
                                                                                                                                     (CAR
                                                                                                                                      V3643)))
                                                                                                                                (LET ((V3644
                                                                                                                                       (shen.lazyderef
                                                                                                                                        (CDR
                                                                                                                                         V3643)
                                                                                                                                        V3750)))
                                                                                                                                  (IF (NULL
                                                                                                                                       V3644)
                                                                                                                                      (LET ((W
                                                                                                                                             (shen.newpv
                                                                                                                                              V3750)))
                                                                                                                                        (LET ((X&&
                                                                                                                                               (shen.newpv
                                                                                                                                                V3750)))
                                                                                                                                          (LET ((B
                                                                                                                                                 (shen.newpv
                                                                                                                                                  V3750)))
                                                                                                                                            (do
                                                                                                                                             (shen.incinfs)
                                                                                                                                             (shen.th*
                                                                                                                                              Y
                                                                                                                                              B
                                                                                                                                              V3749
                                                                                                                                              V3750
                                                                                                                                              (freeze
                                                                                                                                               (bind
                                                                                                                                                X&&
                                                                                                                                                (shen.placeholder)
                                                                                                                                                V3750
                                                                                                                                                (freeze
                                                                                                                                                 (bind
                                                                                                                                                  W
                                                                                                                                                  (shen.ebr
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    X&&
                                                                                                                                                    V3750)
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    X
                                                                                                                                                    V3750)
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    Z
                                                                                                                                                    V3750))
                                                                                                                                                  V3750
                                                                                                                                                  (freeze
                                                                                                                                                   (shen.th*
                                                                                                                                                    W
                                                                                                                                                    V3748
                                                                                                                                                    (CONS
                                                                                                                                                     (CONS
                                                                                                                                                      X&&
                                                                                                                                                      (CONS
                                                                                                                                                       '|:|
                                                                                                                                                       (CONS
                                                                                                                                                        B
                                                                                                                                                        NIL)))
                                                                                                                                                     V3749)
                                                                                                                                                    V3750
                                                                                                                                                    V3751)))))))))))
                                                                                                                                      'false)))
                                                                                                                              'false)))
                                                                                                                      'false)))
                                                                                                              'false))
                                                                                                        'false))
                                                                                                  'false))))
                                                                                       (IF (EQ
                                                                                            Case
                                                                                            'false)
                                                                                           (LET ((Case
                                                                                                  (LET ((V3645
                                                                                                         (shen.lazyderef
                                                                                                          V3747
                                                                                                          V3750)))
                                                                                                    (IF (CONSP
                                                                                                         V3645)
                                                                                                        (LET ((V3646
                                                                                                               (shen.lazyderef
                                                                                                                (CAR
                                                                                                                 V3645)
                                                                                                                V3750)))
                                                                                                          (IF (EQ
                                                                                                               'open
                                                                                                               V3646)
                                                                                                              (LET ((V3647
                                                                                                                     (shen.lazyderef
                                                                                                                      (CDR
                                                                                                                       V3645)
                                                                                                                      V3750)))
                                                                                                                (IF (CONSP
                                                                                                                     V3647)
                                                                                                                    (LET ((FileName
                                                                                                                           (CAR
                                                                                                                            V3647)))
                                                                                                                      (LET ((V3648
                                                                                                                             (shen.lazyderef
                                                                                                                              (CDR
                                                                                                                               V3647)
                                                                                                                              V3750)))
                                                                                                                        (IF (CONSP
                                                                                                                             V3648)
                                                                                                                            (LET ((Direction3578
                                                                                                                                   (CAR
                                                                                                                                    V3648)))
                                                                                                                              (LET ((V3649
                                                                                                                                     (shen.lazyderef
                                                                                                                                      (CDR
                                                                                                                                       V3648)
                                                                                                                                      V3750)))
                                                                                                                                (IF (NULL
                                                                                                                                     V3649)
                                                                                                                                    (LET ((V3650
                                                                                                                                           (shen.lazyderef
                                                                                                                                            V3748
                                                                                                                                            V3750)))
                                                                                                                                      (IF (CONSP
                                                                                                                                           V3650)
                                                                                                                                          (LET ((V3651
                                                                                                                                                 (shen.lazyderef
                                                                                                                                                  (CAR
                                                                                                                                                   V3650)
                                                                                                                                                  V3750)))
                                                                                                                                            (IF (EQ
                                                                                                                                                 'stream
                                                                                                                                                 V3651)
                                                                                                                                                (LET ((V3652
                                                                                                                                                       (shen.lazyderef
                                                                                                                                                        (CDR
                                                                                                                                                         V3650)
                                                                                                                                                        V3750)))
                                                                                                                                                  (IF (CONSP
                                                                                                                                                       V3652)
                                                                                                                                                      (LET ((Direction
                                                                                                                                                             (CAR
                                                                                                                                                              V3652)))
                                                                                                                                                        (LET ((V3653
                                                                                                                                                               (shen.lazyderef
                                                                                                                                                                (CDR
                                                                                                                                                                 V3652)
                                                                                                                                                                V3750)))
                                                                                                                                                          (IF (NULL
                                                                                                                                                               V3653)
                                                                                                                                                              (do
                                                                                                                                                               (shen.incinfs)
                                                                                                                                                               (unify!
                                                                                                                                                                Direction
                                                                                                                                                                Direction3578
                                                                                                                                                                V3750
                                                                                                                                                                (freeze
                                                                                                                                                                 (cut
                                                                                                                                                                  Throwcontrol
                                                                                                                                                                  V3750
                                                                                                                                                                  (freeze
                                                                                                                                                                   (fwhen
                                                                                                                                                                    (element?
                                                                                                                                                                     (shen.lazyderef
                                                                                                                                                                      Direction
                                                                                                                                                                      V3750)
                                                                                                                                                                     (CONS
                                                                                                                                                                      'in
                                                                                                                                                                      (CONS
                                                                                                                                                                       'out
                                                                                                                                                                       NIL)))
                                                                                                                                                                    V3750
                                                                                                                                                                    (freeze
                                                                                                                                                                     (shen.th*
                                                                                                                                                                      FileName
                                                                                                                                                                      'string
                                                                                                                                                                      V3749
                                                                                                                                                                      V3750
                                                                                                                                                                      V3751))))))))
                                                                                                                                                              (IF (shen-cl.true?
                                                                                                                                                                   (shen.pvar?
                                                                                                                                                                    V3653))
                                                                                                                                                                  (do
                                                                                                                                                                   (shen.bindv
                                                                                                                                                                    V3653
                                                                                                                                                                    NIL
                                                                                                                                                                    V3750)
                                                                                                                                                                   (LET ((Result
                                                                                                                                                                          (do
                                                                                                                                                                           (shen.incinfs)
                                                                                                                                                                           (unify!
                                                                                                                                                                            Direction
                                                                                                                                                                            Direction3578
                                                                                                                                                                            V3750
                                                                                                                                                                            (freeze
                                                                                                                                                                             (cut
                                                                                                                                                                              Throwcontrol
                                                                                                                                                                              V3750
                                                                                                                                                                              (freeze
                                                                                                                                                                               (fwhen
                                                                                                                                                                                (element?
                                                                                                                                                                                 (shen.lazyderef
                                                                                                                                                                                  Direction
                                                                                                                                                                                  V3750)
                                                                                                                                                                                 (CONS
                                                                                                                                                                                  'in
                                                                                                                                                                                  (CONS
                                                                                                                                                                                   'out
                                                                                                                                                                                   NIL)))
                                                                                                                                                                                V3750
                                                                                                                                                                                (freeze
                                                                                                                                                                                 (shen.th*
                                                                                                                                                                                  FileName
                                                                                                                                                                                  'string
                                                                                                                                                                                  V3749
                                                                                                                                                                                  V3750
                                                                                                                                                                                  V3751))))))))))
                                                                                                                                                                     (do
                                                                                                                                                                      (shen.unbindv
                                                                                                                                                                       V3653
                                                                                                                                                                       V3750)
                                                                                                                                                                      Result)))
                                                                                                                                                                  'false))))
                                                                                                                                                      (IF (shen-cl.true?
                                                                                                                                                           (shen.pvar?
                                                                                                                                                            V3652))
                                                                                                                                                          (LET ((Direction
                                                                                                                                                                 (shen.newpv
                                                                                                                                                                  V3750)))
                                                                                                                                                            (do
                                                                                                                                                             (shen.bindv
                                                                                                                                                              V3652
                                                                                                                                                              (CONS
                                                                                                                                                               Direction
                                                                                                                                                               NIL)
                                                                                                                                                              V3750)
                                                                                                                                                             (LET ((Result
                                                                                                                                                                    (do
                                                                                                                                                                     (shen.incinfs)
                                                                                                                                                                     (unify!
                                                                                                                                                                      Direction
                                                                                                                                                                      Direction3578
                                                                                                                                                                      V3750
                                                                                                                                                                      (freeze
                                                                                                                                                                       (cut
                                                                                                                                                                        Throwcontrol
                                                                                                                                                                        V3750
                                                                                                                                                                        (freeze
                                                                                                                                                                         (fwhen
                                                                                                                                                                          (element?
                                                                                                                                                                           (shen.lazyderef
                                                                                                                                                                            Direction
                                                                                                                                                                            V3750)
                                                                                                                                                                           (CONS
                                                                                                                                                                            'in
                                                                                                                                                                            (CONS
                                                                                                                                                                             'out
                                                                                                                                                                             NIL)))
                                                                                                                                                                          V3750
                                                                                                                                                                          (freeze
                                                                                                                                                                           (shen.th*
                                                                                                                                                                            FileName
                                                                                                                                                                            'string
                                                                                                                                                                            V3749
                                                                                                                                                                            V3750
                                                                                                                                                                            V3751))))))))))
                                                                                                                                                               (do
                                                                                                                                                                (shen.unbindv
                                                                                                                                                                 V3652
                                                                                                                                                                 V3750)
                                                                                                                                                                Result))))
                                                                                                                                                          'false)))
                                                                                                                                                (IF (shen-cl.true?
                                                                                                                                                     (shen.pvar?
                                                                                                                                                      V3651))
                                                                                                                                                    (do
                                                                                                                                                     (shen.bindv
                                                                                                                                                      V3651
                                                                                                                                                      'stream
                                                                                                                                                      V3750)
                                                                                                                                                     (LET ((Result
                                                                                                                                                            (LET ((V3654
                                                                                                                                                                   (shen.lazyderef
                                                                                                                                                                    (CDR
                                                                                                                                                                     V3650)
                                                                                                                                                                    V3750)))
                                                                                                                                                              (IF (CONSP
                                                                                                                                                                   V3654)
                                                                                                                                                                  (LET ((Direction
                                                                                                                                                                         (CAR
                                                                                                                                                                          V3654)))
                                                                                                                                                                    (LET ((V3655
                                                                                                                                                                           (shen.lazyderef
                                                                                                                                                                            (CDR
                                                                                                                                                                             V3654)
                                                                                                                                                                            V3750)))
                                                                                                                                                                      (IF (NULL
                                                                                                                                                                           V3655)
                                                                                                                                                                          (do
                                                                                                                                                                           (shen.incinfs)
                                                                                                                                                                           (unify!
                                                                                                                                                                            Direction
                                                                                                                                                                            Direction3578
                                                                                                                                                                            V3750
                                                                                                                                                                            (freeze
                                                                                                                                                                             (cut
                                                                                                                                                                              Throwcontrol
                                                                                                                                                                              V3750
                                                                                                                                                                              (freeze
                                                                                                                                                                               (fwhen
                                                                                                                                                                                (element?
                                                                                                                                                                                 (shen.lazyderef
                                                                                                                                                                                  Direction
                                                                                                                                                                                  V3750)
                                                                                                                                                                                 (CONS
                                                                                                                                                                                  'in
                                                                                                                                                                                  (CONS
                                                                                                                                                                                   'out
                                                                                                                                                                                   NIL)))
                                                                                                                                                                                V3750
                                                                                                                                                                                (freeze
                                                                                                                                                                                 (shen.th*
                                                                                                                                                                                  FileName
                                                                                                                                                                                  'string
                                                                                                                                                                                  V3749
                                                                                                                                                                                  V3750
                                                                                                                                                                                  V3751))))))))
                                                                                                                                                                          (IF (shen-cl.true?
                                                                                                                                                                               (shen.pvar?
                                                                                                                                                                                V3655))
                                                                                                                                                                              (do
                                                                                                                                                                               (shen.bindv
                                                                                                                                                                                V3655
                                                                                                                                                                                NIL
                                                                                                                                                                                V3750)
                                                                                                                                                                               (LET ((Result
                                                                                                                                                                                      (do
                                                                                                                                                                                       (shen.incinfs)
                                                                                                                                                                                       (unify!
                                                                                                                                                                                        Direction
                                                                                                                                                                                        Direction3578
                                                                                                                                                                                        V3750
                                                                                                                                                                                        (freeze
                                                                                                                                                                                         (cut
                                                                                                                                                                                          Throwcontrol
                                                                                                                                                                                          V3750
                                                                                                                                                                                          (freeze
                                                                                                                                                                                           (fwhen
                                                                                                                                                                                            (element?
                                                                                                                                                                                             (shen.lazyderef
                                                                                                                                                                                              Direction
                                                                                                                                                                                              V3750)
                                                                                                                                                                                             (CONS
                                                                                                                                                                                              'in
                                                                                                                                                                                              (CONS
                                                                                                                                                                                               'out
                                                                                                                                                                                               NIL)))
                                                                                                                                                                                            V3750
                                                                                                                                                                                            (freeze
                                                                                                                                                                                             (shen.th*
                                                                                                                                                                                              FileName
                                                                                                                                                                                              'string
                                                                                                                                                                                              V3749
                                                                                                                                                                                              V3750
                                                                                                                                                                                              V3751))))))))))
                                                                                                                                                                                 (do
                                                                                                                                                                                  (shen.unbindv
                                                                                                                                                                                   V3655
                                                                                                                                                                                   V3750)
                                                                                                                                                                                  Result)))
                                                                                                                                                                              'false))))
                                                                                                                                                                  (IF (shen-cl.true?
                                                                                                                                                                       (shen.pvar?
                                                                                                                                                                        V3654))
                                                                                                                                                                      (LET ((Direction
                                                                                                                                                                             (shen.newpv
                                                                                                                                                                              V3750)))
                                                                                                                                                                        (do
                                                                                                                                                                         (shen.bindv
                                                                                                                                                                          V3654
                                                                                                                                                                          (CONS
                                                                                                                                                                           Direction
                                                                                                                                                                           NIL)
                                                                                                                                                                          V3750)
                                                                                                                                                                         (LET ((Result
                                                                                                                                                                                (do
                                                                                                                                                                                 (shen.incinfs)
                                                                                                                                                                                 (unify!
                                                                                                                                                                                  Direction
                                                                                                                                                                                  Direction3578
                                                                                                                                                                                  V3750
                                                                                                                                                                                  (freeze
                                                                                                                                                                                   (cut
                                                                                                                                                                                    Throwcontrol
                                                                                                                                                                                    V3750
                                                                                                                                                                                    (freeze
                                                                                                                                                                                     (fwhen
                                                                                                                                                                                      (element?
                                                                                                                                                                                       (shen.lazyderef
                                                                                                                                                                                        Direction
                                                                                                                                                                                        V3750)
                                                                                                                                                                                       (CONS
                                                                                                                                                                                        'in
                                                                                                                                                                                        (CONS
                                                                                                                                                                                         'out
                                                                                                                                                                                         NIL)))
                                                                                                                                                                                      V3750
                                                                                                                                                                                      (freeze
                                                                                                                                                                                       (shen.th*
                                                                                                                                                                                        FileName
                                                                                                                                                                                        'string
                                                                                                                                                                                        V3749
                                                                                                                                                                                        V3750
                                                                                                                                                                                        V3751))))))))))
                                                                                                                                                                           (do
                                                                                                                                                                            (shen.unbindv
                                                                                                                                                                             V3654
                                                                                                                                                                             V3750)
                                                                                                                                                                            Result))))
                                                                                                                                                                      'false)))))
                                                                                                                                                       (do
                                                                                                                                                        (shen.unbindv
                                                                                                                                                         V3651
                                                                                                                                                         V3750)
                                                                                                                                                        Result)))
                                                                                                                                                    'false)))
                                                                                                                                          (IF (shen-cl.true?
                                                                                                                                               (shen.pvar?
                                                                                                                                                V3650))
                                                                                                                                              (LET ((Direction
                                                                                                                                                     (shen.newpv
                                                                                                                                                      V3750)))
                                                                                                                                                (do
                                                                                                                                                 (shen.bindv
                                                                                                                                                  V3650
                                                                                                                                                  (CONS
                                                                                                                                                   'stream
                                                                                                                                                   (CONS
                                                                                                                                                    Direction
                                                                                                                                                    NIL))
                                                                                                                                                  V3750)
                                                                                                                                                 (LET ((Result
                                                                                                                                                        (do
                                                                                                                                                         (shen.incinfs)
                                                                                                                                                         (unify!
                                                                                                                                                          Direction
                                                                                                                                                          Direction3578
                                                                                                                                                          V3750
                                                                                                                                                          (freeze
                                                                                                                                                           (cut
                                                                                                                                                            Throwcontrol
                                                                                                                                                            V3750
                                                                                                                                                            (freeze
                                                                                                                                                             (fwhen
                                                                                                                                                              (element?
                                                                                                                                                               (shen.lazyderef
                                                                                                                                                                Direction
                                                                                                                                                                V3750)
                                                                                                                                                               (CONS
                                                                                                                                                                'in
                                                                                                                                                                (CONS
                                                                                                                                                                 'out
                                                                                                                                                                 NIL)))
                                                                                                                                                              V3750
                                                                                                                                                              (freeze
                                                                                                                                                               (shen.th*
                                                                                                                                                                FileName
                                                                                                                                                                'string
                                                                                                                                                                V3749
                                                                                                                                                                V3750
                                                                                                                                                                V3751))))))))))
                                                                                                                                                   (do
                                                                                                                                                    (shen.unbindv
                                                                                                                                                     V3650
                                                                                                                                                     V3750)
                                                                                                                                                    Result))))
                                                                                                                                              'false)))
                                                                                                                                    'false)))
                                                                                                                            'false)))
                                                                                                                    'false))
                                                                                                              'false))
                                                                                                        'false))))
                                                                                             (IF (EQ
                                                                                                  Case
                                                                                                  'false)
                                                                                                 (LET ((Case
                                                                                                        (LET ((V3656
                                                                                                               (shen.lazyderef
                                                                                                                V3747
                                                                                                                V3750)))
                                                                                                          (IF (CONSP
                                                                                                               V3656)
                                                                                                              (LET ((V3657
                                                                                                                     (shen.lazyderef
                                                                                                                      (CAR
                                                                                                                       V3656)
                                                                                                                      V3750)))
                                                                                                                (IF (EQ
                                                                                                                     'type
                                                                                                                     V3657)
                                                                                                                    (LET ((V3658
                                                                                                                           (shen.lazyderef
                                                                                                                            (CDR
                                                                                                                             V3656)
                                                                                                                            V3750)))
                                                                                                                      (IF (CONSP
                                                                                                                           V3658)
                                                                                                                          (LET ((X
                                                                                                                                 (CAR
                                                                                                                                  V3658)))
                                                                                                                            (LET ((V3659
                                                                                                                                   (shen.lazyderef
                                                                                                                                    (CDR
                                                                                                                                     V3658)
                                                                                                                                    V3750)))
                                                                                                                              (IF (CONSP
                                                                                                                                   V3659)
                                                                                                                                  (LET ((A
                                                                                                                                         (CAR
                                                                                                                                          V3659)))
                                                                                                                                    (LET ((V3660
                                                                                                                                           (shen.lazyderef
                                                                                                                                            (CDR
                                                                                                                                             V3659)
                                                                                                                                            V3750)))
                                                                                                                                      (IF (NULL
                                                                                                                                           V3660)
                                                                                                                                          (do
                                                                                                                                           (shen.incinfs)
                                                                                                                                           (cut
                                                                                                                                            Throwcontrol
                                                                                                                                            V3750
                                                                                                                                            (freeze
                                                                                                                                             (unify
                                                                                                                                              A
                                                                                                                                              V3748
                                                                                                                                              V3750
                                                                                                                                              (freeze
                                                                                                                                               (shen.th*
                                                                                                                                                X
                                                                                                                                                A
                                                                                                                                                V3749
                                                                                                                                                V3750
                                                                                                                                                V3751))))))
                                                                                                                                          'false)))
                                                                                                                                  'false)))
                                                                                                                          'false))
                                                                                                                    'false))
                                                                                                              'false))))
                                                                                                   (IF (EQ
                                                                                                        Case
                                                                                                        'false)
                                                                                                       (LET ((Case
                                                                                                              (LET ((V3661
                                                                                                                     (shen.lazyderef
                                                                                                                      V3747
                                                                                                                      V3750)))
                                                                                                                (IF (CONSP
                                                                                                                     V3661)
                                                                                                                    (LET ((V3662
                                                                                                                           (shen.lazyderef
                                                                                                                            (CAR
                                                                                                                             V3661)
                                                                                                                            V3750)))
                                                                                                                      (IF (EQ
                                                                                                                           'input+
                                                                                                                           V3662)
                                                                                                                          (LET ((V3663
                                                                                                                                 (shen.lazyderef
                                                                                                                                  (CDR
                                                                                                                                   V3661)
                                                                                                                                  V3750)))
                                                                                                                            (IF (CONSP
                                                                                                                                 V3663)
                                                                                                                                (LET ((A
                                                                                                                                       (CAR
                                                                                                                                        V3663)))
                                                                                                                                  (LET ((V3664
                                                                                                                                         (shen.lazyderef
                                                                                                                                          (CDR
                                                                                                                                           V3663)
                                                                                                                                          V3750)))
                                                                                                                                    (IF (CONSP
                                                                                                                                         V3664)
                                                                                                                                        (LET ((Stream
                                                                                                                                               (CAR
                                                                                                                                                V3664)))
                                                                                                                                          (LET ((V3665
                                                                                                                                                 (shen.lazyderef
                                                                                                                                                  (CDR
                                                                                                                                                   V3664)
                                                                                                                                                  V3750)))
                                                                                                                                            (IF (NULL
                                                                                                                                                 V3665)
                                                                                                                                                (LET ((C
                                                                                                                                                       (shen.newpv
                                                                                                                                                        V3750)))
                                                                                                                                                  (do
                                                                                                                                                   (shen.incinfs)
                                                                                                                                                   (bind
                                                                                                                                                    C
                                                                                                                                                    (shen.demodulate
                                                                                                                                                     (shen.lazyderef
                                                                                                                                                      A
                                                                                                                                                      V3750))
                                                                                                                                                    V3750
                                                                                                                                                    (freeze
                                                                                                                                                     (unify
                                                                                                                                                      V3748
                                                                                                                                                      C
                                                                                                                                                      V3750
                                                                                                                                                      (freeze
                                                                                                                                                       (shen.th*
                                                                                                                                                        Stream
                                                                                                                                                        (CONS
                                                                                                                                                         'stream
                                                                                                                                                         (CONS
                                                                                                                                                          'in
                                                                                                                                                          NIL))
                                                                                                                                                        V3749
                                                                                                                                                        V3750
                                                                                                                                                        V3751)))))))
                                                                                                                                                'false)))
                                                                                                                                        'false)))
                                                                                                                                'false))
                                                                                                                          'false))
                                                                                                                    'false))))
                                                                                                         (IF (EQ
                                                                                                              Case
                                                                                                              'false)
                                                                                                             (LET ((Case
                                                                                                                    (LET ((V3666
                                                                                                                           (shen.lazyderef
                                                                                                                            V3747
                                                                                                                            V3750)))
                                                                                                                      (IF (CONSP
                                                                                                                           V3666)
                                                                                                                          (LET ((V3667
                                                                                                                                 (shen.lazyderef
                                                                                                                                  (CAR
                                                                                                                                   V3666)
                                                                                                                                  V3750)))
                                                                                                                            (IF (EQ
                                                                                                                                 'set
                                                                                                                                 V3667)
                                                                                                                                (LET ((V3668
                                                                                                                                       (shen.lazyderef
                                                                                                                                        (CDR
                                                                                                                                         V3666)
                                                                                                                                        V3750)))
                                                                                                                                  (IF (CONSP
                                                                                                                                       V3668)
                                                                                                                                      (LET ((Var
                                                                                                                                             (CAR
                                                                                                                                              V3668)))
                                                                                                                                        (LET ((V3669
                                                                                                                                               (shen.lazyderef
                                                                                                                                                (CDR
                                                                                                                                                 V3668)
                                                                                                                                                V3750)))
                                                                                                                                          (IF (CONSP
                                                                                                                                               V3669)
                                                                                                                                              (LET ((Val
                                                                                                                                                     (CAR
                                                                                                                                                      V3669)))
                                                                                                                                                (LET ((V3670
                                                                                                                                                       (shen.lazyderef
                                                                                                                                                        (CDR
                                                                                                                                                         V3669)
                                                                                                                                                        V3750)))
                                                                                                                                                  (IF (NULL
                                                                                                                                                       V3670)
                                                                                                                                                      (do
                                                                                                                                                       (shen.incinfs)
                                                                                                                                                       (cut
                                                                                                                                                        Throwcontrol
                                                                                                                                                        V3750
                                                                                                                                                        (freeze
                                                                                                                                                         (shen.th*
                                                                                                                                                          Var
                                                                                                                                                          'symbol
                                                                                                                                                          V3749
                                                                                                                                                          V3750
                                                                                                                                                          (freeze
                                                                                                                                                           (cut
                                                                                                                                                            Throwcontrol
                                                                                                                                                            V3750
                                                                                                                                                            (freeze
                                                                                                                                                             (shen.th*
                                                                                                                                                              (CONS
                                                                                                                                                               'value
                                                                                                                                                               (CONS
                                                                                                                                                                Var
                                                                                                                                                                NIL))
                                                                                                                                                              V3748
                                                                                                                                                              V3749
                                                                                                                                                              V3750
                                                                                                                                                              (freeze
                                                                                                                                                               (shen.th*
                                                                                                                                                                Val
                                                                                                                                                                V3748
                                                                                                                                                                V3749
                                                                                                                                                                V3750
                                                                                                                                                                V3751))))))))))
                                                                                                                                                      'false)))
                                                                                                                                              'false)))
                                                                                                                                      'false))
                                                                                                                                'false))
                                                                                                                          'false))))
                                                                                                               (IF (EQ
                                                                                                                    Case
                                                                                                                    'false)
                                                                                                                   (LET ((Case
                                                                                                                          (LET ((NewHyp
                                                                                                                                 (shen.newpv
                                                                                                                                  V3750)))
                                                                                                                            (do
                                                                                                                             (shen.incinfs)
                                                                                                                             (shen.t*-hyps
                                                                                                                              V3749
                                                                                                                              NewHyp
                                                                                                                              V3750
                                                                                                                              (freeze
                                                                                                                               (shen.th*
                                                                                                                                V3747
                                                                                                                                V3748
                                                                                                                                NewHyp
                                                                                                                                V3750
                                                                                                                                V3751)))))))
                                                                                                                     (IF (EQ
                                                                                                                          Case
                                                                                                                          'false)
                                                                                                                         (LET ((Case
                                                                                                                                (LET ((V3671
                                                                                                                                       (shen.lazyderef
                                                                                                                                        V3747
                                                                                                                                        V3750)))
                                                                                                                                  (IF (CONSP
                                                                                                                                       V3671)
                                                                                                                                      (LET ((V3672
                                                                                                                                             (shen.lazyderef
                                                                                                                                              (CAR
                                                                                                                                               V3671)
                                                                                                                                              V3750)))
                                                                                                                                        (IF (EQ
                                                                                                                                             'define
                                                                                                                                             V3672)
                                                                                                                                            (LET ((V3673
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    (CDR
                                                                                                                                                     V3671)
                                                                                                                                                    V3750)))
                                                                                                                                              (IF (CONSP
                                                                                                                                                   V3673)
                                                                                                                                                  (LET ((F
                                                                                                                                                         (CAR
                                                                                                                                                          V3673)))
                                                                                                                                                    (LET ((X
                                                                                                                                                           (CDR
                                                                                                                                                            V3673)))
                                                                                                                                                      (do
                                                                                                                                                       (shen.incinfs)
                                                                                                                                                       (cut
                                                                                                                                                        Throwcontrol
                                                                                                                                                        V3750
                                                                                                                                                        (freeze
                                                                                                                                                         (shen.t*-def
                                                                                                                                                          (CONS
                                                                                                                                                           'define
                                                                                                                                                           (CONS
                                                                                                                                                            F
                                                                                                                                                            X))
                                                                                                                                                          V3748
                                                                                                                                                          V3749
                                                                                                                                                          V3750
                                                                                                                                                          V3751))))))
                                                                                                                                                  'false))
                                                                                                                                            'false))
                                                                                                                                      'false))))
                                                                                                                           (IF (EQ
                                                                                                                                Case
                                                                                                                                'false)
                                                                                                                               (LET ((Case
                                                                                                                                      (LET ((V3674
                                                                                                                                             (shen.lazyderef
                                                                                                                                              V3747
                                                                                                                                              V3750)))
                                                                                                                                        (IF (CONSP
                                                                                                                                             V3674)
                                                                                                                                            (LET ((V3675
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    (CAR
                                                                                                                                                     V3674)
                                                                                                                                                    V3750)))
                                                                                                                                              (IF (EQ
                                                                                                                                                   'defmacro
                                                                                                                                                   V3675)
                                                                                                                                                  (LET ((V3676
                                                                                                                                                         (shen.lazyderef
                                                                                                                                                          V3748
                                                                                                                                                          V3750)))
                                                                                                                                                    (IF (EQ
                                                                                                                                                         'unit
                                                                                                                                                         V3676)
                                                                                                                                                        (do
                                                                                                                                                         (shen.incinfs)
                                                                                                                                                         (cut
                                                                                                                                                          Throwcontrol
                                                                                                                                                          V3750
                                                                                                                                                          V3751))
                                                                                                                                                        (IF (shen-cl.true?
                                                                                                                                                             (shen.pvar?
                                                                                                                                                              V3676))
                                                                                                                                                            (do
                                                                                                                                                             (shen.bindv
                                                                                                                                                              V3676
                                                                                                                                                              'unit
                                                                                                                                                              V3750)
                                                                                                                                                             (LET ((Result
                                                                                                                                                                    (do
                                                                                                                                                                     (shen.incinfs)
                                                                                                                                                                     (cut
                                                                                                                                                                      Throwcontrol
                                                                                                                                                                      V3750
                                                                                                                                                                      V3751))))
                                                                                                                                                               (do
                                                                                                                                                                (shen.unbindv
                                                                                                                                                                 V3676
                                                                                                                                                                 V3750)
                                                                                                                                                                Result)))
                                                                                                                                                            'false)))
                                                                                                                                                  'false))
                                                                                                                                            'false))))
                                                                                                                                 (IF (EQ
                                                                                                                                      Case
                                                                                                                                      'false)
                                                                                                                                     (LET ((Case
                                                                                                                                            (LET ((V3677
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    V3747
                                                                                                                                                    V3750)))
                                                                                                                                              (IF (CONSP
                                                                                                                                                   V3677)
                                                                                                                                                  (LET ((V3678
                                                                                                                                                         (shen.lazyderef
                                                                                                                                                          (CAR
                                                                                                                                                           V3677)
                                                                                                                                                          V3750)))
                                                                                                                                                    (IF (EQ
                                                                                                                                                         'shen.process-datatype
                                                                                                                                                         V3678)
                                                                                                                                                        (LET ((V3679
                                                                                                                                                               (shen.lazyderef
                                                                                                                                                                V3748
                                                                                                                                                                V3750)))
                                                                                                                                                          (IF (EQ
                                                                                                                                                               'symbol
                                                                                                                                                               V3679)
                                                                                                                                                              (do
                                                                                                                                                               (shen.incinfs)
                                                                                                                                                               (thaw
                                                                                                                                                                V3751))
                                                                                                                                                              (IF (shen-cl.true?
                                                                                                                                                                   (shen.pvar?
                                                                                                                                                                    V3679))
                                                                                                                                                                  (do
                                                                                                                                                                   (shen.bindv
                                                                                                                                                                    V3679
                                                                                                                                                                    'symbol
                                                                                                                                                                    V3750)
                                                                                                                                                                   (LET ((Result
                                                                                                                                                                          (do
                                                                                                                                                                           (shen.incinfs)
                                                                                                                                                                           (thaw
                                                                                                                                                                            V3751))))
                                                                                                                                                                     (do
                                                                                                                                                                      (shen.unbindv
                                                                                                                                                                       V3679
                                                                                                                                                                       V3750)
                                                                                                                                                                      Result)))
                                                                                                                                                                  'false)))
                                                                                                                                                        'false))
                                                                                                                                                  'false))))
                                                                                                                                       (IF (EQ
                                                                                                                                            Case
                                                                                                                                            'false)
                                                                                                                                           (LET ((Case
                                                                                                                                                  (LET ((V3680
                                                                                                                                                         (shen.lazyderef
                                                                                                                                                          V3747
                                                                                                                                                          V3750)))
                                                                                                                                                    (IF (CONSP
                                                                                                                                                         V3680)
                                                                                                                                                        (LET ((V3681
                                                                                                                                                               (shen.lazyderef
                                                                                                                                                                (CAR
                                                                                                                                                                 V3680)
                                                                                                                                                                V3750)))
                                                                                                                                                          (IF (EQ
                                                                                                                                                               'shen.synonyms-help
                                                                                                                                                               V3681)
                                                                                                                                                              (LET ((V3682
                                                                                                                                                                     (shen.lazyderef
                                                                                                                                                                      V3748
                                                                                                                                                                      V3750)))
                                                                                                                                                                (IF (EQ
                                                                                                                                                                     'symbol
                                                                                                                                                                     V3682)
                                                                                                                                                                    (do
                                                                                                                                                                     (shen.incinfs)
                                                                                                                                                                     (thaw
                                                                                                                                                                      V3751))
                                                                                                                                                                    (IF (shen-cl.true?
                                                                                                                                                                         (shen.pvar?
                                                                                                                                                                          V3682))
                                                                                                                                                                        (do
                                                                                                                                                                         (shen.bindv
                                                                                                                                                                          V3682
                                                                                                                                                                          'symbol
                                                                                                                                                                          V3750)
                                                                                                                                                                         (LET ((Result
                                                                                                                                                                                (do
                                                                                                                                                                                 (shen.incinfs)
                                                                                                                                                                                 (thaw
                                                                                                                                                                                  V3751))))
                                                                                                                                                                           (do
                                                                                                                                                                            (shen.unbindv
                                                                                                                                                                             V3682
                                                                                                                                                                             V3750)
                                                                                                                                                                            Result)))
                                                                                                                                                                        'false)))
                                                                                                                                                              'false))
                                                                                                                                                        'false))))
                                                                                                                                             (IF (EQ
                                                                                                                                                  Case
                                                                                                                                                  'false)
                                                                                                                                                 (LET ((Datatypes
                                                                                                                                                        (shen.newpv
                                                                                                                                                         V3750)))
                                                                                                                                                   (do
                                                                                                                                                    (shen.incinfs)
                                                                                                                                                    (bind
                                                                                                                                                     Datatypes
                                                                                                                                                     shen.*datatypes*
                                                                                                                                                     V3750
                                                                                                                                                     (freeze
                                                                                                                                                      (shen.udefs*
                                                                                                                                                       (CONS
                                                                                                                                                        V3747
                                                                                                                                                        (CONS
                                                                                                                                                         '|:|
                                                                                                                                                         (CONS
                                                                                                                                                          V3748
                                                                                                                                                          NIL)))
                                                                                                                                                       V3749
                                                                                                                                                       Datatypes
                                                                                                                                                       V3750
                                                                                                                                                       V3751)))))
                                                                                                                                                 Case))
                                                                                                                                           Case))
                                                                                                                                     Case))
                                                                                                                               Case))
                                                                                                                         Case))
                                                                                                                   Case))
                                                                                                             Case))
                                                                                                       Case))
                                                                                                 Case))
                                                                                           Case))
                                                                                     Case))
                                                                               Case))
                                                                         Case))
                                                                   Case))
                                                             Case))
                                                       Case))
                                                 Case))
                                           Case))
                                     Case))
                               Case))
                         Case)))))

(DEFUN shen.t*-hyps (V3756 V3757 V3758 V3759)
  (LET ((Case
         (LET ((V3493 (shen.lazyderef V3756 V3758)))
           (IF (CONSP V3493)
               (LET ((V3494 (shen.lazyderef (CAR V3493) V3758)))
                 (IF (CONSP V3494)
                     (LET ((V3495 (shen.lazyderef (CAR V3494) V3758)))
                       (IF (CONSP V3495)
                           (LET ((V3496 (shen.lazyderef (CAR V3495) V3758)))
                             (IF (EQ 'cons V3496)
                                 (LET ((V3497
                                        (shen.lazyderef (CDR V3495) V3758)))
                                   (IF (CONSP V3497)
                                       (LET ((X (CAR V3497)))
                                         (LET ((V3498
                                                (shen.lazyderef (CDR V3497)
                                                                V3758)))
                                           (IF (CONSP V3498)
                                               (LET ((Y (CAR V3498)))
                                                 (LET ((V3499
                                                        (shen.lazyderef
                                                         (CDR V3498) V3758)))
                                                   (IF (NULL V3499)
                                                       (LET ((V3500
                                                              (shen.lazyderef
                                                               (CDR V3494)
                                                               V3758)))
                                                         (IF (CONSP V3500)
                                                             (LET ((V3501
                                                                    (shen.lazyderef
                                                                     (CAR
                                                                      V3500)
                                                                     V3758)))
                                                               (IF (EQ '|:|
                                                                       V3501)
                                                                   (LET ((V3502
                                                                          (shen.lazyderef
                                                                           (CDR
                                                                            V3500)
                                                                           V3758)))
                                                                     (IF (CONSP
                                                                          V3502)
                                                                         (LET ((V3503
                                                                                (shen.lazyderef
                                                                                 (CAR
                                                                                  V3502)
                                                                                 V3758)))
                                                                           (IF (CONSP
                                                                                V3503)
                                                                               (LET ((V3504
                                                                                      (shen.lazyderef
                                                                                       (CAR
                                                                                        V3503)
                                                                                       V3758)))
                                                                                 (IF (EQ
                                                                                      'list
                                                                                      V3504)
                                                                                     (LET ((V3505
                                                                                            (shen.lazyderef
                                                                                             (CDR
                                                                                              V3503)
                                                                                             V3758)))
                                                                                       (IF (CONSP
                                                                                            V3505)
                                                                                           (LET ((A
                                                                                                  (CAR
                                                                                                   V3505)))
                                                                                             (LET ((V3506
                                                                                                    (shen.lazyderef
                                                                                                     (CDR
                                                                                                      V3505)
                                                                                                     V3758)))
                                                                                               (IF (NULL
                                                                                                    V3506)
                                                                                                   (LET ((V3507
                                                                                                          (shen.lazyderef
                                                                                                           (CDR
                                                                                                            V3502)
                                                                                                           V3758)))
                                                                                                     (IF (NULL
                                                                                                          V3507)
                                                                                                         (LET ((Hyp
                                                                                                                (CDR
                                                                                                                 V3493)))
                                                                                                           (do
                                                                                                            (shen.incinfs)
                                                                                                            (bind
                                                                                                             V3757
                                                                                                             (CONS
                                                                                                              (CONS
                                                                                                               (shen.lazyderef
                                                                                                                X
                                                                                                                V3758)
                                                                                                               (CONS
                                                                                                                '|:|
                                                                                                                (CONS
                                                                                                                 (shen.lazyderef
                                                                                                                  A
                                                                                                                  V3758)
                                                                                                                 NIL)))
                                                                                                              (CONS
                                                                                                               (CONS
                                                                                                                (shen.lazyderef
                                                                                                                 Y
                                                                                                                 V3758)
                                                                                                                (CONS
                                                                                                                 '|:|
                                                                                                                 (CONS
                                                                                                                  (CONS
                                                                                                                   'list
                                                                                                                   (CONS
                                                                                                                    (shen.lazyderef
                                                                                                                     A
                                                                                                                     V3758)
                                                                                                                    NIL))
                                                                                                                  NIL)))
                                                                                                               (shen.lazyderef
                                                                                                                Hyp
                                                                                                                V3758)))
                                                                                                             V3758
                                                                                                             V3759)))
                                                                                                         (IF (shen-cl.true?
                                                                                                              (shen.pvar?
                                                                                                               V3507))
                                                                                                             (do
                                                                                                              (shen.bindv
                                                                                                               V3507
                                                                                                               NIL
                                                                                                               V3758)
                                                                                                              (LET ((Result
                                                                                                                     (LET ((Hyp
                                                                                                                            (CDR
                                                                                                                             V3493)))
                                                                                                                       (do
                                                                                                                        (shen.incinfs)
                                                                                                                        (bind
                                                                                                                         V3757
                                                                                                                         (CONS
                                                                                                                          (CONS
                                                                                                                           (shen.lazyderef
                                                                                                                            X
                                                                                                                            V3758)
                                                                                                                           (CONS
                                                                                                                            '|:|
                                                                                                                            (CONS
                                                                                                                             (shen.lazyderef
                                                                                                                              A
                                                                                                                              V3758)
                                                                                                                             NIL)))
                                                                                                                          (CONS
                                                                                                                           (CONS
                                                                                                                            (shen.lazyderef
                                                                                                                             Y
                                                                                                                             V3758)
                                                                                                                            (CONS
                                                                                                                             '|:|
                                                                                                                             (CONS
                                                                                                                              (CONS
                                                                                                                               'list
                                                                                                                               (CONS
                                                                                                                                (shen.lazyderef
                                                                                                                                 A
                                                                                                                                 V3758)
                                                                                                                                NIL))
                                                                                                                              NIL)))
                                                                                                                           (shen.lazyderef
                                                                                                                            Hyp
                                                                                                                            V3758)))
                                                                                                                         V3758
                                                                                                                         V3759)))))
                                                                                                                (do
                                                                                                                 (shen.unbindv
                                                                                                                  V3507
                                                                                                                  V3758)
                                                                                                                 Result)))
                                                                                                             'false)))
                                                                                                   (IF (shen-cl.true?
                                                                                                        (shen.pvar?
                                                                                                         V3506))
                                                                                                       (do
                                                                                                        (shen.bindv
                                                                                                         V3506
                                                                                                         NIL
                                                                                                         V3758)
                                                                                                        (LET ((Result
                                                                                                               (LET ((V3508
                                                                                                                      (shen.lazyderef
                                                                                                                       (CDR
                                                                                                                        V3502)
                                                                                                                       V3758)))
                                                                                                                 (IF (NULL
                                                                                                                      V3508)
                                                                                                                     (LET ((Hyp
                                                                                                                            (CDR
                                                                                                                             V3493)))
                                                                                                                       (do
                                                                                                                        (shen.incinfs)
                                                                                                                        (bind
                                                                                                                         V3757
                                                                                                                         (CONS
                                                                                                                          (CONS
                                                                                                                           (shen.lazyderef
                                                                                                                            X
                                                                                                                            V3758)
                                                                                                                           (CONS
                                                                                                                            '|:|
                                                                                                                            (CONS
                                                                                                                             (shen.lazyderef
                                                                                                                              A
                                                                                                                              V3758)
                                                                                                                             NIL)))
                                                                                                                          (CONS
                                                                                                                           (CONS
                                                                                                                            (shen.lazyderef
                                                                                                                             Y
                                                                                                                             V3758)
                                                                                                                            (CONS
                                                                                                                             '|:|
                                                                                                                             (CONS
                                                                                                                              (CONS
                                                                                                                               'list
                                                                                                                               (CONS
                                                                                                                                (shen.lazyderef
                                                                                                                                 A
                                                                                                                                 V3758)
                                                                                                                                NIL))
                                                                                                                              NIL)))
                                                                                                                           (shen.lazyderef
                                                                                                                            Hyp
                                                                                                                            V3758)))
                                                                                                                         V3758
                                                                                                                         V3759)))
                                                                                                                     (IF (shen-cl.true?
                                                                                                                          (shen.pvar?
                                                                                                                           V3508))
                                                                                                                         (do
                                                                                                                          (shen.bindv
                                                                                                                           V3508
                                                                                                                           NIL
                                                                                                                           V3758)
                                                                                                                          (LET ((Result
                                                                                                                                 (LET ((Hyp
                                                                                                                                        (CDR
                                                                                                                                         V3493)))
                                                                                                                                   (do
                                                                                                                                    (shen.incinfs)
                                                                                                                                    (bind
                                                                                                                                     V3757
                                                                                                                                     (CONS
                                                                                                                                      (CONS
                                                                                                                                       (shen.lazyderef
                                                                                                                                        X
                                                                                                                                        V3758)
                                                                                                                                       (CONS
                                                                                                                                        '|:|
                                                                                                                                        (CONS
                                                                                                                                         (shen.lazyderef
                                                                                                                                          A
                                                                                                                                          V3758)
                                                                                                                                         NIL)))
                                                                                                                                      (CONS
                                                                                                                                       (CONS
                                                                                                                                        (shen.lazyderef
                                                                                                                                         Y
                                                                                                                                         V3758)
                                                                                                                                        (CONS
                                                                                                                                         '|:|
                                                                                                                                         (CONS
                                                                                                                                          (CONS
                                                                                                                                           'list
                                                                                                                                           (CONS
                                                                                                                                            (shen.lazyderef
                                                                                                                                             A
                                                                                                                                             V3758)
                                                                                                                                            NIL))
                                                                                                                                          NIL)))
                                                                                                                                       (shen.lazyderef
                                                                                                                                        Hyp
                                                                                                                                        V3758)))
                                                                                                                                     V3758
                                                                                                                                     V3759)))))
                                                                                                                            (do
                                                                                                                             (shen.unbindv
                                                                                                                              V3508
                                                                                                                              V3758)
                                                                                                                             Result)))
                                                                                                                         'false)))))
                                                                                                          (do
                                                                                                           (shen.unbindv
                                                                                                            V3506
                                                                                                            V3758)
                                                                                                           Result)))
                                                                                                       'false))))
                                                                                           (IF (shen-cl.true?
                                                                                                (shen.pvar?
                                                                                                 V3505))
                                                                                               (LET ((A
                                                                                                      (shen.newpv
                                                                                                       V3758)))
                                                                                                 (do
                                                                                                  (shen.bindv
                                                                                                   V3505
                                                                                                   (CONS
                                                                                                    A
                                                                                                    NIL)
                                                                                                   V3758)
                                                                                                  (LET ((Result
                                                                                                         (LET ((V3509
                                                                                                                (shen.lazyderef
                                                                                                                 (CDR
                                                                                                                  V3502)
                                                                                                                 V3758)))
                                                                                                           (IF (NULL
                                                                                                                V3509)
                                                                                                               (LET ((Hyp
                                                                                                                      (CDR
                                                                                                                       V3493)))
                                                                                                                 (do
                                                                                                                  (shen.incinfs)
                                                                                                                  (bind
                                                                                                                   V3757
                                                                                                                   (CONS
                                                                                                                    (CONS
                                                                                                                     (shen.lazyderef
                                                                                                                      X
                                                                                                                      V3758)
                                                                                                                     (CONS
                                                                                                                      '|:|
                                                                                                                      (CONS
                                                                                                                       (shen.lazyderef
                                                                                                                        A
                                                                                                                        V3758)
                                                                                                                       NIL)))
                                                                                                                    (CONS
                                                                                                                     (CONS
                                                                                                                      (shen.lazyderef
                                                                                                                       Y
                                                                                                                       V3758)
                                                                                                                      (CONS
                                                                                                                       '|:|
                                                                                                                       (CONS
                                                                                                                        (CONS
                                                                                                                         'list
                                                                                                                         (CONS
                                                                                                                          (shen.lazyderef
                                                                                                                           A
                                                                                                                           V3758)
                                                                                                                          NIL))
                                                                                                                        NIL)))
                                                                                                                     (shen.lazyderef
                                                                                                                      Hyp
                                                                                                                      V3758)))
                                                                                                                   V3758
                                                                                                                   V3759)))
                                                                                                               (IF (shen-cl.true?
                                                                                                                    (shen.pvar?
                                                                                                                     V3509))
                                                                                                                   (do
                                                                                                                    (shen.bindv
                                                                                                                     V3509
                                                                                                                     NIL
                                                                                                                     V3758)
                                                                                                                    (LET ((Result
                                                                                                                           (LET ((Hyp
                                                                                                                                  (CDR
                                                                                                                                   V3493)))
                                                                                                                             (do
                                                                                                                              (shen.incinfs)
                                                                                                                              (bind
                                                                                                                               V3757
                                                                                                                               (CONS
                                                                                                                                (CONS
                                                                                                                                 (shen.lazyderef
                                                                                                                                  X
                                                                                                                                  V3758)
                                                                                                                                 (CONS
                                                                                                                                  '|:|
                                                                                                                                  (CONS
                                                                                                                                   (shen.lazyderef
                                                                                                                                    A
                                                                                                                                    V3758)
                                                                                                                                   NIL)))
                                                                                                                                (CONS
                                                                                                                                 (CONS
                                                                                                                                  (shen.lazyderef
                                                                                                                                   Y
                                                                                                                                   V3758)
                                                                                                                                  (CONS
                                                                                                                                   '|:|
                                                                                                                                   (CONS
                                                                                                                                    (CONS
                                                                                                                                     'list
                                                                                                                                     (CONS
                                                                                                                                      (shen.lazyderef
                                                                                                                                       A
                                                                                                                                       V3758)
                                                                                                                                      NIL))
                                                                                                                                    NIL)))
                                                                                                                                 (shen.lazyderef
                                                                                                                                  Hyp
                                                                                                                                  V3758)))
                                                                                                                               V3758
                                                                                                                               V3759)))))
                                                                                                                      (do
                                                                                                                       (shen.unbindv
                                                                                                                        V3509
                                                                                                                        V3758)
                                                                                                                       Result)))
                                                                                                                   'false)))))
                                                                                                    (do
                                                                                                     (shen.unbindv
                                                                                                      V3505
                                                                                                      V3758)
                                                                                                     Result))))
                                                                                               'false)))
                                                                                     (IF (shen-cl.true?
                                                                                          (shen.pvar?
                                                                                           V3504))
                                                                                         (do
                                                                                          (shen.bindv
                                                                                           V3504
                                                                                           'list
                                                                                           V3758)
                                                                                          (LET ((Result
                                                                                                 (LET ((V3510
                                                                                                        (shen.lazyderef
                                                                                                         (CDR
                                                                                                          V3503)
                                                                                                         V3758)))
                                                                                                   (IF (CONSP
                                                                                                        V3510)
                                                                                                       (LET ((A
                                                                                                              (CAR
                                                                                                               V3510)))
                                                                                                         (LET ((V3511
                                                                                                                (shen.lazyderef
                                                                                                                 (CDR
                                                                                                                  V3510)
                                                                                                                 V3758)))
                                                                                                           (IF (NULL
                                                                                                                V3511)
                                                                                                               (LET ((V3512
                                                                                                                      (shen.lazyderef
                                                                                                                       (CDR
                                                                                                                        V3502)
                                                                                                                       V3758)))
                                                                                                                 (IF (NULL
                                                                                                                      V3512)
                                                                                                                     (LET ((Hyp
                                                                                                                            (CDR
                                                                                                                             V3493)))
                                                                                                                       (do
                                                                                                                        (shen.incinfs)
                                                                                                                        (bind
                                                                                                                         V3757
                                                                                                                         (CONS
                                                                                                                          (CONS
                                                                                                                           (shen.lazyderef
                                                                                                                            X
                                                                                                                            V3758)
                                                                                                                           (CONS
                                                                                                                            '|:|
                                                                                                                            (CONS
                                                                                                                             (shen.lazyderef
                                                                                                                              A
                                                                                                                              V3758)
                                                                                                                             NIL)))
                                                                                                                          (CONS
                                                                                                                           (CONS
                                                                                                                            (shen.lazyderef
                                                                                                                             Y
                                                                                                                             V3758)
                                                                                                                            (CONS
                                                                                                                             '|:|
                                                                                                                             (CONS
                                                                                                                              (CONS
                                                                                                                               'list
                                                                                                                               (CONS
                                                                                                                                (shen.lazyderef
                                                                                                                                 A
                                                                                                                                 V3758)
                                                                                                                                NIL))
                                                                                                                              NIL)))
                                                                                                                           (shen.lazyderef
                                                                                                                            Hyp
                                                                                                                            V3758)))
                                                                                                                         V3758
                                                                                                                         V3759)))
                                                                                                                     (IF (shen-cl.true?
                                                                                                                          (shen.pvar?
                                                                                                                           V3512))
                                                                                                                         (do
                                                                                                                          (shen.bindv
                                                                                                                           V3512
                                                                                                                           NIL
                                                                                                                           V3758)
                                                                                                                          (LET ((Result
                                                                                                                                 (LET ((Hyp
                                                                                                                                        (CDR
                                                                                                                                         V3493)))
                                                                                                                                   (do
                                                                                                                                    (shen.incinfs)
                                                                                                                                    (bind
                                                                                                                                     V3757
                                                                                                                                     (CONS
                                                                                                                                      (CONS
                                                                                                                                       (shen.lazyderef
                                                                                                                                        X
                                                                                                                                        V3758)
                                                                                                                                       (CONS
                                                                                                                                        '|:|
                                                                                                                                        (CONS
                                                                                                                                         (shen.lazyderef
                                                                                                                                          A
                                                                                                                                          V3758)
                                                                                                                                         NIL)))
                                                                                                                                      (CONS
                                                                                                                                       (CONS
                                                                                                                                        (shen.lazyderef
                                                                                                                                         Y
                                                                                                                                         V3758)
                                                                                                                                        (CONS
                                                                                                                                         '|:|
                                                                                                                                         (CONS
                                                                                                                                          (CONS
                                                                                                                                           'list
                                                                                                                                           (CONS
                                                                                                                                            (shen.lazyderef
                                                                                                                                             A
                                                                                                                                             V3758)
                                                                                                                                            NIL))
                                                                                                                                          NIL)))
                                                                                                                                       (shen.lazyderef
                                                                                                                                        Hyp
                                                                                                                                        V3758)))
                                                                                                                                     V3758
                                                                                                                                     V3759)))))
                                                                                                                            (do
                                                                                                                             (shen.unbindv
                                                                                                                              V3512
                                                                                                                              V3758)
                                                                                                                             Result)))
                                                                                                                         'false)))
                                                                                                               (IF (shen-cl.true?
                                                                                                                    (shen.pvar?
                                                                                                                     V3511))
                                                                                                                   (do
                                                                                                                    (shen.bindv
                                                                                                                     V3511
                                                                                                                     NIL
                                                                                                                     V3758)
                                                                                                                    (LET ((Result
                                                                                                                           (LET ((V3513
                                                                                                                                  (shen.lazyderef
                                                                                                                                   (CDR
                                                                                                                                    V3502)
                                                                                                                                   V3758)))
                                                                                                                             (IF (NULL
                                                                                                                                  V3513)
                                                                                                                                 (LET ((Hyp
                                                                                                                                        (CDR
                                                                                                                                         V3493)))
                                                                                                                                   (do
                                                                                                                                    (shen.incinfs)
                                                                                                                                    (bind
                                                                                                                                     V3757
                                                                                                                                     (CONS
                                                                                                                                      (CONS
                                                                                                                                       (shen.lazyderef
                                                                                                                                        X
                                                                                                                                        V3758)
                                                                                                                                       (CONS
                                                                                                                                        '|:|
                                                                                                                                        (CONS
                                                                                                                                         (shen.lazyderef
                                                                                                                                          A
                                                                                                                                          V3758)
                                                                                                                                         NIL)))
                                                                                                                                      (CONS
                                                                                                                                       (CONS
                                                                                                                                        (shen.lazyderef
                                                                                                                                         Y
                                                                                                                                         V3758)
                                                                                                                                        (CONS
                                                                                                                                         '|:|
                                                                                                                                         (CONS
                                                                                                                                          (CONS
                                                                                                                                           'list
                                                                                                                                           (CONS
                                                                                                                                            (shen.lazyderef
                                                                                                                                             A
                                                                                                                                             V3758)
                                                                                                                                            NIL))
                                                                                                                                          NIL)))
                                                                                                                                       (shen.lazyderef
                                                                                                                                        Hyp
                                                                                                                                        V3758)))
                                                                                                                                     V3758
                                                                                                                                     V3759)))
                                                                                                                                 (IF (shen-cl.true?
                                                                                                                                      (shen.pvar?
                                                                                                                                       V3513))
                                                                                                                                     (do
                                                                                                                                      (shen.bindv
                                                                                                                                       V3513
                                                                                                                                       NIL
                                                                                                                                       V3758)
                                                                                                                                      (LET ((Result
                                                                                                                                             (LET ((Hyp
                                                                                                                                                    (CDR
                                                                                                                                                     V3493)))
                                                                                                                                               (do
                                                                                                                                                (shen.incinfs)
                                                                                                                                                (bind
                                                                                                                                                 V3757
                                                                                                                                                 (CONS
                                                                                                                                                  (CONS
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    X
                                                                                                                                                    V3758)
                                                                                                                                                   (CONS
                                                                                                                                                    '|:|
                                                                                                                                                    (CONS
                                                                                                                                                     (shen.lazyderef
                                                                                                                                                      A
                                                                                                                                                      V3758)
                                                                                                                                                     NIL)))
                                                                                                                                                  (CONS
                                                                                                                                                   (CONS
                                                                                                                                                    (shen.lazyderef
                                                                                                                                                     Y
                                                                                                                                                     V3758)
                                                                                                                                                    (CONS
                                                                                                                                                     '|:|
                                                                                                                                                     (CONS
                                                                                                                                                      (CONS
                                                                                                                                                       'list
                                                                                                                                                       (CONS
                                                                                                                                                        (shen.lazyderef
                                                                                                                                                         A
                                                                                                                                                         V3758)
                                                                                                                                                        NIL))
                                                                                                                                                      NIL)))
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    Hyp
                                                                                                                                                    V3758)))
                                                                                                                                                 V3758
                                                                                                                                                 V3759)))))
                                                                                                                                        (do
                                                                                                                                         (shen.unbindv
                                                                                                                                          V3513
                                                                                                                                          V3758)
                                                                                                                                         Result)))
                                                                                                                                     'false)))))
                                                                                                                      (do
                                                                                                                       (shen.unbindv
                                                                                                                        V3511
                                                                                                                        V3758)
                                                                                                                       Result)))
                                                                                                                   'false))))
                                                                                                       (IF (shen-cl.true?
                                                                                                            (shen.pvar?
                                                                                                             V3510))
                                                                                                           (LET ((A
                                                                                                                  (shen.newpv
                                                                                                                   V3758)))
                                                                                                             (do
                                                                                                              (shen.bindv
                                                                                                               V3510
                                                                                                               (CONS
                                                                                                                A
                                                                                                                NIL)
                                                                                                               V3758)
                                                                                                              (LET ((Result
                                                                                                                     (LET ((V3514
                                                                                                                            (shen.lazyderef
                                                                                                                             (CDR
                                                                                                                              V3502)
                                                                                                                             V3758)))
                                                                                                                       (IF (NULL
                                                                                                                            V3514)
                                                                                                                           (LET ((Hyp
                                                                                                                                  (CDR
                                                                                                                                   V3493)))
                                                                                                                             (do
                                                                                                                              (shen.incinfs)
                                                                                                                              (bind
                                                                                                                               V3757
                                                                                                                               (CONS
                                                                                                                                (CONS
                                                                                                                                 (shen.lazyderef
                                                                                                                                  X
                                                                                                                                  V3758)
                                                                                                                                 (CONS
                                                                                                                                  '|:|
                                                                                                                                  (CONS
                                                                                                                                   (shen.lazyderef
                                                                                                                                    A
                                                                                                                                    V3758)
                                                                                                                                   NIL)))
                                                                                                                                (CONS
                                                                                                                                 (CONS
                                                                                                                                  (shen.lazyderef
                                                                                                                                   Y
                                                                                                                                   V3758)
                                                                                                                                  (CONS
                                                                                                                                   '|:|
                                                                                                                                   (CONS
                                                                                                                                    (CONS
                                                                                                                                     'list
                                                                                                                                     (CONS
                                                                                                                                      (shen.lazyderef
                                                                                                                                       A
                                                                                                                                       V3758)
                                                                                                                                      NIL))
                                                                                                                                    NIL)))
                                                                                                                                 (shen.lazyderef
                                                                                                                                  Hyp
                                                                                                                                  V3758)))
                                                                                                                               V3758
                                                                                                                               V3759)))
                                                                                                                           (IF (shen-cl.true?
                                                                                                                                (shen.pvar?
                                                                                                                                 V3514))
                                                                                                                               (do
                                                                                                                                (shen.bindv
                                                                                                                                 V3514
                                                                                                                                 NIL
                                                                                                                                 V3758)
                                                                                                                                (LET ((Result
                                                                                                                                       (LET ((Hyp
                                                                                                                                              (CDR
                                                                                                                                               V3493)))
                                                                                                                                         (do
                                                                                                                                          (shen.incinfs)
                                                                                                                                          (bind
                                                                                                                                           V3757
                                                                                                                                           (CONS
                                                                                                                                            (CONS
                                                                                                                                             (shen.lazyderef
                                                                                                                                              X
                                                                                                                                              V3758)
                                                                                                                                             (CONS
                                                                                                                                              '|:|
                                                                                                                                              (CONS
                                                                                                                                               (shen.lazyderef
                                                                                                                                                A
                                                                                                                                                V3758)
                                                                                                                                               NIL)))
                                                                                                                                            (CONS
                                                                                                                                             (CONS
                                                                                                                                              (shen.lazyderef
                                                                                                                                               Y
                                                                                                                                               V3758)
                                                                                                                                              (CONS
                                                                                                                                               '|:|
                                                                                                                                               (CONS
                                                                                                                                                (CONS
                                                                                                                                                 'list
                                                                                                                                                 (CONS
                                                                                                                                                  (shen.lazyderef
                                                                                                                                                   A
                                                                                                                                                   V3758)
                                                                                                                                                  NIL))
                                                                                                                                                NIL)))
                                                                                                                                             (shen.lazyderef
                                                                                                                                              Hyp
                                                                                                                                              V3758)))
                                                                                                                                           V3758
                                                                                                                                           V3759)))))
                                                                                                                                  (do
                                                                                                                                   (shen.unbindv
                                                                                                                                    V3514
                                                                                                                                    V3758)
                                                                                                                                   Result)))
                                                                                                                               'false)))))
                                                                                                                (do
                                                                                                                 (shen.unbindv
                                                                                                                  V3510
                                                                                                                  V3758)
                                                                                                                 Result))))
                                                                                                           'false)))))
                                                                                            (do
                                                                                             (shen.unbindv
                                                                                              V3504
                                                                                              V3758)
                                                                                             Result)))
                                                                                         'false)))
                                                                               (IF (shen-cl.true?
                                                                                    (shen.pvar?
                                                                                     V3503))
                                                                                   (LET ((A
                                                                                          (shen.newpv
                                                                                           V3758)))
                                                                                     (do
                                                                                      (shen.bindv
                                                                                       V3503
                                                                                       (CONS
                                                                                        'list
                                                                                        (CONS
                                                                                         A
                                                                                         NIL))
                                                                                       V3758)
                                                                                      (LET ((Result
                                                                                             (LET ((V3515
                                                                                                    (shen.lazyderef
                                                                                                     (CDR
                                                                                                      V3502)
                                                                                                     V3758)))
                                                                                               (IF (NULL
                                                                                                    V3515)
                                                                                                   (LET ((Hyp
                                                                                                          (CDR
                                                                                                           V3493)))
                                                                                                     (do
                                                                                                      (shen.incinfs)
                                                                                                      (bind
                                                                                                       V3757
                                                                                                       (CONS
                                                                                                        (CONS
                                                                                                         (shen.lazyderef
                                                                                                          X
                                                                                                          V3758)
                                                                                                         (CONS
                                                                                                          '|:|
                                                                                                          (CONS
                                                                                                           (shen.lazyderef
                                                                                                            A
                                                                                                            V3758)
                                                                                                           NIL)))
                                                                                                        (CONS
                                                                                                         (CONS
                                                                                                          (shen.lazyderef
                                                                                                           Y
                                                                                                           V3758)
                                                                                                          (CONS
                                                                                                           '|:|
                                                                                                           (CONS
                                                                                                            (CONS
                                                                                                             'list
                                                                                                             (CONS
                                                                                                              (shen.lazyderef
                                                                                                               A
                                                                                                               V3758)
                                                                                                              NIL))
                                                                                                            NIL)))
                                                                                                         (shen.lazyderef
                                                                                                          Hyp
                                                                                                          V3758)))
                                                                                                       V3758
                                                                                                       V3759)))
                                                                                                   (IF (shen-cl.true?
                                                                                                        (shen.pvar?
                                                                                                         V3515))
                                                                                                       (do
                                                                                                        (shen.bindv
                                                                                                         V3515
                                                                                                         NIL
                                                                                                         V3758)
                                                                                                        (LET ((Result
                                                                                                               (LET ((Hyp
                                                                                                                      (CDR
                                                                                                                       V3493)))
                                                                                                                 (do
                                                                                                                  (shen.incinfs)
                                                                                                                  (bind
                                                                                                                   V3757
                                                                                                                   (CONS
                                                                                                                    (CONS
                                                                                                                     (shen.lazyderef
                                                                                                                      X
                                                                                                                      V3758)
                                                                                                                     (CONS
                                                                                                                      '|:|
                                                                                                                      (CONS
                                                                                                                       (shen.lazyderef
                                                                                                                        A
                                                                                                                        V3758)
                                                                                                                       NIL)))
                                                                                                                    (CONS
                                                                                                                     (CONS
                                                                                                                      (shen.lazyderef
                                                                                                                       Y
                                                                                                                       V3758)
                                                                                                                      (CONS
                                                                                                                       '|:|
                                                                                                                       (CONS
                                                                                                                        (CONS
                                                                                                                         'list
                                                                                                                         (CONS
                                                                                                                          (shen.lazyderef
                                                                                                                           A
                                                                                                                           V3758)
                                                                                                                          NIL))
                                                                                                                        NIL)))
                                                                                                                     (shen.lazyderef
                                                                                                                      Hyp
                                                                                                                      V3758)))
                                                                                                                   V3758
                                                                                                                   V3759)))))
                                                                                                          (do
                                                                                                           (shen.unbindv
                                                                                                            V3515
                                                                                                            V3758)
                                                                                                           Result)))
                                                                                                       'false)))))
                                                                                        (do
                                                                                         (shen.unbindv
                                                                                          V3503
                                                                                          V3758)
                                                                                         Result))))
                                                                                   'false)))
                                                                         'false))
                                                                   'false))
                                                             'false))
                                                       'false)))
                                               'false)))
                                       'false))
                                 'false))
                           'false))
                     'false))
               'false))))
    (IF (EQ Case 'false)
        (LET ((Case
               (LET ((V3516 (shen.lazyderef V3756 V3758)))
                 (IF (CONSP V3516)
                     (LET ((V3517 (shen.lazyderef (CAR V3516) V3758)))
                       (IF (CONSP V3517)
                           (LET ((V3518 (shen.lazyderef (CAR V3517) V3758)))
                             (IF (CONSP V3518)
                                 (LET ((V3519
                                        (shen.lazyderef (CAR V3518) V3758)))
                                   (IF (EQ '@p V3519)
                                       (LET ((V3520
                                              (shen.lazyderef (CDR V3518)
                                                              V3758)))
                                         (IF (CONSP V3520)
                                             (LET ((X (CAR V3520)))
                                               (LET ((V3521
                                                      (shen.lazyderef
                                                       (CDR V3520) V3758)))
                                                 (IF (CONSP V3521)
                                                     (LET ((Y (CAR V3521)))
                                                       (LET ((V3522
                                                              (shen.lazyderef
                                                               (CDR V3521)
                                                               V3758)))
                                                         (IF (NULL V3522)
                                                             (LET ((V3523
                                                                    (shen.lazyderef
                                                                     (CDR
                                                                      V3517)
                                                                     V3758)))
                                                               (IF (CONSP
                                                                    V3523)
                                                                   (LET ((V3524
                                                                          (shen.lazyderef
                                                                           (CAR
                                                                            V3523)
                                                                           V3758)))
                                                                     (IF (EQ
                                                                          '|:|
                                                                          V3524)
                                                                         (LET ((V3525
                                                                                (shen.lazyderef
                                                                                 (CDR
                                                                                  V3523)
                                                                                 V3758)))
                                                                           (IF (CONSP
                                                                                V3525)
                                                                               (LET ((V3526
                                                                                      (shen.lazyderef
                                                                                       (CAR
                                                                                        V3525)
                                                                                       V3758)))
                                                                                 (IF (CONSP
                                                                                      V3526)
                                                                                     (LET ((A
                                                                                            (CAR
                                                                                             V3526)))
                                                                                       (LET ((V3527
                                                                                              (shen.lazyderef
                                                                                               (CDR
                                                                                                V3526)
                                                                                               V3758)))
                                                                                         (IF (CONSP
                                                                                              V3527)
                                                                                             (LET ((V3528
                                                                                                    (shen.lazyderef
                                                                                                     (CAR
                                                                                                      V3527)
                                                                                                     V3758)))
                                                                                               (IF (EQ
                                                                                                    '*
                                                                                                    V3528)
                                                                                                   (LET ((V3529
                                                                                                          (shen.lazyderef
                                                                                                           (CDR
                                                                                                            V3527)
                                                                                                           V3758)))
                                                                                                     (IF (CONSP
                                                                                                          V3529)
                                                                                                         (LET ((B
                                                                                                                (CAR
                                                                                                                 V3529)))
                                                                                                           (LET ((V3530
                                                                                                                  (shen.lazyderef
                                                                                                                   (CDR
                                                                                                                    V3529)
                                                                                                                   V3758)))
                                                                                                             (IF (NULL
                                                                                                                  V3530)
                                                                                                                 (LET ((V3531
                                                                                                                        (shen.lazyderef
                                                                                                                         (CDR
                                                                                                                          V3525)
                                                                                                                         V3758)))
                                                                                                                   (IF (NULL
                                                                                                                        V3531)
                                                                                                                       (LET ((Hyp
                                                                                                                              (CDR
                                                                                                                               V3516)))
                                                                                                                         (do
                                                                                                                          (shen.incinfs)
                                                                                                                          (bind
                                                                                                                           V3757
                                                                                                                           (CONS
                                                                                                                            (CONS
                                                                                                                             (shen.lazyderef
                                                                                                                              X
                                                                                                                              V3758)
                                                                                                                             (CONS
                                                                                                                              '|:|
                                                                                                                              (CONS
                                                                                                                               (shen.lazyderef
                                                                                                                                A
                                                                                                                                V3758)
                                                                                                                               NIL)))
                                                                                                                            (CONS
                                                                                                                             (CONS
                                                                                                                              (shen.lazyderef
                                                                                                                               Y
                                                                                                                               V3758)
                                                                                                                              (CONS
                                                                                                                               '|:|
                                                                                                                               (CONS
                                                                                                                                (shen.lazyderef
                                                                                                                                 B
                                                                                                                                 V3758)
                                                                                                                                NIL)))
                                                                                                                             (shen.lazyderef
                                                                                                                              Hyp
                                                                                                                              V3758)))
                                                                                                                           V3758
                                                                                                                           V3759)))
                                                                                                                       (IF (shen-cl.true?
                                                                                                                            (shen.pvar?
                                                                                                                             V3531))
                                                                                                                           (do
                                                                                                                            (shen.bindv
                                                                                                                             V3531
                                                                                                                             NIL
                                                                                                                             V3758)
                                                                                                                            (LET ((Result
                                                                                                                                   (LET ((Hyp
                                                                                                                                          (CDR
                                                                                                                                           V3516)))
                                                                                                                                     (do
                                                                                                                                      (shen.incinfs)
                                                                                                                                      (bind
                                                                                                                                       V3757
                                                                                                                                       (CONS
                                                                                                                                        (CONS
                                                                                                                                         (shen.lazyderef
                                                                                                                                          X
                                                                                                                                          V3758)
                                                                                                                                         (CONS
                                                                                                                                          '|:|
                                                                                                                                          (CONS
                                                                                                                                           (shen.lazyderef
                                                                                                                                            A
                                                                                                                                            V3758)
                                                                                                                                           NIL)))
                                                                                                                                        (CONS
                                                                                                                                         (CONS
                                                                                                                                          (shen.lazyderef
                                                                                                                                           Y
                                                                                                                                           V3758)
                                                                                                                                          (CONS
                                                                                                                                           '|:|
                                                                                                                                           (CONS
                                                                                                                                            (shen.lazyderef
                                                                                                                                             B
                                                                                                                                             V3758)
                                                                                                                                            NIL)))
                                                                                                                                         (shen.lazyderef
                                                                                                                                          Hyp
                                                                                                                                          V3758)))
                                                                                                                                       V3758
                                                                                                                                       V3759)))))
                                                                                                                              (do
                                                                                                                               (shen.unbindv
                                                                                                                                V3531
                                                                                                                                V3758)
                                                                                                                               Result)))
                                                                                                                           'false)))
                                                                                                                 (IF (shen-cl.true?
                                                                                                                      (shen.pvar?
                                                                                                                       V3530))
                                                                                                                     (do
                                                                                                                      (shen.bindv
                                                                                                                       V3530
                                                                                                                       NIL
                                                                                                                       V3758)
                                                                                                                      (LET ((Result
                                                                                                                             (LET ((V3532
                                                                                                                                    (shen.lazyderef
                                                                                                                                     (CDR
                                                                                                                                      V3525)
                                                                                                                                     V3758)))
                                                                                                                               (IF (NULL
                                                                                                                                    V3532)
                                                                                                                                   (LET ((Hyp
                                                                                                                                          (CDR
                                                                                                                                           V3516)))
                                                                                                                                     (do
                                                                                                                                      (shen.incinfs)
                                                                                                                                      (bind
                                                                                                                                       V3757
                                                                                                                                       (CONS
                                                                                                                                        (CONS
                                                                                                                                         (shen.lazyderef
                                                                                                                                          X
                                                                                                                                          V3758)
                                                                                                                                         (CONS
                                                                                                                                          '|:|
                                                                                                                                          (CONS
                                                                                                                                           (shen.lazyderef
                                                                                                                                            A
                                                                                                                                            V3758)
                                                                                                                                           NIL)))
                                                                                                                                        (CONS
                                                                                                                                         (CONS
                                                                                                                                          (shen.lazyderef
                                                                                                                                           Y
                                                                                                                                           V3758)
                                                                                                                                          (CONS
                                                                                                                                           '|:|
                                                                                                                                           (CONS
                                                                                                                                            (shen.lazyderef
                                                                                                                                             B
                                                                                                                                             V3758)
                                                                                                                                            NIL)))
                                                                                                                                         (shen.lazyderef
                                                                                                                                          Hyp
                                                                                                                                          V3758)))
                                                                                                                                       V3758
                                                                                                                                       V3759)))
                                                                                                                                   (IF (shen-cl.true?
                                                                                                                                        (shen.pvar?
                                                                                                                                         V3532))
                                                                                                                                       (do
                                                                                                                                        (shen.bindv
                                                                                                                                         V3532
                                                                                                                                         NIL
                                                                                                                                         V3758)
                                                                                                                                        (LET ((Result
                                                                                                                                               (LET ((Hyp
                                                                                                                                                      (CDR
                                                                                                                                                       V3516)))
                                                                                                                                                 (do
                                                                                                                                                  (shen.incinfs)
                                                                                                                                                  (bind
                                                                                                                                                   V3757
                                                                                                                                                   (CONS
                                                                                                                                                    (CONS
                                                                                                                                                     (shen.lazyderef
                                                                                                                                                      X
                                                                                                                                                      V3758)
                                                                                                                                                     (CONS
                                                                                                                                                      '|:|
                                                                                                                                                      (CONS
                                                                                                                                                       (shen.lazyderef
                                                                                                                                                        A
                                                                                                                                                        V3758)
                                                                                                                                                       NIL)))
                                                                                                                                                    (CONS
                                                                                                                                                     (CONS
                                                                                                                                                      (shen.lazyderef
                                                                                                                                                       Y
                                                                                                                                                       V3758)
                                                                                                                                                      (CONS
                                                                                                                                                       '|:|
                                                                                                                                                       (CONS
                                                                                                                                                        (shen.lazyderef
                                                                                                                                                         B
                                                                                                                                                         V3758)
                                                                                                                                                        NIL)))
                                                                                                                                                     (shen.lazyderef
                                                                                                                                                      Hyp
                                                                                                                                                      V3758)))
                                                                                                                                                   V3758
                                                                                                                                                   V3759)))))
                                                                                                                                          (do
                                                                                                                                           (shen.unbindv
                                                                                                                                            V3532
                                                                                                                                            V3758)
                                                                                                                                           Result)))
                                                                                                                                       'false)))))
                                                                                                                        (do
                                                                                                                         (shen.unbindv
                                                                                                                          V3530
                                                                                                                          V3758)
                                                                                                                         Result)))
                                                                                                                     'false))))
                                                                                                         (IF (shen-cl.true?
                                                                                                              (shen.pvar?
                                                                                                               V3529))
                                                                                                             (LET ((B
                                                                                                                    (shen.newpv
                                                                                                                     V3758)))
                                                                                                               (do
                                                                                                                (shen.bindv
                                                                                                                 V3529
                                                                                                                 (CONS
                                                                                                                  B
                                                                                                                  NIL)
                                                                                                                 V3758)
                                                                                                                (LET ((Result
                                                                                                                       (LET ((V3533
                                                                                                                              (shen.lazyderef
                                                                                                                               (CDR
                                                                                                                                V3525)
                                                                                                                               V3758)))
                                                                                                                         (IF (NULL
                                                                                                                              V3533)
                                                                                                                             (LET ((Hyp
                                                                                                                                    (CDR
                                                                                                                                     V3516)))
                                                                                                                               (do
                                                                                                                                (shen.incinfs)
                                                                                                                                (bind
                                                                                                                                 V3757
                                                                                                                                 (CONS
                                                                                                                                  (CONS
                                                                                                                                   (shen.lazyderef
                                                                                                                                    X
                                                                                                                                    V3758)
                                                                                                                                   (CONS
                                                                                                                                    '|:|
                                                                                                                                    (CONS
                                                                                                                                     (shen.lazyderef
                                                                                                                                      A
                                                                                                                                      V3758)
                                                                                                                                     NIL)))
                                                                                                                                  (CONS
                                                                                                                                   (CONS
                                                                                                                                    (shen.lazyderef
                                                                                                                                     Y
                                                                                                                                     V3758)
                                                                                                                                    (CONS
                                                                                                                                     '|:|
                                                                                                                                     (CONS
                                                                                                                                      (shen.lazyderef
                                                                                                                                       B
                                                                                                                                       V3758)
                                                                                                                                      NIL)))
                                                                                                                                   (shen.lazyderef
                                                                                                                                    Hyp
                                                                                                                                    V3758)))
                                                                                                                                 V3758
                                                                                                                                 V3759)))
                                                                                                                             (IF (shen-cl.true?
                                                                                                                                  (shen.pvar?
                                                                                                                                   V3533))
                                                                                                                                 (do
                                                                                                                                  (shen.bindv
                                                                                                                                   V3533
                                                                                                                                   NIL
                                                                                                                                   V3758)
                                                                                                                                  (LET ((Result
                                                                                                                                         (LET ((Hyp
                                                                                                                                                (CDR
                                                                                                                                                 V3516)))
                                                                                                                                           (do
                                                                                                                                            (shen.incinfs)
                                                                                                                                            (bind
                                                                                                                                             V3757
                                                                                                                                             (CONS
                                                                                                                                              (CONS
                                                                                                                                               (shen.lazyderef
                                                                                                                                                X
                                                                                                                                                V3758)
                                                                                                                                               (CONS
                                                                                                                                                '|:|
                                                                                                                                                (CONS
                                                                                                                                                 (shen.lazyderef
                                                                                                                                                  A
                                                                                                                                                  V3758)
                                                                                                                                                 NIL)))
                                                                                                                                              (CONS
                                                                                                                                               (CONS
                                                                                                                                                (shen.lazyderef
                                                                                                                                                 Y
                                                                                                                                                 V3758)
                                                                                                                                                (CONS
                                                                                                                                                 '|:|
                                                                                                                                                 (CONS
                                                                                                                                                  (shen.lazyderef
                                                                                                                                                   B
                                                                                                                                                   V3758)
                                                                                                                                                  NIL)))
                                                                                                                                               (shen.lazyderef
                                                                                                                                                Hyp
                                                                                                                                                V3758)))
                                                                                                                                             V3758
                                                                                                                                             V3759)))))
                                                                                                                                    (do
                                                                                                                                     (shen.unbindv
                                                                                                                                      V3533
                                                                                                                                      V3758)
                                                                                                                                     Result)))
                                                                                                                                 'false)))))
                                                                                                                  (do
                                                                                                                   (shen.unbindv
                                                                                                                    V3529
                                                                                                                    V3758)
                                                                                                                   Result))))
                                                                                                             'false)))
                                                                                                   (IF (shen-cl.true?
                                                                                                        (shen.pvar?
                                                                                                         V3528))
                                                                                                       (do
                                                                                                        (shen.bindv
                                                                                                         V3528
                                                                                                         '*
                                                                                                         V3758)
                                                                                                        (LET ((Result
                                                                                                               (LET ((V3534
                                                                                                                      (shen.lazyderef
                                                                                                                       (CDR
                                                                                                                        V3527)
                                                                                                                       V3758)))
                                                                                                                 (IF (CONSP
                                                                                                                      V3534)
                                                                                                                     (LET ((B
                                                                                                                            (CAR
                                                                                                                             V3534)))
                                                                                                                       (LET ((V3535
                                                                                                                              (shen.lazyderef
                                                                                                                               (CDR
                                                                                                                                V3534)
                                                                                                                               V3758)))
                                                                                                                         (IF (NULL
                                                                                                                              V3535)
                                                                                                                             (LET ((V3536
                                                                                                                                    (shen.lazyderef
                                                                                                                                     (CDR
                                                                                                                                      V3525)
                                                                                                                                     V3758)))
                                                                                                                               (IF (NULL
                                                                                                                                    V3536)
                                                                                                                                   (LET ((Hyp
                                                                                                                                          (CDR
                                                                                                                                           V3516)))
                                                                                                                                     (do
                                                                                                                                      (shen.incinfs)
                                                                                                                                      (bind
                                                                                                                                       V3757
                                                                                                                                       (CONS
                                                                                                                                        (CONS
                                                                                                                                         (shen.lazyderef
                                                                                                                                          X
                                                                                                                                          V3758)
                                                                                                                                         (CONS
                                                                                                                                          '|:|
                                                                                                                                          (CONS
                                                                                                                                           (shen.lazyderef
                                                                                                                                            A
                                                                                                                                            V3758)
                                                                                                                                           NIL)))
                                                                                                                                        (CONS
                                                                                                                                         (CONS
                                                                                                                                          (shen.lazyderef
                                                                                                                                           Y
                                                                                                                                           V3758)
                                                                                                                                          (CONS
                                                                                                                                           '|:|
                                                                                                                                           (CONS
                                                                                                                                            (shen.lazyderef
                                                                                                                                             B
                                                                                                                                             V3758)
                                                                                                                                            NIL)))
                                                                                                                                         (shen.lazyderef
                                                                                                                                          Hyp
                                                                                                                                          V3758)))
                                                                                                                                       V3758
                                                                                                                                       V3759)))
                                                                                                                                   (IF (shen-cl.true?
                                                                                                                                        (shen.pvar?
                                                                                                                                         V3536))
                                                                                                                                       (do
                                                                                                                                        (shen.bindv
                                                                                                                                         V3536
                                                                                                                                         NIL
                                                                                                                                         V3758)
                                                                                                                                        (LET ((Result
                                                                                                                                               (LET ((Hyp
                                                                                                                                                      (CDR
                                                                                                                                                       V3516)))
                                                                                                                                                 (do
                                                                                                                                                  (shen.incinfs)
                                                                                                                                                  (bind
                                                                                                                                                   V3757
                                                                                                                                                   (CONS
                                                                                                                                                    (CONS
                                                                                                                                                     (shen.lazyderef
                                                                                                                                                      X
                                                                                                                                                      V3758)
                                                                                                                                                     (CONS
                                                                                                                                                      '|:|
                                                                                                                                                      (CONS
                                                                                                                                                       (shen.lazyderef
                                                                                                                                                        A
                                                                                                                                                        V3758)
                                                                                                                                                       NIL)))
                                                                                                                                                    (CONS
                                                                                                                                                     (CONS
                                                                                                                                                      (shen.lazyderef
                                                                                                                                                       Y
                                                                                                                                                       V3758)
                                                                                                                                                      (CONS
                                                                                                                                                       '|:|
                                                                                                                                                       (CONS
                                                                                                                                                        (shen.lazyderef
                                                                                                                                                         B
                                                                                                                                                         V3758)
                                                                                                                                                        NIL)))
                                                                                                                                                     (shen.lazyderef
                                                                                                                                                      Hyp
                                                                                                                                                      V3758)))
                                                                                                                                                   V3758
                                                                                                                                                   V3759)))))
                                                                                                                                          (do
                                                                                                                                           (shen.unbindv
                                                                                                                                            V3536
                                                                                                                                            V3758)
                                                                                                                                           Result)))
                                                                                                                                       'false)))
                                                                                                                             (IF (shen-cl.true?
                                                                                                                                  (shen.pvar?
                                                                                                                                   V3535))
                                                                                                                                 (do
                                                                                                                                  (shen.bindv
                                                                                                                                   V3535
                                                                                                                                   NIL
                                                                                                                                   V3758)
                                                                                                                                  (LET ((Result
                                                                                                                                         (LET ((V3537
                                                                                                                                                (shen.lazyderef
                                                                                                                                                 (CDR
                                                                                                                                                  V3525)
                                                                                                                                                 V3758)))
                                                                                                                                           (IF (NULL
                                                                                                                                                V3537)
                                                                                                                                               (LET ((Hyp
                                                                                                                                                      (CDR
                                                                                                                                                       V3516)))
                                                                                                                                                 (do
                                                                                                                                                  (shen.incinfs)
                                                                                                                                                  (bind
                                                                                                                                                   V3757
                                                                                                                                                   (CONS
                                                                                                                                                    (CONS
                                                                                                                                                     (shen.lazyderef
                                                                                                                                                      X
                                                                                                                                                      V3758)
                                                                                                                                                     (CONS
                                                                                                                                                      '|:|
                                                                                                                                                      (CONS
                                                                                                                                                       (shen.lazyderef
                                                                                                                                                        A
                                                                                                                                                        V3758)
                                                                                                                                                       NIL)))
                                                                                                                                                    (CONS
                                                                                                                                                     (CONS
                                                                                                                                                      (shen.lazyderef
                                                                                                                                                       Y
                                                                                                                                                       V3758)
                                                                                                                                                      (CONS
                                                                                                                                                       '|:|
                                                                                                                                                       (CONS
                                                                                                                                                        (shen.lazyderef
                                                                                                                                                         B
                                                                                                                                                         V3758)
                                                                                                                                                        NIL)))
                                                                                                                                                     (shen.lazyderef
                                                                                                                                                      Hyp
                                                                                                                                                      V3758)))
                                                                                                                                                   V3758
                                                                                                                                                   V3759)))
                                                                                                                                               (IF (shen-cl.true?
                                                                                                                                                    (shen.pvar?
                                                                                                                                                     V3537))
                                                                                                                                                   (do
                                                                                                                                                    (shen.bindv
                                                                                                                                                     V3537
                                                                                                                                                     NIL
                                                                                                                                                     V3758)
                                                                                                                                                    (LET ((Result
                                                                                                                                                           (LET ((Hyp
                                                                                                                                                                  (CDR
                                                                                                                                                                   V3516)))
                                                                                                                                                             (do
                                                                                                                                                              (shen.incinfs)
                                                                                                                                                              (bind
                                                                                                                                                               V3757
                                                                                                                                                               (CONS
                                                                                                                                                                (CONS
                                                                                                                                                                 (shen.lazyderef
                                                                                                                                                                  X
                                                                                                                                                                  V3758)
                                                                                                                                                                 (CONS
                                                                                                                                                                  '|:|
                                                                                                                                                                  (CONS
                                                                                                                                                                   (shen.lazyderef
                                                                                                                                                                    A
                                                                                                                                                                    V3758)
                                                                                                                                                                   NIL)))
                                                                                                                                                                (CONS
                                                                                                                                                                 (CONS
                                                                                                                                                                  (shen.lazyderef
                                                                                                                                                                   Y
                                                                                                                                                                   V3758)
                                                                                                                                                                  (CONS
                                                                                                                                                                   '|:|
                                                                                                                                                                   (CONS
                                                                                                                                                                    (shen.lazyderef
                                                                                                                                                                     B
                                                                                                                                                                     V3758)
                                                                                                                                                                    NIL)))
                                                                                                                                                                 (shen.lazyderef
                                                                                                                                                                  Hyp
                                                                                                                                                                  V3758)))
                                                                                                                                                               V3758
                                                                                                                                                               V3759)))))
                                                                                                                                                      (do
                                                                                                                                                       (shen.unbindv
                                                                                                                                                        V3537
                                                                                                                                                        V3758)
                                                                                                                                                       Result)))
                                                                                                                                                   'false)))))
                                                                                                                                    (do
                                                                                                                                     (shen.unbindv
                                                                                                                                      V3535
                                                                                                                                      V3758)
                                                                                                                                     Result)))
                                                                                                                                 'false))))
                                                                                                                     (IF (shen-cl.true?
                                                                                                                          (shen.pvar?
                                                                                                                           V3534))
                                                                                                                         (LET ((B
                                                                                                                                (shen.newpv
                                                                                                                                 V3758)))
                                                                                                                           (do
                                                                                                                            (shen.bindv
                                                                                                                             V3534
                                                                                                                             (CONS
                                                                                                                              B
                                                                                                                              NIL)
                                                                                                                             V3758)
                                                                                                                            (LET ((Result
                                                                                                                                   (LET ((V3538
                                                                                                                                          (shen.lazyderef
                                                                                                                                           (CDR
                                                                                                                                            V3525)
                                                                                                                                           V3758)))
                                                                                                                                     (IF (NULL
                                                                                                                                          V3538)
                                                                                                                                         (LET ((Hyp
                                                                                                                                                (CDR
                                                                                                                                                 V3516)))
                                                                                                                                           (do
                                                                                                                                            (shen.incinfs)
                                                                                                                                            (bind
                                                                                                                                             V3757
                                                                                                                                             (CONS
                                                                                                                                              (CONS
                                                                                                                                               (shen.lazyderef
                                                                                                                                                X
                                                                                                                                                V3758)
                                                                                                                                               (CONS
                                                                                                                                                '|:|
                                                                                                                                                (CONS
                                                                                                                                                 (shen.lazyderef
                                                                                                                                                  A
                                                                                                                                                  V3758)
                                                                                                                                                 NIL)))
                                                                                                                                              (CONS
                                                                                                                                               (CONS
                                                                                                                                                (shen.lazyderef
                                                                                                                                                 Y
                                                                                                                                                 V3758)
                                                                                                                                                (CONS
                                                                                                                                                 '|:|
                                                                                                                                                 (CONS
                                                                                                                                                  (shen.lazyderef
                                                                                                                                                   B
                                                                                                                                                   V3758)
                                                                                                                                                  NIL)))
                                                                                                                                               (shen.lazyderef
                                                                                                                                                Hyp
                                                                                                                                                V3758)))
                                                                                                                                             V3758
                                                                                                                                             V3759)))
                                                                                                                                         (IF (shen-cl.true?
                                                                                                                                              (shen.pvar?
                                                                                                                                               V3538))
                                                                                                                                             (do
                                                                                                                                              (shen.bindv
                                                                                                                                               V3538
                                                                                                                                               NIL
                                                                                                                                               V3758)
                                                                                                                                              (LET ((Result
                                                                                                                                                     (LET ((Hyp
                                                                                                                                                            (CDR
                                                                                                                                                             V3516)))
                                                                                                                                                       (do
                                                                                                                                                        (shen.incinfs)
                                                                                                                                                        (bind
                                                                                                                                                         V3757
                                                                                                                                                         (CONS
                                                                                                                                                          (CONS
                                                                                                                                                           (shen.lazyderef
                                                                                                                                                            X
                                                                                                                                                            V3758)
                                                                                                                                                           (CONS
                                                                                                                                                            '|:|
                                                                                                                                                            (CONS
                                                                                                                                                             (shen.lazyderef
                                                                                                                                                              A
                                                                                                                                                              V3758)
                                                                                                                                                             NIL)))
                                                                                                                                                          (CONS
                                                                                                                                                           (CONS
                                                                                                                                                            (shen.lazyderef
                                                                                                                                                             Y
                                                                                                                                                             V3758)
                                                                                                                                                            (CONS
                                                                                                                                                             '|:|
                                                                                                                                                             (CONS
                                                                                                                                                              (shen.lazyderef
                                                                                                                                                               B
                                                                                                                                                               V3758)
                                                                                                                                                              NIL)))
                                                                                                                                                           (shen.lazyderef
                                                                                                                                                            Hyp
                                                                                                                                                            V3758)))
                                                                                                                                                         V3758
                                                                                                                                                         V3759)))))
                                                                                                                                                (do
                                                                                                                                                 (shen.unbindv
                                                                                                                                                  V3538
                                                                                                                                                  V3758)
                                                                                                                                                 Result)))
                                                                                                                                             'false)))))
                                                                                                                              (do
                                                                                                                               (shen.unbindv
                                                                                                                                V3534
                                                                                                                                V3758)
                                                                                                                               Result))))
                                                                                                                         'false)))))
                                                                                                          (do
                                                                                                           (shen.unbindv
                                                                                                            V3528
                                                                                                            V3758)
                                                                                                           Result)))
                                                                                                       'false)))
                                                                                             (IF (shen-cl.true?
                                                                                                  (shen.pvar?
                                                                                                   V3527))
                                                                                                 (LET ((B
                                                                                                        (shen.newpv
                                                                                                         V3758)))
                                                                                                   (do
                                                                                                    (shen.bindv
                                                                                                     V3527
                                                                                                     (CONS
                                                                                                      '*
                                                                                                      (CONS
                                                                                                       B
                                                                                                       NIL))
                                                                                                     V3758)
                                                                                                    (LET ((Result
                                                                                                           (LET ((V3539
                                                                                                                  (shen.lazyderef
                                                                                                                   (CDR
                                                                                                                    V3525)
                                                                                                                   V3758)))
                                                                                                             (IF (NULL
                                                                                                                  V3539)
                                                                                                                 (LET ((Hyp
                                                                                                                        (CDR
                                                                                                                         V3516)))
                                                                                                                   (do
                                                                                                                    (shen.incinfs)
                                                                                                                    (bind
                                                                                                                     V3757
                                                                                                                     (CONS
                                                                                                                      (CONS
                                                                                                                       (shen.lazyderef
                                                                                                                        X
                                                                                                                        V3758)
                                                                                                                       (CONS
                                                                                                                        '|:|
                                                                                                                        (CONS
                                                                                                                         (shen.lazyderef
                                                                                                                          A
                                                                                                                          V3758)
                                                                                                                         NIL)))
                                                                                                                      (CONS
                                                                                                                       (CONS
                                                                                                                        (shen.lazyderef
                                                                                                                         Y
                                                                                                                         V3758)
                                                                                                                        (CONS
                                                                                                                         '|:|
                                                                                                                         (CONS
                                                                                                                          (shen.lazyderef
                                                                                                                           B
                                                                                                                           V3758)
                                                                                                                          NIL)))
                                                                                                                       (shen.lazyderef
                                                                                                                        Hyp
                                                                                                                        V3758)))
                                                                                                                     V3758
                                                                                                                     V3759)))
                                                                                                                 (IF (shen-cl.true?
                                                                                                                      (shen.pvar?
                                                                                                                       V3539))
                                                                                                                     (do
                                                                                                                      (shen.bindv
                                                                                                                       V3539
                                                                                                                       NIL
                                                                                                                       V3758)
                                                                                                                      (LET ((Result
                                                                                                                             (LET ((Hyp
                                                                                                                                    (CDR
                                                                                                                                     V3516)))
                                                                                                                               (do
                                                                                                                                (shen.incinfs)
                                                                                                                                (bind
                                                                                                                                 V3757
                                                                                                                                 (CONS
                                                                                                                                  (CONS
                                                                                                                                   (shen.lazyderef
                                                                                                                                    X
                                                                                                                                    V3758)
                                                                                                                                   (CONS
                                                                                                                                    '|:|
                                                                                                                                    (CONS
                                                                                                                                     (shen.lazyderef
                                                                                                                                      A
                                                                                                                                      V3758)
                                                                                                                                     NIL)))
                                                                                                                                  (CONS
                                                                                                                                   (CONS
                                                                                                                                    (shen.lazyderef
                                                                                                                                     Y
                                                                                                                                     V3758)
                                                                                                                                    (CONS
                                                                                                                                     '|:|
                                                                                                                                     (CONS
                                                                                                                                      (shen.lazyderef
                                                                                                                                       B
                                                                                                                                       V3758)
                                                                                                                                      NIL)))
                                                                                                                                   (shen.lazyderef
                                                                                                                                    Hyp
                                                                                                                                    V3758)))
                                                                                                                                 V3758
                                                                                                                                 V3759)))))
                                                                                                                        (do
                                                                                                                         (shen.unbindv
                                                                                                                          V3539
                                                                                                                          V3758)
                                                                                                                         Result)))
                                                                                                                     'false)))))
                                                                                                      (do
                                                                                                       (shen.unbindv
                                                                                                        V3527
                                                                                                        V3758)
                                                                                                       Result))))
                                                                                                 'false))))
                                                                                     (IF (shen-cl.true?
                                                                                          (shen.pvar?
                                                                                           V3526))
                                                                                         (LET ((A
                                                                                                (shen.newpv
                                                                                                 V3758)))
                                                                                           (LET ((B
                                                                                                  (shen.newpv
                                                                                                   V3758)))
                                                                                             (do
                                                                                              (shen.bindv
                                                                                               V3526
                                                                                               (CONS
                                                                                                A
                                                                                                (CONS
                                                                                                 '*
                                                                                                 (CONS
                                                                                                  B
                                                                                                  NIL)))
                                                                                               V3758)
                                                                                              (LET ((Result
                                                                                                     (LET ((V3540
                                                                                                            (shen.lazyderef
                                                                                                             (CDR
                                                                                                              V3525)
                                                                                                             V3758)))
                                                                                                       (IF (NULL
                                                                                                            V3540)
                                                                                                           (LET ((Hyp
                                                                                                                  (CDR
                                                                                                                   V3516)))
                                                                                                             (do
                                                                                                              (shen.incinfs)
                                                                                                              (bind
                                                                                                               V3757
                                                                                                               (CONS
                                                                                                                (CONS
                                                                                                                 (shen.lazyderef
                                                                                                                  X
                                                                                                                  V3758)
                                                                                                                 (CONS
                                                                                                                  '|:|
                                                                                                                  (CONS
                                                                                                                   (shen.lazyderef
                                                                                                                    A
                                                                                                                    V3758)
                                                                                                                   NIL)))
                                                                                                                (CONS
                                                                                                                 (CONS
                                                                                                                  (shen.lazyderef
                                                                                                                   Y
                                                                                                                   V3758)
                                                                                                                  (CONS
                                                                                                                   '|:|
                                                                                                                   (CONS
                                                                                                                    (shen.lazyderef
                                                                                                                     B
                                                                                                                     V3758)
                                                                                                                    NIL)))
                                                                                                                 (shen.lazyderef
                                                                                                                  Hyp
                                                                                                                  V3758)))
                                                                                                               V3758
                                                                                                               V3759)))
                                                                                                           (IF (shen-cl.true?
                                                                                                                (shen.pvar?
                                                                                                                 V3540))
                                                                                                               (do
                                                                                                                (shen.bindv
                                                                                                                 V3540
                                                                                                                 NIL
                                                                                                                 V3758)
                                                                                                                (LET ((Result
                                                                                                                       (LET ((Hyp
                                                                                                                              (CDR
                                                                                                                               V3516)))
                                                                                                                         (do
                                                                                                                          (shen.incinfs)
                                                                                                                          (bind
                                                                                                                           V3757
                                                                                                                           (CONS
                                                                                                                            (CONS
                                                                                                                             (shen.lazyderef
                                                                                                                              X
                                                                                                                              V3758)
                                                                                                                             (CONS
                                                                                                                              '|:|
                                                                                                                              (CONS
                                                                                                                               (shen.lazyderef
                                                                                                                                A
                                                                                                                                V3758)
                                                                                                                               NIL)))
                                                                                                                            (CONS
                                                                                                                             (CONS
                                                                                                                              (shen.lazyderef
                                                                                                                               Y
                                                                                                                               V3758)
                                                                                                                              (CONS
                                                                                                                               '|:|
                                                                                                                               (CONS
                                                                                                                                (shen.lazyderef
                                                                                                                                 B
                                                                                                                                 V3758)
                                                                                                                                NIL)))
                                                                                                                             (shen.lazyderef
                                                                                                                              Hyp
                                                                                                                              V3758)))
                                                                                                                           V3758
                                                                                                                           V3759)))))
                                                                                                                  (do
                                                                                                                   (shen.unbindv
                                                                                                                    V3540
                                                                                                                    V3758)
                                                                                                                   Result)))
                                                                                                               'false)))))
                                                                                                (do
                                                                                                 (shen.unbindv
                                                                                                  V3526
                                                                                                  V3758)
                                                                                                 Result)))))
                                                                                         'false)))
                                                                               'false))
                                                                         'false))
                                                                   'false))
                                                             'false)))
                                                     'false)))
                                             'false))
                                       'false))
                                 'false))
                           'false))
                     'false))))
          (IF (EQ Case 'false)
              (LET ((Case
                     (LET ((V3541 (shen.lazyderef V3756 V3758)))
                       (IF (CONSP V3541)
                           (LET ((V3542 (shen.lazyderef (CAR V3541) V3758)))
                             (IF (CONSP V3542)
                                 (LET ((V3543
                                        (shen.lazyderef (CAR V3542) V3758)))
                                   (IF (CONSP V3543)
                                       (LET ((V3544
                                              (shen.lazyderef (CAR V3543)
                                                              V3758)))
                                         (IF (EQ '@v V3544)
                                             (LET ((V3545
                                                    (shen.lazyderef (CDR V3543)
                                                                    V3758)))
                                               (IF (CONSP V3545)
                                                   (LET ((X (CAR V3545)))
                                                     (LET ((V3546
                                                            (shen.lazyderef
                                                             (CDR V3545)
                                                             V3758)))
                                                       (IF (CONSP V3546)
                                                           (LET ((Y
                                                                  (CAR V3546)))
                                                             (LET ((V3547
                                                                    (shen.lazyderef
                                                                     (CDR
                                                                      V3546)
                                                                     V3758)))
                                                               (IF (NULL V3547)
                                                                   (LET ((V3548
                                                                          (shen.lazyderef
                                                                           (CDR
                                                                            V3542)
                                                                           V3758)))
                                                                     (IF (CONSP
                                                                          V3548)
                                                                         (LET ((V3549
                                                                                (shen.lazyderef
                                                                                 (CAR
                                                                                  V3548)
                                                                                 V3758)))
                                                                           (IF (EQ
                                                                                '|:|
                                                                                V3549)
                                                                               (LET ((V3550
                                                                                      (shen.lazyderef
                                                                                       (CDR
                                                                                        V3548)
                                                                                       V3758)))
                                                                                 (IF (CONSP
                                                                                      V3550)
                                                                                     (LET ((V3551
                                                                                            (shen.lazyderef
                                                                                             (CAR
                                                                                              V3550)
                                                                                             V3758)))
                                                                                       (IF (CONSP
                                                                                            V3551)
                                                                                           (LET ((V3552
                                                                                                  (shen.lazyderef
                                                                                                   (CAR
                                                                                                    V3551)
                                                                                                   V3758)))
                                                                                             (IF (EQ
                                                                                                  'vector
                                                                                                  V3552)
                                                                                                 (LET ((V3553
                                                                                                        (shen.lazyderef
                                                                                                         (CDR
                                                                                                          V3551)
                                                                                                         V3758)))
                                                                                                   (IF (CONSP
                                                                                                        V3553)
                                                                                                       (LET ((A
                                                                                                              (CAR
                                                                                                               V3553)))
                                                                                                         (LET ((V3554
                                                                                                                (shen.lazyderef
                                                                                                                 (CDR
                                                                                                                  V3553)
                                                                                                                 V3758)))
                                                                                                           (IF (NULL
                                                                                                                V3554)
                                                                                                               (LET ((V3555
                                                                                                                      (shen.lazyderef
                                                                                                                       (CDR
                                                                                                                        V3550)
                                                                                                                       V3758)))
                                                                                                                 (IF (NULL
                                                                                                                      V3555)
                                                                                                                     (LET ((Hyp
                                                                                                                            (CDR
                                                                                                                             V3541)))
                                                                                                                       (do
                                                                                                                        (shen.incinfs)
                                                                                                                        (bind
                                                                                                                         V3757
                                                                                                                         (CONS
                                                                                                                          (CONS
                                                                                                                           (shen.lazyderef
                                                                                                                            X
                                                                                                                            V3758)
                                                                                                                           (CONS
                                                                                                                            '|:|
                                                                                                                            (CONS
                                                                                                                             (shen.lazyderef
                                                                                                                              A
                                                                                                                              V3758)
                                                                                                                             NIL)))
                                                                                                                          (CONS
                                                                                                                           (CONS
                                                                                                                            (shen.lazyderef
                                                                                                                             Y
                                                                                                                             V3758)
                                                                                                                            (CONS
                                                                                                                             '|:|
                                                                                                                             (CONS
                                                                                                                              (CONS
                                                                                                                               'vector
                                                                                                                               (CONS
                                                                                                                                (shen.lazyderef
                                                                                                                                 A
                                                                                                                                 V3758)
                                                                                                                                NIL))
                                                                                                                              NIL)))
                                                                                                                           (shen.lazyderef
                                                                                                                            Hyp
                                                                                                                            V3758)))
                                                                                                                         V3758
                                                                                                                         V3759)))
                                                                                                                     (IF (shen-cl.true?
                                                                                                                          (shen.pvar?
                                                                                                                           V3555))
                                                                                                                         (do
                                                                                                                          (shen.bindv
                                                                                                                           V3555
                                                                                                                           NIL
                                                                                                                           V3758)
                                                                                                                          (LET ((Result
                                                                                                                                 (LET ((Hyp
                                                                                                                                        (CDR
                                                                                                                                         V3541)))
                                                                                                                                   (do
                                                                                                                                    (shen.incinfs)
                                                                                                                                    (bind
                                                                                                                                     V3757
                                                                                                                                     (CONS
                                                                                                                                      (CONS
                                                                                                                                       (shen.lazyderef
                                                                                                                                        X
                                                                                                                                        V3758)
                                                                                                                                       (CONS
                                                                                                                                        '|:|
                                                                                                                                        (CONS
                                                                                                                                         (shen.lazyderef
                                                                                                                                          A
                                                                                                                                          V3758)
                                                                                                                                         NIL)))
                                                                                                                                      (CONS
                                                                                                                                       (CONS
                                                                                                                                        (shen.lazyderef
                                                                                                                                         Y
                                                                                                                                         V3758)
                                                                                                                                        (CONS
                                                                                                                                         '|:|
                                                                                                                                         (CONS
                                                                                                                                          (CONS
                                                                                                                                           'vector
                                                                                                                                           (CONS
                                                                                                                                            (shen.lazyderef
                                                                                                                                             A
                                                                                                                                             V3758)
                                                                                                                                            NIL))
                                                                                                                                          NIL)))
                                                                                                                                       (shen.lazyderef
                                                                                                                                        Hyp
                                                                                                                                        V3758)))
                                                                                                                                     V3758
                                                                                                                                     V3759)))))
                                                                                                                            (do
                                                                                                                             (shen.unbindv
                                                                                                                              V3555
                                                                                                                              V3758)
                                                                                                                             Result)))
                                                                                                                         'false)))
                                                                                                               (IF (shen-cl.true?
                                                                                                                    (shen.pvar?
                                                                                                                     V3554))
                                                                                                                   (do
                                                                                                                    (shen.bindv
                                                                                                                     V3554
                                                                                                                     NIL
                                                                                                                     V3758)
                                                                                                                    (LET ((Result
                                                                                                                           (LET ((V3556
                                                                                                                                  (shen.lazyderef
                                                                                                                                   (CDR
                                                                                                                                    V3550)
                                                                                                                                   V3758)))
                                                                                                                             (IF (NULL
                                                                                                                                  V3556)
                                                                                                                                 (LET ((Hyp
                                                                                                                                        (CDR
                                                                                                                                         V3541)))
                                                                                                                                   (do
                                                                                                                                    (shen.incinfs)
                                                                                                                                    (bind
                                                                                                                                     V3757
                                                                                                                                     (CONS
                                                                                                                                      (CONS
                                                                                                                                       (shen.lazyderef
                                                                                                                                        X
                                                                                                                                        V3758)
                                                                                                                                       (CONS
                                                                                                                                        '|:|
                                                                                                                                        (CONS
                                                                                                                                         (shen.lazyderef
                                                                                                                                          A
                                                                                                                                          V3758)
                                                                                                                                         NIL)))
                                                                                                                                      (CONS
                                                                                                                                       (CONS
                                                                                                                                        (shen.lazyderef
                                                                                                                                         Y
                                                                                                                                         V3758)
                                                                                                                                        (CONS
                                                                                                                                         '|:|
                                                                                                                                         (CONS
                                                                                                                                          (CONS
                                                                                                                                           'vector
                                                                                                                                           (CONS
                                                                                                                                            (shen.lazyderef
                                                                                                                                             A
                                                                                                                                             V3758)
                                                                                                                                            NIL))
                                                                                                                                          NIL)))
                                                                                                                                       (shen.lazyderef
                                                                                                                                        Hyp
                                                                                                                                        V3758)))
                                                                                                                                     V3758
                                                                                                                                     V3759)))
                                                                                                                                 (IF (shen-cl.true?
                                                                                                                                      (shen.pvar?
                                                                                                                                       V3556))
                                                                                                                                     (do
                                                                                                                                      (shen.bindv
                                                                                                                                       V3556
                                                                                                                                       NIL
                                                                                                                                       V3758)
                                                                                                                                      (LET ((Result
                                                                                                                                             (LET ((Hyp
                                                                                                                                                    (CDR
                                                                                                                                                     V3541)))
                                                                                                                                               (do
                                                                                                                                                (shen.incinfs)
                                                                                                                                                (bind
                                                                                                                                                 V3757
                                                                                                                                                 (CONS
                                                                                                                                                  (CONS
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    X
                                                                                                                                                    V3758)
                                                                                                                                                   (CONS
                                                                                                                                                    '|:|
                                                                                                                                                    (CONS
                                                                                                                                                     (shen.lazyderef
                                                                                                                                                      A
                                                                                                                                                      V3758)
                                                                                                                                                     NIL)))
                                                                                                                                                  (CONS
                                                                                                                                                   (CONS
                                                                                                                                                    (shen.lazyderef
                                                                                                                                                     Y
                                                                                                                                                     V3758)
                                                                                                                                                    (CONS
                                                                                                                                                     '|:|
                                                                                                                                                     (CONS
                                                                                                                                                      (CONS
                                                                                                                                                       'vector
                                                                                                                                                       (CONS
                                                                                                                                                        (shen.lazyderef
                                                                                                                                                         A
                                                                                                                                                         V3758)
                                                                                                                                                        NIL))
                                                                                                                                                      NIL)))
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    Hyp
                                                                                                                                                    V3758)))
                                                                                                                                                 V3758
                                                                                                                                                 V3759)))))
                                                                                                                                        (do
                                                                                                                                         (shen.unbindv
                                                                                                                                          V3556
                                                                                                                                          V3758)
                                                                                                                                         Result)))
                                                                                                                                     'false)))))
                                                                                                                      (do
                                                                                                                       (shen.unbindv
                                                                                                                        V3554
                                                                                                                        V3758)
                                                                                                                       Result)))
                                                                                                                   'false))))
                                                                                                       (IF (shen-cl.true?
                                                                                                            (shen.pvar?
                                                                                                             V3553))
                                                                                                           (LET ((A
                                                                                                                  (shen.newpv
                                                                                                                   V3758)))
                                                                                                             (do
                                                                                                              (shen.bindv
                                                                                                               V3553
                                                                                                               (CONS
                                                                                                                A
                                                                                                                NIL)
                                                                                                               V3758)
                                                                                                              (LET ((Result
                                                                                                                     (LET ((V3557
                                                                                                                            (shen.lazyderef
                                                                                                                             (CDR
                                                                                                                              V3550)
                                                                                                                             V3758)))
                                                                                                                       (IF (NULL
                                                                                                                            V3557)
                                                                                                                           (LET ((Hyp
                                                                                                                                  (CDR
                                                                                                                                   V3541)))
                                                                                                                             (do
                                                                                                                              (shen.incinfs)
                                                                                                                              (bind
                                                                                                                               V3757
                                                                                                                               (CONS
                                                                                                                                (CONS
                                                                                                                                 (shen.lazyderef
                                                                                                                                  X
                                                                                                                                  V3758)
                                                                                                                                 (CONS
                                                                                                                                  '|:|
                                                                                                                                  (CONS
                                                                                                                                   (shen.lazyderef
                                                                                                                                    A
                                                                                                                                    V3758)
                                                                                                                                   NIL)))
                                                                                                                                (CONS
                                                                                                                                 (CONS
                                                                                                                                  (shen.lazyderef
                                                                                                                                   Y
                                                                                                                                   V3758)
                                                                                                                                  (CONS
                                                                                                                                   '|:|
                                                                                                                                   (CONS
                                                                                                                                    (CONS
                                                                                                                                     'vector
                                                                                                                                     (CONS
                                                                                                                                      (shen.lazyderef
                                                                                                                                       A
                                                                                                                                       V3758)
                                                                                                                                      NIL))
                                                                                                                                    NIL)))
                                                                                                                                 (shen.lazyderef
                                                                                                                                  Hyp
                                                                                                                                  V3758)))
                                                                                                                               V3758
                                                                                                                               V3759)))
                                                                                                                           (IF (shen-cl.true?
                                                                                                                                (shen.pvar?
                                                                                                                                 V3557))
                                                                                                                               (do
                                                                                                                                (shen.bindv
                                                                                                                                 V3557
                                                                                                                                 NIL
                                                                                                                                 V3758)
                                                                                                                                (LET ((Result
                                                                                                                                       (LET ((Hyp
                                                                                                                                              (CDR
                                                                                                                                               V3541)))
                                                                                                                                         (do
                                                                                                                                          (shen.incinfs)
                                                                                                                                          (bind
                                                                                                                                           V3757
                                                                                                                                           (CONS
                                                                                                                                            (CONS
                                                                                                                                             (shen.lazyderef
                                                                                                                                              X
                                                                                                                                              V3758)
                                                                                                                                             (CONS
                                                                                                                                              '|:|
                                                                                                                                              (CONS
                                                                                                                                               (shen.lazyderef
                                                                                                                                                A
                                                                                                                                                V3758)
                                                                                                                                               NIL)))
                                                                                                                                            (CONS
                                                                                                                                             (CONS
                                                                                                                                              (shen.lazyderef
                                                                                                                                               Y
                                                                                                                                               V3758)
                                                                                                                                              (CONS
                                                                                                                                               '|:|
                                                                                                                                               (CONS
                                                                                                                                                (CONS
                                                                                                                                                 'vector
                                                                                                                                                 (CONS
                                                                                                                                                  (shen.lazyderef
                                                                                                                                                   A
                                                                                                                                                   V3758)
                                                                                                                                                  NIL))
                                                                                                                                                NIL)))
                                                                                                                                             (shen.lazyderef
                                                                                                                                              Hyp
                                                                                                                                              V3758)))
                                                                                                                                           V3758
                                                                                                                                           V3759)))))
                                                                                                                                  (do
                                                                                                                                   (shen.unbindv
                                                                                                                                    V3557
                                                                                                                                    V3758)
                                                                                                                                   Result)))
                                                                                                                               'false)))))
                                                                                                                (do
                                                                                                                 (shen.unbindv
                                                                                                                  V3553
                                                                                                                  V3758)
                                                                                                                 Result))))
                                                                                                           'false)))
                                                                                                 (IF (shen-cl.true?
                                                                                                      (shen.pvar?
                                                                                                       V3552))
                                                                                                     (do
                                                                                                      (shen.bindv
                                                                                                       V3552
                                                                                                       'vector
                                                                                                       V3758)
                                                                                                      (LET ((Result
                                                                                                             (LET ((V3558
                                                                                                                    (shen.lazyderef
                                                                                                                     (CDR
                                                                                                                      V3551)
                                                                                                                     V3758)))
                                                                                                               (IF (CONSP
                                                                                                                    V3558)
                                                                                                                   (LET ((A
                                                                                                                          (CAR
                                                                                                                           V3558)))
                                                                                                                     (LET ((V3559
                                                                                                                            (shen.lazyderef
                                                                                                                             (CDR
                                                                                                                              V3558)
                                                                                                                             V3758)))
                                                                                                                       (IF (NULL
                                                                                                                            V3559)
                                                                                                                           (LET ((V3560
                                                                                                                                  (shen.lazyderef
                                                                                                                                   (CDR
                                                                                                                                    V3550)
                                                                                                                                   V3758)))
                                                                                                                             (IF (NULL
                                                                                                                                  V3560)
                                                                                                                                 (LET ((Hyp
                                                                                                                                        (CDR
                                                                                                                                         V3541)))
                                                                                                                                   (do
                                                                                                                                    (shen.incinfs)
                                                                                                                                    (bind
                                                                                                                                     V3757
                                                                                                                                     (CONS
                                                                                                                                      (CONS
                                                                                                                                       (shen.lazyderef
                                                                                                                                        X
                                                                                                                                        V3758)
                                                                                                                                       (CONS
                                                                                                                                        '|:|
                                                                                                                                        (CONS
                                                                                                                                         (shen.lazyderef
                                                                                                                                          A
                                                                                                                                          V3758)
                                                                                                                                         NIL)))
                                                                                                                                      (CONS
                                                                                                                                       (CONS
                                                                                                                                        (shen.lazyderef
                                                                                                                                         Y
                                                                                                                                         V3758)
                                                                                                                                        (CONS
                                                                                                                                         '|:|
                                                                                                                                         (CONS
                                                                                                                                          (CONS
                                                                                                                                           'vector
                                                                                                                                           (CONS
                                                                                                                                            (shen.lazyderef
                                                                                                                                             A
                                                                                                                                             V3758)
                                                                                                                                            NIL))
                                                                                                                                          NIL)))
                                                                                                                                       (shen.lazyderef
                                                                                                                                        Hyp
                                                                                                                                        V3758)))
                                                                                                                                     V3758
                                                                                                                                     V3759)))
                                                                                                                                 (IF (shen-cl.true?
                                                                                                                                      (shen.pvar?
                                                                                                                                       V3560))
                                                                                                                                     (do
                                                                                                                                      (shen.bindv
                                                                                                                                       V3560
                                                                                                                                       NIL
                                                                                                                                       V3758)
                                                                                                                                      (LET ((Result
                                                                                                                                             (LET ((Hyp
                                                                                                                                                    (CDR
                                                                                                                                                     V3541)))
                                                                                                                                               (do
                                                                                                                                                (shen.incinfs)
                                                                                                                                                (bind
                                                                                                                                                 V3757
                                                                                                                                                 (CONS
                                                                                                                                                  (CONS
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    X
                                                                                                                                                    V3758)
                                                                                                                                                   (CONS
                                                                                                                                                    '|:|
                                                                                                                                                    (CONS
                                                                                                                                                     (shen.lazyderef
                                                                                                                                                      A
                                                                                                                                                      V3758)
                                                                                                                                                     NIL)))
                                                                                                                                                  (CONS
                                                                                                                                                   (CONS
                                                                                                                                                    (shen.lazyderef
                                                                                                                                                     Y
                                                                                                                                                     V3758)
                                                                                                                                                    (CONS
                                                                                                                                                     '|:|
                                                                                                                                                     (CONS
                                                                                                                                                      (CONS
                                                                                                                                                       'vector
                                                                                                                                                       (CONS
                                                                                                                                                        (shen.lazyderef
                                                                                                                                                         A
                                                                                                                                                         V3758)
                                                                                                                                                        NIL))
                                                                                                                                                      NIL)))
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    Hyp
                                                                                                                                                    V3758)))
                                                                                                                                                 V3758
                                                                                                                                                 V3759)))))
                                                                                                                                        (do
                                                                                                                                         (shen.unbindv
                                                                                                                                          V3560
                                                                                                                                          V3758)
                                                                                                                                         Result)))
                                                                                                                                     'false)))
                                                                                                                           (IF (shen-cl.true?
                                                                                                                                (shen.pvar?
                                                                                                                                 V3559))
                                                                                                                               (do
                                                                                                                                (shen.bindv
                                                                                                                                 V3559
                                                                                                                                 NIL
                                                                                                                                 V3758)
                                                                                                                                (LET ((Result
                                                                                                                                       (LET ((V3561
                                                                                                                                              (shen.lazyderef
                                                                                                                                               (CDR
                                                                                                                                                V3550)
                                                                                                                                               V3758)))
                                                                                                                                         (IF (NULL
                                                                                                                                              V3561)
                                                                                                                                             (LET ((Hyp
                                                                                                                                                    (CDR
                                                                                                                                                     V3541)))
                                                                                                                                               (do
                                                                                                                                                (shen.incinfs)
                                                                                                                                                (bind
                                                                                                                                                 V3757
                                                                                                                                                 (CONS
                                                                                                                                                  (CONS
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    X
                                                                                                                                                    V3758)
                                                                                                                                                   (CONS
                                                                                                                                                    '|:|
                                                                                                                                                    (CONS
                                                                                                                                                     (shen.lazyderef
                                                                                                                                                      A
                                                                                                                                                      V3758)
                                                                                                                                                     NIL)))
                                                                                                                                                  (CONS
                                                                                                                                                   (CONS
                                                                                                                                                    (shen.lazyderef
                                                                                                                                                     Y
                                                                                                                                                     V3758)
                                                                                                                                                    (CONS
                                                                                                                                                     '|:|
                                                                                                                                                     (CONS
                                                                                                                                                      (CONS
                                                                                                                                                       'vector
                                                                                                                                                       (CONS
                                                                                                                                                        (shen.lazyderef
                                                                                                                                                         A
                                                                                                                                                         V3758)
                                                                                                                                                        NIL))
                                                                                                                                                      NIL)))
                                                                                                                                                   (shen.lazyderef
                                                                                                                                                    Hyp
                                                                                                                                                    V3758)))
                                                                                                                                                 V3758
                                                                                                                                                 V3759)))
                                                                                                                                             (IF (shen-cl.true?
                                                                                                                                                  (shen.pvar?
                                                                                                                                                   V3561))
                                                                                                                                                 (do
                                                                                                                                                  (shen.bindv
                                                                                                                                                   V3561
                                                                                                                                                   NIL
                                                                                                                                                   V3758)
                                                                                                                                                  (LET ((Result
                                                                                                                                                         (LET ((Hyp
                                                                                                                                                                (CDR
                                                                                                                                                                 V3541)))
                                                                                                                                                           (do
                                                                                                                                                            (shen.incinfs)
                                                                                                                                                            (bind
                                                                                                                                                             V3757
                                                                                                                                                             (CONS
                                                                                                                                                              (CONS
                                                                                                                                                               (shen.lazyderef
                                                                                                                                                                X
                                                                                                                                                                V3758)
                                                                                                                                                               (CONS
                                                                                                                                                                '|:|
                                                                                                                                                                (CONS
                                                                                                                                                                 (shen.lazyderef
                                                                                                                                                                  A
                                                                                                                                                                  V3758)
                                                                                                                                                                 NIL)))
                                                                                                                                                              (CONS
                                                                                                                                                               (CONS
                                                                                                                                                                (shen.lazyderef
                                                                                                                                                                 Y
                                                                                                                                                                 V3758)
                                                                                                                                                                (CONS
                                                                                                                                                                 '|:|
                                                                                                                                                                 (CONS
                                                                                                                                                                  (CONS
                                                                                                                                                                   'vector
                                                                                                                                                                   (CONS
                                                                                                                                                                    (shen.lazyderef
                                                                                                                                                                     A
                                                                                                                                                                     V3758)
                                                                                                                                                                    NIL))
                                                                                                                                                                  NIL)))
                                                                                                                                                               (shen.lazyderef
                                                                                                                                                                Hyp
                                                                                                                                                                V3758)))
                                                                                                                                                             V3758
                                                                                                                                                             V3759)))))
                                                                                                                                                    (do
                                                                                                                                                     (shen.unbindv
                                                                                                                                                      V3561
                                                                                                                                                      V3758)
                                                                                                                                                     Result)))
                                                                                                                                                 'false)))))
                                                                                                                                  (do
                                                                                                                                   (shen.unbindv
                                                                                                                                    V3559
                                                                                                                                    V3758)
                                                                                                                                   Result)))
                                                                                                                               'false))))
                                                                                                                   (IF (shen-cl.true?
                                                                                                                        (shen.pvar?
                                                                                                                         V3558))
                                                                                                                       (LET ((A
                                                                                                                              (shen.newpv
                                                                                                                               V3758)))
                                                                                                                         (do
                                                                                                                          (shen.bindv
                                                                                                                           V3558
                                                                                                                           (CONS
                                                                                                                            A
                                                                                                                            NIL)
                                                                                                                           V3758)
                                                                                                                          (LET ((Result
                                                                                                                                 (LET ((V3562
                                                                                                                                        (shen.lazyderef
                                                                                                                                         (CDR
                                                                                                                                          V3550)
                                                                                                                                         V3758)))
                                                                                                                                   (IF (NULL
                                                                                                                                        V3562)
                                                                                                                                       (LET ((Hyp
                                                                                                                                              (CDR
                                                                                                                                               V3541)))
                                                                                                                                         (do
                                                                                                                                          (shen.incinfs)
                                                                                                                                          (bind
                                                                                                                                           V3757
                                                                                                                                           (CONS
                                                                                                                                            (CONS
                                                                                                                                             (shen.lazyderef
                                                                                                                                              X
                                                                                                                                              V3758)
                                                                                                                                             (CONS
                                                                                                                                              '|:|
                                                                                                                                              (CONS
                                                                                                                                               (shen.lazyderef
                                                                                                                                                A
                                                                                                                                                V3758)
                                                                                                                                               NIL)))
                                                                                                                                            (CONS
                                                                                                                                             (CONS
                                                                                                                                              (shen.lazyderef
                                                                                                                                               Y
                                                                                                                                               V3758)
                                                                                                                                              (CONS
                                                                                                                                               '|:|
                                                                                                                                               (CONS
                                                                                                                                                (CONS
                                                                                                                                                 'vector
                                                                                                                                                 (CONS
                                                                                                                                                  (shen.lazyderef
                                                                                                                                                   A
                                                                                                                                                   V3758)
                                                                                                                                                  NIL))
                                                                                                                                                NIL)))
                                                                                                                                             (shen.lazyderef
                                                                                                                                              Hyp
                                                                                                                                              V3758)))
                                                                                                                                           V3758
                                                                                                                                           V3759)))
                                                                                                                                       (IF (shen-cl.true?
                                                                                                                                            (shen.pvar?
                                                                                                                                             V3562))
                                                                                                                                           (do
                                                                                                                                            (shen.bindv
                                                                                                                                             V3562
                                                                                                                                             NIL
                                                                                                                                             V3758)
                                                                                                                                            (LET ((Result
                                                                                                                                                   (LET ((Hyp
                                                                                                                                                          (CDR
                                                                                                                                                           V3541)))
                                                                                                                                                     (do
                                                                                                                                                      (shen.incinfs)
                                                                                                                                                      (bind
                                                                                                                                                       V3757
                                                                                                                                                       (CONS
                                                                                                                                                        (CONS
                                                                                                                                                         (shen.lazyderef
                                                                                                                                                          X
                                                                                                                                                          V3758)
                                                                                                                                                         (CONS
                                                                                                                                                          '|:|
                                                                                                                                                          (CONS
                                                                                                                                                           (shen.lazyderef
                                                                                                                                                            A
                                                                                                                                                            V3758)
                                                                                                                                                           NIL)))
                                                                                                                                                        (CONS
                                                                                                                                                         (CONS
                                                                                                                                                          (shen.lazyderef
                                                                                                                                                           Y
                                                                                                                                                           V3758)
                                                                                                                                                          (CONS
                                                                                                                                                           '|:|
                                                                                                                                                           (CONS
                                                                                                                                                            (CONS
                                                                                                                                                             'vector
                                                                                                                                                             (CONS
                                                                                                                                                              (shen.lazyderef
                                                                                                                                                               A
                                                                                                                                                               V3758)
                                                                                                                                                              NIL))
                                                                                                                                                            NIL)))
                                                                                                                                                         (shen.lazyderef
                                                                                                                                                          Hyp
                                                                                                                                                          V3758)))
                                                                                                                                                       V3758
                                                                                                                                                       V3759)))))
                                                                                                                                              (do
                                                                                                                                               (shen.unbindv
                                                                                                                                                V3562
                                                                                                                                                V3758)
                                                                                                                                               Result)))
                                                                                                                                           'false)))))
                                                                                                                            (do
                                                                                                                             (shen.unbindv
                                                                                                                              V3558
                                                                                                                              V3758)
                                                                                                                             Result))))
                                                                                                                       'false)))))
                                                                                                        (do
                                                                                                         (shen.unbindv
                                                                                                          V3552
                                                                                                          V3758)
                                                                                                         Result)))
                                                                                                     'false)))
                                                                                           (IF (shen-cl.true?
                                                                                                (shen.pvar?
                                                                                                 V3551))
                                                                                               (LET ((A
                                                                                                      (shen.newpv
                                                                                                       V3758)))
                                                                                                 (do
                                                                                                  (shen.bindv
                                                                                                   V3551
                                                                                                   (CONS
                                                                                                    'vector
                                                                                                    (CONS
                                                                                                     A
                                                                                                     NIL))
                                                                                                   V3758)
                                                                                                  (LET ((Result
                                                                                                         (LET ((V3563
                                                                                                                (shen.lazyderef
                                                                                                                 (CDR
                                                                                                                  V3550)
                                                                                                                 V3758)))
                                                                                                           (IF (NULL
                                                                                                                V3563)
                                                                                                               (LET ((Hyp
                                                                                                                      (CDR
                                                                                                                       V3541)))
                                                                                                                 (do
                                                                                                                  (shen.incinfs)
                                                                                                                  (bind
                                                                                                                   V3757
                                                                                                                   (CONS
                                                                                                                    (CONS
                                                                                                                     (shen.lazyderef
                                                                                                                      X
                                                                                                                      V3758)
                                                                                                                     (CONS
                                                                                                                      '|:|
                                                                                                                      (CONS
                                                                                                                       (shen.lazyderef
                                                                                                                        A
                                                                                                                        V3758)
                                                                                                                       NIL)))
                                                                                                                    (CONS
                                                                                                                     (CONS
                                                                                                                      (shen.lazyderef
                                                                                                                       Y
                                                                                                                       V3758)
                                                                                                                      (CONS
                                                                                                                       '|:|
                                                                                                                       (CONS
                                                                                                                        (CONS
                                                                                                                         'vector
                                                                                                                         (CONS
                                                                                                                          (shen.lazyderef
                                                                                                                           A
                                                                                                                           V3758)
                                                                                                                          NIL))
                                                                                                                        NIL)))
                                                                                                                     (shen.lazyderef
                                                                                                                      Hyp
                                                                                                                      V3758)))
                                                                                                                   V3758
                                                                                                                   V3759)))
                                                                                                               (IF (shen-cl.true?
                                                                                                                    (shen.pvar?
                                                                                                                     V3563))
                                                                                                                   (do
                                                                                                                    (shen.bindv
                                                                                                                     V3563
                                                                                                                     NIL
                                                                                                                     V3758)
                                                                                                                    (LET ((Result
                                                                                                                           (LET ((Hyp
                                                                                                                                  (CDR
                                                                                                                                   V3541)))
                                                                                                                             (do
                                                                                                                              (shen.incinfs)
                                                                                                                              (bind
                                                                                                                               V3757
                                                                                                                               (CONS
                                                                                                                                (CONS
                                                                                                                                 (shen.lazyderef
                                                                                                                                  X
                                                                                                                                  V3758)
                                                                                                                                 (CONS
                                                                                                                                  '|:|
                                                                                                                                  (CONS
                                                                                                                                   (shen.lazyderef
                                                                                                                                    A
                                                                                                                                    V3758)
                                                                                                                                   NIL)))
                                                                                                                                (CONS
                                                                                                                                 (CONS
                                                                                                                                  (shen.lazyderef
                                                                                                                                   Y
                                                                                                                                   V3758)
                                                                                                                                  (CONS
                                                                                                                                   '|:|
                                                                                                                                   (CONS
                                                                                                                                    (CONS
                                                                                                                                     'vector
                                                                                                                                     (CONS
                                                                                                                                      (shen.lazyderef
                                                                                                                                       A
                                                                                                                                       V3758)
                                                                                                                                      NIL))
                                                                                                                                    NIL)))
                                                                                                                                 (shen.lazyderef
                                                                                                                                  Hyp
                                                                                                                                  V3758)))
                                                                                                                               V3758
                                                                                                                               V3759)))))
                                                                                                                      (do
                                                                                                                       (shen.unbindv
                                                                                                                        V3563
                                                                                                                        V3758)
                                                                                                                       Result)))
                                                                                                                   'false)))))
                                                                                                    (do
                                                                                                     (shen.unbindv
                                                                                                      V3551
                                                                                                      V3758)
                                                                                                     Result))))
                                                                                               'false)))
                                                                                     'false))
                                                                               'false))
                                                                         'false))
                                                                   'false)))
                                                           'false)))
                                                   'false))
                                             'false))
                                       'false))
                                 'false))
                           'false))))
                (IF (EQ Case 'false)
                    (LET ((Case
                           (LET ((V3564 (shen.lazyderef V3756 V3758)))
                             (IF (CONSP V3564)
                                 (LET ((V3565
                                        (shen.lazyderef (CAR V3564) V3758)))
                                   (IF (CONSP V3565)
                                       (LET ((V3566
                                              (shen.lazyderef (CAR V3565)
                                                              V3758)))
                                         (IF (CONSP V3566)
                                             (LET ((V3567
                                                    (shen.lazyderef (CAR V3566)
                                                                    V3758)))
                                               (IF (EQ '@s V3567)
                                                   (LET ((V3568
                                                          (shen.lazyderef
                                                           (CDR V3566) V3758)))
                                                     (IF (CONSP V3568)
                                                         (LET ((X (CAR V3568)))
                                                           (LET ((V3569
                                                                  (shen.lazyderef
                                                                   (CDR V3568)
                                                                   V3758)))
                                                             (IF (CONSP V3569)
                                                                 (LET ((Y
                                                                        (CAR
                                                                         V3569)))
                                                                   (LET ((V3570
                                                                          (shen.lazyderef
                                                                           (CDR
                                                                            V3569)
                                                                           V3758)))
                                                                     (IF (NULL
                                                                          V3570)
                                                                         (LET ((V3571
                                                                                (shen.lazyderef
                                                                                 (CDR
                                                                                  V3565)
                                                                                 V3758)))
                                                                           (IF (CONSP
                                                                                V3571)
                                                                               (LET ((V3572
                                                                                      (shen.lazyderef
                                                                                       (CAR
                                                                                        V3571)
                                                                                       V3758)))
                                                                                 (IF (EQ
                                                                                      '|:|
                                                                                      V3572)
                                                                                     (LET ((V3573
                                                                                            (shen.lazyderef
                                                                                             (CDR
                                                                                              V3571)
                                                                                             V3758)))
                                                                                       (IF (CONSP
                                                                                            V3573)
                                                                                           (LET ((V3574
                                                                                                  (shen.lazyderef
                                                                                                   (CAR
                                                                                                    V3573)
                                                                                                   V3758)))
                                                                                             (IF (EQ
                                                                                                  'string
                                                                                                  V3574)
                                                                                                 (LET ((V3575
                                                                                                        (shen.lazyderef
                                                                                                         (CDR
                                                                                                          V3573)
                                                                                                         V3758)))
                                                                                                   (IF (NULL
                                                                                                        V3575)
                                                                                                       (LET ((Hyp
                                                                                                              (CDR
                                                                                                               V3564)))
                                                                                                         (do
                                                                                                          (shen.incinfs)
                                                                                                          (bind
                                                                                                           V3757
                                                                                                           (CONS
                                                                                                            (CONS
                                                                                                             (shen.lazyderef
                                                                                                              X
                                                                                                              V3758)
                                                                                                             (CONS
                                                                                                              '|:|
                                                                                                              (CONS
                                                                                                               'string
                                                                                                               NIL)))
                                                                                                            (CONS
                                                                                                             (CONS
                                                                                                              (shen.lazyderef
                                                                                                               Y
                                                                                                               V3758)
                                                                                                              (CONS
                                                                                                               '|:|
                                                                                                               (CONS
                                                                                                                'string
                                                                                                                NIL)))
                                                                                                             (shen.lazyderef
                                                                                                              Hyp
                                                                                                              V3758)))
                                                                                                           V3758
                                                                                                           V3759)))
                                                                                                       (IF (shen-cl.true?
                                                                                                            (shen.pvar?
                                                                                                             V3575))
                                                                                                           (do
                                                                                                            (shen.bindv
                                                                                                             V3575
                                                                                                             NIL
                                                                                                             V3758)
                                                                                                            (LET ((Result
                                                                                                                   (LET ((Hyp
                                                                                                                          (CDR
                                                                                                                           V3564)))
                                                                                                                     (do
                                                                                                                      (shen.incinfs)
                                                                                                                      (bind
                                                                                                                       V3757
                                                                                                                       (CONS
                                                                                                                        (CONS
                                                                                                                         (shen.lazyderef
                                                                                                                          X
                                                                                                                          V3758)
                                                                                                                         (CONS
                                                                                                                          '|:|
                                                                                                                          (CONS
                                                                                                                           'string
                                                                                                                           NIL)))
                                                                                                                        (CONS
                                                                                                                         (CONS
                                                                                                                          (shen.lazyderef
                                                                                                                           Y
                                                                                                                           V3758)
                                                                                                                          (CONS
                                                                                                                           '|:|
                                                                                                                           (CONS
                                                                                                                            'string
                                                                                                                            NIL)))
                                                                                                                         (shen.lazyderef
                                                                                                                          Hyp
                                                                                                                          V3758)))
                                                                                                                       V3758
                                                                                                                       V3759)))))
                                                                                                              (do
                                                                                                               (shen.unbindv
                                                                                                                V3575
                                                                                                                V3758)
                                                                                                               Result)))
                                                                                                           'false)))
                                                                                                 (IF (shen-cl.true?
                                                                                                      (shen.pvar?
                                                                                                       V3574))
                                                                                                     (do
                                                                                                      (shen.bindv
                                                                                                       V3574
                                                                                                       'string
                                                                                                       V3758)
                                                                                                      (LET ((Result
                                                                                                             (LET ((V3576
                                                                                                                    (shen.lazyderef
                                                                                                                     (CDR
                                                                                                                      V3573)
                                                                                                                     V3758)))
                                                                                                               (IF (NULL
                                                                                                                    V3576)
                                                                                                                   (LET ((Hyp
                                                                                                                          (CDR
                                                                                                                           V3564)))
                                                                                                                     (do
                                                                                                                      (shen.incinfs)
                                                                                                                      (bind
                                                                                                                       V3757
                                                                                                                       (CONS
                                                                                                                        (CONS
                                                                                                                         (shen.lazyderef
                                                                                                                          X
                                                                                                                          V3758)
                                                                                                                         (CONS
                                                                                                                          '|:|
                                                                                                                          (CONS
                                                                                                                           'string
                                                                                                                           NIL)))
                                                                                                                        (CONS
                                                                                                                         (CONS
                                                                                                                          (shen.lazyderef
                                                                                                                           Y
                                                                                                                           V3758)
                                                                                                                          (CONS
                                                                                                                           '|:|
                                                                                                                           (CONS
                                                                                                                            'string
                                                                                                                            NIL)))
                                                                                                                         (shen.lazyderef
                                                                                                                          Hyp
                                                                                                                          V3758)))
                                                                                                                       V3758
                                                                                                                       V3759)))
                                                                                                                   (IF (shen-cl.true?
                                                                                                                        (shen.pvar?
                                                                                                                         V3576))
                                                                                                                       (do
                                                                                                                        (shen.bindv
                                                                                                                         V3576
                                                                                                                         NIL
                                                                                                                         V3758)
                                                                                                                        (LET ((Result
                                                                                                                               (LET ((Hyp
                                                                                                                                      (CDR
                                                                                                                                       V3564)))
                                                                                                                                 (do
                                                                                                                                  (shen.incinfs)
                                                                                                                                  (bind
                                                                                                                                   V3757
                                                                                                                                   (CONS
                                                                                                                                    (CONS
                                                                                                                                     (shen.lazyderef
                                                                                                                                      X
                                                                                                                                      V3758)
                                                                                                                                     (CONS
                                                                                                                                      '|:|
                                                                                                                                      (CONS
                                                                                                                                       'string
                                                                                                                                       NIL)))
                                                                                                                                    (CONS
                                                                                                                                     (CONS
                                                                                                                                      (shen.lazyderef
                                                                                                                                       Y
                                                                                                                                       V3758)
                                                                                                                                      (CONS
                                                                                                                                       '|:|
                                                                                                                                       (CONS
                                                                                                                                        'string
                                                                                                                                        NIL)))
                                                                                                                                     (shen.lazyderef
                                                                                                                                      Hyp
                                                                                                                                      V3758)))
                                                                                                                                   V3758
                                                                                                                                   V3759)))))
                                                                                                                          (do
                                                                                                                           (shen.unbindv
                                                                                                                            V3576
                                                                                                                            V3758)
                                                                                                                           Result)))
                                                                                                                       'false)))))
                                                                                                        (do
                                                                                                         (shen.unbindv
                                                                                                          V3574
                                                                                                          V3758)
                                                                                                         Result)))
                                                                                                     'false)))
                                                                                           'false))
                                                                                     'false))
                                                                               'false))
                                                                         'false)))
                                                                 'false)))
                                                         'false))
                                                   'false))
                                             'false))
                                       'false))
                                 'false))))
                      (IF (EQ Case 'false)
                          (LET ((V3577 (shen.lazyderef V3756 V3758)))
                            (IF (CONSP V3577)
                                (LET ((X (CAR V3577)))
                                  (LET ((Hyp (CDR V3577)))
                                    (LET ((NewHyps (shen.newpv V3758)))
                                      (do (shen.incinfs)
                                          (bind V3757
                                                (CONS (shen.lazyderef X V3758)
                                                      (shen.lazyderef NewHyps
                                                                      V3758))
                                                V3758
                                                (freeze
                                                 (shen.t*-hyps Hyp NewHyps
                                                  V3758 V3759)))))))
                                'false))
                          Case))
                    Case))
              Case))
        Case)))

(DEFUN shen.show (V3776 V3777 V3778 V3779)
  (COND
   ((shen-cl.true? shen.*spy*)
    (do (shen.line)
        (do (shen.show-p (shen.deref V3776 V3778))
            (do (nl 1)
                (do (nl 1)
                    (do (shen.show-assumptions (shen.deref V3777 V3778) 1)
                        (do
                         (shen.prhush "
> "
                                      (stoutput))
                         (do (shen.pause-for-user) (thaw V3779)))))))))
   (T (thaw V3779))))

(DEFUN shen.line ()
  (LET ((Infs (inferences)))
    (shen.prhush
     (cn "____________________________________________________________ "
         (shen.app Infs
                   (cn " inference"
                       (shen.app
                        (IF (IF (NUMBERP Infs)
                                (= Infs 1))
                            ""
                            "s")
                        " 
?- "
                        'shen.a))
                   'shen.a))
     (stoutput))))

(DEFUN shen.show-p (V3781)
  (COND
   ((AND (CONSP V3781)
         (AND (CONSP (CDR V3781))
              (AND (EQ '|:| (CAR (CDR V3781)))
                   (AND (CONSP (CDR (CDR V3781)))
                        (NULL (CDR (CDR (CDR V3781))))))))
    (shen.prhush
     (shen.app (CAR V3781)
               (cn " : " (shen.app (CAR (CDR (CDR V3781))) "" 'shen.r))
               'shen.r)
     (stoutput)))
   (T (shen.prhush (shen.app V3781 "" 'shen.r) (stoutput)))))

(DEFUN shen.show-assumptions (V3786 V3787)
  (COND ((NULL V3786) 'shen.skip)
        ((CONSP V3786)
         (do (shen.prhush (shen.app V3787 ". " 'shen.a) (stoutput))
             (do (shen.show-p (CAR V3786))
                 (do (nl 1)
                     (shen.show-assumptions (CDR V3786) (shen.add V3787 1))))))
        (T (shen.f_error 'shen.show-assumptions))))

(DEFUN shen.pause-for-user ()
  (LET ((Byte (read-byte (stinput))))
    (IF (IF (NUMBERP Byte)
            (= Byte 94))
        (simple-error "input aborted
")
        (nl 1))))

(DEFUN shen.typedf? (V3789) (cons? (assoc V3789 shen.*signedfuncs*)))

(DEFUN shen.sigf (V3791) (concat 'shen.type-signature-of- V3791))

(DEFUN shen.placeholder () (gensym '&&))

(DEFUN shen.base (V3796 V3797 V3798 V3799)
  (LET ((Case
         (LET ((V3480 (shen.lazyderef V3797 V3798)))
           (IF (EQ 'number V3480)
               (do (shen.incinfs)
                   (fwhen (number? (shen.lazyderef V3796 V3798)) V3798 V3799))
               (IF (shen-cl.true? (shen.pvar? V3480))
                   (do (shen.bindv V3480 'number V3798)
                       (LET ((Result
                              (do (shen.incinfs)
                                  (fwhen (number? (shen.lazyderef V3796 V3798))
                                         V3798 V3799))))
                         (do (shen.unbindv V3480 V3798) Result)))
                   'false)))))
    (IF (EQ Case 'false)
        (LET ((Case
               (LET ((V3481 (shen.lazyderef V3797 V3798)))
                 (IF (EQ 'boolean V3481)
                     (do (shen.incinfs)
                         (fwhen (boolean? (shen.lazyderef V3796 V3798)) V3798
                                V3799))
                     (IF (shen-cl.true? (shen.pvar? V3481))
                         (do (shen.bindv V3481 'boolean V3798)
                             (LET ((Result
                                    (do (shen.incinfs)
                                        (fwhen
                                         (boolean?
                                          (shen.lazyderef V3796 V3798))
                                         V3798 V3799))))
                               (do (shen.unbindv V3481 V3798) Result)))
                         'false)))))
          (IF (EQ Case 'false)
              (LET ((Case
                     (LET ((V3482 (shen.lazyderef V3797 V3798)))
                       (IF (EQ 'string V3482)
                           (do (shen.incinfs)
                               (fwhen (string? (shen.lazyderef V3796 V3798))
                                      V3798 V3799))
                           (IF (shen-cl.true? (shen.pvar? V3482))
                               (do (shen.bindv V3482 'string V3798)
                                   (LET ((Result
                                          (do (shen.incinfs)
                                              (fwhen
                                               (string?
                                                (shen.lazyderef V3796 V3798))
                                               V3798 V3799))))
                                     (do (shen.unbindv V3482 V3798) Result)))
                               'false)))))
                (IF (EQ Case 'false)
                    (LET ((Case
                           (LET ((V3483 (shen.lazyderef V3797 V3798)))
                             (IF (EQ 'symbol V3483)
                                 (do (shen.incinfs)
                                     (fwhen
                                      (symbol? (shen.lazyderef V3796 V3798))
                                      V3798
                                      (freeze
                                       (fwhen
                                        (not
                                         (shen.ue?
                                          (shen.lazyderef V3796 V3798)))
                                        V3798 V3799))))
                                 (IF (shen-cl.true? (shen.pvar? V3483))
                                     (do (shen.bindv V3483 'symbol V3798)
                                         (LET ((Result
                                                (do (shen.incinfs)
                                                    (fwhen
                                                     (symbol?
                                                      (shen.lazyderef V3796
                                                                      V3798))
                                                     V3798
                                                     (freeze
                                                      (fwhen
                                                       (not
                                                        (shen.ue?
                                                         (shen.lazyderef V3796
                                                                         V3798)))
                                                       V3798 V3799))))))
                                           (do (shen.unbindv V3483 V3798)
                                               Result)))
                                     'false)))))
                      (IF (EQ Case 'false)
                          (LET ((V3484 (shen.lazyderef V3796 V3798)))
                            (IF (NULL V3484)
                                (LET ((V3485 (shen.lazyderef V3797 V3798)))
                                  (IF (CONSP V3485)
                                      (LET ((V3486
                                             (shen.lazyderef (CAR V3485)
                                                             V3798)))
                                        (IF (EQ 'list V3486)
                                            (LET ((V3487
                                                   (shen.lazyderef (CDR V3485)
                                                                   V3798)))
                                              (IF (CONSP V3487)
                                                  (LET ((A (CAR V3487)))
                                                    (LET ((V3488
                                                           (shen.lazyderef
                                                            (CDR V3487) V3798)))
                                                      (IF (NULL V3488)
                                                          (do (shen.incinfs)
                                                              (thaw V3799))
                                                          (IF (shen-cl.true?
                                                               (shen.pvar?
                                                                V3488))
                                                              (do
                                                               (shen.bindv
                                                                V3488 NIL
                                                                V3798)
                                                               (LET ((Result
                                                                      (do
                                                                       (shen.incinfs)
                                                                       (thaw
                                                                        V3799))))
                                                                 (do
                                                                  (shen.unbindv
                                                                   V3488 V3798)
                                                                  Result)))
                                                              'false))))
                                                  (IF (shen-cl.true?
                                                       (shen.pvar? V3487))
                                                      (LET ((A
                                                             (shen.newpv
                                                              V3798)))
                                                        (do
                                                         (shen.bindv V3487
                                                                     (CONS A
                                                                           NIL)
                                                                     V3798)
                                                         (LET ((Result
                                                                (do
                                                                 (shen.incinfs)
                                                                 (thaw V3799))))
                                                           (do
                                                            (shen.unbindv V3487
                                                                          V3798)
                                                            Result))))
                                                      'false)))
                                            (IF (shen-cl.true?
                                                 (shen.pvar? V3486))
                                                (do
                                                 (shen.bindv V3486 'list V3798)
                                                 (LET ((Result
                                                        (LET ((V3489
                                                               (shen.lazyderef
                                                                (CDR V3485)
                                                                V3798)))
                                                          (IF (CONSP V3489)
                                                              (LET ((A
                                                                     (CAR
                                                                      V3489)))
                                                                (LET ((V3490
                                                                       (shen.lazyderef
                                                                        (CDR
                                                                         V3489)
                                                                        V3798)))
                                                                  (IF (NULL
                                                                       V3490)
                                                                      (do
                                                                       (shen.incinfs)
                                                                       (thaw
                                                                        V3799))
                                                                      (IF (shen-cl.true?
                                                                           (shen.pvar?
                                                                            V3490))
                                                                          (do
                                                                           (shen.bindv
                                                                            V3490
                                                                            NIL
                                                                            V3798)
                                                                           (LET ((Result
                                                                                  (do
                                                                                   (shen.incinfs)
                                                                                   (thaw
                                                                                    V3799))))
                                                                             (do
                                                                              (shen.unbindv
                                                                               V3490
                                                                               V3798)
                                                                              Result)))
                                                                          'false))))
                                                              (IF (shen-cl.true?
                                                                   (shen.pvar?
                                                                    V3489))
                                                                  (LET ((A
                                                                         (shen.newpv
                                                                          V3798)))
                                                                    (do
                                                                     (shen.bindv
                                                                      V3489
                                                                      (CONS A
                                                                            NIL)
                                                                      V3798)
                                                                     (LET ((Result
                                                                            (do
                                                                             (shen.incinfs)
                                                                             (thaw
                                                                              V3799))))
                                                                       (do
                                                                        (shen.unbindv
                                                                         V3489
                                                                         V3798)
                                                                        Result))))
                                                                  'false)))))
                                                   (do
                                                    (shen.unbindv V3486 V3798)
                                                    Result)))
                                                'false)))
                                      (IF (shen-cl.true? (shen.pvar? V3485))
                                          (LET ((A (shen.newpv V3798)))
                                            (do
                                             (shen.bindv V3485
                                                         (CONS 'list
                                                               (CONS A NIL))
                                                         V3798)
                                             (LET ((Result
                                                    (do (shen.incinfs)
                                                        (thaw V3799))))
                                               (do (shen.unbindv V3485 V3798)
                                                   Result))))
                                          'false)))
                                'false))
                          Case))
                    Case))
              Case))
        Case)))

(DEFUN shen.by_hypothesis (V3805 V3806 V3807 V3808 V3809)
  (LET ((Case
         (LET ((V3471 (shen.lazyderef V3807 V3808)))
           (IF (CONSP V3471)
               (LET ((V3472 (shen.lazyderef (CAR V3471) V3808)))
                 (IF (CONSP V3472)
                     (LET ((Y (CAR V3472)))
                       (LET ((V3473 (shen.lazyderef (CDR V3472) V3808)))
                         (IF (CONSP V3473)
                             (LET ((V3474 (shen.lazyderef (CAR V3473) V3808)))
                               (IF (EQ '|:| V3474)
                                   (LET ((V3475
                                          (shen.lazyderef (CDR V3473) V3808)))
                                     (IF (CONSP V3475)
                                         (LET ((B (CAR V3475)))
                                           (LET ((V3476
                                                  (shen.lazyderef (CDR V3475)
                                                                  V3808)))
                                             (IF (NULL V3476)
                                                 (do (shen.incinfs)
                                                     (identical V3805 Y V3808
                                                                (freeze
                                                                 (unify! V3806
                                                                         B
                                                                         V3808
                                                                         V3809))))
                                                 'false)))
                                         'false))
                                   'false))
                             'false)))
                     'false))
               'false))))
    (IF (EQ Case 'false)
        (LET ((V3477 (shen.lazyderef V3807 V3808)))
          (IF (CONSP V3477)
              (LET ((Hyp (CDR V3477)))
                (do (shen.incinfs)
                    (shen.by_hypothesis V3805 V3806 Hyp V3808 V3809)))
              'false))
        Case)))

(DEFUN shen.t*-def (V3815 V3816 V3817 V3818 V3819)
  (LET ((V3465 (shen.lazyderef V3815 V3818)))
    (IF (CONSP V3465)
        (LET ((V3466 (shen.lazyderef (CAR V3465) V3818)))
          (IF (EQ 'define V3466)
              (LET ((V3467 (shen.lazyderef (CDR V3465) V3818)))
                (IF (CONSP V3467)
                    (LET ((F (CAR V3467)))
                      (LET ((X (CDR V3467)))
                        (LET ((Y (shen.newpv V3818)))
                          (LET ((E (shen.newpv V3818)))
                            (do (shen.incinfs)
                                (shen.t*-defh
                                 (compile #'(LAMBDA (Y) (shen.<sig+rules> Y)) X
                                          #'(LAMBDA (E)
                                              (IF (CONSP E)
                                                  (simple-error
                                                   (cn "parse error here: "
                                                       (shen.app E "
"
                                                                 'shen.s)))
                                                  (simple-error "parse error
"))))
                                 F V3816 V3817 V3818 V3819))))))
                    'false))
              'false))
        'false)))

(DEFUN shen.t*-defh (V3826 V3827 V3828 V3829 V3830 V3831)
  (LET ((V3461 (shen.lazyderef V3826 V3830)))
    (IF (CONSP V3461)
        (LET ((Sig (CAR V3461)))
          (LET ((Rules (CDR V3461)))
            (do (shen.incinfs)
                (shen.t*-defhh Sig (shen.ue-sig Sig) V3827 V3828 V3829 Rules
                 V3830 V3831))))
        'false)))

(DEFUN shen.t*-defhh (V3840 V3841 V3842 V3843 V3844 V3845 V3846 V3847)
  (do (shen.incinfs)
      (shen.t*-rules V3845 V3841 1 V3842
       (CONS (CONS V3842 (CONS '|:| (CONS V3841 NIL))) V3844) V3846
       (freeze (shen.memo V3842 V3840 V3843 V3846 V3847)))))

(DEFUN shen.memo (V3853 V3854 V3855 V3856 V3857)
  (LET ((Jnk (shen.newpv V3856)))
    (do (shen.incinfs)
        (unify! V3855 V3854 V3856
                (freeze
                 (bind Jnk
                       (declare (shen.lazyderef V3853 V3856)
                                (shen.lazyderef V3855 V3856))
                       V3856 V3857))))))

(DEFUN shen.<sig+rules> (V3859)
  (LET ((Parse_shen.<signature> (shen.<signature> V3859)))
    (IF (NOT (EQ (fail) Parse_shen.<signature>))
        (LET ((Parse_shen.<non-ll-rules>
               (shen.<non-ll-rules> Parse_shen.<signature>)))
          (IF (NOT (EQ (fail) Parse_shen.<non-ll-rules>))
              (shen.pair (CAR Parse_shen.<non-ll-rules>)
                         (CONS (shen.hdtl Parse_shen.<signature>)
                               (shen.hdtl Parse_shen.<non-ll-rules>)))
              (fail)))
        (fail))))

(DEFUN shen.<non-ll-rules> (V3861)
  (LET ((YaccParse
         (LET ((Parse_shen.<rule> (shen.<rule> V3861)))
           (IF (NOT (EQ (fail) Parse_shen.<rule>))
               (LET ((Parse_shen.<non-ll-rules>
                      (shen.<non-ll-rules> Parse_shen.<rule>)))
                 (IF (NOT (EQ (fail) Parse_shen.<non-ll-rules>))
                     (shen.pair (CAR Parse_shen.<non-ll-rules>)
                                (CONS (shen.hdtl Parse_shen.<rule>)
                                      (shen.hdtl Parse_shen.<non-ll-rules>)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_shen.<rule> (shen.<rule> V3861)))
          (IF (NOT (EQ (fail) Parse_shen.<rule>))
              (shen.pair (CAR Parse_shen.<rule>)
                         (CONS (shen.hdtl Parse_shen.<rule>) NIL))
              (fail)))
        YaccParse)))

(DEFUN shen.ue (V3863)
  (COND
   ((AND (CONSP V3863)
         (AND (CONSP (CDR V3863))
              (AND (NULL (CDR (CDR V3863))) (EQ (CAR V3863) 'protect))))
    V3863)
   ((CONSP V3863) (map #'(LAMBDA (Z) (shen.ue Z)) V3863))
   ((shen-cl.true? (variable? V3863)) (concat '&& V3863)) (T V3863)))

(DEFUN shen.ue-sig (V3865)
  (COND ((CONSP V3865) (map #'(LAMBDA (Z) (shen.ue-sig Z)) V3865))
        ((shen-cl.true? (variable? V3865)) (concat '&&& V3865)) (T V3865)))

(DEFUN shen.ues (V3871)
  (COND ((shen-cl.true? (shen.ue? V3871)) (CONS V3871 NIL))
        ((CONSP V3871) (union (shen.ues (CAR V3871)) (shen.ues (CDR V3871))))
        (T NIL)))

(DEFUN shen.ue? (V3873) (and (symbol? V3873) (shen.ue-h? (str V3873))))

(DEFUN shen.ue-h? (V3881)
  (COND
   ((AND (shen-cl.true? (shen.+string? V3881))
         (AND (EQUAL "&" (pos V3881 0))
              (AND (shen-cl.true? (shen.+string? (tlstr V3881)))
                   (EQUAL "&" (pos (tlstr V3881) 0)))))
    'true)
   (T 'false)))

(DEFUN shen.t*-rules (V3889 V3890 V3891 V3892 V3893 V3894 V3895)
  (LET ((Throwcontrol (shen.catchpoint)))
    (shen.cutpoint Throwcontrol
                   (LET ((Case
                          (LET ((V3445 (shen.lazyderef V3889 V3894)))
                            (IF (NULL V3445)
                                (do (shen.incinfs) (thaw V3895))
                                'false))))
                     (IF (EQ Case 'false)
                         (LET ((Case
                                (LET ((V3446 (shen.lazyderef V3889 V3894)))
                                  (IF (CONSP V3446)
                                      (LET ((Rule (CAR V3446)))
                                        (LET ((Rules (CDR V3446)))
                                          (do (shen.incinfs)
                                              (shen.t*-rule (shen.ue Rule)
                                               V3890 V3893 V3894
                                               (freeze
                                                (cut Throwcontrol V3894
                                                     (freeze
                                                      (shen.t*-rules Rules
                                                       V3890 (shen.add V3891 1)
                                                       V3892 V3893 V3894
                                                       V3895))))))))
                                      'false))))
                           (IF (EQ Case 'false)
                               (LET ((Err (shen.newpv V3894)))
                                 (do (shen.incinfs)
                                     (bind Err
                                           (simple-error
                                            (cn "type error in rule "
                                                (shen.app
                                                 (shen.lazyderef V3891 V3894)
                                                 (cn " of "
                                                     (shen.app
                                                      (shen.lazyderef V3892
                                                                      V3894)
                                                      "" 'shen.a))
                                                 'shen.a)))
                                           V3894 V3895)))
                               Case))
                         Case)))))

(DEFUN shen.t*-rule (V3901 V3902 V3903 V3904 V3905)
  (LET ((Throwcontrol (shen.catchpoint)))
    (shen.cutpoint Throwcontrol
                   (LET ((V3437 (shen.lazyderef V3901 V3904)))
                     (IF (CONSP V3437)
                         (LET ((Patterns (CAR V3437)))
                           (LET ((V3438 (shen.lazyderef (CDR V3437) V3904)))
                             (IF (CONSP V3438)
                                 (LET ((Action (CAR V3438)))
                                   (LET ((V3439
                                          (shen.lazyderef (CDR V3438) V3904)))
                                     (IF (NULL V3439)
                                         (LET ((NewHyps (shen.newpv V3904)))
                                           (do (shen.incinfs)
                                               (shen.newhyps
                                                (shen.placeholders Patterns)
                                                V3903 NewHyps V3904
                                                (freeze
                                                 (shen.t*-patterns Patterns
                                                  V3902 NewHyps V3904
                                                  (freeze
                                                   (cut Throwcontrol V3904
                                                        (freeze
                                                         (shen.t*-action
                                                          (shen.curry
                                                           (shen.ue Action))
                                                          (shen.result-type
                                                           Patterns V3902)
                                                          (shen.patthyps
                                                           Patterns V3902
                                                           V3903)
                                                          V3904 V3905)))))))))
                                         'false)))
                                 'false)))
                         'false)))))

(DEFUN shen.placeholders (V3911)
  (COND ((shen-cl.true? (shen.ue? V3911)) (CONS V3911 NIL))
        ((CONSP V3911)
         (union (shen.placeholders (CAR V3911))
                (shen.placeholders (CDR V3911))))
        (T NIL)))

(DEFUN shen.newhyps (V3917 V3918 V3919 V3920 V3921)
  (LET ((Case
         (LET ((V3424 (shen.lazyderef V3917 V3920)))
           (IF (NULL V3424)
               (do (shen.incinfs) (unify! V3919 V3918 V3920 V3921))
               'false))))
    (IF (EQ Case 'false)
        (LET ((V3425 (shen.lazyderef V3917 V3920)))
          (IF (CONSP V3425)
              (LET ((V3420 (CAR V3425)))
                (LET ((Vs (CDR V3425)))
                  (LET ((V3426 (shen.lazyderef V3919 V3920)))
                    (IF (CONSP V3426)
                        (LET ((V3427 (shen.lazyderef (CAR V3426) V3920)))
                          (IF (CONSP V3427)
                              (LET ((V (CAR V3427)))
                                (LET ((V3428
                                       (shen.lazyderef (CDR V3427) V3920)))
                                  (IF (CONSP V3428)
                                      (LET ((V3429
                                             (shen.lazyderef (CAR V3428)
                                                             V3920)))
                                        (IF (EQ '|:| V3429)
                                            (LET ((V3430
                                                   (shen.lazyderef (CDR V3428)
                                                                   V3920)))
                                              (IF (CONSP V3430)
                                                  (LET ((A (CAR V3430)))
                                                    (LET ((V3431
                                                           (shen.lazyderef
                                                            (CDR V3430) V3920)))
                                                      (IF (NULL V3431)
                                                          (LET ((NewHyp
                                                                 (CDR V3426)))
                                                            (do (shen.incinfs)
                                                                (unify! V V3420
                                                                        V3920
                                                                        (freeze
                                                                         (shen.newhyps
                                                                          Vs
                                                                          V3918
                                                                          NewHyp
                                                                          V3920
                                                                          V3921)))))
                                                          (IF (shen-cl.true?
                                                               (shen.pvar?
                                                                V3431))
                                                              (do
                                                               (shen.bindv
                                                                V3431 NIL
                                                                V3920)
                                                               (LET ((Result
                                                                      (LET ((NewHyp
                                                                             (CDR
                                                                              V3426)))
                                                                        (do
                                                                         (shen.incinfs)
                                                                         (unify!
                                                                          V
                                                                          V3420
                                                                          V3920
                                                                          (freeze
                                                                           (shen.newhyps
                                                                            Vs
                                                                            V3918
                                                                            NewHyp
                                                                            V3920
                                                                            V3921)))))))
                                                                 (do
                                                                  (shen.unbindv
                                                                   V3431 V3920)
                                                                  Result)))
                                                              'false))))
                                                  (IF (shen-cl.true?
                                                       (shen.pvar? V3430))
                                                      (LET ((A
                                                             (shen.newpv
                                                              V3920)))
                                                        (do
                                                         (shen.bindv V3430
                                                                     (CONS A
                                                                           NIL)
                                                                     V3920)
                                                         (LET ((Result
                                                                (LET ((NewHyp
                                                                       (CDR
                                                                        V3426)))
                                                                  (do
                                                                   (shen.incinfs)
                                                                   (unify! V
                                                                           V3420
                                                                           V3920
                                                                           (freeze
                                                                            (shen.newhyps
                                                                             Vs
                                                                             V3918
                                                                             NewHyp
                                                                             V3920
                                                                             V3921)))))))
                                                           (do
                                                            (shen.unbindv V3430
                                                                          V3920)
                                                            Result))))
                                                      'false)))
                                            (IF (shen-cl.true?
                                                 (shen.pvar? V3429))
                                                (do
                                                 (shen.bindv V3429 '|:| V3920)
                                                 (LET ((Result
                                                        (LET ((V3432
                                                               (shen.lazyderef
                                                                (CDR V3428)
                                                                V3920)))
                                                          (IF (CONSP V3432)
                                                              (LET ((A
                                                                     (CAR
                                                                      V3432)))
                                                                (LET ((V3433
                                                                       (shen.lazyderef
                                                                        (CDR
                                                                         V3432)
                                                                        V3920)))
                                                                  (IF (NULL
                                                                       V3433)
                                                                      (LET ((NewHyp
                                                                             (CDR
                                                                              V3426)))
                                                                        (do
                                                                         (shen.incinfs)
                                                                         (unify!
                                                                          V
                                                                          V3420
                                                                          V3920
                                                                          (freeze
                                                                           (shen.newhyps
                                                                            Vs
                                                                            V3918
                                                                            NewHyp
                                                                            V3920
                                                                            V3921)))))
                                                                      (IF (shen-cl.true?
                                                                           (shen.pvar?
                                                                            V3433))
                                                                          (do
                                                                           (shen.bindv
                                                                            V3433
                                                                            NIL
                                                                            V3920)
                                                                           (LET ((Result
                                                                                  (LET ((NewHyp
                                                                                         (CDR
                                                                                          V3426)))
                                                                                    (do
                                                                                     (shen.incinfs)
                                                                                     (unify!
                                                                                      V
                                                                                      V3420
                                                                                      V3920
                                                                                      (freeze
                                                                                       (shen.newhyps
                                                                                        Vs
                                                                                        V3918
                                                                                        NewHyp
                                                                                        V3920
                                                                                        V3921)))))))
                                                                             (do
                                                                              (shen.unbindv
                                                                               V3433
                                                                               V3920)
                                                                              Result)))
                                                                          'false))))
                                                              (IF (shen-cl.true?
                                                                   (shen.pvar?
                                                                    V3432))
                                                                  (LET ((A
                                                                         (shen.newpv
                                                                          V3920)))
                                                                    (do
                                                                     (shen.bindv
                                                                      V3432
                                                                      (CONS A
                                                                            NIL)
                                                                      V3920)
                                                                     (LET ((Result
                                                                            (LET ((NewHyp
                                                                                   (CDR
                                                                                    V3426)))
                                                                              (do
                                                                               (shen.incinfs)
                                                                               (unify!
                                                                                V
                                                                                V3420
                                                                                V3920
                                                                                (freeze
                                                                                 (shen.newhyps
                                                                                  Vs
                                                                                  V3918
                                                                                  NewHyp
                                                                                  V3920
                                                                                  V3921)))))))
                                                                       (do
                                                                        (shen.unbindv
                                                                         V3432
                                                                         V3920)
                                                                        Result))))
                                                                  'false)))))
                                                   (do
                                                    (shen.unbindv V3429 V3920)
                                                    Result)))
                                                'false)))
                                      (IF (shen-cl.true? (shen.pvar? V3428))
                                          (LET ((A (shen.newpv V3920)))
                                            (do
                                             (shen.bindv V3428
                                                         (CONS '|:|
                                                               (CONS A NIL))
                                                         V3920)
                                             (LET ((Result
                                                    (LET ((NewHyp (CDR V3426)))
                                                      (do (shen.incinfs)
                                                          (unify! V V3420 V3920
                                                                  (freeze
                                                                   (shen.newhyps
                                                                    Vs V3918
                                                                    NewHyp
                                                                    V3920
                                                                    V3921)))))))
                                               (do (shen.unbindv V3428 V3920)
                                                   Result))))
                                          'false))))
                              (IF (shen-cl.true? (shen.pvar? V3427))
                                  (LET ((V (shen.newpv V3920)))
                                    (LET ((A (shen.newpv V3920)))
                                      (do
                                       (shen.bindv V3427
                                                   (CONS V
                                                         (CONS '|:|
                                                               (CONS A NIL)))
                                                   V3920)
                                       (LET ((Result
                                              (LET ((NewHyp (CDR V3426)))
                                                (do (shen.incinfs)
                                                    (unify! V V3420 V3920
                                                            (freeze
                                                             (shen.newhyps Vs
                                                              V3918 NewHyp
                                                              V3920 V3921)))))))
                                         (do (shen.unbindv V3427 V3920)
                                             Result)))))
                                  'false)))
                        (IF (shen-cl.true? (shen.pvar? V3426))
                            (LET ((V (shen.newpv V3920)))
                              (LET ((A (shen.newpv V3920)))
                                (LET ((NewHyp (shen.newpv V3920)))
                                  (do
                                   (shen.bindv V3426
                                               (CONS
                                                (CONS V
                                                      (CONS '|:| (CONS A NIL)))
                                                NewHyp)
                                               V3920)
                                   (LET ((Result
                                          (do (shen.incinfs)
                                              (unify! V V3420 V3920
                                                      (freeze
                                                       (shen.newhyps Vs V3918
                                                        NewHyp V3920 V3921))))))
                                     (do (shen.unbindv V3426 V3920)
                                         Result))))))
                            'false)))))
              'false))
        Case)))

(DEFUN shen.patthyps (V3927 V3928 V3929)
  (COND ((NULL V3927) V3929)
        ((AND (CONSP V3927)
              (AND (CONSP V3928)
                   (AND (CONSP (CDR V3928))
                        (AND (EQ '--> (CAR (CDR V3928)))
                             (AND (CONSP (CDR (CDR V3928)))
                                  (NULL (CDR (CDR (CDR V3928)))))))))
         (adjoin (CONS (CAR V3927) (CONS '|:| (CONS (CAR V3928) NIL)))
                 (shen.patthyps (CDR V3927) (CAR (CDR (CDR V3928))) V3929)))
        (T (shen.f_error 'shen.patthyps))))

(DEFUN shen.result-type (V3936 V3937)
  (COND
   ((AND (NULL V3936)
         (AND (CONSP V3937)
              (AND (EQ '--> (CAR V3937))
                   (AND (CONSP (CDR V3937)) (NULL (CDR (CDR V3937)))))))
    (CAR (CDR V3937)))
   ((NULL V3936) V3937)
   ((AND (CONSP V3936)
         (AND (CONSP V3937)
              (AND (CONSP (CDR V3937))
                   (AND (EQ '--> (CAR (CDR V3937)))
                        (AND (CONSP (CDR (CDR V3937)))
                             (NULL (CDR (CDR (CDR V3937)))))))))
    (shen.result-type (CDR V3936) (CAR (CDR (CDR V3937)))))
   (T (shen.f_error 'shen.result-type))))

(DEFUN shen.t*-patterns (V3943 V3944 V3945 V3946 V3947)
  (LET ((Case
         (LET ((V3412 (shen.lazyderef V3943 V3946)))
           (IF (NULL V3412)
               (do (shen.incinfs) (thaw V3947))
               'false))))
    (IF (EQ Case 'false)
        (LET ((V3413 (shen.lazyderef V3943 V3946)))
          (IF (CONSP V3413)
              (LET ((Pattern (CAR V3413)))
                (LET ((Patterns (CDR V3413)))
                  (LET ((V3414 (shen.lazyderef V3944 V3946)))
                    (IF (CONSP V3414)
                        (LET ((A (CAR V3414)))
                          (LET ((V3415 (shen.lazyderef (CDR V3414) V3946)))
                            (IF (CONSP V3415)
                                (LET ((V3416
                                       (shen.lazyderef (CAR V3415) V3946)))
                                  (IF (EQ '--> V3416)
                                      (LET ((V3417
                                             (shen.lazyderef (CDR V3415)
                                                             V3946)))
                                        (IF (CONSP V3417)
                                            (LET ((B (CAR V3417)))
                                              (LET ((V3418
                                                     (shen.lazyderef
                                                      (CDR V3417) V3946)))
                                                (IF (NULL V3418)
                                                    (do (shen.incinfs)
                                                        (shen.t*
                                                         (CONS Pattern
                                                               (CONS '|:|
                                                                     (CONS A
                                                                           NIL)))
                                                         V3945 V3946
                                                         (freeze
                                                          (shen.t*-patterns
                                                           Patterns B V3945
                                                           V3946 V3947))))
                                                    'false)))
                                            'false))
                                      'false))
                                'false)))
                        'false))))
              'false))
        Case)))

(DEFUN shen.t*-action (V3953 V3954 V3955 V3956 V3957)
  (LET ((Throwcontrol (shen.catchpoint)))
    (shen.cutpoint Throwcontrol
                   (LET ((Case
                          (LET ((V3389 (shen.lazyderef V3953 V3956)))
                            (IF (CONSP V3389)
                                (LET ((V3390
                                       (shen.lazyderef (CAR V3389) V3956)))
                                  (IF (EQ 'where V3390)
                                      (LET ((V3391
                                             (shen.lazyderef (CDR V3389)
                                                             V3956)))
                                        (IF (CONSP V3391)
                                            (LET ((P (CAR V3391)))
                                              (LET ((V3392
                                                     (shen.lazyderef
                                                      (CDR V3391) V3956)))
                                                (IF (CONSP V3392)
                                                    (LET ((Action (CAR V3392)))
                                                      (LET ((V3393
                                                             (shen.lazyderef
                                                              (CDR V3392)
                                                              V3956)))
                                                        (IF (NULL V3393)
                                                            (do (shen.incinfs)
                                                                (cut
                                                                 Throwcontrol
                                                                 V3956
                                                                 (freeze
                                                                  (shen.t*
                                                                   (CONS P
                                                                         (CONS
                                                                          '|:|
                                                                          (CONS
                                                                           'boolean
                                                                           NIL)))
                                                                   V3955 V3956
                                                                   (freeze
                                                                    (cut
                                                                     Throwcontrol
                                                                     V3956
                                                                     (freeze
                                                                      (shen.t*-action
                                                                       Action
                                                                       V3954
                                                                       (CONS
                                                                        (CONS P
                                                                              (CONS
                                                                               '|:|
                                                                               (CONS
                                                                                'verified
                                                                                NIL)))
                                                                        V3955)
                                                                       V3956
                                                                       V3957))))))))
                                                            'false)))
                                                    'false)))
                                            'false))
                                      'false))
                                'false))))
                     (IF (EQ Case 'false)
                         (LET ((Case
                                (LET ((V3394 (shen.lazyderef V3953 V3956)))
                                  (IF (CONSP V3394)
                                      (LET ((V3395
                                             (shen.lazyderef (CAR V3394)
                                                             V3956)))
                                        (IF (EQ 'shen.choicepoint! V3395)
                                            (LET ((V3396
                                                   (shen.lazyderef (CDR V3394)
                                                                   V3956)))
                                              (IF (CONSP V3396)
                                                  (LET ((V3397
                                                         (shen.lazyderef
                                                          (CAR V3396) V3956)))
                                                    (IF (CONSP V3397)
                                                        (LET ((V3398
                                                               (shen.lazyderef
                                                                (CAR V3397)
                                                                V3956)))
                                                          (IF (CONSP V3398)
                                                              (LET ((V3399
                                                                     (shen.lazyderef
                                                                      (CAR
                                                                       V3398)
                                                                      V3956)))
                                                                (IF (EQ
                                                                     'fail-if
                                                                     V3399)
                                                                    (LET ((V3400
                                                                           (shen.lazyderef
                                                                            (CDR
                                                                             V3398)
                                                                            V3956)))
                                                                      (IF (CONSP
                                                                           V3400)
                                                                          (LET ((F
                                                                                 (CAR
                                                                                  V3400)))
                                                                            (LET ((V3401
                                                                                   (shen.lazyderef
                                                                                    (CDR
                                                                                     V3400)
                                                                                    V3956)))
                                                                              (IF (NULL
                                                                                   V3401)
                                                                                  (LET ((V3402
                                                                                         (shen.lazyderef
                                                                                          (CDR
                                                                                           V3397)
                                                                                          V3956)))
                                                                                    (IF (CONSP
                                                                                         V3402)
                                                                                        (LET ((Action
                                                                                               (CAR
                                                                                                V3402)))
                                                                                          (LET ((V3403
                                                                                                 (shen.lazyderef
                                                                                                  (CDR
                                                                                                   V3402)
                                                                                                  V3956)))
                                                                                            (IF (NULL
                                                                                                 V3403)
                                                                                                (LET ((V3404
                                                                                                       (shen.lazyderef
                                                                                                        (CDR
                                                                                                         V3396)
                                                                                                        V3956)))
                                                                                                  (IF (NULL
                                                                                                       V3404)
                                                                                                      (do
                                                                                                       (shen.incinfs)
                                                                                                       (cut
                                                                                                        Throwcontrol
                                                                                                        V3956
                                                                                                        (freeze
                                                                                                         (shen.t*-action
                                                                                                          (CONS
                                                                                                           'where
                                                                                                           (CONS
                                                                                                            (CONS
                                                                                                             'not
                                                                                                             (CONS
                                                                                                              (CONS
                                                                                                               F
                                                                                                               (CONS
                                                                                                                Action
                                                                                                                NIL))
                                                                                                              NIL))
                                                                                                            (CONS
                                                                                                             Action
                                                                                                             NIL)))
                                                                                                          V3954
                                                                                                          V3955
                                                                                                          V3956
                                                                                                          V3957))))
                                                                                                      'false))
                                                                                                'false)))
                                                                                        'false))
                                                                                  'false)))
                                                                          'false))
                                                                    'false))
                                                              'false))
                                                        'false))
                                                  'false))
                                            'false))
                                      'false))))
                           (IF (EQ Case 'false)
                               (LET ((Case
                                      (LET ((V3405
                                             (shen.lazyderef V3953 V3956)))
                                        (IF (CONSP V3405)
                                            (LET ((V3406
                                                   (shen.lazyderef (CAR V3405)
                                                                   V3956)))
                                              (IF (EQ 'shen.choicepoint! V3406)
                                                  (LET ((V3407
                                                         (shen.lazyderef
                                                          (CDR V3405) V3956)))
                                                    (IF (CONSP V3407)
                                                        (LET ((Action
                                                               (CAR V3407)))
                                                          (LET ((V3408
                                                                 (shen.lazyderef
                                                                  (CDR V3407)
                                                                  V3956)))
                                                            (IF (NULL V3408)
                                                                (do
                                                                 (shen.incinfs)
                                                                 (cut
                                                                  Throwcontrol
                                                                  V3956
                                                                  (freeze
                                                                   (shen.t*-action
                                                                    (CONS
                                                                     'where
                                                                     (CONS
                                                                      (CONS
                                                                       'not
                                                                       (CONS
                                                                        (CONS
                                                                         (CONS
                                                                          '=
                                                                          (CONS
                                                                           Action
                                                                           NIL))
                                                                         (CONS
                                                                          (CONS
                                                                           'fail
                                                                           NIL)
                                                                          NIL))
                                                                        NIL))
                                                                      (CONS
                                                                       Action
                                                                       NIL)))
                                                                    V3954 V3955
                                                                    V3956
                                                                    V3957))))
                                                                'false)))
                                                        'false))
                                                  'false))
                                            'false))))
                                 (IF (EQ Case 'false)
                                     (do (shen.incinfs)
                                         (shen.t*
                                          (CONS V3953
                                                (CONS '|:| (CONS V3954 NIL)))
                                          V3955 V3956 V3957))
                                     Case))
                               Case))
                         Case)))))

(DEFUN findall (V3963 V3964 V3965 V3966 V3967)
  (LET ((B (shen.newpv V3966)))
    (LET ((A (shen.newpv V3966)))
      (do (shen.incinfs)
          (bind A (gensym 'shen.a) V3966
                (freeze
                 (bind B (set (shen.lazyderef A V3966) NIL) V3966
                       (freeze
                        (shen.findallhelp V3963 V3964 V3965 A V3966
                         V3967)))))))))

(DEFUN shen.findallhelp (V3974 V3975 V3976 V3977 V3978 V3979)
  (LET ((Case
         (do (shen.incinfs)
             (call V3975 V3978
                   (freeze
                    (shen.remember V3977 V3974 V3978
                     (freeze (fwhen 'false V3978 V3979))))))))
    (IF (EQ Case 'false)
        (do (shen.incinfs)
            (bind V3976 (value (shen.lazyderef V3977 V3978)) V3978 V3979))
        Case)))

(DEFUN shen.remember (V3984 V3985 V3986 V3987)
  (LET ((B (shen.newpv V3986)))
    (do (shen.incinfs)
        (bind B
              (set (shen.deref V3984 V3986)
                   (CONS (shen.deref V3985 V3986)
                         (value (shen.deref V3984 V3986))))
              V3986 V3987))))

