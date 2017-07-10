
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

(DEFUN shen.shen->kl (V1378 V1379)
  (compile #'(LAMBDA (X) (shen.<define> X)) (CONS V1378 V1379)
   #'(LAMBDA (X) (shen.shen-syntax-error V1378 X))))

(DEFUN shen.shen-syntax-error (V1386 V1387)
  (COND
   ((CONSP V1387)
    (simple-error
     (cn "syntax error in "
         (shen.app V1386
          (cn " here:

 "
              (shen.app (shen.next-50 50 (CAR V1387)) "
"
               'shen.a))
          'shen.a))))
   (T
    (simple-error
     (cn "syntax error in "
         (shen.app V1386 "
"
          'shen.a))))))

(DEFUN shen.<define> (V1389)
  (LET ((YaccParse
         (LET ((Parse_shen.<name> (shen.<name> V1389)))
           (IF (NOT (EQ (fail) Parse_shen.<name>))
               (LET ((Parse_shen.<signature>
                      (shen.<signature> Parse_shen.<name>)))
                 (IF (NOT (EQ (fail) Parse_shen.<signature>))
                     (LET ((Parse_shen.<rules>
                            (shen.<rules> Parse_shen.<signature>)))
                       (IF (NOT (EQ (fail) Parse_shen.<rules>))
                           (shen.pair (CAR Parse_shen.<rules>)
                            (shen.compile_to_machine_code
                             (shen.hdtl Parse_shen.<name>)
                             (shen.hdtl Parse_shen.<rules>)))
                           (fail)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_shen.<name> (shen.<name> V1389)))
          (IF (NOT (EQ (fail) Parse_shen.<name>))
              (LET ((Parse_shen.<rules> (shen.<rules> Parse_shen.<name>)))
                (IF (NOT (EQ (fail) Parse_shen.<rules>))
                    (shen.pair (CAR Parse_shen.<rules>)
                     (shen.compile_to_machine_code
                      (shen.hdtl Parse_shen.<name>)
                      (shen.hdtl Parse_shen.<rules>)))
                    (fail)))
              (fail)))
        YaccParse)))

(DEFUN shen.<name> (V1391)
  (IF (CONSP (CAR V1391))
      (LET ((Parse_X (CAR (CAR V1391))))
        (shen.pair (CAR (shen.pair (CDR (CAR V1391)) (shen.hdtl V1391)))
         (IF (AND (shen-cl.true? (symbol? Parse_X))
                  (NOT (shen-cl.true? (shen.sysfunc? Parse_X))))
             Parse_X
             (simple-error
              (shen.app Parse_X " is not a legitimate function name.
"
               'shen.a)))))
      (fail)))

(DEFUN shen.sysfunc? (V1393)
  (element? V1393
   (get (intern "shen") 'shen.external-symbols *property-vector*)))

(DEFUN shen.<signature> (V1395)
  (IF (AND (CONSP (CAR V1395)) (EQ '{ (CAR (CAR V1395))))
      (LET ((Parse_shen.<signature-help>
             (shen.<signature-help>
              (shen.pair (CDR (CAR V1395)) (shen.hdtl V1395)))))
        (IF (NOT (EQ (fail) Parse_shen.<signature-help>))
            (IF (AND (CONSP (CAR Parse_shen.<signature-help>))
                     (EQ '} (CAR (CAR Parse_shen.<signature-help>))))
                (shen.pair
                 (CAR
                  (shen.pair (CDR (CAR Parse_shen.<signature-help>))
                   (shen.hdtl Parse_shen.<signature-help>)))
                 (shen.demodulate
                  (shen.curry-type (shen.hdtl Parse_shen.<signature-help>))))
                (fail))
            (fail)))
      (fail)))

(DEFUN shen.curry-type (V1397)
  (COND
   ((AND (CONSP V1397)
         (AND (CONSP (CDR V1397))
              (AND (EQ '--> (CAR (CDR V1397)))
                   (AND (CONSP (CDR (CDR V1397)))
                        (AND (CONSP (CDR (CDR (CDR V1397))))
                             (EQ '--> (CAR (CDR (CDR (CDR V1397))))))))))
    (shen.curry-type
     (CONS (CAR V1397) (CONS '--> (CONS (CDR (CDR V1397)) NIL)))))
   ((AND (CONSP V1397)
         (AND (CONSP (CDR V1397))
              (AND (EQ '* (CAR (CDR V1397)))
                   (AND (CONSP (CDR (CDR V1397)))
                        (AND (CONSP (CDR (CDR (CDR V1397))))
                             (EQ '* (CAR (CDR (CDR (CDR V1397))))))))))
    (shen.curry-type
     (CONS (CAR V1397) (CONS '* (CONS (CDR (CDR V1397)) NIL)))))
   ((CONSP V1397) (map #'(LAMBDA (Z) (shen.curry-type Z)) V1397)) (T V1397)))

(DEFUN shen.<signature-help> (V1399)
  (LET ((YaccParse
         (IF (CONSP (CAR V1399))
             (LET ((Parse_X (CAR (CAR V1399))))
               (LET ((Parse_shen.<signature-help>
                      (shen.<signature-help>
                       (shen.pair (CDR (CAR V1399)) (shen.hdtl V1399)))))
                 (IF (NOT (EQ (fail) Parse_shen.<signature-help>))
                     (IF (NOT
                          (shen-cl.true?
                           (element? Parse_X (CONS '{ (CONS '} NIL)))))
                         (shen.pair (CAR Parse_shen.<signature-help>)
                          (CONS Parse_X
                                (shen.hdtl Parse_shen.<signature-help>)))
                         (fail))
                     (fail))))
             (fail))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_<e> (<e> V1399)))
          (IF (NOT (EQ (fail) Parse_<e>))
              (shen.pair (CAR Parse_<e>) NIL)
              (fail)))
        YaccParse)))

(DEFUN shen.<rules> (V1401)
  (LET ((YaccParse
         (LET ((Parse_shen.<rule> (shen.<rule> V1401)))
           (IF (NOT (EQ (fail) Parse_shen.<rule>))
               (LET ((Parse_shen.<rules> (shen.<rules> Parse_shen.<rule>)))
                 (IF (NOT (EQ (fail) Parse_shen.<rules>))
                     (shen.pair (CAR Parse_shen.<rules>)
                      (CONS (shen.linearise (shen.hdtl Parse_shen.<rule>))
                            (shen.hdtl Parse_shen.<rules>)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_shen.<rule> (shen.<rule> V1401)))
          (IF (NOT (EQ (fail) Parse_shen.<rule>))
              (shen.pair (CAR Parse_shen.<rule>)
               (CONS (shen.linearise (shen.hdtl Parse_shen.<rule>)) NIL))
              (fail)))
        YaccParse)))

(DEFUN shen.<rule> (V1403)
  (LET ((YaccParse
         (LET ((Parse_shen.<patterns> (shen.<patterns> V1403)))
           (IF (NOT (EQ (fail) Parse_shen.<patterns>))
               (IF (AND (CONSP (CAR Parse_shen.<patterns>))
                        (EQ '-> (CAR (CAR Parse_shen.<patterns>))))
                   (LET ((Parse_shen.<action>
                          (shen.<action>
                           (shen.pair (CDR (CAR Parse_shen.<patterns>))
                            (shen.hdtl Parse_shen.<patterns>)))))
                     (IF (NOT (EQ (fail) Parse_shen.<action>))
                         (IF (AND (CONSP (CAR Parse_shen.<action>))
                                  (EQ 'where (CAR (CAR Parse_shen.<action>))))
                             (LET ((Parse_shen.<guard>
                                    (shen.<guard>
                                     (shen.pair (CDR (CAR Parse_shen.<action>))
                                      (shen.hdtl Parse_shen.<action>)))))
                               (IF (NOT (EQ (fail) Parse_shen.<guard>))
                                   (shen.pair (CAR Parse_shen.<guard>)
                                    (CONS (shen.hdtl Parse_shen.<patterns>)
                                          (CONS
                                           (CONS 'where
                                                 (CONS
                                                  (shen.hdtl
                                                   Parse_shen.<guard>)
                                                  (CONS
                                                   (shen.hdtl
                                                    Parse_shen.<action>)
                                                   NIL)))
                                           NIL)))
                                   (fail)))
                             (fail))
                         (fail)))
                   (fail))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((YaccParse
               (LET ((Parse_shen.<patterns> (shen.<patterns> V1403)))
                 (IF (NOT (EQ (fail) Parse_shen.<patterns>))
                     (IF (AND (CONSP (CAR Parse_shen.<patterns>))
                              (EQ '-> (CAR (CAR Parse_shen.<patterns>))))
                         (LET ((Parse_shen.<action>
                                (shen.<action>
                                 (shen.pair (CDR (CAR Parse_shen.<patterns>))
                                  (shen.hdtl Parse_shen.<patterns>)))))
                           (IF (NOT (EQ (fail) Parse_shen.<action>))
                               (shen.pair (CAR Parse_shen.<action>)
                                (CONS (shen.hdtl Parse_shen.<patterns>)
                                      (CONS (shen.hdtl Parse_shen.<action>)
                                            NIL)))
                               (fail)))
                         (fail))
                     (fail)))))
          (IF (EQ YaccParse (fail))
              (LET ((YaccParse
                     (LET ((Parse_shen.<patterns> (shen.<patterns> V1403)))
                       (IF (NOT (EQ (fail) Parse_shen.<patterns>))
                           (IF (AND (CONSP (CAR Parse_shen.<patterns>))
                                    (EQ '<- (CAR (CAR Parse_shen.<patterns>))))
                               (LET ((Parse_shen.<action>
                                      (shen.<action>
                                       (shen.pair
                                        (CDR (CAR Parse_shen.<patterns>))
                                        (shen.hdtl Parse_shen.<patterns>)))))
                                 (IF (NOT (EQ (fail) Parse_shen.<action>))
                                     (IF (AND (CONSP (CAR Parse_shen.<action>))
                                              (EQ 'where
                                                  (CAR
                                                   (CAR Parse_shen.<action>))))
                                         (LET ((Parse_shen.<guard>
                                                (shen.<guard>
                                                 (shen.pair
                                                  (CDR
                                                   (CAR Parse_shen.<action>))
                                                  (shen.hdtl
                                                   Parse_shen.<action>)))))
                                           (IF (NOT
                                                (EQ (fail) Parse_shen.<guard>))
                                               (shen.pair
                                                (CAR Parse_shen.<guard>)
                                                (CONS
                                                 (shen.hdtl
                                                  Parse_shen.<patterns>)
                                                 (CONS
                                                  (CONS 'where
                                                        (CONS
                                                         (shen.hdtl
                                                          Parse_shen.<guard>)
                                                         (CONS
                                                          (CONS
                                                           'shen.choicepoint!
                                                           (CONS
                                                            (shen.hdtl
                                                             Parse_shen.<action>)
                                                            NIL))
                                                          NIL)))
                                                  NIL)))
                                               (fail)))
                                         (fail))
                                     (fail)))
                               (fail))
                           (fail)))))
                (IF (EQ YaccParse (fail))
                    (LET ((Parse_shen.<patterns> (shen.<patterns> V1403)))
                      (IF (NOT (EQ (fail) Parse_shen.<patterns>))
                          (IF (AND (CONSP (CAR Parse_shen.<patterns>))
                                   (EQ '<- (CAR (CAR Parse_shen.<patterns>))))
                              (LET ((Parse_shen.<action>
                                     (shen.<action>
                                      (shen.pair
                                       (CDR (CAR Parse_shen.<patterns>))
                                       (shen.hdtl Parse_shen.<patterns>)))))
                                (IF (NOT (EQ (fail) Parse_shen.<action>))
                                    (shen.pair (CAR Parse_shen.<action>)
                                     (CONS (shen.hdtl Parse_shen.<patterns>)
                                           (CONS
                                            (CONS 'shen.choicepoint!
                                                  (CONS
                                                   (shen.hdtl
                                                    Parse_shen.<action>)
                                                   NIL))
                                            NIL)))
                                    (fail)))
                              (fail))
                          (fail)))
                    YaccParse))
              YaccParse))
        YaccParse)))

(DEFUN shen.fail_if (V1406 V1407)
  (IF (shen-cl.true? (shen.apply V1406 (LIST V1407)))
      (fail)
      V1407))

(DEFUN shen.succeeds? (V1413) (COND ((EQ V1413 (fail)) 'false) (T 'true)))

(DEFUN shen.<patterns> (V1415)
  (LET ((YaccParse
         (LET ((Parse_shen.<pattern> (shen.<pattern> V1415)))
           (IF (NOT (EQ (fail) Parse_shen.<pattern>))
               (LET ((Parse_shen.<patterns>
                      (shen.<patterns> Parse_shen.<pattern>)))
                 (IF (NOT (EQ (fail) Parse_shen.<patterns>))
                     (shen.pair (CAR Parse_shen.<patterns>)
                      (CONS (shen.hdtl Parse_shen.<pattern>)
                            (shen.hdtl Parse_shen.<patterns>)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_<e> (<e> V1415)))
          (IF (NOT (EQ (fail) Parse_<e>))
              (shen.pair (CAR Parse_<e>) NIL)
              (fail)))
        YaccParse)))

(DEFUN shen.<pattern> (V1422)
  (LET ((YaccParse
         (IF (AND (CONSP (CAR V1422)) (CONSP (CAR (CAR V1422))))
             (IF (AND
                  (CONSP (CAR (shen.pair (CAR (CAR V1422)) (CAR (CDR V1422)))))
                  (EQ '@p
                      (CAR
                       (CAR (shen.pair (CAR (CAR V1422)) (CAR (CDR V1422)))))))
                 (LET ((Parse_shen.<pattern1>
                        (shen.<pattern1>
                         (shen.pair
                          (CDR
                           (CAR
                            (shen.pair (CAR (CAR V1422)) (CAR (CDR V1422)))))
                          (shen.hdtl
                           (shen.pair (CAR (CAR V1422)) (CAR (CDR V1422))))))))
                   (IF (NOT (EQ (fail) Parse_shen.<pattern1>))
                       (LET ((Parse_shen.<pattern2>
                              (shen.<pattern2> Parse_shen.<pattern1>)))
                         (IF (NOT (EQ (fail) Parse_shen.<pattern2>))
                             (shen.pair
                              (CAR
                               (shen.pair (CDR (CAR V1422)) (CAR (CDR V1422))))
                              (CONS '@p
                                    (CONS (shen.hdtl Parse_shen.<pattern1>)
                                          (CONS
                                           (shen.hdtl Parse_shen.<pattern2>)
                                           NIL))))
                             (fail)))
                       (fail)))
                 (fail))
             (fail))))
    (IF (EQ YaccParse (fail))
        (LET ((YaccParse
               (IF (AND (CONSP (CAR V1422)) (CONSP (CAR (CAR V1422))))
                   (IF (AND
                        (CONSP
                         (CAR (shen.pair (CAR (CAR V1422)) (CAR (CDR V1422)))))
                        (EQ 'cons
                            (CAR
                             (CAR
                              (shen.pair (CAR (CAR V1422))
                               (CAR (CDR V1422)))))))
                       (LET ((Parse_shen.<pattern1>
                              (shen.<pattern1>
                               (shen.pair
                                (CDR
                                 (CAR
                                  (shen.pair (CAR (CAR V1422))
                                   (CAR (CDR V1422)))))
                                (shen.hdtl
                                 (shen.pair (CAR (CAR V1422))
                                  (CAR (CDR V1422))))))))
                         (IF (NOT (EQ (fail) Parse_shen.<pattern1>))
                             (LET ((Parse_shen.<pattern2>
                                    (shen.<pattern2> Parse_shen.<pattern1>)))
                               (IF (NOT (EQ (fail) Parse_shen.<pattern2>))
                                   (shen.pair
                                    (CAR
                                     (shen.pair (CDR (CAR V1422))
                                      (CAR (CDR V1422))))
                                    (CONS 'cons
                                          (CONS
                                           (shen.hdtl Parse_shen.<pattern1>)
                                           (CONS
                                            (shen.hdtl Parse_shen.<pattern2>)
                                            NIL))))
                                   (fail)))
                             (fail)))
                       (fail))
                   (fail))))
          (IF (EQ YaccParse (fail))
              (LET ((YaccParse
                     (IF (AND (CONSP (CAR V1422)) (CONSP (CAR (CAR V1422))))
                         (IF (AND
                              (CONSP
                               (CAR
                                (shen.pair (CAR (CAR V1422))
                                 (CAR (CDR V1422)))))
                              (EQ '@v
                                  (CAR
                                   (CAR
                                    (shen.pair (CAR (CAR V1422))
                                     (CAR (CDR V1422)))))))
                             (LET ((Parse_shen.<pattern1>
                                    (shen.<pattern1>
                                     (shen.pair
                                      (CDR
                                       (CAR
                                        (shen.pair (CAR (CAR V1422))
                                         (CAR (CDR V1422)))))
                                      (shen.hdtl
                                       (shen.pair (CAR (CAR V1422))
                                        (CAR (CDR V1422))))))))
                               (IF (NOT (EQ (fail) Parse_shen.<pattern1>))
                                   (LET ((Parse_shen.<pattern2>
                                          (shen.<pattern2>
                                           Parse_shen.<pattern1>)))
                                     (IF (NOT
                                          (EQ (fail) Parse_shen.<pattern2>))
                                         (shen.pair
                                          (CAR
                                           (shen.pair (CDR (CAR V1422))
                                            (CAR (CDR V1422))))
                                          (CONS '@v
                                                (CONS
                                                 (shen.hdtl
                                                  Parse_shen.<pattern1>)
                                                 (CONS
                                                  (shen.hdtl
                                                   Parse_shen.<pattern2>)
                                                  NIL))))
                                         (fail)))
                                   (fail)))
                             (fail))
                         (fail))))
                (IF (EQ YaccParse (fail))
                    (LET ((YaccParse
                           (IF (AND (CONSP (CAR V1422))
                                    (CONSP (CAR (CAR V1422))))
                               (IF (AND
                                    (CONSP
                                     (CAR
                                      (shen.pair (CAR (CAR V1422))
                                       (CAR (CDR V1422)))))
                                    (EQ '@s
                                        (CAR
                                         (CAR
                                          (shen.pair (CAR (CAR V1422))
                                           (CAR (CDR V1422)))))))
                                   (LET ((Parse_shen.<pattern1>
                                          (shen.<pattern1>
                                           (shen.pair
                                            (CDR
                                             (CAR
                                              (shen.pair (CAR (CAR V1422))
                                               (CAR (CDR V1422)))))
                                            (shen.hdtl
                                             (shen.pair (CAR (CAR V1422))
                                              (CAR (CDR V1422))))))))
                                     (IF (NOT
                                          (EQ (fail) Parse_shen.<pattern1>))
                                         (LET ((Parse_shen.<pattern2>
                                                (shen.<pattern2>
                                                 Parse_shen.<pattern1>)))
                                           (IF (NOT
                                                (EQ (fail)
                                                    Parse_shen.<pattern2>))
                                               (shen.pair
                                                (CAR
                                                 (shen.pair (CDR (CAR V1422))
                                                  (CAR (CDR V1422))))
                                                (CONS '@s
                                                      (CONS
                                                       (shen.hdtl
                                                        Parse_shen.<pattern1>)
                                                       (CONS
                                                        (shen.hdtl
                                                         Parse_shen.<pattern2>)
                                                        NIL))))
                                               (fail)))
                                         (fail)))
                                   (fail))
                               (fail))))
                      (IF (EQ YaccParse (fail))
                          (LET ((YaccParse
                                 (IF (AND (CONSP (CAR V1422))
                                          (CONSP (CAR (CAR V1422))))
                                     (IF (AND
                                          (CONSP
                                           (CAR
                                            (shen.pair (CAR (CAR V1422))
                                             (CAR (CDR V1422)))))
                                          (EQ 'vector
                                              (CAR
                                               (CAR
                                                (shen.pair (CAR (CAR V1422))
                                                 (CAR (CDR V1422)))))))
                                         (IF (AND
                                              (CONSP
                                               (CAR
                                                (shen.pair
                                                 (CDR
                                                  (CAR
                                                   (shen.pair (CAR (CAR V1422))
                                                    (CAR (CDR V1422)))))
                                                 (shen.hdtl
                                                  (shen.pair (CAR (CAR V1422))
                                                   (CAR (CDR V1422)))))))
                                              (IF (NUMBERP
                                                   (CAR
                                                    (CAR
                                                     (shen.pair
                                                      (CDR
                                                       (CAR
                                                        (shen.pair
                                                         (CAR (CAR V1422))
                                                         (CAR (CDR V1422)))))
                                                      (shen.hdtl
                                                       (shen.pair
                                                        (CAR (CAR V1422))
                                                        (CAR (CDR V1422))))))))
                                                  (=
                                                   (CAR
                                                    (CAR
                                                     (shen.pair
                                                      (CDR
                                                       (CAR
                                                        (shen.pair
                                                         (CAR (CAR V1422))
                                                         (CAR (CDR V1422)))))
                                                      (shen.hdtl
                                                       (shen.pair
                                                        (CAR (CAR V1422))
                                                        (CAR (CDR V1422)))))))
                                                   0)))
                                             (shen.pair
                                              (CAR
                                               (shen.pair (CDR (CAR V1422))
                                                (CAR (CDR V1422))))
                                              (CONS 'vector (CONS 0 NIL)))
                                             (fail))
                                         (fail))
                                     (fail))))
                            (IF (EQ YaccParse (fail))
                                (LET ((YaccParse
                                       (IF (CONSP (CAR V1422))
                                           (LET ((Parse_X (CAR (CAR V1422))))
                                             (IF (CONSP Parse_X)
                                                 (shen.pair
                                                  (CAR
                                                   (shen.pair (CDR (CAR V1422))
                                                    (shen.hdtl V1422)))
                                                  (shen.constructor-error
                                                   Parse_X))
                                                 (fail)))
                                           (fail))))
                                  (IF (EQ YaccParse (fail))
                                      (LET ((Parse_shen.<simple_pattern>
                                             (shen.<simple_pattern> V1422)))
                                        (IF (NOT
                                             (EQ (fail)
                                                 Parse_shen.<simple_pattern>))
                                            (shen.pair
                                             (CAR Parse_shen.<simple_pattern>)
                                             (shen.hdtl
                                              Parse_shen.<simple_pattern>))
                                            (fail)))
                                      YaccParse))
                                YaccParse))
                          YaccParse))
                    YaccParse))
              YaccParse))
        YaccParse)))

(DEFUN shen.constructor-error (V1424)
  (simple-error
   (shen.app V1424 " is not a legitimate constructor
"
    'shen.a)))

(DEFUN shen.<simple_pattern> (V1426)
  (LET ((YaccParse
         (IF (CONSP (CAR V1426))
             (LET ((Parse_X (CAR (CAR V1426))))
               (IF (EQ Parse_X '_)
                   (shen.pair
                    (CAR (shen.pair (CDR (CAR V1426)) (shen.hdtl V1426)))
                    (gensym 'Parse_Y))
                   (fail)))
             (fail))))
    (IF (EQ YaccParse (fail))
        (IF (CONSP (CAR V1426))
            (LET ((Parse_X (CAR (CAR V1426))))
              (IF (NOT
                   (shen-cl.true?
                    (element? Parse_X (CONS '-> (CONS '<- NIL)))))
                  (shen.pair
                   (CAR (shen.pair (CDR (CAR V1426)) (shen.hdtl V1426)))
                   Parse_X)
                  (fail)))
            (fail))
        YaccParse)))

(DEFUN shen.<pattern1> (V1428)
  (LET ((Parse_shen.<pattern> (shen.<pattern> V1428)))
    (IF (NOT (EQ (fail) Parse_shen.<pattern>))
        (shen.pair (CAR Parse_shen.<pattern>) (shen.hdtl Parse_shen.<pattern>))
        (fail))))

(DEFUN shen.<pattern2> (V1430)
  (LET ((Parse_shen.<pattern> (shen.<pattern> V1430)))
    (IF (NOT (EQ (fail) Parse_shen.<pattern>))
        (shen.pair (CAR Parse_shen.<pattern>) (shen.hdtl Parse_shen.<pattern>))
        (fail))))

(DEFUN shen.<action> (V1432)
  (IF (CONSP (CAR V1432))
      (LET ((Parse_X (CAR (CAR V1432))))
        (shen.pair (CAR (shen.pair (CDR (CAR V1432)) (shen.hdtl V1432)))
         Parse_X))
      (fail)))

(DEFUN shen.<guard> (V1434)
  (IF (CONSP (CAR V1434))
      (LET ((Parse_X (CAR (CAR V1434))))
        (shen.pair (CAR (shen.pair (CDR (CAR V1434)) (shen.hdtl V1434)))
         Parse_X))
      (fail)))

(DEFUN shen.compile_to_machine_code (V1437 V1438)
  (LET ((Lambda+ (shen.compile_to_lambda+ V1437 V1438)))
    (LET ((KL (shen.compile_to_kl V1437 Lambda+)))
      (LET ((Record (shen.record-source V1437 KL)))
        KL))))

(DEFUN shen.record-source (V1443 V1444)
  (COND ((shen-cl.true? shen.*installing-kl*) 'shen.skip)
        (T (put V1443 'shen.source V1444 *property-vector*))))

(DEFUN shen.compile_to_lambda+ (V1447 V1448)
  (LET ((Arity (shen.aritycheck V1447 V1448)))
    (LET ((UpDateSymbolTable (shen.update-symbol-table V1447 Arity)))
      (LET ((Free
             (for-each #'(LAMBDA (Rule) (shen.free_variable_check V1447 Rule))
              V1448)))
        (LET ((Variables (shen.parameters Arity)))
          (LET ((Strip (map #'(LAMBDA (X) (shen.strip-protect X)) V1448)))
            (LET ((Abstractions
                   (map #'(LAMBDA (X) (shen.abstract_rule X)) Strip)))
              (LET ((Applications
                     (map #'(LAMBDA (X) (shen.application_build Variables X))
                      Abstractions)))
                (CONS Variables (CONS Applications NIL))))))))))

(DEFUN shen.update-symbol-table (V1451 V1452)
  (COND
   ((IF (NUMBERP V1452)
        (= V1452 0))
    'shen.skip)
   (T
    (put V1451 'shen.lambda-form (eval-kl (shen.lambda-form V1451 V1452))
     *property-vector*))))

(DEFUN shen.free_variable_check (V1455 V1456)
  (COND
   ((AND (CONSP V1456) (AND (CONSP (CDR V1456)) (NULL (CDR (CDR V1456)))))
    (LET ((Bound (shen.extract_vars (CAR V1456))))
      (LET ((Free (shen.extract_free_vars Bound (CAR (CDR V1456)))))
        (shen.free_variable_warnings V1455 Free))))
   (T (shen.f_error 'shen.free_variable_check))))

(DEFUN shen.extract_vars (V1458)
  (COND ((shen-cl.true? (variable? V1458)) (CONS V1458 NIL))
        ((CONSP V1458)
         (union (shen.extract_vars (CAR V1458))
          (shen.extract_vars (CDR V1458))))
        (T NIL)))

(DEFUN shen.extract_free_vars (V1470 V1471)
  (COND
   ((AND (CONSP V1471)
         (AND (CONSP (CDR V1471))
              (AND (NULL (CDR (CDR V1471))) (EQ (CAR V1471) 'protect))))
    NIL)
   ((AND (shen-cl.true? (variable? V1471))
         (NOT (shen-cl.true? (element? V1471 V1470))))
    (CONS V1471 NIL))
   ((AND (CONSP V1471)
         (AND (EQ 'lambda (CAR V1471))
              (AND (CONSP (CDR V1471))
                   (AND (CONSP (CDR (CDR V1471)))
                        (NULL (CDR (CDR (CDR V1471))))))))
    (shen.extract_free_vars (CONS (CAR (CDR V1471)) V1470)
     (CAR (CDR (CDR V1471)))))
   ((AND (CONSP V1471)
         (AND (EQ 'let (CAR V1471))
              (AND (CONSP (CDR V1471))
                   (AND (CONSP (CDR (CDR V1471)))
                        (AND (CONSP (CDR (CDR (CDR V1471))))
                             (NULL (CDR (CDR (CDR (CDR V1471))))))))))
    (union (shen.extract_free_vars V1470 (CAR (CDR (CDR V1471))))
     (shen.extract_free_vars (CONS (CAR (CDR V1471)) V1470)
      (CAR (CDR (CDR (CDR V1471)))))))
   ((CONSP V1471)
    (union (shen.extract_free_vars V1470 (CAR V1471))
     (shen.extract_free_vars V1470 (CDR V1471))))
   (T NIL)))

(DEFUN shen.free_variable_warnings (V1476 V1477)
  (COND ((NULL V1477) '_)
        (T
         (simple-error
          (cn "error: the following variables are free in "
              (shen.app V1476
               (cn ": " (shen.app (shen.list_variables V1477) "" 'shen.a))
               'shen.a))))))

(DEFUN shen.list_variables (V1479)
  (COND ((AND (CONSP V1479) (NULL (CDR V1479))) (cn (str (CAR V1479)) "."))
        ((CONSP V1479)
         (cn (str (CAR V1479)) (cn ", " (shen.list_variables (CDR V1479)))))
        (T (shen.f_error 'shen.list_variables))))

(DEFUN shen.strip-protect (V1481)
  (COND
   ((AND (CONSP V1481)
         (AND (CONSP (CDR V1481))
              (AND (NULL (CDR (CDR V1481))) (EQ (CAR V1481) 'protect))))
    (shen.strip-protect (CAR (CDR V1481))))
   ((CONSP V1481) (map #'(LAMBDA (Z) (shen.strip-protect Z)) V1481)) (T V1481)))

(DEFUN shen.linearise (V1483)
  (COND
   ((AND (CONSP V1483) (AND (CONSP (CDR V1483)) (NULL (CDR (CDR V1483)))))
    (shen.linearise_help (shen.flatten (CAR V1483)) (CAR V1483)
     (CAR (CDR V1483))))
   (T (shen.f_error 'shen.linearise))))

(DEFUN shen.flatten (V1485)
  (COND ((NULL V1485) NIL)
        ((CONSP V1485)
         (APPEND (shen.flatten (CAR V1485)) (shen.flatten (CDR V1485))))
        (T (CONS V1485 NIL))))

(DEFUN shen.linearise_help (V1489 V1490 V1491)
  (COND ((NULL V1489) (CONS V1490 (CONS V1491 NIL)))
        ((CONSP V1489)
         (IF (AND (shen-cl.true? (variable? (CAR V1489)))
                  (shen-cl.true? (element? (CAR V1489) (CDR V1489))))
             (LET ((Var (gensym (CAR V1489))))
               (LET ((NewAction
                      (CONS 'where
                            (CONS (CONS '= (CONS (CAR V1489) (CONS Var NIL)))
                                  (CONS V1491 NIL)))))
                 (LET ((NewPatts (shen.linearise_X (CAR V1489) Var V1490)))
                   (shen.linearise_help (CDR V1489) NewPatts NewAction))))
             (shen.linearise_help (CDR V1489) V1490 V1491)))
        (T (shen.f_error 'shen.linearise_help))))

(DEFUN shen.linearise_X (V1504 V1505 V1506)
  (COND ((shen.ABSEQUAL V1506 V1504) V1505)
        ((CONSP V1506)
         (LET ((L (shen.linearise_X V1504 V1505 (CAR V1506))))
           (IF (shen.ABSEQUAL L (CAR V1506))
               (CONS (CAR V1506) (shen.linearise_X V1504 V1505 (CDR V1506)))
               (CONS L (CDR V1506)))))
        (T V1506)))

(DEFUN shen.aritycheck (V1509 V1510)
  (COND
   ((AND (CONSP V1510)
         (AND (CONSP (CAR V1510))
              (AND (CONSP (CDR (CAR V1510)))
                   (AND (NULL (CDR (CDR (CAR V1510)))) (NULL (CDR V1510))))))
    (do (shen.aritycheck-action (CAR (CDR (CAR V1510))))
     (shen.aritycheck-name V1509 (arity V1509) (length (CAR (CAR V1510))))))
   ((AND (CONSP V1510)
         (AND (CONSP (CAR V1510))
              (AND (CONSP (CDR (CAR V1510)))
                   (AND (NULL (CDR (CDR (CAR V1510))))
                        (AND (CONSP (CDR V1510))
                             (AND (CONSP (CAR (CDR V1510)))
                                  (AND (CONSP (CDR (CAR (CDR V1510))))
                                       (NULL
                                        (CDR (CDR (CAR (CDR V1510))))))))))))
    (IF (shen.ABSEQUAL (length (CAR (CAR V1510)))
                       (length (CAR (CAR (CDR V1510)))))
        (do (shen.aritycheck-action (CAR (CDR (CAR V1510))))
         (shen.aritycheck V1509 (CDR V1510)))
        (simple-error
         (cn "arity error in "
             (shen.app V1509 "
"
              'shen.a)))))
   (T (shen.f_error 'shen.aritycheck))))

(DEFUN shen.aritycheck-name (V1523 V1524 V1525)
  (COND
   ((IF (NUMBERP V1524)
        (= V1524 -1))
    V1525)
   ((shen.ABSEQUAL V1525 V1524) V1525)
   (T
    (do
     (shen.prhush
      (cn "
warning: changing the arity of "
          (shen.app V1523 " can cause errors.
"
           'shen.a))
      (stoutput))
     V1525))))

(DEFUN shen.aritycheck-action (V1531)
  (COND
   ((CONSP V1531)
    (do (shen.aah (CAR V1531) (CDR V1531))
     (for-each #'(LAMBDA (Y) (shen.aritycheck-action Y)) V1531)))
   (T 'shen.skip)))

(DEFUN shen.aah (V1534 V1535)
  (LET ((Arity (arity V1534)))
    (LET ((Len (length V1535)))
      (IF (AND (> Arity -1) (> Len Arity))
          (shen.prhush
           (cn "warning: "
               (shen.app V1534
                (cn " might not like "
                    (shen.app Len
                     (cn " argument"
                         (shen.app
                          (IF (> Len 1)
                              "s"
                              "")
                          ".
"
                          'shen.a))
                     'shen.a))
                'shen.a))
           (stoutput))
          'shen.skip))))

(DEFUN shen.abstract_rule (V1537)
  (COND
   ((AND (CONSP V1537) (AND (CONSP (CDR V1537)) (NULL (CDR (CDR V1537)))))
    (shen.abstraction_build (CAR V1537) (CAR (CDR V1537))))
   (T (shen.f_error 'shen.abstract_rule))))

(DEFUN shen.abstraction_build (V1540 V1541)
  (COND ((NULL V1540) V1541)
        ((CONSP V1540)
         (CONS '/.
               (CONS (CAR V1540)
                     (CONS (shen.abstraction_build (CDR V1540) V1541) NIL))))
        (T (shen.f_error 'shen.abstraction_build))))

(DEFUN shen.parameters (V1543)
  (COND
   ((IF (NUMBERP V1543)
        (= V1543 0))
    NIL)
   (T (CONS (gensym 'V) (shen.parameters (shen.subtract V1543 1))))))

(DEFUN shen.application_build (V1546 V1547)
  (COND ((NULL V1546) V1547)
        ((CONSP V1546)
         (shen.application_build (CDR V1546)
          (CONS V1547 (CONS (CAR V1546) NIL))))
        (T (shen.f_error 'shen.application_build))))

(DEFUN shen.compile_to_kl (V1550 V1551)
  (COND
   ((AND (CONSP V1551) (AND (CONSP (CDR V1551)) (NULL (CDR (CDR V1551)))))
    (LET ((Arity (shen.store-arity V1550 (length (CAR V1551)))))
      (LET ((Reduce (map #'(LAMBDA (X) (shen.reduce X)) (CAR (CDR V1551)))))
        (LET ((CondExpression (shen.cond-expression V1550 (CAR V1551) Reduce)))
          (LET ((TypeTable
                 (IF (shen-cl.true? shen.*optimise*)
                     (shen.typextable (shen.get-type V1550) (CAR V1551))
                     'shen.skip)))
            (LET ((TypedCondExpression
                   (IF (shen-cl.true? shen.*optimise*)
                       (shen.assign-types (CAR V1551) TypeTable CondExpression)
                       CondExpression)))
              (CONS 'defun
                    (CONS V1550
                          (CONS (CAR V1551)
                                (CONS TypedCondExpression NIL))))))))))
   (T (shen.f_error 'shen.compile_to_kl))))

(DEFUN shen.get-type (V1557)
  (COND ((CONSP V1557) 'shen.skip)
        (T
         (LET ((FType (assoc V1557 shen.*signedfuncs*)))
           (IF (NULL FType)
               'shen.skip
               (CDR FType))))))

(DEFUN shen.typextable (V1568 V1569)
  (COND
   ((AND (CONSP V1568)
         (AND (CONSP (CDR V1568))
              (AND (EQ '--> (CAR (CDR V1568)))
                   (AND (CONSP (CDR (CDR V1568)))
                        (AND (NULL (CDR (CDR (CDR V1568)))) (CONSP V1569))))))
    (IF (shen-cl.true? (variable? (CAR V1568)))
        (shen.typextable (CAR (CDR (CDR V1568))) (CDR V1569))
        (CONS (CONS (CAR V1569) (CAR V1568))
              (shen.typextable (CAR (CDR (CDR V1568))) (CDR V1569)))))
   (T NIL)))

(DEFUN shen.assign-types (V1573 V1574 V1575)
  (COND
   ((AND (CONSP V1575)
         (AND (EQ 'let (CAR V1575))
              (AND (CONSP (CDR V1575))
                   (AND (CONSP (CDR (CDR V1575)))
                        (AND (CONSP (CDR (CDR (CDR V1575))))
                             (NULL (CDR (CDR (CDR (CDR V1575))))))))))
    (CONS 'let
          (CONS (CAR (CDR V1575))
                (CONS (shen.assign-types V1573 V1574 (CAR (CDR (CDR V1575))))
                      (CONS
                       (shen.assign-types (CONS (CAR (CDR V1575)) V1573) V1574
                        (CAR (CDR (CDR (CDR V1575)))))
                       NIL)))))
   ((AND (CONSP V1575)
         (AND (EQ 'lambda (CAR V1575))
              (AND (CONSP (CDR V1575))
                   (AND (CONSP (CDR (CDR V1575)))
                        (NULL (CDR (CDR (CDR V1575))))))))
    (CONS 'lambda
          (CONS (CAR (CDR V1575))
                (CONS
                 (shen.assign-types (CONS (CAR (CDR V1575)) V1573) V1574
                  (CAR (CDR (CDR V1575))))
                 NIL))))
   ((AND (CONSP V1575) (EQ 'cond (CAR V1575)))
    (CONS 'cond
          (map
           #'(LAMBDA (Y)
               (CONS (shen.assign-types V1573 V1574 (CAR Y))
                     (CONS (shen.assign-types V1573 V1574 (CAR (CDR Y))) NIL)))
           (CDR V1575))))
   ((CONSP V1575)
    (LET ((NewTable (shen.typextable (shen.get-type (CAR V1575)) (CDR V1575))))
      (CONS (CAR V1575)
            (map
             #'(LAMBDA (Y) (shen.assign-types V1573 (APPEND V1574 NewTable) Y))
             (CDR V1575)))))
   (T
    (LET ((AtomType (assoc V1575 V1574)))
      (IF (CONSP AtomType)
          (CONS 'type (CONS V1575 (CONS (CDR AtomType) NIL)))
          (IF (shen-cl.true? (element? V1575 V1573))
              V1575
              (shen.atom-type V1575)))))))

(DEFUN shen.atom-type (V1577)
  (IF (STRINGP V1577)
      (CONS 'type (CONS V1577 (CONS 'string NIL)))
      (IF (NUMBERP V1577)
          (CONS 'type (CONS V1577 (CONS 'number NIL)))
          (IF (shen-cl.true? (boolean? V1577))
              (CONS 'type (CONS V1577 (CONS 'boolean NIL)))
              (IF (shen-cl.true? (symbol? V1577))
                  (CONS 'type (CONS V1577 (CONS 'symbol NIL)))
                  V1577)))))

(DEFUN shen.store-arity (V1582 V1583)
  (COND ((shen-cl.true? shen.*installing-kl*) 'shen.skip)
        (T (put V1582 'arity V1583 *property-vector*))))

(DEFUN shen.reduce (V1585)
  (do (set 'shen.*teststack* NIL)
   (LET ((Result (shen.reduce_help V1585)))
     (CONS (CONS '|:| (CONS 'shen.tests (REVERSE shen.*teststack*)))
           (CONS Result NIL)))))

(DEFUN shen.reduce_help (V1587)
  (COND
   ((AND (CONSP V1587)
         (AND (CONSP (CAR V1587))
              (AND (EQ '/. (CAR (CAR V1587)))
                   (AND (CONSP (CDR (CAR V1587)))
                        (AND (CONSP (CAR (CDR (CAR V1587))))
                             (AND (EQ 'cons (CAR (CAR (CDR (CAR V1587)))))
                                  (AND (CONSP (CDR (CAR (CDR (CAR V1587)))))
                                       (AND
                                        (CONSP
                                         (CDR (CDR (CAR (CDR (CAR V1587))))))
                                        (AND
                                         (NULL
                                          (CDR
                                           (CDR
                                            (CDR (CAR (CDR (CAR V1587)))))))
                                         (AND (CONSP (CDR (CDR (CAR V1587))))
                                              (AND
                                               (NULL
                                                (CDR (CDR (CDR (CAR V1587)))))
                                               (AND (CONSP (CDR V1587))
                                                    (NULL
                                                     (CDR
                                                      (CDR V1587)))))))))))))))
    (do (shen.add_test (CONS 'cons? (CDR V1587)))
     (LET ((Abstraction
            (CONS '/.
                  (CONS (CAR (CDR (CAR (CDR (CAR V1587)))))
                        (CONS
                         (CONS '/.
                               (CONS (CAR (CDR (CDR (CAR (CDR (CAR V1587))))))
                                     (CONS
                                      (shen.ebr (CAR (CDR V1587))
                                       (CAR (CDR (CAR V1587)))
                                       (CAR (CDR (CDR (CAR V1587)))))
                                      NIL)))
                         NIL)))))
       (LET ((Application
              (CONS (CONS Abstraction (CONS (CONS 'hd (CDR V1587)) NIL))
                    (CONS (CONS 'tl (CDR V1587)) NIL))))
         (shen.reduce_help Application)))))
   ((AND (CONSP V1587)
         (AND (CONSP (CAR V1587))
              (AND (EQ '/. (CAR (CAR V1587)))
                   (AND (CONSP (CDR (CAR V1587)))
                        (AND (CONSP (CAR (CDR (CAR V1587))))
                             (AND (EQ '@p (CAR (CAR (CDR (CAR V1587)))))
                                  (AND (CONSP (CDR (CAR (CDR (CAR V1587)))))
                                       (AND
                                        (CONSP
                                         (CDR (CDR (CAR (CDR (CAR V1587))))))
                                        (AND
                                         (NULL
                                          (CDR
                                           (CDR
                                            (CDR (CAR (CDR (CAR V1587)))))))
                                         (AND (CONSP (CDR (CDR (CAR V1587))))
                                              (AND
                                               (NULL
                                                (CDR (CDR (CDR (CAR V1587)))))
                                               (AND (CONSP (CDR V1587))
                                                    (NULL
                                                     (CDR
                                                      (CDR V1587)))))))))))))))
    (do (shen.add_test (CONS 'tuple? (CDR V1587)))
     (LET ((Abstraction
            (CONS '/.
                  (CONS (CAR (CDR (CAR (CDR (CAR V1587)))))
                        (CONS
                         (CONS '/.
                               (CONS (CAR (CDR (CDR (CAR (CDR (CAR V1587))))))
                                     (CONS
                                      (shen.ebr (CAR (CDR V1587))
                                       (CAR (CDR (CAR V1587)))
                                       (CAR (CDR (CDR (CAR V1587)))))
                                      NIL)))
                         NIL)))))
       (LET ((Application
              (CONS (CONS Abstraction (CONS (CONS 'fst (CDR V1587)) NIL))
                    (CONS (CONS 'snd (CDR V1587)) NIL))))
         (shen.reduce_help Application)))))
   ((AND (CONSP V1587)
         (AND (CONSP (CAR V1587))
              (AND (EQ '/. (CAR (CAR V1587)))
                   (AND (CONSP (CDR (CAR V1587)))
                        (AND (CONSP (CAR (CDR (CAR V1587))))
                             (AND (EQ '@v (CAR (CAR (CDR (CAR V1587)))))
                                  (AND (CONSP (CDR (CAR (CDR (CAR V1587)))))
                                       (AND
                                        (CONSP
                                         (CDR (CDR (CAR (CDR (CAR V1587))))))
                                        (AND
                                         (NULL
                                          (CDR
                                           (CDR
                                            (CDR (CAR (CDR (CAR V1587)))))))
                                         (AND (CONSP (CDR (CDR (CAR V1587))))
                                              (AND
                                               (NULL
                                                (CDR (CDR (CDR (CAR V1587)))))
                                               (AND (CONSP (CDR V1587))
                                                    (NULL
                                                     (CDR
                                                      (CDR V1587)))))))))))))))
    (do (shen.add_test (CONS 'shen.+vector? (CDR V1587)))
     (LET ((Abstraction
            (CONS '/.
                  (CONS (CAR (CDR (CAR (CDR (CAR V1587)))))
                        (CONS
                         (CONS '/.
                               (CONS (CAR (CDR (CDR (CAR (CDR (CAR V1587))))))
                                     (CONS
                                      (shen.ebr (CAR (CDR V1587))
                                       (CAR (CDR (CAR V1587)))
                                       (CAR (CDR (CDR (CAR V1587)))))
                                      NIL)))
                         NIL)))))
       (LET ((Application
              (CONS (CONS Abstraction (CONS (CONS 'hdv (CDR V1587)) NIL))
                    (CONS (CONS 'tlv (CDR V1587)) NIL))))
         (shen.reduce_help Application)))))
   ((AND (CONSP V1587)
         (AND (CONSP (CAR V1587))
              (AND (EQ '/. (CAR (CAR V1587)))
                   (AND (CONSP (CDR (CAR V1587)))
                        (AND (CONSP (CAR (CDR (CAR V1587))))
                             (AND (EQ '@s (CAR (CAR (CDR (CAR V1587)))))
                                  (AND (CONSP (CDR (CAR (CDR (CAR V1587)))))
                                       (AND
                                        (CONSP
                                         (CDR (CDR (CAR (CDR (CAR V1587))))))
                                        (AND
                                         (NULL
                                          (CDR
                                           (CDR
                                            (CDR (CAR (CDR (CAR V1587)))))))
                                         (AND (CONSP (CDR (CDR (CAR V1587))))
                                              (AND
                                               (NULL
                                                (CDR (CDR (CDR (CAR V1587)))))
                                               (AND (CONSP (CDR V1587))
                                                    (NULL
                                                     (CDR
                                                      (CDR V1587)))))))))))))))
    (do (shen.add_test (CONS 'shen.+string? (CDR V1587)))
     (LET ((Abstraction
            (CONS '/.
                  (CONS (CAR (CDR (CAR (CDR (CAR V1587)))))
                        (CONS
                         (CONS '/.
                               (CONS (CAR (CDR (CDR (CAR (CDR (CAR V1587))))))
                                     (CONS
                                      (shen.ebr (CAR (CDR V1587))
                                       (CAR (CDR (CAR V1587)))
                                       (CAR (CDR (CDR (CAR V1587)))))
                                      NIL)))
                         NIL)))))
       (LET ((Application
              (CONS
               (CONS Abstraction
                     (CONS (CONS 'pos (CONS (CAR (CDR V1587)) (CONS 0 NIL)))
                           NIL))
               (CONS (CONS 'tlstr (CDR V1587)) NIL))))
         (shen.reduce_help Application)))))
   ((AND (CONSP V1587)
         (AND (CONSP (CAR V1587))
              (AND (EQ '/. (CAR (CAR V1587)))
                   (AND (CONSP (CDR (CAR V1587)))
                        (AND (CONSP (CDR (CDR (CAR V1587))))
                             (AND (NULL (CDR (CDR (CDR (CAR V1587)))))
                                  (AND (CONSP (CDR V1587))
                                       (AND (NULL (CDR (CDR V1587)))
                                            (NOT
                                             (shen-cl.true?
                                              (variable?
                                               (CAR
                                                (CDR (CAR V1587))))))))))))))
    (do (shen.add_test (CONS '= (CONS (CAR (CDR (CAR V1587))) (CDR V1587))))
     (shen.reduce_help (CAR (CDR (CDR (CAR V1587)))))))
   ((AND (CONSP V1587)
         (AND (CONSP (CAR V1587))
              (AND (EQ '/. (CAR (CAR V1587)))
                   (AND (CONSP (CDR (CAR V1587)))
                        (AND (CONSP (CDR (CDR (CAR V1587))))
                             (AND (NULL (CDR (CDR (CDR (CAR V1587)))))
                                  (AND (CONSP (CDR V1587))
                                       (NULL (CDR (CDR V1587))))))))))
    (shen.reduce_help
     (shen.ebr (CAR (CDR V1587)) (CAR (CDR (CAR V1587)))
      (CAR (CDR (CDR (CAR V1587)))))))
   ((AND (CONSP V1587)
         (AND (EQ 'where (CAR V1587))
              (AND (CONSP (CDR V1587))
                   (AND (CONSP (CDR (CDR V1587)))
                        (NULL (CDR (CDR (CDR V1587))))))))
    (do (shen.add_test (CAR (CDR V1587)))
     (shen.reduce_help (CAR (CDR (CDR V1587))))))
   ((AND (CONSP V1587) (AND (CONSP (CDR V1587)) (NULL (CDR (CDR V1587)))))
    (LET ((Z (shen.reduce_help (CAR V1587))))
      (IF (shen.ABSEQUAL (CAR V1587) Z)
          V1587
          (shen.reduce_help (CONS Z (CDR V1587))))))
   (T V1587)))

(DEFUN shen.+string? (V1589)
  (COND ((EQUAL "" V1589) 'false) (T (string? V1589))))

(DEFUN shen.+vector? (V1591)
  (and (absvector? V1591) (shen.greater? (<-address V1591 0) 0)))

(DEFUN shen.ebr (V1605 V1606 V1607)
  (COND ((shen.ABSEQUAL V1607 V1606) V1605)
        ((AND (CONSP V1607)
              (AND (EQ '/. (CAR V1607))
                   (AND (CONSP (CDR V1607))
                        (AND (CONSP (CDR (CDR V1607)))
                             (AND (NULL (CDR (CDR (CDR V1607))))
                                  (> (occurrences V1606 (CAR (CDR V1607)))
                                     0))))))
         V1607)
        ((AND (CONSP V1607)
              (AND (EQ 'lambda (CAR V1607))
                   (AND (CONSP (CDR V1607))
                        (AND (CONSP (CDR (CDR V1607)))
                             (AND (NULL (CDR (CDR (CDR V1607))))
                                  (> (occurrences V1606 (CAR (CDR V1607)))
                                     0))))))
         V1607)
        ((AND (CONSP V1607)
              (AND (EQ 'let (CAR V1607))
                   (AND (CONSP (CDR V1607))
                        (AND (CONSP (CDR (CDR V1607)))
                             (AND (CONSP (CDR (CDR (CDR V1607))))
                                  (AND (NULL (CDR (CDR (CDR (CDR V1607)))))
                                       (shen.ABSEQUAL (CAR (CDR V1607))
                                                      V1606)))))))
         (CONS 'let
               (CONS (CAR (CDR V1607))
                     (CONS
                      (shen.ebr V1605 (CAR (CDR V1607))
                       (CAR (CDR (CDR V1607))))
                      (CDR (CDR (CDR V1607)))))))
        ((CONSP V1607)
         (CONS (shen.ebr V1605 V1606 (CAR V1607))
               (shen.ebr V1605 V1606 (CDR V1607))))
        (T V1607)))

(DEFUN shen.add_test (V1609)
  (set 'shen.*teststack* (CONS V1609 shen.*teststack*)))

(DEFUN shen.cond-expression (V1613 V1614 V1615)
  (LET ((Err (shen.err-condition V1613)))
    (LET ((Cases (shen.case-form V1615 Err)))
      (LET ((EncodeChoices (shen.encode-choices Cases V1613)))
        (shen.cond-form EncodeChoices)))))

(DEFUN shen.cond-form (V1619)
  (COND
   ((AND (CONSP V1619)
         (AND (CONSP (CAR V1619))
              (AND (EQ 'true (CAR (CAR V1619)))
                   (AND (CONSP (CDR (CAR V1619)))
                        (NULL (CDR (CDR (CAR V1619))))))))
    (CAR (CDR (CAR V1619))))
   (T (CONS 'cond V1619))))

(DEFUN shen.encode-choices (V1624 V1625)
  (COND ((NULL V1624) NIL)
        ((AND (CONSP V1624)
              (AND (CONSP (CAR V1624))
                   (AND (EQ 'true (CAR (CAR V1624)))
                        (AND (CONSP (CDR (CAR V1624)))
                             (AND (CONSP (CAR (CDR (CAR V1624))))
                                  (AND
                                   (EQ 'shen.choicepoint!
                                       (CAR (CAR (CDR (CAR V1624)))))
                                   (AND (CONSP (CDR (CAR (CDR (CAR V1624)))))
                                        (AND
                                         (NULL
                                          (CDR (CDR (CAR (CDR (CAR V1624))))))
                                         (AND (NULL (CDR (CDR (CAR V1624))))
                                              (NULL (CDR V1624)))))))))))
         (CONS
          (CONS 'true
                (CONS
                 (CONS 'let
                       (CONS 'Result
                             (CONS (CAR (CDR (CAR (CDR (CAR V1624)))))
                                   (CONS
                                    (CONS 'if
                                          (CONS
                                           (CONS '=
                                                 (CONS 'Result
                                                       (CONS (CONS 'fail NIL)
                                                             NIL)))
                                           (CONS
                                            (IF (shen-cl.true?
                                                 shen.*installing-kl*)
                                                (CONS 'shen.sys-error
                                                      (CONS V1625 NIL))
                                                (CONS 'shen.f_error
                                                      (CONS V1625 NIL)))
                                            (CONS 'Result NIL))))
                                    NIL))))
                 NIL))
          NIL))
        ((AND (CONSP V1624)
              (AND (CONSP (CAR V1624))
                   (AND (EQ 'true (CAR (CAR V1624)))
                        (AND (CONSP (CDR (CAR V1624)))
                             (AND (CONSP (CAR (CDR (CAR V1624))))
                                  (AND
                                   (EQ 'shen.choicepoint!
                                       (CAR (CAR (CDR (CAR V1624)))))
                                   (AND (CONSP (CDR (CAR (CDR (CAR V1624)))))
                                        (AND
                                         (NULL
                                          (CDR (CDR (CAR (CDR (CAR V1624))))))
                                         (NULL (CDR (CDR (CAR V1624))))))))))))
         (CONS
          (CONS 'true
                (CONS
                 (CONS 'let
                       (CONS 'Result
                             (CONS (CAR (CDR (CAR (CDR (CAR V1624)))))
                                   (CONS
                                    (CONS 'if
                                          (CONS
                                           (CONS '=
                                                 (CONS 'Result
                                                       (CONS (CONS 'fail NIL)
                                                             NIL)))
                                           (CONS
                                            (shen.cond-form
                                             (shen.encode-choices (CDR V1624)
                                              V1625))
                                            (CONS 'Result NIL))))
                                    NIL))))
                 NIL))
          NIL))
        ((AND (CONSP V1624)
              (AND (CONSP (CAR V1624))
                   (AND (CONSP (CDR (CAR V1624)))
                        (AND (CONSP (CAR (CDR (CAR V1624))))
                             (AND
                              (EQ 'shen.choicepoint!
                                  (CAR (CAR (CDR (CAR V1624)))))
                              (AND (CONSP (CDR (CAR (CDR (CAR V1624)))))
                                   (AND
                                    (NULL (CDR (CDR (CAR (CDR (CAR V1624))))))
                                    (NULL (CDR (CDR (CAR V1624)))))))))))
         (CONS
          (CONS 'true
                (CONS
                 (CONS 'let
                       (CONS 'Freeze
                             (CONS
                              (CONS 'freeze
                                    (CONS
                                     (shen.cond-form
                                      (shen.encode-choices (CDR V1624) V1625))
                                     NIL))
                              (CONS
                               (CONS 'if
                                     (CONS (CAR (CAR V1624))
                                           (CONS
                                            (CONS 'let
                                                  (CONS 'Result
                                                        (CONS
                                                         (CAR
                                                          (CDR
                                                           (CAR
                                                            (CDR
                                                             (CAR V1624)))))
                                                         (CONS
                                                          (CONS 'if
                                                                (CONS
                                                                 (CONS '=
                                                                       (CONS
                                                                        'Result
                                                                        (CONS
                                                                         (CONS
                                                                          'fail
                                                                          NIL)
                                                                         NIL)))
                                                                 (CONS
                                                                  (CONS 'thaw
                                                                        (CONS
                                                                         'Freeze
                                                                         NIL))
                                                                  (CONS 'Result
                                                                        NIL))))
                                                          NIL))))
                                            (CONS
                                             (CONS 'thaw (CONS 'Freeze NIL))
                                             NIL))))
                               NIL))))
                 NIL))
          NIL))
        ((AND (CONSP V1624)
              (AND (CONSP (CAR V1624))
                   (AND (CONSP (CDR (CAR V1624)))
                        (NULL (CDR (CDR (CAR V1624)))))))
         (CONS (CAR V1624) (shen.encode-choices (CDR V1624) V1625)))
        (T (shen.f_error 'shen.encode-choices))))

(DEFUN shen.case-form (V1632 V1633)
  (COND ((NULL V1632) (CONS V1633 NIL))
        ((AND (CONSP V1632)
              (AND (CONSP (CAR V1632))
                   (AND (CONSP (CAR (CAR V1632)))
                        (AND (EQ '|:| (CAR (CAR (CAR V1632))))
                             (AND (CONSP (CDR (CAR (CAR V1632))))
                                  (AND
                                   (EQ 'shen.tests
                                       (CAR (CDR (CAR (CAR V1632)))))
                                   (AND (NULL (CDR (CDR (CAR (CAR V1632)))))
                                        (AND (CONSP (CDR (CAR V1632)))
                                             (AND
                                              (CONSP (CAR (CDR (CAR V1632))))
                                              (AND
                                               (EQ 'shen.choicepoint!
                                                   (CAR
                                                    (CAR (CDR (CAR V1632)))))
                                               (AND
                                                (CONSP
                                                 (CDR (CAR (CDR (CAR V1632)))))
                                                (AND
                                                 (NULL
                                                  (CDR
                                                   (CDR
                                                    (CAR (CDR (CAR V1632))))))
                                                 (NULL
                                                  (CDR
                                                   (CDR
                                                    (CAR V1632))))))))))))))))
         (CONS (CONS 'true (CDR (CAR V1632)))
               (shen.case-form (CDR V1632) V1633)))
        ((AND (CONSP V1632)
              (AND (CONSP (CAR V1632))
                   (AND (CONSP (CAR (CAR V1632)))
                        (AND (EQ '|:| (CAR (CAR (CAR V1632))))
                             (AND (CONSP (CDR (CAR (CAR V1632))))
                                  (AND
                                   (EQ 'shen.tests
                                       (CAR (CDR (CAR (CAR V1632)))))
                                   (AND (NULL (CDR (CDR (CAR (CAR V1632)))))
                                        (AND (CONSP (CDR (CAR V1632)))
                                             (NULL
                                              (CDR (CDR (CAR V1632))))))))))))
         (CONS (CONS 'true (CDR (CAR V1632))) NIL))
        ((AND (CONSP V1632)
              (AND (CONSP (CAR V1632))
                   (AND (CONSP (CAR (CAR V1632)))
                        (AND (EQ '|:| (CAR (CAR (CAR V1632))))
                             (AND (CONSP (CDR (CAR (CAR V1632))))
                                  (AND
                                   (EQ 'shen.tests
                                       (CAR (CDR (CAR (CAR V1632)))))
                                   (AND (CONSP (CDR (CAR V1632)))
                                        (NULL (CDR (CDR (CAR V1632)))))))))))
         (CONS
          (CONS (shen.embed-and (CDR (CDR (CAR (CAR V1632)))))
                (CDR (CAR V1632)))
          (shen.case-form (CDR V1632) V1633)))
        (T (shen.f_error 'shen.case-form))))

(DEFUN shen.embed-and (V1635)
  (COND ((AND (CONSP V1635) (NULL (CDR V1635))) (CAR V1635))
        ((CONSP V1635)
         (CONS 'and
               (CONS (CAR V1635) (CONS (shen.embed-and (CDR V1635)) NIL))))
        (T (shen.f_error 'shen.embed-and))))

(DEFUN shen.err-condition (V1637)
  (CONS 'true (CONS (CONS 'shen.f_error (CONS V1637 NIL)) NIL)))

(DEFUN shen.sys-error (V1639)
  (simple-error
   (cn "system function "
       (shen.app V1639 ": unexpected argument
"
        'shen.a))))

