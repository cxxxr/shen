
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

(DEFUN read-char-code (V2352) (read-byte V2352))

(DEFUN read-file-as-bytelist (V2354)
  (shen.read-file-as-Xlist V2354 #'(LAMBDA (S) (read-byte S))))

(DEFUN read-file-as-charlist (V2356)
  (shen.read-file-as-Xlist V2356 #'(LAMBDA (S) (read-char-code S))))

(DEFUN shen.read-file-as-Xlist (V2359 V2360)
  (LET ((Stream (open V2359 'in)))
    (LET ((X (shen.apply V2360 (LIST Stream))))
      (LET ((Xs (shen.read-file-as-Xlist-help Stream V2360 X NIL)))
        (LET ((Close (close Stream)))
          (REVERSE Xs))))))

(DEFUN shen.read-file-as-Xlist-help (V2365 V2366 V2367 V2368)
  (COND
   ((IF (NUMBERP V2367)
        (= V2367 -1))
    V2368)
   (T
    (shen.read-file-as-Xlist-help V2365 V2366 (shen.apply V2366 (LIST V2365))
     (CONS V2367 V2368)))))

(DEFUN read-file-as-string (V2370)
  (LET ((Stream (open V2370 'in)))
    (shen.rfas-h Stream (read-char-code Stream) "")))

(DEFUN shen.rfas-h (V2374 V2375 V2376)
  (COND
   ((IF (NUMBERP V2375)
        (= V2375 -1))
    (do (close V2374) V2376))
   (T (shen.rfas-h V2374 (read-char-code V2374) (cn V2376 (n->string V2375))))))

(DEFUN input (V2378) (eval-kl (read V2378)))

(DEFUN input+ (V2381 V2382)
  (LET ((Mono? (shen.monotype V2381)))
    (LET ((Input (read V2382)))
      (IF (EQ 'false (shen.typecheck Input (shen.demodulate V2381)))
          (simple-error
           (cn "type error: "
               (shen.app Input
                (cn " is not of type "
                    (shen.app V2381 "
"
                     'shen.r))
                'shen.r)))
          (eval-kl Input)))))

(DEFUN shen.monotype (V2384)
  (COND ((CONSP V2384) (map #'(LAMBDA (Z) (shen.monotype Z)) V2384))
        (T
         (IF (shen-cl.true? (variable? V2384))
             (simple-error
              (cn "input+ expects a monotype: not "
                  (shen.app V2384 "
"
                   'shen.a)))
             V2384))))

(DEFUN read (V2386) (CAR (shen.read-loop V2386 (read-char-code V2386) NIL)))

(DEFUN it () shen.*it*)

(DEFUN shen.read-loop (V2394 V2395 V2396)
  (COND
   ((IF (NUMBERP V2395)
        (= V2395 94))
    (simple-error "read aborted"))
   ((IF (NUMBERP V2395)
        (= V2395 -1))
    (IF (NULL V2396)
        (simple-error "error: empty stream")
        (compile #'(LAMBDA (X) (shen.<st_input> X)) V2396 #'(LAMBDA (E) E))))
   ((shen-cl.true? (shen.terminator? V2395))
    (LET ((AllChars (APPEND V2396 (CONS V2395 NIL))))
      (LET ((It (shen.record-it AllChars)))
        (LET ((Read
               (compile #'(LAMBDA (X) (shen.<st_input> X)) AllChars
                        #'(LAMBDA (E) 'shen.nextbyte))))
          (IF (OR (EQ Read 'shen.nextbyte) (NULL Read))
              (shen.read-loop V2394 (read-char-code V2394) AllChars)
              Read)))))
   (T
    (shen.read-loop V2394 (read-char-code V2394)
     (APPEND V2396 (CONS V2395 NIL))))))

(DEFUN shen.terminator? (V2398)
  (element? V2398
            (CONS 9
                  (CONS 10
                        (CONS 13
                              (CONS 32 (CONS 34 (CONS 41 (CONS 93 NIL)))))))))

(DEFUN lineread (V2400) (shen.lineread-loop (read-char-code V2400) NIL V2400))

(DEFUN shen.lineread-loop (V2405 V2406 V2407)
  (COND
   ((IF (NUMBERP V2405)
        (= V2405 -1))
    (IF (NULL V2406)
        (simple-error "empty stream")
        (compile #'(LAMBDA (X) (shen.<st_input> X)) V2406 #'(LAMBDA (E) E))))
   ((shen.ABSEQUAL V2405 (shen.hat)) (simple-error "line read aborted"))
   ((shen-cl.true?
     (element? V2405 (CONS (shen.newline) (CONS (shen.carriage-return) NIL))))
    (LET ((Line
           (compile #'(LAMBDA (X) (shen.<st_input> X)) V2406
                    #'(LAMBDA (E) 'shen.nextline))))
      (LET ((It (shen.record-it V2406)))
        (IF (OR (EQ Line 'shen.nextline) (NULL Line))
            (shen.lineread-loop (read-char-code V2407)
             (APPEND V2406 (CONS V2405 NIL)) V2407)
            Line))))
   (T
    (shen.lineread-loop (read-char-code V2407) (APPEND V2406 (CONS V2405 NIL))
     V2407))))

(DEFUN shen.record-it (V2409)
  (LET ((TrimLeft (shen.trim-whitespace V2409)))
    (LET ((TrimRight (shen.trim-whitespace (REVERSE TrimLeft))))
      (LET ((Trimmed (REVERSE TrimRight)))
        (shen.record-it-h Trimmed)))))

(DEFUN shen.trim-whitespace (V2411)
  (COND
   ((AND (CONSP V2411)
         (shen-cl.true?
          (element? (CAR V2411) (CONS 9 (CONS 10 (CONS 13 (CONS 32 NIL)))))))
    (shen.trim-whitespace (CDR V2411)))
   (T V2411)))

(DEFUN shen.record-it-h (V2413)
  (do (set 'shen.*it* (shen.cn-all (map #'(LAMBDA (X) (n->string X)) V2413)))
      V2413))

(DEFUN shen.cn-all (V2415)
  (COND ((NULL V2415) "")
        ((CONSP V2415) (cn (CAR V2415) (shen.cn-all (CDR V2415))))
        (T (shen.f_error 'shen.cn-all))))

(DEFUN read-file (V2417)
  (LET ((Charlist (read-file-as-charlist V2417)))
    (compile #'(LAMBDA (X) (shen.<st_input> X)) Charlist
             #'(LAMBDA (X) (shen.read-error X)))))

(DEFUN read-from-string (V2419)
  (LET ((Ns (map #'(LAMBDA (X) (string->n X)) (explode V2419))))
    (compile #'(LAMBDA (X) (shen.<st_input> X)) Ns
             #'(LAMBDA (X) (shen.read-error X)))))

(DEFUN shen.read-error (V2427)
  (COND
   ((AND (CONSP V2427)
         (AND (CONSP (CAR V2427))
              (AND (CONSP (CDR V2427)) (NULL (CDR (CDR V2427))))))
    (simple-error
     (cn "read error here:

 "
         (shen.app (shen.compress-50 50 (CAR V2427)) "
"
          'shen.a))))
   (T
    (simple-error "read error
"))))

(DEFUN shen.compress-50 (V2434 V2435)
  (COND ((NULL V2435) "")
        ((IF (NUMBERP V2434)
             (= V2434 0))
         "")
        ((CONSP V2435)
         (cn (n->string (CAR V2435))
             (shen.compress-50 (shen.subtract V2434 1) (CDR V2435))))
        (T (shen.f_error 'shen.compress-50))))

(DEFUN shen.<st_input> (V2437)
  (LET ((YaccParse
         (LET ((Parse_shen.<lsb> (shen.<lsb> V2437)))
           (IF (NOT (EQ (fail) Parse_shen.<lsb>))
               (LET ((Parse_shen.<st_input1>
                      (shen.<st_input1> Parse_shen.<lsb>)))
                 (IF (NOT (EQ (fail) Parse_shen.<st_input1>))
                     (LET ((Parse_shen.<rsb>
                            (shen.<rsb> Parse_shen.<st_input1>)))
                       (IF (NOT (EQ (fail) Parse_shen.<rsb>))
                           (LET ((Parse_shen.<st_input2>
                                  (shen.<st_input2> Parse_shen.<rsb>)))
                             (IF (NOT (EQ (fail) Parse_shen.<st_input2>))
                                 (shen.pair (CAR Parse_shen.<st_input2>)
                                            (CONS
                                             (macroexpand
                                              (shen.cons_form
                                               (shen.hdtl
                                                Parse_shen.<st_input1>)))
                                             (shen.hdtl
                                              Parse_shen.<st_input2>)))
                                 (fail)))
                           (fail)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((YaccParse
               (LET ((Parse_shen.<lrb> (shen.<lrb> V2437)))
                 (IF (NOT (EQ (fail) Parse_shen.<lrb>))
                     (LET ((Parse_shen.<st_input1>
                            (shen.<st_input1> Parse_shen.<lrb>)))
                       (IF (NOT (EQ (fail) Parse_shen.<st_input1>))
                           (LET ((Parse_shen.<rrb>
                                  (shen.<rrb> Parse_shen.<st_input1>)))
                             (IF (NOT (EQ (fail) Parse_shen.<rrb>))
                                 (LET ((Parse_shen.<st_input2>
                                        (shen.<st_input2> Parse_shen.<rrb>)))
                                   (IF (NOT (EQ (fail) Parse_shen.<st_input2>))
                                       (shen.pair (CAR Parse_shen.<st_input2>)
                                                  (shen.package-macro
                                                   (macroexpand
                                                    (shen.hdtl
                                                     Parse_shen.<st_input1>))
                                                   (shen.hdtl
                                                    Parse_shen.<st_input2>)))
                                       (fail)))
                                 (fail)))
                           (fail)))
                     (fail)))))
          (IF (EQ YaccParse (fail))
              (LET ((YaccParse
                     (LET ((Parse_shen.<lcurly> (shen.<lcurly> V2437)))
                       (IF (NOT (EQ (fail) Parse_shen.<lcurly>))
                           (LET ((Parse_shen.<st_input>
                                  (shen.<st_input> Parse_shen.<lcurly>)))
                             (IF (NOT (EQ (fail) Parse_shen.<st_input>))
                                 (shen.pair (CAR Parse_shen.<st_input>)
                                            (CONS '{
                                                  (shen.hdtl
                                                   Parse_shen.<st_input>)))
                                 (fail)))
                           (fail)))))
                (IF (EQ YaccParse (fail))
                    (LET ((YaccParse
                           (LET ((Parse_shen.<rcurly> (shen.<rcurly> V2437)))
                             (IF (NOT (EQ (fail) Parse_shen.<rcurly>))
                                 (LET ((Parse_shen.<st_input>
                                        (shen.<st_input> Parse_shen.<rcurly>)))
                                   (IF (NOT (EQ (fail) Parse_shen.<st_input>))
                                       (shen.pair (CAR Parse_shen.<st_input>)
                                                  (CONS '}
                                                        (shen.hdtl
                                                         Parse_shen.<st_input>)))
                                       (fail)))
                                 (fail)))))
                      (IF (EQ YaccParse (fail))
                          (LET ((YaccParse
                                 (LET ((Parse_shen.<bar> (shen.<bar> V2437)))
                                   (IF (NOT (EQ (fail) Parse_shen.<bar>))
                                       (LET ((Parse_shen.<st_input>
                                              (shen.<st_input>
                                               Parse_shen.<bar>)))
                                         (IF (NOT
                                              (EQ (fail)
                                                  Parse_shen.<st_input>))
                                             (shen.pair
                                              (CAR Parse_shen.<st_input>)
                                              (CONS 'bar!
                                                    (shen.hdtl
                                                     Parse_shen.<st_input>)))
                                             (fail)))
                                       (fail)))))
                            (IF (EQ YaccParse (fail))
                                (LET ((YaccParse
                                       (LET ((Parse_shen.<semicolon>
                                              (shen.<semicolon> V2437)))
                                         (IF (NOT
                                              (EQ (fail)
                                                  Parse_shen.<semicolon>))
                                             (LET ((Parse_shen.<st_input>
                                                    (shen.<st_input>
                                                     Parse_shen.<semicolon>)))
                                               (IF (NOT
                                                    (EQ (fail)
                                                        Parse_shen.<st_input>))
                                                   (shen.pair
                                                    (CAR Parse_shen.<st_input>)
                                                    (CONS '|;|
                                                          (shen.hdtl
                                                           Parse_shen.<st_input>)))
                                                   (fail)))
                                             (fail)))))
                                  (IF (EQ YaccParse (fail))
                                      (LET ((YaccParse
                                             (LET ((Parse_shen.<colon>
                                                    (shen.<colon> V2437)))
                                               (IF (NOT
                                                    (EQ (fail)
                                                        Parse_shen.<colon>))
                                                   (LET ((Parse_shen.<equal>
                                                          (shen.<equal>
                                                           Parse_shen.<colon>)))
                                                     (IF (NOT
                                                          (EQ (fail)
                                                              Parse_shen.<equal>))
                                                         (LET ((Parse_shen.<st_input>
                                                                (shen.<st_input>
                                                                 Parse_shen.<equal>)))
                                                           (IF (NOT
                                                                (EQ (fail)
                                                                    Parse_shen.<st_input>))
                                                               (shen.pair
                                                                (CAR
                                                                 Parse_shen.<st_input>)
                                                                (CONS '|:=|
                                                                      (shen.hdtl
                                                                       Parse_shen.<st_input>)))
                                                               (fail)))
                                                         (fail)))
                                                   (fail)))))
                                        (IF (EQ YaccParse (fail))
                                            (LET ((YaccParse
                                                   (LET ((Parse_shen.<colon>
                                                          (shen.<colon> V2437)))
                                                     (IF (NOT
                                                          (EQ (fail)
                                                              Parse_shen.<colon>))
                                                         (LET ((Parse_shen.<minus>
                                                                (shen.<minus>
                                                                 Parse_shen.<colon>)))
                                                           (IF (NOT
                                                                (EQ (fail)
                                                                    Parse_shen.<minus>))
                                                               (LET ((Parse_shen.<st_input>
                                                                      (shen.<st_input>
                                                                       Parse_shen.<minus>)))
                                                                 (IF (NOT
                                                                      (EQ
                                                                       (fail)
                                                                       Parse_shen.<st_input>))
                                                                     (shen.pair
                                                                      (CAR
                                                                       Parse_shen.<st_input>)
                                                                      (CONS
                                                                       '|:-|
                                                                       (shen.hdtl
                                                                        Parse_shen.<st_input>)))
                                                                     (fail)))
                                                               (fail)))
                                                         (fail)))))
                                              (IF (EQ YaccParse (fail))
                                                  (LET ((YaccParse
                                                         (LET ((Parse_shen.<colon>
                                                                (shen.<colon>
                                                                 V2437)))
                                                           (IF (NOT
                                                                (EQ (fail)
                                                                    Parse_shen.<colon>))
                                                               (LET ((Parse_shen.<st_input>
                                                                      (shen.<st_input>
                                                                       Parse_shen.<colon>)))
                                                                 (IF (NOT
                                                                      (EQ
                                                                       (fail)
                                                                       Parse_shen.<st_input>))
                                                                     (shen.pair
                                                                      (CAR
                                                                       Parse_shen.<st_input>)
                                                                      (CONS
                                                                       '|:|
                                                                       (shen.hdtl
                                                                        Parse_shen.<st_input>)))
                                                                     (fail)))
                                                               (fail)))))
                                                    (IF (EQ YaccParse (fail))
                                                        (LET ((YaccParse
                                                               (LET ((Parse_shen.<comma>
                                                                      (shen.<comma>
                                                                       V2437)))
                                                                 (IF (NOT
                                                                      (EQ
                                                                       (fail)
                                                                       Parse_shen.<comma>))
                                                                     (LET ((Parse_shen.<st_input>
                                                                            (shen.<st_input>
                                                                             Parse_shen.<comma>)))
                                                                       (IF (NOT
                                                                            (EQ
                                                                             (fail)
                                                                             Parse_shen.<st_input>))
                                                                           (shen.pair
                                                                            (CAR
                                                                             Parse_shen.<st_input>)
                                                                            (CONS
                                                                             (intern
                                                                              ",")
                                                                             (shen.hdtl
                                                                              Parse_shen.<st_input>)))
                                                                           (fail)))
                                                                     (fail)))))
                                                          (IF (EQ YaccParse
                                                                  (fail))
                                                              (LET ((YaccParse
                                                                     (LET ((Parse_shen.<comment>
                                                                            (shen.<comment>
                                                                             V2437)))
                                                                       (IF (NOT
                                                                            (EQ
                                                                             (fail)
                                                                             Parse_shen.<comment>))
                                                                           (LET ((Parse_shen.<st_input>
                                                                                  (shen.<st_input>
                                                                                   Parse_shen.<comment>)))
                                                                             (IF (NOT
                                                                                  (EQ
                                                                                   (fail)
                                                                                   Parse_shen.<st_input>))
                                                                                 (shen.pair
                                                                                  (CAR
                                                                                   Parse_shen.<st_input>)
                                                                                  (shen.hdtl
                                                                                   Parse_shen.<st_input>))
                                                                                 (fail)))
                                                                           (fail)))))
                                                                (IF (EQ
                                                                     YaccParse
                                                                     (fail))
                                                                    (LET ((YaccParse
                                                                           (LET ((Parse_shen.<atom>
                                                                                  (shen.<atom>
                                                                                   V2437)))
                                                                             (IF (NOT
                                                                                  (EQ
                                                                                   (fail)
                                                                                   Parse_shen.<atom>))
                                                                                 (LET ((Parse_shen.<st_input>
                                                                                        (shen.<st_input>
                                                                                         Parse_shen.<atom>)))
                                                                                   (IF (NOT
                                                                                        (EQ
                                                                                         (fail)
                                                                                         Parse_shen.<st_input>))
                                                                                       (shen.pair
                                                                                        (CAR
                                                                                         Parse_shen.<st_input>)
                                                                                        (CONS
                                                                                         (macroexpand
                                                                                          (shen.hdtl
                                                                                           Parse_shen.<atom>))
                                                                                         (shen.hdtl
                                                                                          Parse_shen.<st_input>)))
                                                                                       (fail)))
                                                                                 (fail)))))
                                                                      (IF (EQ
                                                                           YaccParse
                                                                           (fail))
                                                                          (LET ((YaccParse
                                                                                 (LET ((Parse_shen.<whitespaces>
                                                                                        (shen.<whitespaces>
                                                                                         V2437)))
                                                                                   (IF (NOT
                                                                                        (EQ
                                                                                         (fail)
                                                                                         Parse_shen.<whitespaces>))
                                                                                       (LET ((Parse_shen.<st_input>
                                                                                              (shen.<st_input>
                                                                                               Parse_shen.<whitespaces>)))
                                                                                         (IF (NOT
                                                                                              (EQ
                                                                                               (fail)
                                                                                               Parse_shen.<st_input>))
                                                                                             (shen.pair
                                                                                              (CAR
                                                                                               Parse_shen.<st_input>)
                                                                                              (shen.hdtl
                                                                                               Parse_shen.<st_input>))
                                                                                             (fail)))
                                                                                       (fail)))))
                                                                            (IF (EQ
                                                                                 YaccParse
                                                                                 (fail))
                                                                                (LET ((Parse_<e>
                                                                                       (<e>
                                                                                        V2437)))
                                                                                  (IF (NOT
                                                                                       (EQ
                                                                                        (fail)
                                                                                        Parse_<e>))
                                                                                      (shen.pair
                                                                                       (CAR
                                                                                        Parse_<e>)
                                                                                       NIL)
                                                                                      (fail)))
                                                                                YaccParse))
                                                                          YaccParse))
                                                                    YaccParse))
                                                              YaccParse))
                                                        YaccParse))
                                                  YaccParse))
                                            YaccParse))
                                      YaccParse))
                                YaccParse))
                          YaccParse))
                    YaccParse))
              YaccParse))
        YaccParse)))

(DEFUN shen.<lsb> (V2439)
  (IF (AND (CONSP (CAR V2439))
           (IF (NUMBERP (CAR (CAR V2439)))
               (= (CAR (CAR V2439)) 91)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2439)) (shen.hdtl V2439)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<rsb> (V2441)
  (IF (AND (CONSP (CAR V2441))
           (IF (NUMBERP (CAR (CAR V2441)))
               (= (CAR (CAR V2441)) 93)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2441)) (shen.hdtl V2441)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<lcurly> (V2443)
  (IF (AND (CONSP (CAR V2443))
           (IF (NUMBERP (CAR (CAR V2443)))
               (= (CAR (CAR V2443)) 123)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2443)) (shen.hdtl V2443)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<rcurly> (V2445)
  (IF (AND (CONSP (CAR V2445))
           (IF (NUMBERP (CAR (CAR V2445)))
               (= (CAR (CAR V2445)) 125)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2445)) (shen.hdtl V2445)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<bar> (V2447)
  (IF (AND (CONSP (CAR V2447))
           (IF (NUMBERP (CAR (CAR V2447)))
               (= (CAR (CAR V2447)) 124)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2447)) (shen.hdtl V2447)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<semicolon> (V2449)
  (IF (AND (CONSP (CAR V2449))
           (IF (NUMBERP (CAR (CAR V2449)))
               (= (CAR (CAR V2449)) 59)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2449)) (shen.hdtl V2449)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<colon> (V2451)
  (IF (AND (CONSP (CAR V2451))
           (IF (NUMBERP (CAR (CAR V2451)))
               (= (CAR (CAR V2451)) 58)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2451)) (shen.hdtl V2451)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<comma> (V2453)
  (IF (AND (CONSP (CAR V2453))
           (IF (NUMBERP (CAR (CAR V2453)))
               (= (CAR (CAR V2453)) 44)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2453)) (shen.hdtl V2453)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<equal> (V2455)
  (IF (AND (CONSP (CAR V2455))
           (IF (NUMBERP (CAR (CAR V2455)))
               (= (CAR (CAR V2455)) 61)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2455)) (shen.hdtl V2455)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<minus> (V2457)
  (IF (AND (CONSP (CAR V2457))
           (IF (NUMBERP (CAR (CAR V2457)))
               (= (CAR (CAR V2457)) 45)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2457)) (shen.hdtl V2457)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<lrb> (V2459)
  (IF (AND (CONSP (CAR V2459))
           (IF (NUMBERP (CAR (CAR V2459)))
               (= (CAR (CAR V2459)) 40)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2459)) (shen.hdtl V2459)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<rrb> (V2461)
  (IF (AND (CONSP (CAR V2461))
           (IF (NUMBERP (CAR (CAR V2461)))
               (= (CAR (CAR V2461)) 41)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2461)) (shen.hdtl V2461)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<atom> (V2463)
  (LET ((YaccParse
         (LET ((Parse_shen.<str> (shen.<str> V2463)))
           (IF (NOT (EQ (fail) Parse_shen.<str>))
               (shen.pair (CAR Parse_shen.<str>)
                          (shen.control-chars (shen.hdtl Parse_shen.<str>)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((YaccParse
               (LET ((Parse_shen.<number> (shen.<number> V2463)))
                 (IF (NOT (EQ (fail) Parse_shen.<number>))
                     (shen.pair (CAR Parse_shen.<number>)
                                (shen.hdtl Parse_shen.<number>))
                     (fail)))))
          (IF (EQ YaccParse (fail))
              (LET ((Parse_shen.<sym> (shen.<sym> V2463)))
                (IF (NOT (EQ (fail) Parse_shen.<sym>))
                    (shen.pair (CAR Parse_shen.<sym>)
                               (IF (EQUAL (shen.hdtl Parse_shen.<sym>) "<>")
                                   (CONS 'vector (CONS 0 NIL))
                                   (intern (shen.hdtl Parse_shen.<sym>))))
                    (fail)))
              YaccParse))
        YaccParse)))

(DEFUN shen.control-chars (V2465)
  (COND ((NULL V2465) "")
        ((AND (CONSP V2465)
              (AND (EQUAL "c" (CAR V2465))
                   (AND (CONSP (CDR V2465)) (EQUAL "#" (CAR (CDR V2465))))))
         (LET ((CodePoint (shen.code-point (CDR (CDR V2465)))))
           (LET ((AfterCodePoint (shen.after-codepoint (CDR (CDR V2465)))))
             (@s (n->string (shen.decimalise CodePoint))
                 (shen.control-chars AfterCodePoint)))))
        ((CONSP V2465) (@s (CAR V2465) (shen.control-chars (CDR V2465))))
        (T (shen.f_error 'shen.control-chars))))

(DEFUN shen.code-point (V2469)
  (COND ((AND (CONSP V2469) (EQUAL ";" (CAR V2469))) "")
        ((AND (CONSP V2469)
              (shen-cl.true?
               (element? (CAR V2469)
                         (CONS "0"
                               (CONS "1"
                                     (CONS "2"
                                           (CONS "3"
                                                 (CONS "4"
                                                       (CONS "5"
                                                             (CONS "6"
                                                                   (CONS "7"
                                                                         (CONS
                                                                          "8"
                                                                          (CONS
                                                                           "9"
                                                                           (CONS
                                                                            "0"
                                                                            NIL))))))))))))))
         (CONS (CAR V2469) (shen.code-point (CDR V2469))))
        (T
         (simple-error
          (cn "code point parse error "
              (shen.app V2469 "
"
               'shen.a))))))

(DEFUN shen.after-codepoint (V2475)
  (COND ((NULL V2475) NIL)
        ((AND (CONSP V2475) (EQUAL ";" (CAR V2475))) (CDR V2475))
        ((CONSP V2475) (shen.after-codepoint (CDR V2475)))
        (T (shen.f_error 'shen.after-codepoint))))

(DEFUN shen.decimalise (V2477)
  (shen.pre (REVERSE (shen.digits->integers V2477)) 0))

(DEFUN shen.digits->integers (V2483)
  (COND
   ((AND (CONSP V2483) (EQUAL "0" (CAR V2483)))
    (CONS 0 (shen.digits->integers (CDR V2483))))
   ((AND (CONSP V2483) (EQUAL "1" (CAR V2483)))
    (CONS 1 (shen.digits->integers (CDR V2483))))
   ((AND (CONSP V2483) (EQUAL "2" (CAR V2483)))
    (CONS 2 (shen.digits->integers (CDR V2483))))
   ((AND (CONSP V2483) (EQUAL "3" (CAR V2483)))
    (CONS 3 (shen.digits->integers (CDR V2483))))
   ((AND (CONSP V2483) (EQUAL "4" (CAR V2483)))
    (CONS 4 (shen.digits->integers (CDR V2483))))
   ((AND (CONSP V2483) (EQUAL "5" (CAR V2483)))
    (CONS 5 (shen.digits->integers (CDR V2483))))
   ((AND (CONSP V2483) (EQUAL "6" (CAR V2483)))
    (CONS 6 (shen.digits->integers (CDR V2483))))
   ((AND (CONSP V2483) (EQUAL "7" (CAR V2483)))
    (CONS 7 (shen.digits->integers (CDR V2483))))
   ((AND (CONSP V2483) (EQUAL "8" (CAR V2483)))
    (CONS 8 (shen.digits->integers (CDR V2483))))
   ((AND (CONSP V2483) (EQUAL "9" (CAR V2483)))
    (CONS 9 (shen.digits->integers (CDR V2483))))
   (T NIL)))

(DEFUN shen.<sym> (V2485)
  (LET ((Parse_shen.<alpha> (shen.<alpha> V2485)))
    (IF (NOT (EQ (fail) Parse_shen.<alpha>))
        (LET ((Parse_shen.<alphanums> (shen.<alphanums> Parse_shen.<alpha>)))
          (IF (NOT (EQ (fail) Parse_shen.<alphanums>))
              (shen.pair (CAR Parse_shen.<alphanums>)
                         (@s (shen.hdtl Parse_shen.<alpha>)
                             (shen.hdtl Parse_shen.<alphanums>)))
              (fail)))
        (fail))))

(DEFUN shen.<alphanums> (V2487)
  (LET ((YaccParse
         (LET ((Parse_shen.<alphanum> (shen.<alphanum> V2487)))
           (IF (NOT (EQ (fail) Parse_shen.<alphanum>))
               (LET ((Parse_shen.<alphanums>
                      (shen.<alphanums> Parse_shen.<alphanum>)))
                 (IF (NOT (EQ (fail) Parse_shen.<alphanums>))
                     (shen.pair (CAR Parse_shen.<alphanums>)
                                (@s (shen.hdtl Parse_shen.<alphanum>)
                                    (shen.hdtl Parse_shen.<alphanums>)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_<e> (<e> V2487)))
          (IF (NOT (EQ (fail) Parse_<e>))
              (shen.pair (CAR Parse_<e>) "")
              (fail)))
        YaccParse)))

(DEFUN shen.<alphanum> (V2489)
  (LET ((YaccParse
         (LET ((Parse_shen.<alpha> (shen.<alpha> V2489)))
           (IF (NOT (EQ (fail) Parse_shen.<alpha>))
               (shen.pair (CAR Parse_shen.<alpha>)
                          (shen.hdtl Parse_shen.<alpha>))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_shen.<num> (shen.<num> V2489)))
          (IF (NOT (EQ (fail) Parse_shen.<num>))
              (shen.pair (CAR Parse_shen.<num>) (shen.hdtl Parse_shen.<num>))
              (fail)))
        YaccParse)))

(DEFUN shen.<num> (V2491)
  (IF (CONSP (CAR V2491))
      (LET ((Parse_Char (CAR (CAR V2491))))
        (IF (shen-cl.true? (shen.numbyte? Parse_Char))
            (shen.pair (CAR (shen.pair (CDR (CAR V2491)) (shen.hdtl V2491)))
                       (n->string Parse_Char))
            (fail)))
      (fail)))

(DEFUN shen.numbyte? (V2497)
  (COND
   ((IF (NUMBERP V2497)
        (= V2497 48))
    'true)
   ((IF (NUMBERP V2497)
        (= V2497 49))
    'true)
   ((IF (NUMBERP V2497)
        (= V2497 50))
    'true)
   ((IF (NUMBERP V2497)
        (= V2497 51))
    'true)
   ((IF (NUMBERP V2497)
        (= V2497 52))
    'true)
   ((IF (NUMBERP V2497)
        (= V2497 53))
    'true)
   ((IF (NUMBERP V2497)
        (= V2497 54))
    'true)
   ((IF (NUMBERP V2497)
        (= V2497 55))
    'true)
   ((IF (NUMBERP V2497)
        (= V2497 56))
    'true)
   ((IF (NUMBERP V2497)
        (= V2497 57))
    'true)
   (T 'false)))

(DEFUN shen.<alpha> (V2499)
  (IF (CONSP (CAR V2499))
      (LET ((Parse_Char (CAR (CAR V2499))))
        (IF (shen-cl.true? (shen.symbol-code? Parse_Char))
            (shen.pair (CAR (shen.pair (CDR (CAR V2499)) (shen.hdtl V2499)))
                       (n->string Parse_Char))
            (fail)))
      (fail)))

(DEFUN shen.symbol-code? (V2501)
  (or (shen.equal? V2501 126)
      (or (and (shen.greater? V2501 94) (shen.less? V2501 123))
          (or (and (shen.greater? V2501 59) (shen.less? V2501 91))
              (or
               (and (shen.greater? V2501 41)
                    (and (shen.less? V2501 58) (not (shen.equal? V2501 44))))
               (or (and (shen.greater? V2501 34) (shen.less? V2501 40))
                   (shen.equal? V2501 33)))))))

(DEFUN shen.<str> (V2503)
  (LET ((Parse_shen.<dbq> (shen.<dbq> V2503)))
    (IF (NOT (EQ (fail) Parse_shen.<dbq>))
        (LET ((Parse_shen.<strcontents> (shen.<strcontents> Parse_shen.<dbq>)))
          (IF (NOT (EQ (fail) Parse_shen.<strcontents>))
              (LET ((Parse_shen.<dbq> (shen.<dbq> Parse_shen.<strcontents>)))
                (IF (NOT (EQ (fail) Parse_shen.<dbq>))
                    (shen.pair (CAR Parse_shen.<dbq>)
                               (shen.hdtl Parse_shen.<strcontents>))
                    (fail)))
              (fail)))
        (fail))))

(DEFUN shen.<dbq> (V2505)
  (IF (CONSP (CAR V2505))
      (LET ((Parse_Char (CAR (CAR V2505))))
        (IF (IF (NUMBERP Parse_Char)
                (= Parse_Char 34))
            (shen.pair (CAR (shen.pair (CDR (CAR V2505)) (shen.hdtl V2505)))
                       Parse_Char)
            (fail)))
      (fail)))

(DEFUN shen.<strcontents> (V2507)
  (LET ((YaccParse
         (LET ((Parse_shen.<strc> (shen.<strc> V2507)))
           (IF (NOT (EQ (fail) Parse_shen.<strc>))
               (LET ((Parse_shen.<strcontents>
                      (shen.<strcontents> Parse_shen.<strc>)))
                 (IF (NOT (EQ (fail) Parse_shen.<strcontents>))
                     (shen.pair (CAR Parse_shen.<strcontents>)
                                (CONS (shen.hdtl Parse_shen.<strc>)
                                      (shen.hdtl Parse_shen.<strcontents>)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_<e> (<e> V2507)))
          (IF (NOT (EQ (fail) Parse_<e>))
              (shen.pair (CAR Parse_<e>) NIL)
              (fail)))
        YaccParse)))

(DEFUN shen.<byte> (V2509)
  (IF (CONSP (CAR V2509))
      (LET ((Parse_Char (CAR (CAR V2509))))
        (shen.pair (CAR (shen.pair (CDR (CAR V2509)) (shen.hdtl V2509)))
                   (n->string Parse_Char)))
      (fail)))

(DEFUN shen.<strc> (V2511)
  (IF (CONSP (CAR V2511))
      (LET ((Parse_Char (CAR (CAR V2511))))
        (IF (NOT
             (IF (NUMBERP Parse_Char)
                 (= Parse_Char 34)))
            (shen.pair (CAR (shen.pair (CDR (CAR V2511)) (shen.hdtl V2511)))
                       (n->string Parse_Char))
            (fail)))
      (fail)))

(DEFUN shen.<number> (V2513)
  (LET ((YaccParse
         (LET ((Parse_shen.<minus> (shen.<minus> V2513)))
           (IF (NOT (EQ (fail) Parse_shen.<minus>))
               (LET ((Parse_shen.<number> (shen.<number> Parse_shen.<minus>)))
                 (IF (NOT (EQ (fail) Parse_shen.<number>))
                     (shen.pair (CAR Parse_shen.<number>)
                                (shen.subtract 0
                                               (shen.hdtl
                                                Parse_shen.<number>)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((YaccParse
               (LET ((Parse_shen.<plus> (shen.<plus> V2513)))
                 (IF (NOT (EQ (fail) Parse_shen.<plus>))
                     (LET ((Parse_shen.<number>
                            (shen.<number> Parse_shen.<plus>)))
                       (IF (NOT (EQ (fail) Parse_shen.<number>))
                           (shen.pair (CAR Parse_shen.<number>)
                                      (shen.hdtl Parse_shen.<number>))
                           (fail)))
                     (fail)))))
          (IF (EQ YaccParse (fail))
              (LET ((YaccParse
                     (LET ((Parse_shen.<predigits> (shen.<predigits> V2513)))
                       (IF (NOT (EQ (fail) Parse_shen.<predigits>))
                           (LET ((Parse_shen.<stop>
                                  (shen.<stop> Parse_shen.<predigits>)))
                             (IF (NOT (EQ (fail) Parse_shen.<stop>))
                                 (LET ((Parse_shen.<postdigits>
                                        (shen.<postdigits> Parse_shen.<stop>)))
                                   (IF (NOT
                                        (EQ (fail) Parse_shen.<postdigits>))
                                       (LET ((Parse_shen.<E>
                                              (shen.<E>
                                               Parse_shen.<postdigits>)))
                                         (IF (NOT (EQ (fail) Parse_shen.<E>))
                                             (LET ((Parse_shen.<log10>
                                                    (shen.<log10>
                                                     Parse_shen.<E>)))
                                               (IF (NOT
                                                    (EQ (fail)
                                                        Parse_shen.<log10>))
                                                   (shen.pair
                                                    (CAR Parse_shen.<log10>)
                                                    (shen.multiply
                                                     (shen.expt 10
                                                      (shen.hdtl
                                                       Parse_shen.<log10>))
                                                     (shen.add
                                                      (shen.pre
                                                       (REVERSE
                                                        (shen.hdtl
                                                         Parse_shen.<predigits>))
                                                       0)
                                                      (shen.post
                                                       (shen.hdtl
                                                        Parse_shen.<postdigits>)
                                                       1))))
                                                   (fail)))
                                             (fail)))
                                       (fail)))
                                 (fail)))
                           (fail)))))
                (IF (EQ YaccParse (fail))
                    (LET ((YaccParse
                           (LET ((Parse_shen.<digits> (shen.<digits> V2513)))
                             (IF (NOT (EQ (fail) Parse_shen.<digits>))
                                 (LET ((Parse_shen.<E>
                                        (shen.<E> Parse_shen.<digits>)))
                                   (IF (NOT (EQ (fail) Parse_shen.<E>))
                                       (LET ((Parse_shen.<log10>
                                              (shen.<log10> Parse_shen.<E>)))
                                         (IF (NOT
                                              (EQ (fail) Parse_shen.<log10>))
                                             (shen.pair
                                              (CAR Parse_shen.<log10>)
                                              (shen.multiply
                                               (shen.expt 10
                                                (shen.hdtl Parse_shen.<log10>))
                                               (shen.pre
                                                (REVERSE
                                                 (shen.hdtl
                                                  Parse_shen.<digits>))
                                                0)))
                                             (fail)))
                                       (fail)))
                                 (fail)))))
                      (IF (EQ YaccParse (fail))
                          (LET ((YaccParse
                                 (LET ((Parse_shen.<predigits>
                                        (shen.<predigits> V2513)))
                                   (IF (NOT (EQ (fail) Parse_shen.<predigits>))
                                       (LET ((Parse_shen.<stop>
                                              (shen.<stop>
                                               Parse_shen.<predigits>)))
                                         (IF (NOT
                                              (EQ (fail) Parse_shen.<stop>))
                                             (LET ((Parse_shen.<postdigits>
                                                    (shen.<postdigits>
                                                     Parse_shen.<stop>)))
                                               (IF (NOT
                                                    (EQ (fail)
                                                        Parse_shen.<postdigits>))
                                                   (shen.pair
                                                    (CAR
                                                     Parse_shen.<postdigits>)
                                                    (shen.add
                                                     (shen.pre
                                                      (REVERSE
                                                       (shen.hdtl
                                                        Parse_shen.<predigits>))
                                                      0)
                                                     (shen.post
                                                      (shen.hdtl
                                                       Parse_shen.<postdigits>)
                                                      1)))
                                                   (fail)))
                                             (fail)))
                                       (fail)))))
                            (IF (EQ YaccParse (fail))
                                (LET ((Parse_shen.<digits>
                                       (shen.<digits> V2513)))
                                  (IF (NOT (EQ (fail) Parse_shen.<digits>))
                                      (shen.pair (CAR Parse_shen.<digits>)
                                                 (shen.pre
                                                  (REVERSE
                                                   (shen.hdtl
                                                    Parse_shen.<digits>))
                                                  0))
                                      (fail)))
                                YaccParse))
                          YaccParse))
                    YaccParse))
              YaccParse))
        YaccParse)))

(DEFUN shen.<E> (V2515)
  (IF (AND (CONSP (CAR V2515))
           (IF (NUMBERP (CAR (CAR V2515)))
               (= (CAR (CAR V2515)) 101)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2515)) (shen.hdtl V2515)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<log10> (V2517)
  (LET ((YaccParse
         (LET ((Parse_shen.<minus> (shen.<minus> V2517)))
           (IF (NOT (EQ (fail) Parse_shen.<minus>))
               (LET ((Parse_shen.<digits> (shen.<digits> Parse_shen.<minus>)))
                 (IF (NOT (EQ (fail) Parse_shen.<digits>))
                     (shen.pair (CAR Parse_shen.<digits>)
                                (shen.subtract 0
                                               (shen.pre
                                                (REVERSE
                                                 (shen.hdtl
                                                  Parse_shen.<digits>))
                                                0)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_shen.<digits> (shen.<digits> V2517)))
          (IF (NOT (EQ (fail) Parse_shen.<digits>))
              (shen.pair (CAR Parse_shen.<digits>)
                         (shen.pre (REVERSE (shen.hdtl Parse_shen.<digits>))
                          0))
              (fail)))
        YaccParse)))

(DEFUN shen.<plus> (V2519)
  (IF (CONSP (CAR V2519))
      (LET ((Parse_Char (CAR (CAR V2519))))
        (IF (IF (NUMBERP Parse_Char)
                (= Parse_Char 43))
            (shen.pair (CAR (shen.pair (CDR (CAR V2519)) (shen.hdtl V2519)))
                       Parse_Char)
            (fail)))
      (fail)))

(DEFUN shen.<stop> (V2521)
  (IF (CONSP (CAR V2521))
      (LET ((Parse_Char (CAR (CAR V2521))))
        (IF (IF (NUMBERP Parse_Char)
                (= Parse_Char 46))
            (shen.pair (CAR (shen.pair (CDR (CAR V2521)) (shen.hdtl V2521)))
                       Parse_Char)
            (fail)))
      (fail)))

(DEFUN shen.<predigits> (V2523)
  (LET ((YaccParse
         (LET ((Parse_shen.<digits> (shen.<digits> V2523)))
           (IF (NOT (EQ (fail) Parse_shen.<digits>))
               (shen.pair (CAR Parse_shen.<digits>)
                          (shen.hdtl Parse_shen.<digits>))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_<e> (<e> V2523)))
          (IF (NOT (EQ (fail) Parse_<e>))
              (shen.pair (CAR Parse_<e>) NIL)
              (fail)))
        YaccParse)))

(DEFUN shen.<postdigits> (V2525)
  (LET ((Parse_shen.<digits> (shen.<digits> V2525)))
    (IF (NOT (EQ (fail) Parse_shen.<digits>))
        (shen.pair (CAR Parse_shen.<digits>) (shen.hdtl Parse_shen.<digits>))
        (fail))))

(DEFUN shen.<digits> (V2527)
  (LET ((YaccParse
         (LET ((Parse_shen.<digit> (shen.<digit> V2527)))
           (IF (NOT (EQ (fail) Parse_shen.<digit>))
               (LET ((Parse_shen.<digits> (shen.<digits> Parse_shen.<digit>)))
                 (IF (NOT (EQ (fail) Parse_shen.<digits>))
                     (shen.pair (CAR Parse_shen.<digits>)
                                (CONS (shen.hdtl Parse_shen.<digit>)
                                      (shen.hdtl Parse_shen.<digits>)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_shen.<digit> (shen.<digit> V2527)))
          (IF (NOT (EQ (fail) Parse_shen.<digit>))
              (shen.pair (CAR Parse_shen.<digit>)
                         (CONS (shen.hdtl Parse_shen.<digit>) NIL))
              (fail)))
        YaccParse)))

(DEFUN shen.<digit> (V2529)
  (IF (CONSP (CAR V2529))
      (LET ((Parse_X (CAR (CAR V2529))))
        (IF (shen-cl.true? (shen.numbyte? Parse_X))
            (shen.pair (CAR (shen.pair (CDR (CAR V2529)) (shen.hdtl V2529)))
                       (shen.byte->digit Parse_X))
            (fail)))
      (fail)))

(DEFUN shen.byte->digit (V2531)
  (COND
   ((IF (NUMBERP V2531)
        (= V2531 48))
    0)
   ((IF (NUMBERP V2531)
        (= V2531 49))
    1)
   ((IF (NUMBERP V2531)
        (= V2531 50))
    2)
   ((IF (NUMBERP V2531)
        (= V2531 51))
    3)
   ((IF (NUMBERP V2531)
        (= V2531 52))
    4)
   ((IF (NUMBERP V2531)
        (= V2531 53))
    5)
   ((IF (NUMBERP V2531)
        (= V2531 54))
    6)
   ((IF (NUMBERP V2531)
        (= V2531 55))
    7)
   ((IF (NUMBERP V2531)
        (= V2531 56))
    8)
   ((IF (NUMBERP V2531)
        (= V2531 57))
    9)
   (T (shen.f_error 'shen.byte->digit))))

(DEFUN shen.pre (V2536 V2537)
  (COND ((NULL V2536) 0)
        ((CONSP V2536)
         (shen.add (shen.multiply (shen.expt 10 V2537) (CAR V2536))
                   (shen.pre (CDR V2536) (shen.add V2537 1))))
        (T (shen.f_error 'shen.pre))))

(DEFUN shen.post (V2542 V2543)
  (COND ((NULL V2542) 0)
        ((CONSP V2542)
         (shen.add
          (shen.multiply (shen.expt 10 (shen.subtract 0 V2543)) (CAR V2542))
          (shen.post (CDR V2542) (shen.add V2543 1))))
        (T (shen.f_error 'shen.post))))

(DEFUN shen.expt (V2548 V2549)
  (COND
   ((IF (NUMBERP V2549)
        (= V2549 0))
    1)
   ((> V2549 0)
    (shen.multiply V2548 (shen.expt V2548 (shen.subtract V2549 1))))
   (T
    (shen.multiply 1
                   (shen.divide (shen.expt V2548 (shen.add V2549 1)) V2548)))))

(DEFUN shen.<st_input1> (V2551)
  (LET ((Parse_shen.<st_input> (shen.<st_input> V2551)))
    (IF (NOT (EQ (fail) Parse_shen.<st_input>))
        (shen.pair (CAR Parse_shen.<st_input>)
                   (shen.hdtl Parse_shen.<st_input>))
        (fail))))

(DEFUN shen.<st_input2> (V2553)
  (LET ((Parse_shen.<st_input> (shen.<st_input> V2553)))
    (IF (NOT (EQ (fail) Parse_shen.<st_input>))
        (shen.pair (CAR Parse_shen.<st_input>)
                   (shen.hdtl Parse_shen.<st_input>))
        (fail))))

(DEFUN shen.<comment> (V2555)
  (LET ((YaccParse
         (LET ((Parse_shen.<singleline> (shen.<singleline> V2555)))
           (IF (NOT (EQ (fail) Parse_shen.<singleline>))
               (shen.pair (CAR Parse_shen.<singleline>) 'shen.skip)
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_shen.<multiline> (shen.<multiline> V2555)))
          (IF (NOT (EQ (fail) Parse_shen.<multiline>))
              (shen.pair (CAR Parse_shen.<multiline>) 'shen.skip)
              (fail)))
        YaccParse)))

(DEFUN shen.<singleline> (V2557)
  (LET ((Parse_shen.<backslash> (shen.<backslash> V2557)))
    (IF (NOT (EQ (fail) Parse_shen.<backslash>))
        (LET ((Parse_shen.<backslash>
               (shen.<backslash> Parse_shen.<backslash>)))
          (IF (NOT (EQ (fail) Parse_shen.<backslash>))
              (LET ((Parse_shen.<anysingle>
                     (shen.<anysingle> Parse_shen.<backslash>)))
                (IF (NOT (EQ (fail) Parse_shen.<anysingle>))
                    (LET ((Parse_shen.<return>
                           (shen.<return> Parse_shen.<anysingle>)))
                      (IF (NOT (EQ (fail) Parse_shen.<return>))
                          (shen.pair (CAR Parse_shen.<return>) 'shen.skip)
                          (fail)))
                    (fail)))
              (fail)))
        (fail))))

(DEFUN shen.<backslash> (V2559)
  (IF (AND (CONSP (CAR V2559))
           (IF (NUMBERP (CAR (CAR V2559)))
               (= (CAR (CAR V2559)) 92)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2559)) (shen.hdtl V2559)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<anysingle> (V2561)
  (LET ((YaccParse
         (LET ((Parse_shen.<non-return> (shen.<non-return> V2561)))
           (IF (NOT (EQ (fail) Parse_shen.<non-return>))
               (LET ((Parse_shen.<anysingle>
                      (shen.<anysingle> Parse_shen.<non-return>)))
                 (IF (NOT (EQ (fail) Parse_shen.<anysingle>))
                     (shen.pair (CAR Parse_shen.<anysingle>) 'shen.skip)
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_<e> (<e> V2561)))
          (IF (NOT (EQ (fail) Parse_<e>))
              (shen.pair (CAR Parse_<e>) 'shen.skip)
              (fail)))
        YaccParse)))

(DEFUN shen.<non-return> (V2563)
  (IF (CONSP (CAR V2563))
      (LET ((Parse_X (CAR (CAR V2563))))
        (IF (NOT (shen-cl.true? (element? Parse_X (CONS 10 (CONS 13 NIL)))))
            (shen.pair (CAR (shen.pair (CDR (CAR V2563)) (shen.hdtl V2563)))
                       'shen.skip)
            (fail)))
      (fail)))

(DEFUN shen.<return> (V2565)
  (IF (CONSP (CAR V2565))
      (LET ((Parse_X (CAR (CAR V2565))))
        (IF (shen-cl.true? (element? Parse_X (CONS 10 (CONS 13 NIL))))
            (shen.pair (CAR (shen.pair (CDR (CAR V2565)) (shen.hdtl V2565)))
                       'shen.skip)
            (fail)))
      (fail)))

(DEFUN shen.<multiline> (V2567)
  (LET ((Parse_shen.<backslash> (shen.<backslash> V2567)))
    (IF (NOT (EQ (fail) Parse_shen.<backslash>))
        (LET ((Parse_shen.<times> (shen.<times> Parse_shen.<backslash>)))
          (IF (NOT (EQ (fail) Parse_shen.<times>))
              (LET ((Parse_shen.<anymulti>
                     (shen.<anymulti> Parse_shen.<times>)))
                (IF (NOT (EQ (fail) Parse_shen.<anymulti>))
                    (shen.pair (CAR Parse_shen.<anymulti>) 'shen.skip)
                    (fail)))
              (fail)))
        (fail))))

(DEFUN shen.<times> (V2569)
  (IF (AND (CONSP (CAR V2569))
           (IF (NUMBERP (CAR (CAR V2569)))
               (= (CAR (CAR V2569)) 42)))
      (shen.pair (CAR (shen.pair (CDR (CAR V2569)) (shen.hdtl V2569)))
                 'shen.skip)
      (fail)))

(DEFUN shen.<anymulti> (V2571)
  (LET ((YaccParse
         (LET ((Parse_shen.<comment> (shen.<comment> V2571)))
           (IF (NOT (EQ (fail) Parse_shen.<comment>))
               (LET ((Parse_shen.<anymulti>
                      (shen.<anymulti> Parse_shen.<comment>)))
                 (IF (NOT (EQ (fail) Parse_shen.<anymulti>))
                     (shen.pair (CAR Parse_shen.<anymulti>) 'shen.skip)
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((YaccParse
               (LET ((Parse_shen.<times> (shen.<times> V2571)))
                 (IF (NOT (EQ (fail) Parse_shen.<times>))
                     (LET ((Parse_shen.<backslash>
                            (shen.<backslash> Parse_shen.<times>)))
                       (IF (NOT (EQ (fail) Parse_shen.<backslash>))
                           (shen.pair (CAR Parse_shen.<backslash>) 'shen.skip)
                           (fail)))
                     (fail)))))
          (IF (EQ YaccParse (fail))
              (IF (CONSP (CAR V2571))
                  (LET ((Parse_X (CAR (CAR V2571))))
                    (LET ((Parse_shen.<anymulti>
                           (shen.<anymulti>
                            (shen.pair (CDR (CAR V2571)) (shen.hdtl V2571)))))
                      (IF (NOT (EQ (fail) Parse_shen.<anymulti>))
                          (shen.pair (CAR Parse_shen.<anymulti>) 'shen.skip)
                          (fail))))
                  (fail))
              YaccParse))
        YaccParse)))

(DEFUN shen.<whitespaces> (V2573)
  (LET ((YaccParse
         (LET ((Parse_shen.<whitespace> (shen.<whitespace> V2573)))
           (IF (NOT (EQ (fail) Parse_shen.<whitespace>))
               (LET ((Parse_shen.<whitespaces>
                      (shen.<whitespaces> Parse_shen.<whitespace>)))
                 (IF (NOT (EQ (fail) Parse_shen.<whitespaces>))
                     (shen.pair (CAR Parse_shen.<whitespaces>) 'shen.skip)
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_shen.<whitespace> (shen.<whitespace> V2573)))
          (IF (NOT (EQ (fail) Parse_shen.<whitespace>))
              (shen.pair (CAR Parse_shen.<whitespace>) 'shen.skip)
              (fail)))
        YaccParse)))

(DEFUN shen.<whitespace> (V2575)
  (IF (CONSP (CAR V2575))
      (LET ((Parse_X (CAR (CAR V2575))))
        (IF (shen-cl.true?
             (LET ((Parse_Case Parse_X))
               (or (shen.equal? Parse_Case 32)
                   (or (shen.equal? Parse_Case 13)
                       (or (shen.equal? Parse_Case 10)
                           (shen.equal? Parse_Case 9))))))
            (shen.pair (CAR (shen.pair (CDR (CAR V2575)) (shen.hdtl V2575)))
                       'shen.skip)
            (fail)))
      (fail)))

(DEFUN shen.cons_form (V2577)
  (COND ((NULL V2577) NIL)
        ((AND (CONSP V2577)
              (AND (CONSP (CDR V2577))
                   (AND (CONSP (CDR (CDR V2577)))
                        (AND (NULL (CDR (CDR (CDR V2577))))
                             (EQ (CAR (CDR V2577)) 'bar!)))))
         (CONS 'cons (CONS (CAR V2577) (CDR (CDR V2577)))))
        ((CONSP V2577)
         (CONS 'cons
               (CONS (CAR V2577) (CONS (shen.cons_form (CDR V2577)) NIL))))
        (T (shen.f_error 'shen.cons_form))))

(DEFUN shen.package-macro (V2582 V2583)
  (COND
   ((AND (CONSP V2582)
         (AND (EQ '$ (CAR V2582))
              (AND (CONSP (CDR V2582)) (NULL (CDR (CDR V2582))))))
    (APPEND (explode (CAR (CDR V2582))) V2583))
   ((AND (CONSP V2582)
         (AND (EQ 'package (CAR V2582))
              (AND (CONSP (CDR V2582))
                   (AND (EQ 'null (CAR (CDR V2582)))
                        (CONSP (CDR (CDR V2582)))))))
    (APPEND (CDR (CDR (CDR V2582))) V2583))
   ((AND (CONSP V2582)
         (AND (EQ 'package (CAR V2582))
              (AND (CONSP (CDR V2582)) (CONSP (CDR (CDR V2582))))))
    (LET ((ListofExceptions (shen.eval-without-macros (CAR (CDR (CDR V2582))))))
      (LET ((External
             (shen.record-exceptions ListofExceptions (CAR (CDR V2582)))))
        (LET ((PackageNameDot (intern (cn (str (CAR (CDR V2582))) "."))))
          (LET ((ExpPackageNameDot (explode PackageNameDot)))
            (LET ((Packaged
                   (shen.packageh PackageNameDot ListofExceptions
                    (CDR (CDR (CDR V2582))) ExpPackageNameDot)))
              (LET ((Internal
                     (shen.record-internal (CAR (CDR V2582))
                      (shen.internal-symbols ExpPackageNameDot Packaged))))
                (APPEND Packaged V2583))))))))
   (T (CONS V2582 V2583))))

(DEFUN shen.record-exceptions (V2586 V2587)
  (LET ((CurrExceptions
         (get/or V2587 'shen.external-symbols (freeze NIL) *property-vector*)))
    (LET ((AllExceptions (union V2586 CurrExceptions)))
      (put V2587 'shen.external-symbols AllExceptions *property-vector*))))

(DEFUN shen.record-internal (V2590 V2591)
  (put V2590 'shen.internal-symbols
       (union V2591
              (get/or V2590 'shen.internal-symbols (freeze NIL)
                      *property-vector*))
       *property-vector*))

(DEFUN shen.internal-symbols (V2602 V2603)
  (COND
   ((AND (shen-cl.true? (symbol? V2603))
         (shen-cl.true? (shen.prefix? V2602 (explode V2603))))
    (CONS V2603 NIL))
   ((CONSP V2603)
    (union (shen.internal-symbols V2602 (CAR V2603))
           (shen.internal-symbols V2602 (CDR V2603))))
   (T NIL)))

(DEFUN shen.packageh (V2620 V2621 V2622 V2623)
  (COND
   ((CONSP V2622)
    (CONS (shen.packageh V2620 V2621 (CAR V2622) V2623)
          (shen.packageh V2620 V2621 (CDR V2622) V2623)))
   ((OR (shen-cl.true? (shen.sysfunc? V2622))
        (OR (shen-cl.true? (variable? V2622))
            (OR (shen-cl.true? (element? V2622 V2621))
                (OR (shen-cl.true? (shen.doubleunderline? V2622))
                    (shen-cl.true? (shen.singleunderline? V2622))))))
    V2622)
   ((AND (shen-cl.true? (symbol? V2622))
         (shen-cl.true?
          (LET ((ExplodeX (explode V2622)))
            (and
             (not
              (shen.prefix?
               (CONS "s" (CONS "h" (CONS "e" (CONS "n" (CONS "." NIL)))))
               ExplodeX))
             (not (shen.prefix? V2623 ExplodeX))))))
    (concat V2620 V2622))
   (T V2622)))

