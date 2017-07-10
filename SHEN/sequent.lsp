
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

(DEFUN shen.datatype-error (V2629)
  (COND
   ((AND (CONSP V2629) (AND (CONSP (CDR V2629)) (NULL (CDR (CDR V2629)))))
    (simple-error
     (cn "datatype syntax error here:

 "
         (shen.app (shen.next-50 50 (CAR V2629)) "
"
          'shen.a))))
   (T (shen.f_error 'shen.datatype-error))))

(DEFUN shen.<datatype-rules> (V2631)
  (LET ((YaccParse
         (LET ((Parse_shen.<datatype-rule> (shen.<datatype-rule> V2631)))
           (IF (NOT (EQ (fail) Parse_shen.<datatype-rule>))
               (LET ((Parse_shen.<datatype-rules>
                      (shen.<datatype-rules> Parse_shen.<datatype-rule>)))
                 (IF (NOT (EQ (fail) Parse_shen.<datatype-rules>))
                     (shen.pair (CAR Parse_shen.<datatype-rules>)
                      (CONS (shen.hdtl Parse_shen.<datatype-rule>)
                            (shen.hdtl Parse_shen.<datatype-rules>)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_<e> (<e> V2631)))
          (IF (NOT (EQ (fail) Parse_<e>))
              (shen.pair (CAR Parse_<e>) NIL)
              (fail)))
        YaccParse)))

(DEFUN shen.<datatype-rule> (V2633)
  (LET ((YaccParse
         (LET ((Parse_shen.<side-conditions> (shen.<side-conditions> V2633)))
           (IF (NOT (EQ (fail) Parse_shen.<side-conditions>))
               (LET ((Parse_shen.<premises>
                      (shen.<premises> Parse_shen.<side-conditions>)))
                 (IF (NOT (EQ (fail) Parse_shen.<premises>))
                     (LET ((Parse_shen.<singleunderline>
                            (shen.<singleunderline> Parse_shen.<premises>)))
                       (IF (NOT (EQ (fail) Parse_shen.<singleunderline>))
                           (LET ((Parse_shen.<conclusion>
                                  (shen.<conclusion>
                                   Parse_shen.<singleunderline>)))
                             (IF (NOT (EQ (fail) Parse_shen.<conclusion>))
                                 (shen.pair (CAR Parse_shen.<conclusion>)
                                  (shen.sequent 'shen.single
                                   (CONS
                                    (shen.hdtl Parse_shen.<side-conditions>)
                                    (CONS (shen.hdtl Parse_shen.<premises>)
                                          (CONS
                                           (shen.hdtl Parse_shen.<conclusion>)
                                           NIL)))))
                                 (fail)))
                           (fail)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_shen.<side-conditions> (shen.<side-conditions> V2633)))
          (IF (NOT (EQ (fail) Parse_shen.<side-conditions>))
              (LET ((Parse_shen.<premises>
                     (shen.<premises> Parse_shen.<side-conditions>)))
                (IF (NOT (EQ (fail) Parse_shen.<premises>))
                    (LET ((Parse_shen.<doubleunderline>
                           (shen.<doubleunderline> Parse_shen.<premises>)))
                      (IF (NOT (EQ (fail) Parse_shen.<doubleunderline>))
                          (LET ((Parse_shen.<conclusion>
                                 (shen.<conclusion>
                                  Parse_shen.<doubleunderline>)))
                            (IF (NOT (EQ (fail) Parse_shen.<conclusion>))
                                (shen.pair (CAR Parse_shen.<conclusion>)
                                 (shen.sequent 'shen.double
                                  (CONS
                                   (shen.hdtl Parse_shen.<side-conditions>)
                                   (CONS (shen.hdtl Parse_shen.<premises>)
                                         (CONS
                                          (shen.hdtl Parse_shen.<conclusion>)
                                          NIL)))))
                                (fail)))
                          (fail)))
                    (fail)))
              (fail)))
        YaccParse)))

(DEFUN shen.<side-conditions> (V2635)
  (LET ((YaccParse
         (LET ((Parse_shen.<side-condition> (shen.<side-condition> V2635)))
           (IF (NOT (EQ (fail) Parse_shen.<side-condition>))
               (LET ((Parse_shen.<side-conditions>
                      (shen.<side-conditions> Parse_shen.<side-condition>)))
                 (IF (NOT (EQ (fail) Parse_shen.<side-conditions>))
                     (shen.pair (CAR Parse_shen.<side-conditions>)
                      (CONS (shen.hdtl Parse_shen.<side-condition>)
                            (shen.hdtl Parse_shen.<side-conditions>)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_<e> (<e> V2635)))
          (IF (NOT (EQ (fail) Parse_<e>))
              (shen.pair (CAR Parse_<e>) NIL)
              (fail)))
        YaccParse)))

(DEFUN shen.<side-condition> (V2637)
  (LET ((YaccParse
         (IF (AND (CONSP (CAR V2637)) (EQ 'if (CAR (CAR V2637))))
             (LET ((Parse_shen.<expr>
                    (shen.<expr>
                     (shen.pair (CDR (CAR V2637)) (shen.hdtl V2637)))))
               (IF (NOT (EQ (fail) Parse_shen.<expr>))
                   (shen.pair (CAR Parse_shen.<expr>)
                    (CONS 'if (CONS (shen.hdtl Parse_shen.<expr>) NIL)))
                   (fail)))
             (fail))))
    (IF (EQ YaccParse (fail))
        (IF (AND (CONSP (CAR V2637)) (EQ 'let (CAR (CAR V2637))))
            (LET ((Parse_shen.<variable?>
                   (shen.<variable?>
                    (shen.pair (CDR (CAR V2637)) (shen.hdtl V2637)))))
              (IF (NOT (EQ (fail) Parse_shen.<variable?>))
                  (LET ((Parse_shen.<expr>
                         (shen.<expr> Parse_shen.<variable?>)))
                    (IF (NOT (EQ (fail) Parse_shen.<expr>))
                        (shen.pair (CAR Parse_shen.<expr>)
                         (CONS 'let
                               (CONS (shen.hdtl Parse_shen.<variable?>)
                                     (CONS (shen.hdtl Parse_shen.<expr>)
                                           NIL))))
                        (fail)))
                  (fail)))
            (fail))
        YaccParse)))

(DEFUN shen.<variable?> (V2639)
  (IF (CONSP (CAR V2639))
      (LET ((Parse_X (CAR (CAR V2639))))
        (IF (shen-cl.true? (variable? Parse_X))
            (shen.pair (CAR (shen.pair (CDR (CAR V2639)) (shen.hdtl V2639)))
             Parse_X)
            (fail)))
      (fail)))

(DEFUN shen.<expr> (V2641)
  (IF (CONSP (CAR V2641))
      (LET ((Parse_X (CAR (CAR V2641))))
        (IF (NOT
             (OR (shen-cl.true? (element? Parse_X (CONS '>> (CONS '|;| NIL))))
                 (OR (shen-cl.true? (shen.singleunderline? Parse_X))
                     (shen-cl.true? (shen.doubleunderline? Parse_X)))))
            (shen.pair (CAR (shen.pair (CDR (CAR V2641)) (shen.hdtl V2641)))
             (shen.remove-bar Parse_X))
            (fail)))
      (fail)))

(DEFUN shen.remove-bar (V2643)
  (COND
   ((AND (CONSP V2643)
         (AND (CONSP (CDR V2643))
              (AND (CONSP (CDR (CDR V2643)))
                   (AND (NULL (CDR (CDR (CDR V2643))))
                        (EQ (CAR (CDR V2643)) 'bar!)))))
    (CONS (CAR V2643) (CAR (CDR (CDR V2643)))))
   ((CONSP V2643)
    (CONS (shen.remove-bar (CAR V2643)) (shen.remove-bar (CDR V2643))))
   (T V2643)))

(DEFUN shen.<premises> (V2645)
  (LET ((YaccParse
         (LET ((Parse_shen.<premise> (shen.<premise> V2645)))
           (IF (NOT (EQ (fail) Parse_shen.<premise>))
               (LET ((Parse_shen.<semicolon-symbol>
                      (shen.<semicolon-symbol> Parse_shen.<premise>)))
                 (IF (NOT (EQ (fail) Parse_shen.<semicolon-symbol>))
                     (LET ((Parse_shen.<premises>
                            (shen.<premises> Parse_shen.<semicolon-symbol>)))
                       (IF (NOT (EQ (fail) Parse_shen.<premises>))
                           (shen.pair (CAR Parse_shen.<premises>)
                            (CONS (shen.hdtl Parse_shen.<premise>)
                                  (shen.hdtl Parse_shen.<premises>)))
                           (fail)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_<e> (<e> V2645)))
          (IF (NOT (EQ (fail) Parse_<e>))
              (shen.pair (CAR Parse_<e>) NIL)
              (fail)))
        YaccParse)))

(DEFUN shen.<semicolon-symbol> (V2647)
  (IF (CONSP (CAR V2647))
      (LET ((Parse_X (CAR (CAR V2647))))
        (IF (EQ Parse_X '|;|)
            (shen.pair (CAR (shen.pair (CDR (CAR V2647)) (shen.hdtl V2647)))
             'shen.skip)
            (fail)))
      (fail)))

(DEFUN shen.<premise> (V2649)
  (LET ((YaccParse
         (IF (AND (CONSP (CAR V2649)) (EQ '! (CAR (CAR V2649))))
             (shen.pair (CAR (shen.pair (CDR (CAR V2649)) (shen.hdtl V2649)))
              '!)
             (fail))))
    (IF (EQ YaccParse (fail))
        (LET ((YaccParse
               (LET ((Parse_shen.<formulae> (shen.<formulae> V2649)))
                 (IF (NOT (EQ (fail) Parse_shen.<formulae>))
                     (IF (AND (CONSP (CAR Parse_shen.<formulae>))
                              (EQ '>> (CAR (CAR Parse_shen.<formulae>))))
                         (LET ((Parse_shen.<formula>
                                (shen.<formula>
                                 (shen.pair (CDR (CAR Parse_shen.<formulae>))
                                  (shen.hdtl Parse_shen.<formulae>)))))
                           (IF (NOT (EQ (fail) Parse_shen.<formula>))
                               (shen.pair (CAR Parse_shen.<formula>)
                                (shen.sequent (shen.hdtl Parse_shen.<formulae>)
                                 (shen.hdtl Parse_shen.<formula>)))
                               (fail)))
                         (fail))
                     (fail)))))
          (IF (EQ YaccParse (fail))
              (LET ((Parse_shen.<formula> (shen.<formula> V2649)))
                (IF (NOT (EQ (fail) Parse_shen.<formula>))
                    (shen.pair (CAR Parse_shen.<formula>)
                     (shen.sequent NIL (shen.hdtl Parse_shen.<formula>)))
                    (fail)))
              YaccParse))
        YaccParse)))

(DEFUN shen.<conclusion> (V2651)
  (LET ((YaccParse
         (LET ((Parse_shen.<formulae> (shen.<formulae> V2651)))
           (IF (NOT (EQ (fail) Parse_shen.<formulae>))
               (IF (AND (CONSP (CAR Parse_shen.<formulae>))
                        (EQ '>> (CAR (CAR Parse_shen.<formulae>))))
                   (LET ((Parse_shen.<formula>
                          (shen.<formula>
                           (shen.pair (CDR (CAR Parse_shen.<formulae>))
                            (shen.hdtl Parse_shen.<formulae>)))))
                     (IF (NOT (EQ (fail) Parse_shen.<formula>))
                         (LET ((Parse_shen.<semicolon-symbol>
                                (shen.<semicolon-symbol> Parse_shen.<formula>)))
                           (IF (NOT (EQ (fail) Parse_shen.<semicolon-symbol>))
                               (shen.pair (CAR Parse_shen.<semicolon-symbol>)
                                (shen.sequent (shen.hdtl Parse_shen.<formulae>)
                                 (shen.hdtl Parse_shen.<formula>)))
                               (fail)))
                         (fail)))
                   (fail))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_shen.<formula> (shen.<formula> V2651)))
          (IF (NOT (EQ (fail) Parse_shen.<formula>))
              (LET ((Parse_shen.<semicolon-symbol>
                     (shen.<semicolon-symbol> Parse_shen.<formula>)))
                (IF (NOT (EQ (fail) Parse_shen.<semicolon-symbol>))
                    (shen.pair (CAR Parse_shen.<semicolon-symbol>)
                     (shen.sequent NIL (shen.hdtl Parse_shen.<formula>)))
                    (fail)))
              (fail)))
        YaccParse)))

(DEFUN shen.sequent (V2654 V2655) (@p V2654 V2655))

(DEFUN shen.<formulae> (V2657)
  (LET ((YaccParse
         (LET ((Parse_shen.<formula> (shen.<formula> V2657)))
           (IF (NOT (EQ (fail) Parse_shen.<formula>))
               (LET ((Parse_shen.<comma-symbol>
                      (shen.<comma-symbol> Parse_shen.<formula>)))
                 (IF (NOT (EQ (fail) Parse_shen.<comma-symbol>))
                     (LET ((Parse_shen.<formulae>
                            (shen.<formulae> Parse_shen.<comma-symbol>)))
                       (IF (NOT (EQ (fail) Parse_shen.<formulae>))
                           (shen.pair (CAR Parse_shen.<formulae>)
                            (CONS (shen.hdtl Parse_shen.<formula>)
                                  (shen.hdtl Parse_shen.<formulae>)))
                           (fail)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((YaccParse
               (LET ((Parse_shen.<formula> (shen.<formula> V2657)))
                 (IF (NOT (EQ (fail) Parse_shen.<formula>))
                     (shen.pair (CAR Parse_shen.<formula>)
                      (CONS (shen.hdtl Parse_shen.<formula>) NIL))
                     (fail)))))
          (IF (EQ YaccParse (fail))
              (LET ((Parse_<e> (<e> V2657)))
                (IF (NOT (EQ (fail) Parse_<e>))
                    (shen.pair (CAR Parse_<e>) NIL)
                    (fail)))
              YaccParse))
        YaccParse)))

(DEFUN shen.<comma-symbol> (V2659)
  (IF (CONSP (CAR V2659))
      (LET ((Parse_X (CAR (CAR V2659))))
        (IF (shen.ABSEQUAL Parse_X (intern ","))
            (shen.pair (CAR (shen.pair (CDR (CAR V2659)) (shen.hdtl V2659)))
             'shen.skip)
            (fail)))
      (fail)))

(DEFUN shen.<formula> (V2661)
  (LET ((YaccParse
         (LET ((Parse_shen.<expr> (shen.<expr> V2661)))
           (IF (NOT (EQ (fail) Parse_shen.<expr>))
               (IF (AND (CONSP (CAR Parse_shen.<expr>))
                        (EQ '|:| (CAR (CAR Parse_shen.<expr>))))
                   (LET ((Parse_shen.<type>
                          (shen.<type>
                           (shen.pair (CDR (CAR Parse_shen.<expr>))
                            (shen.hdtl Parse_shen.<expr>)))))
                     (IF (NOT (EQ (fail) Parse_shen.<type>))
                         (shen.pair (CAR Parse_shen.<type>)
                          (CONS (shen.curry (shen.hdtl Parse_shen.<expr>))
                                (CONS '|:|
                                      (CONS
                                       (shen.demodulate
                                        (shen.hdtl Parse_shen.<type>))
                                       NIL))))
                         (fail)))
                   (fail))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_shen.<expr> (shen.<expr> V2661)))
          (IF (NOT (EQ (fail) Parse_shen.<expr>))
              (shen.pair (CAR Parse_shen.<expr>) (shen.hdtl Parse_shen.<expr>))
              (fail)))
        YaccParse)))

(DEFUN shen.<type> (V2663)
  (LET ((Parse_shen.<expr> (shen.<expr> V2663)))
    (IF (NOT (EQ (fail) Parse_shen.<expr>))
        (shen.pair (CAR Parse_shen.<expr>)
         (shen.curry-type (shen.hdtl Parse_shen.<expr>)))
        (fail))))

(DEFUN shen.<doubleunderline> (V2665)
  (IF (CONSP (CAR V2665))
      (LET ((Parse_X (CAR (CAR V2665))))
        (IF (shen-cl.true? (shen.doubleunderline? Parse_X))
            (shen.pair (CAR (shen.pair (CDR (CAR V2665)) (shen.hdtl V2665)))
             Parse_X)
            (fail)))
      (fail)))

(DEFUN shen.<singleunderline> (V2667)
  (IF (CONSP (CAR V2667))
      (LET ((Parse_X (CAR (CAR V2667))))
        (IF (shen-cl.true? (shen.singleunderline? Parse_X))
            (shen.pair (CAR (shen.pair (CDR (CAR V2667)) (shen.hdtl V2667)))
             Parse_X)
            (fail)))
      (fail)))

(DEFUN shen.singleunderline? (V2669)
  (and (symbol? V2669) (shen.sh? (str V2669))))

(DEFUN shen.sh? (V2671)
  (COND ((EQUAL "_" V2671) 'true)
        (T (and (shen.equal? (pos V2671 0) "_") (shen.sh? (tlstr V2671))))))

(DEFUN shen.doubleunderline? (V2673)
  (and (symbol? V2673) (shen.dh? (str V2673))))

(DEFUN shen.dh? (V2675)
  (COND ((EQUAL "=" V2675) 'true)
        (T (and (shen.equal? (pos V2675 0) "=") (shen.dh? (tlstr V2675))))))

(DEFUN shen.process-datatype (V2678 V2679)
  (shen.remember-datatype
   (shen.s-prolog (shen.rules->horn-clauses V2678 V2679))))

(DEFUN shen.remember-datatype (V2685)
  (COND
   ((CONSP V2685)
    (do (set 'shen.*datatypes* (adjoin (CAR V2685) shen.*datatypes*))
        (do (set 'shen.*alldatatypes* (adjoin (CAR V2685) shen.*alldatatypes*))
            (CAR V2685))))
   (T (shen.f_error 'shen.remember-datatype))))

(DEFUN shen.rules->horn-clauses (V2690 V2691)
  (COND ((NULL V2691) NIL)
        ((AND (CONSP V2691)
              (AND (shen-cl.true? (tuple? (CAR V2691)))
                   (EQ 'shen.single (fst (CAR V2691)))))
         (CONS (shen.rule->horn-clause V2690 (snd (CAR V2691)))
               (shen.rules->horn-clauses V2690 (CDR V2691))))
        ((AND (CONSP V2691)
              (AND (shen-cl.true? (tuple? (CAR V2691)))
                   (EQ 'shen.double (fst (CAR V2691)))))
         (shen.rules->horn-clauses V2690
          (APPEND (shen.double->singles (snd (CAR V2691))) (CDR V2691))))
        (T (shen.f_error 'shen.rules->horn-clauses))))

(DEFUN shen.double->singles (V2693)
  (CONS (shen.right-rule V2693) (CONS (shen.left-rule V2693) NIL)))

(DEFUN shen.right-rule (V2695) (@p 'shen.single V2695))

(DEFUN shen.left-rule (V2697)
  (COND
   ((AND (CONSP V2697)
         (AND (CONSP (CDR V2697))
              (AND (CONSP (CDR (CDR V2697)))
                   (AND (shen-cl.true? (tuple? (CAR (CDR (CDR V2697)))))
                        (AND (NULL (fst (CAR (CDR (CDR V2697)))))
                             (NULL (CDR (CDR (CDR V2697)))))))))
    (LET ((Q (gensym 'Qv)))
      (LET ((NewConclusion (@p (CONS (snd (CAR (CDR (CDR V2697)))) NIL) Q)))
        (LET ((NewPremises
               (CONS
                (@p (map #'(LAMBDA (X) (shen.right->left X)) (CAR (CDR V2697)))
                    Q)
                NIL)))
          (@p 'shen.single
              (CONS (CAR V2697)
                    (CONS NewPremises (CONS NewConclusion NIL))))))))
   (T (shen.f_error 'shen.left-rule))))

(DEFUN shen.right->left (V2703)
  (COND ((AND (shen-cl.true? (tuple? V2703)) (NULL (fst V2703))) (snd V2703))
        (T
         (simple-error "syntax error with ==========
"))))

(DEFUN shen.rule->horn-clause (V2706 V2707)
  (COND
   ((AND (CONSP V2707)
         (AND (CONSP (CDR V2707))
              (AND (CONSP (CDR (CDR V2707)))
                   (AND (shen-cl.true? (tuple? (CAR (CDR (CDR V2707)))))
                        (NULL (CDR (CDR (CDR V2707))))))))
    (CONS (shen.rule->horn-clause-head V2706 (snd (CAR (CDR (CDR V2707)))))
          (CONS '|:-|
                (CONS
                 (shen.rule->horn-clause-body (CAR V2707) (CAR (CDR V2707))
                  (fst (CAR (CDR (CDR V2707)))))
                 NIL))))
   (T (shen.f_error 'shen.rule->horn-clause))))

(DEFUN shen.rule->horn-clause-head (V2710 V2711)
  (CONS V2710 (CONS (shen.mode-ify V2711) (CONS 'Context_1957 NIL))))

(DEFUN shen.mode-ify (V2713)
  (COND
   ((AND (CONSP V2713)
         (AND (CONSP (CDR V2713))
              (AND (EQ '|:| (CAR (CDR V2713)))
                   (AND (CONSP (CDR (CDR V2713)))
                        (NULL (CDR (CDR (CDR V2713))))))))
    (CONS 'mode
          (CONS
           (CONS (CAR V2713)
                 (CONS '|:|
                       (CONS
                        (CONS 'mode
                              (CONS (CAR (CDR (CDR V2713))) (CONS '+ NIL)))
                        NIL)))
           (CONS '- NIL))))
   (T V2713)))

(DEFUN shen.rule->horn-clause-body (V2717 V2718 V2719)
  (LET ((Variables (map #'(LAMBDA (X) (shen.extract_vars X)) V2719)))
    (LET ((Predicates (map #'(LAMBDA (X) (gensym 'shen.cl)) V2719)))
      (LET ((SearchLiterals
             (shen.construct-search-literals Predicates Variables 'Context_1957
              'Context1_1957)))
        (LET ((SearchClauses
               (shen.construct-search-clauses Predicates V2719 Variables)))
          (LET ((SideLiterals (shen.construct-side-literals V2717)))
            (LET ((PremissLiterals
                   (map
                    #'(LAMBDA (X)
                        (shen.construct-premiss-literal X (empty? V2719)))
                    V2718)))
              (APPEND SearchLiterals
                      (APPEND SideLiterals PremissLiterals)))))))))

(DEFUN shen.construct-search-literals (V2728 V2729 V2730 V2731)
  (COND ((AND (NULL V2728) (NULL V2729)) NIL)
        (T (shen.csl-help V2728 V2729 V2730 V2731))))

(DEFUN shen.csl-help (V2738 V2739 V2740 V2741)
  (COND
   ((AND (NULL V2738) (NULL V2739))
    (CONS (CONS 'bind (CONS 'ContextOut_1957 (CONS V2740 NIL))) NIL))
   ((AND (CONSP V2738) (CONSP V2739))
    (CONS (CONS (CAR V2738) (CONS V2740 (CONS V2741 (CAR V2739))))
          (shen.csl-help (CDR V2738) (CDR V2739) V2741 (gensym 'Context))))
   (T (shen.f_error 'shen.csl-help))))

(DEFUN shen.construct-search-clauses (V2745 V2746 V2747)
  (COND ((AND (NULL V2745) (AND (NULL V2746) (NULL V2747))) 'shen.skip)
        ((AND (CONSP V2745) (AND (CONSP V2746) (CONSP V2747)))
         (do (shen.construct-search-clause (CAR V2745) (CAR V2746) (CAR V2747))
             (shen.construct-search-clauses (CDR V2745) (CDR V2746)
              (CDR V2747))))
        (T (shen.f_error 'shen.construct-search-clauses))))

(DEFUN shen.construct-search-clause (V2751 V2752 V2753)
  (shen.s-prolog
   (CONS (shen.construct-base-search-clause V2751 V2752 V2753)
         (CONS (shen.construct-recursive-search-clause V2751 V2752 V2753)
               NIL))))

(DEFUN shen.construct-base-search-clause (V2757 V2758 V2759)
  (CONS
   (CONS V2757
         (CONS (CONS (shen.mode-ify V2758) 'In_1957) (CONS 'In_1957 V2759)))
   (CONS '|:-| (CONS NIL NIL))))

(DEFUN shen.construct-recursive-search-clause (V2763 V2764 V2765)
  (CONS
   (CONS V2763
         (CONS (CONS 'Assumption_1957 'Assumptions_1957)
               (CONS (CONS 'Assumption_1957 'Out_1957) V2765)))
   (CONS '|:-|
         (CONS
          (CONS (CONS V2763 (CONS 'Assumptions_1957 (CONS 'Out_1957 V2765)))
                NIL)
          NIL))))

(DEFUN shen.construct-side-literals (V2771)
  (COND ((NULL V2771) NIL)
        ((AND (CONSP V2771)
              (AND (CONSP (CAR V2771))
                   (AND (EQ 'if (CAR (CAR V2771)))
                        (AND (CONSP (CDR (CAR V2771)))
                             (NULL (CDR (CDR (CAR V2771))))))))
         (CONS (CONS 'when (CDR (CAR V2771)))
               (shen.construct-side-literals (CDR V2771))))
        ((AND (CONSP V2771)
              (AND (CONSP (CAR V2771))
                   (AND (EQ 'let (CAR (CAR V2771)))
                        (AND (CONSP (CDR (CAR V2771)))
                             (AND (CONSP (CDR (CDR (CAR V2771))))
                                  (NULL (CDR (CDR (CDR (CAR V2771))))))))))
         (CONS (CONS 'is (CDR (CAR V2771)))
               (shen.construct-side-literals (CDR V2771))))
        ((CONSP V2771) (shen.construct-side-literals (CDR V2771)))
        (T (shen.f_error 'shen.construct-side-literals))))

(DEFUN shen.construct-premiss-literal (V2778 V2779)
  (COND
   ((shen-cl.true? (tuple? V2778))
    (CONS 'shen.t*
          (CONS (shen.recursive_cons_form (snd V2778))
                (CONS (shen.construct-context V2779 (fst V2778)) NIL))))
   ((EQ '! V2778) (CONS 'cut (CONS 'Throwcontrol NIL)))
   (T (shen.f_error 'shen.construct-premiss-literal))))

(DEFUN shen.construct-context (V2782 V2783)
  (COND ((AND (EQ 'true V2782) (NULL V2783)) 'Context_1957)
        ((AND (EQ 'false V2782) (NULL V2783)) 'ContextOut_1957)
        ((CONSP V2783)
         (CONS 'cons
               (CONS (shen.recursive_cons_form (CAR V2783))
                     (CONS (shen.construct-context V2782 (CDR V2783)) NIL))))
        (T (shen.f_error 'shen.construct-context))))

(DEFUN shen.recursive_cons_form (V2785)
  (COND
   ((CONSP V2785)
    (CONS 'cons
          (CONS (shen.recursive_cons_form (CAR V2785))
                (CONS (shen.recursive_cons_form (CDR V2785)) NIL))))
   (T V2785)))

(DEFUN preclude (V2787)
  (shen.preclude-h (map #'(LAMBDA (X) (shen.intern-type X)) V2787)))

(DEFUN shen.preclude-h (V2789)
  (LET ((FilterDatatypes
         (set 'shen.*datatypes* (difference shen.*datatypes* V2789))))
    shen.*datatypes*))

(DEFUN include (V2791)
  (shen.include-h (map #'(LAMBDA (X) (shen.intern-type X)) V2791)))

(DEFUN shen.include-h (V2793)
  (LET ((ValidTypes (intersection V2793 shen.*alldatatypes*)))
    (LET ((NewDatatypes
           (set 'shen.*datatypes* (union ValidTypes shen.*datatypes*))))
      shen.*datatypes*)))

(DEFUN preclude-all-but (V2795)
  (shen.preclude-h
   (difference shen.*alldatatypes*
               (map #'(LAMBDA (X) (shen.intern-type X)) V2795))))

(DEFUN include-all-but (V2797)
  (shen.include-h
   (difference shen.*alldatatypes*
               (map #'(LAMBDA (X) (shen.intern-type X)) V2797))))

(DEFUN shen.synonyms-help (V2803)
  (COND
   ((NULL V2803)
    (shen.update-demodulation-function shen.*tc*
     (mapcan #'(LAMBDA (X) (shen.demod-rule X)) shen.*synonyms*)))
   ((AND (CONSP V2803) (CONSP (CDR V2803)))
    (LET ((Vs
           (difference (shen.extract_vars (CAR (CDR V2803)))
                       (shen.extract_vars (CAR V2803)))))
      (IF (NULL Vs)
          (do
           (shen.pushnew (CONS (CAR V2803) (CONS (CAR (CDR V2803)) NIL))
            'shen.*synonyms*)
           (shen.synonyms-help (CDR (CDR V2803))))
          (shen.free_variable_warnings (CAR (CDR V2803)) Vs))))
   (T
    (simple-error "odd number of synonyms
"))))

(DEFUN shen.pushnew (V2806 V2807)
  (IF (shen-cl.true? (element? V2806 (value V2807)))
      (value V2807)
      (set V2807 (CONS V2806 (value V2807)))))

(DEFUN shen.demod-rule (V2809)
  (COND
   ((AND (CONSP V2809) (AND (CONSP (CDR V2809)) (NULL (CDR (CDR V2809)))))
    (CONS (shen.rcons_form (CAR V2809))
          (CONS '-> (CONS (shen.rcons_form (CAR (CDR V2809))) NIL))))
   (T (shen.f_error 'shen.demod-rule))))

(DEFUN shen.lambda-of-defun (V2815)
  (COND
   ((AND (CONSP V2815)
         (AND (EQ 'defun (CAR V2815))
              (AND (CONSP (CDR V2815))
                   (AND (CONSP (CDR (CDR V2815)))
                        (AND (CONSP (CAR (CDR (CDR V2815))))
                             (AND (NULL (CDR (CAR (CDR (CDR V2815)))))
                                  (AND (CONSP (CDR (CDR (CDR V2815))))
                                       (NULL
                                        (CDR (CDR (CDR (CDR V2815))))))))))))
    (eval
     (CONS '/. (CONS (CAR (CAR (CDR (CDR V2815)))) (CDR (CDR (CDR V2815)))))))
   (T (shen.f_error 'shen.lambda-of-defun))))

(DEFUN shen.update-demodulation-function (V2818 V2819)
  (do (tc '-)
      (do
       (set 'shen.*demodulation-function*
            (shen.lambda-of-defun
             (shen.elim-def
              (CONS 'define
                    (CONS 'shen.demod (APPEND V2819 (shen.default-rule)))))))
       (do
        (IF (shen-cl.true? V2818)
            (tc '+)
            'shen.skip)
        'synonyms))))

(DEFUN shen.default-rule () (CONS 'X (CONS '-> (CONS 'X NIL))))

