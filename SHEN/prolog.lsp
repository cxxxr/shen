
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

(DEFUN shen.<defprolog> (V1803)
  (LET ((Parse_shen.<predicate*> (shen.<predicate*> V1803)))
    (IF (NOT (EQ (fail) Parse_shen.<predicate*>))
        (LET ((Parse_shen.<clauses*> (shen.<clauses*> Parse_shen.<predicate*>)))
          (IF (NOT (EQ (fail) Parse_shen.<clauses*>))
              (shen.pair (CAR Parse_shen.<clauses*>)
                         (CAR
                          (shen.prolog->shen
                           (map
                            #'(LAMBDA (Parse_X)
                                (shen.insert-predicate
                                 (shen.hdtl Parse_shen.<predicate*>) Parse_X))
                            (shen.hdtl Parse_shen.<clauses*>)))))
              (fail)))
        (fail))))

(DEFUN shen.prolog-error (V1812 V1813)
  (COND
   ((AND (CONSP V1813) (AND (CONSP (CDR V1813)) (NULL (CDR (CDR V1813)))))
    (simple-error
     (cn "prolog syntax error in "
         (shen.app V1812
          (cn " here:

 "
              (shen.app (shen.next-50 50 (CAR V1813)) "
"
               'shen.a))
          'shen.a))))
   (T
    (simple-error
     (cn "prolog syntax error in "
         (shen.app V1812 "
"
          'shen.a))))))

(DEFUN shen.next-50 (V1820 V1821)
  (COND ((NULL V1821) "")
        ((IF (NUMBERP V1820)
             (= V1820 0))
         "")
        ((CONSP V1821)
         (cn (shen.decons-string (CAR V1821))
             (shen.next-50 (shen.subtract V1820 1) (CDR V1821))))
        (T (shen.f_error 'shen.next-50))))

(DEFUN shen.decons-string (V1823)
  (COND
   ((AND (CONSP V1823)
         (AND (EQ 'cons (CAR V1823))
              (AND (CONSP (CDR V1823))
                   (AND (CONSP (CDR (CDR V1823)))
                        (NULL (CDR (CDR (CDR V1823))))))))
    (shen.app (shen.eval-cons V1823) " " 'shen.s))
   (T (shen.app V1823 " " 'shen.r))))

(DEFUN shen.insert-predicate (V1826 V1827)
  (COND
   ((AND (CONSP V1827) (AND (CONSP (CDR V1827)) (NULL (CDR (CDR V1827)))))
    (CONS (CONS V1826 (CAR V1827)) (CONS '|:-| (CDR V1827))))
   (T (shen.f_error 'shen.insert-predicate))))

(DEFUN shen.<predicate*> (V1829)
  (IF (CONSP (CAR V1829))
      (LET ((Parse_X (CAR (CAR V1829))))
        (shen.pair (CAR (shen.pair (CDR (CAR V1829)) (shen.hdtl V1829)))
                   Parse_X))
      (fail)))

(DEFUN shen.<clauses*> (V1831)
  (LET ((YaccParse
         (LET ((Parse_shen.<clause*> (shen.<clause*> V1831)))
           (IF (NOT (EQ (fail) Parse_shen.<clause*>))
               (LET ((Parse_shen.<clauses*>
                      (shen.<clauses*> Parse_shen.<clause*>)))
                 (IF (NOT (EQ (fail) Parse_shen.<clauses*>))
                     (shen.pair (CAR Parse_shen.<clauses*>)
                                (CONS (shen.hdtl Parse_shen.<clause*>)
                                      (shen.hdtl Parse_shen.<clauses*>)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_<e> (<e> V1831)))
          (IF (NOT (EQ (fail) Parse_<e>))
              (shen.pair (CAR Parse_<e>) NIL)
              (fail)))
        YaccParse)))

(DEFUN shen.<clause*> (V1833)
  (LET ((Parse_shen.<head*> (shen.<head*> V1833)))
    (IF (NOT (EQ (fail) Parse_shen.<head*>))
        (IF (AND (CONSP (CAR Parse_shen.<head*>))
                 (EQ '<-- (CAR (CAR Parse_shen.<head*>))))
            (LET ((Parse_shen.<body*>
                   (shen.<body*>
                    (shen.pair (CDR (CAR Parse_shen.<head*>))
                               (shen.hdtl Parse_shen.<head*>)))))
              (IF (NOT (EQ (fail) Parse_shen.<body*>))
                  (LET ((Parse_shen.<end*> (shen.<end*> Parse_shen.<body*>)))
                    (IF (NOT (EQ (fail) Parse_shen.<end*>))
                        (shen.pair (CAR Parse_shen.<end*>)
                                   (CONS (shen.hdtl Parse_shen.<head*>)
                                         (CONS (shen.hdtl Parse_shen.<body*>)
                                               NIL)))
                        (fail)))
                  (fail)))
            (fail))
        (fail))))

(DEFUN shen.<head*> (V1835)
  (LET ((YaccParse
         (LET ((Parse_shen.<term*> (shen.<term*> V1835)))
           (IF (NOT (EQ (fail) Parse_shen.<term*>))
               (LET ((Parse_shen.<head*> (shen.<head*> Parse_shen.<term*>)))
                 (IF (NOT (EQ (fail) Parse_shen.<head*>))
                     (shen.pair (CAR Parse_shen.<head*>)
                                (CONS (shen.hdtl Parse_shen.<term*>)
                                      (shen.hdtl Parse_shen.<head*>)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_<e> (<e> V1835)))
          (IF (NOT (EQ (fail) Parse_<e>))
              (shen.pair (CAR Parse_<e>) NIL)
              (fail)))
        YaccParse)))

(DEFUN shen.<term*> (V1837)
  (IF (CONSP (CAR V1837))
      (LET ((Parse_X (CAR (CAR V1837))))
        (IF (AND (NOT (EQ '<-- Parse_X))
                 (shen-cl.true? (shen.legitimate-term? Parse_X)))
            (shen.pair (CAR (shen.pair (CDR (CAR V1837)) (shen.hdtl V1837)))
                       (shen.eval-cons Parse_X))
            (fail)))
      (fail)))

(DEFUN shen.legitimate-term? (V1843)
  (COND
   ((AND (CONSP V1843)
         (AND (EQ 'cons (CAR V1843))
              (AND (CONSP (CDR V1843))
                   (AND (CONSP (CDR (CDR V1843)))
                        (NULL (CDR (CDR (CDR V1843))))))))
    (and (shen.legitimate-term? (CAR (CDR V1843)))
         (shen.legitimate-term? (CAR (CDR (CDR V1843))))))
   ((AND (CONSP V1843)
         (AND (EQ 'mode (CAR V1843))
              (AND (CONSP (CDR V1843))
                   (AND (CONSP (CDR (CDR V1843)))
                        (AND (EQ '+ (CAR (CDR (CDR V1843))))
                             (NULL (CDR (CDR (CDR V1843)))))))))
    (shen.legitimate-term? (CAR (CDR V1843))))
   ((AND (CONSP V1843)
         (AND (EQ 'mode (CAR V1843))
              (AND (CONSP (CDR V1843))
                   (AND (CONSP (CDR (CDR V1843)))
                        (AND (EQ '- (CAR (CDR (CDR V1843))))
                             (NULL (CDR (CDR (CDR V1843)))))))))
    (shen.legitimate-term? (CAR (CDR V1843))))
   ((CONSP V1843) 'false) (T 'true)))

(DEFUN shen.eval-cons (V1845)
  (COND
   ((AND (CONSP V1845)
         (AND (EQ 'cons (CAR V1845))
              (AND (CONSP (CDR V1845))
                   (AND (CONSP (CDR (CDR V1845)))
                        (NULL (CDR (CDR (CDR V1845))))))))
    (CONS (shen.eval-cons (CAR (CDR V1845)))
          (shen.eval-cons (CAR (CDR (CDR V1845))))))
   ((AND (CONSP V1845)
         (AND (EQ 'mode (CAR V1845))
              (AND (CONSP (CDR V1845))
                   (AND (CONSP (CDR (CDR V1845)))
                        (NULL (CDR (CDR (CDR V1845))))))))
    (CONS 'mode (CONS (shen.eval-cons (CAR (CDR V1845))) (CDR (CDR V1845)))))
   (T V1845)))

(DEFUN shen.<body*> (V1847)
  (LET ((YaccParse
         (LET ((Parse_shen.<literal*> (shen.<literal*> V1847)))
           (IF (NOT (EQ (fail) Parse_shen.<literal*>))
               (LET ((Parse_shen.<body*> (shen.<body*> Parse_shen.<literal*>)))
                 (IF (NOT (EQ (fail) Parse_shen.<body*>))
                     (shen.pair (CAR Parse_shen.<body*>)
                                (CONS (shen.hdtl Parse_shen.<literal*>)
                                      (shen.hdtl Parse_shen.<body*>)))
                     (fail)))
               (fail)))))
    (IF (EQ YaccParse (fail))
        (LET ((Parse_<e> (<e> V1847)))
          (IF (NOT (EQ (fail) Parse_<e>))
              (shen.pair (CAR Parse_<e>) NIL)
              (fail)))
        YaccParse)))

(DEFUN shen.<literal*> (V1849)
  (LET ((YaccParse
         (IF (AND (CONSP (CAR V1849)) (EQ '! (CAR (CAR V1849))))
             (shen.pair (CAR (shen.pair (CDR (CAR V1849)) (shen.hdtl V1849)))
                        (CONS 'cut (CONS (intern "Throwcontrol") NIL)))
             (fail))))
    (IF (EQ YaccParse (fail))
        (IF (CONSP (CAR V1849))
            (LET ((Parse_X (CAR (CAR V1849))))
              (IF (CONSP Parse_X)
                  (shen.pair
                   (CAR (shen.pair (CDR (CAR V1849)) (shen.hdtl V1849)))
                   Parse_X)
                  (fail)))
            (fail))
        YaccParse)))

(DEFUN shen.<end*> (V1851)
  (IF (CONSP (CAR V1851))
      (LET ((Parse_X (CAR (CAR V1851))))
        (IF (EQ Parse_X '|;|)
            (shen.pair (CAR (shen.pair (CDR (CAR V1851)) (shen.hdtl V1851)))
                       Parse_X)
            (fail)))
      (fail)))

(DEFUN cut (V1855 V1856 V1857)
  (LET ((Result (thaw V1857)))
    (IF (EQ Result 'false)
        V1855
        Result)))

(DEFUN shen.insert_modes (V1859)
  (COND
   ((AND (CONSP V1859)
         (AND (EQ 'mode (CAR V1859))
              (AND (CONSP (CDR V1859))
                   (AND (CONSP (CDR (CDR V1859)))
                        (NULL (CDR (CDR (CDR V1859))))))))
    V1859)
   ((NULL V1859) NIL)
   ((CONSP V1859)
    (CONS (CONS 'mode (CONS (CAR V1859) (CONS '+ NIL)))
          (CONS 'mode (CONS (shen.insert_modes (CDR V1859)) (CONS '- NIL)))))
   (T V1859)))

(DEFUN shen.s-prolog (V1861)
  (map #'(LAMBDA (X) (eval X)) (shen.prolog->shen V1861)))

(DEFUN shen.prolog->shen (V1863)
  (map #'(LAMBDA (X) (shen.compile_prolog_procedure X))
       (shen.group_clauses
        (map #'(LAMBDA (X) (shen.s-prolog_clause X))
             (mapcan #'(LAMBDA (X) (shen.head_abstraction X)) V1863)))))

(DEFUN shen.s-prolog_clause (V1865)
  (COND
   ((AND (CONSP V1865)
         (AND (CONSP (CDR V1865))
              (AND (EQ '|:-| (CAR (CDR V1865)))
                   (AND (CONSP (CDR (CDR V1865)))
                        (NULL (CDR (CDR (CDR V1865))))))))
    (CONS (CAR V1865)
          (CONS '|:-|
                (CONS
                 (map #'(LAMBDA (X) (shen.s-prolog_literal X))
                      (CAR (CDR (CDR V1865))))
                 NIL))))
   (T (shen.f_error 'shen.s-prolog_clause))))

(DEFUN shen.head_abstraction (V1867)
  (COND
   ((AND (CONSP V1867)
         (AND (CONSP (CDR V1867))
              (AND (EQ '|:-| (CAR (CDR V1867)))
                   (AND (CONSP (CDR (CDR V1867)))
                        (AND (NULL (CDR (CDR (CDR V1867))))
                             (shen-cl.true?
                              (trap-error
                               (shen.less? (shen.complexity_head (CAR V1867))
                                           shen.*maxcomplexity*)
                               #'(LAMBDA (_) 'false))))))))
    (CONS V1867 NIL))
   ((AND (CONSP V1867)
         (AND (CONSP (CAR V1867))
              (AND (CONSP (CDR V1867))
                   (AND (EQ '|:-| (CAR (CDR V1867)))
                        (AND (CONSP (CDR (CDR V1867)))
                             (NULL (CDR (CDR (CDR V1867)))))))))
    (LET ((Terms (map #'(LAMBDA (Y) (gensym 'V)) (CDR (CAR V1867)))))
      (LET ((XTerms (shen.rcons_form (shen.remove_modes (CDR (CAR V1867))))))
        (LET ((Literal
               (CONS 'unify (CONS (shen.cons_form Terms) (CONS XTerms NIL)))))
          (LET ((Clause
                 (CONS (CONS (CAR (CAR V1867)) Terms)
                       (CONS '|:-|
                             (CONS (CONS Literal (CAR (CDR (CDR V1867))))
                                   NIL)))))
            (CONS Clause NIL))))))
   (T (shen.f_error 'shen.head_abstraction))))

(DEFUN shen.complexity_head (V1873)
  (COND
   ((CONSP V1873)
    (shen.safe-product (map #'(LAMBDA (X) (shen.complexity X)) (CDR V1873))))
   (T (shen.f_error 'shen.complexity_head))))

(DEFUN shen.safe-multiply (V1876 V1877) (shen.multiply V1876 V1877))

(DEFUN shen.complexity (V1886)
  (COND
   ((AND (CONSP V1886)
         (AND (EQ 'mode (CAR V1886))
              (AND (CONSP (CDR V1886))
                   (AND (CONSP (CAR (CDR V1886)))
                        (AND (EQ 'mode (CAR (CAR (CDR V1886))))
                             (AND (CONSP (CDR (CAR (CDR V1886))))
                                  (AND (CONSP (CDR (CDR (CAR (CDR V1886)))))
                                       (AND
                                        (NULL
                                         (CDR (CDR (CDR (CAR (CDR V1886))))))
                                        (AND (CONSP (CDR (CDR V1886)))
                                             (NULL
                                              (CDR (CDR (CDR V1886)))))))))))))
    (shen.complexity (CAR (CDR V1886))))
   ((AND (CONSP V1886)
         (AND (EQ 'mode (CAR V1886))
              (AND (CONSP (CDR V1886))
                   (AND (CONSP (CAR (CDR V1886)))
                        (AND (CONSP (CDR (CDR V1886)))
                             (AND (EQ '+ (CAR (CDR (CDR V1886))))
                                  (NULL (CDR (CDR (CDR V1886))))))))))
    (shen.safe-multiply 2
     (shen.safe-multiply
      (shen.complexity
       (CONS 'mode (CONS (CAR (CAR (CDR V1886))) (CDR (CDR V1886)))))
      (shen.complexity
       (CONS 'mode (CONS (CDR (CAR (CDR V1886))) (CDR (CDR V1886))))))))
   ((AND (CONSP V1886)
         (AND (EQ 'mode (CAR V1886))
              (AND (CONSP (CDR V1886))
                   (AND (CONSP (CAR (CDR V1886)))
                        (AND (CONSP (CDR (CDR V1886)))
                             (AND (EQ '- (CAR (CDR (CDR V1886))))
                                  (NULL (CDR (CDR (CDR V1886))))))))))
    (shen.safe-multiply
     (shen.complexity
      (CONS 'mode (CONS (CAR (CAR (CDR V1886))) (CDR (CDR V1886)))))
     (shen.complexity
      (CONS 'mode (CONS (CDR (CAR (CDR V1886))) (CDR (CDR V1886)))))))
   ((AND (CONSP V1886)
         (AND (EQ 'mode (CAR V1886))
              (AND (CONSP (CDR V1886))
                   (AND (CONSP (CDR (CDR V1886)))
                        (AND (NULL (CDR (CDR (CDR V1886))))
                             (shen-cl.true? (variable? (CAR (CDR V1886)))))))))
    1)
   ((AND (CONSP V1886)
         (AND (EQ 'mode (CAR V1886))
              (AND (CONSP (CDR V1886))
                   (AND (CONSP (CDR (CDR V1886)))
                        (AND (EQ '+ (CAR (CDR (CDR V1886))))
                             (NULL (CDR (CDR (CDR V1886)))))))))
    2)
   ((AND (CONSP V1886)
         (AND (EQ 'mode (CAR V1886))
              (AND (CONSP (CDR V1886))
                   (AND (CONSP (CDR (CDR V1886)))
                        (AND (EQ '- (CAR (CDR (CDR V1886))))
                             (NULL (CDR (CDR (CDR V1886)))))))))
    1)
   (T (shen.complexity (CONS 'mode (CONS V1886 (CONS '+ NIL)))))))

(DEFUN shen.safe-product (V1888)
  (COND ((NULL V1888) 1)
        ((CONSP V1888)
         (shen.safe-multiply (CAR V1888) (shen.safe-product (CDR V1888))))
        (T (shen.f_error 'shen.safe-product))))

(DEFUN shen.s-prolog_literal (V1890)
  (COND
   ((AND (CONSP V1890)
         (AND (EQ 'is (CAR V1890))
              (AND (CONSP (CDR V1890))
                   (AND (CONSP (CDR (CDR V1890)))
                        (NULL (CDR (CDR (CDR V1890))))))))
    (CONS 'bind
          (CONS (CAR (CDR V1890))
                (CONS (shen.insert_deref (CAR (CDR (CDR V1890)))) NIL))))
   ((AND (CONSP V1890)
         (AND (EQ 'when (CAR V1890))
              (AND (CONSP (CDR V1890)) (NULL (CDR (CDR V1890))))))
    (CONS 'fwhen (CONS (shen.insert_deref (CAR (CDR V1890))) NIL)))
   ((AND (CONSP V1890)
         (AND (EQ 'bind (CAR V1890))
              (AND (CONSP (CDR V1890))
                   (AND (CONSP (CDR (CDR V1890)))
                        (NULL (CDR (CDR (CDR V1890))))))))
    (CONS 'bind
          (CONS (CAR (CDR V1890))
                (CONS (shen.insert_lazyderef (CAR (CDR (CDR V1890)))) NIL))))
   ((AND (CONSP V1890)
         (AND (EQ 'fwhen (CAR V1890))
              (AND (CONSP (CDR V1890)) (NULL (CDR (CDR V1890))))))
    (CONS 'fwhen (CONS (shen.insert_lazyderef (CAR (CDR V1890))) NIL)))
   ((CONSP V1890) V1890) (T (shen.f_error 'shen.s-prolog_literal))))

(DEFUN shen.insert_deref (V1892)
  (COND
   ((shen-cl.true? (variable? V1892))
    (CONS 'shen.deref (CONS V1892 (CONS 'ProcessN NIL))))
   ((CONSP V1892)
    (CONS (shen.insert_deref (CAR V1892)) (shen.insert_deref (CDR V1892))))
   (T V1892)))

(DEFUN shen.insert_lazyderef (V1894)
  (COND
   ((shen-cl.true? (variable? V1894))
    (CONS 'shen.lazyderef (CONS V1894 (CONS 'ProcessN NIL))))
   ((CONSP V1894)
    (CONS (shen.insert_lazyderef (CAR V1894))
          (shen.insert_lazyderef (CDR V1894))))
   (T V1894)))

(DEFUN shen.group_clauses (V1896)
  (COND ((NULL V1896) NIL)
        ((CONSP V1896)
         (LET ((Group
                (shen.collect
                 #'(LAMBDA (X) (shen.same_predicate? (CAR V1896) X)) V1896)))
           (LET ((Rest (difference V1896 Group)))
             (CONS Group (shen.group_clauses Rest)))))
        (T (shen.f_error 'shen.group_clauses))))

(DEFUN shen.collect (V1901 V1902)
  (COND ((NULL V1902) NIL)
        ((CONSP V1902)
         (IF (shen-cl.true? (shen.apply V1901 (LIST (CAR V1902))))
             (CONS (CAR V1902) (shen.collect V1901 (CDR V1902)))
             (shen.collect V1901 (CDR V1902))))
        (T (shen.f_error 'shen.collect))))

(DEFUN shen.same_predicate? (V1921 V1922)
  (COND
   ((AND (CONSP V1921)
         (AND (CONSP (CAR V1921)) (AND (CONSP V1922) (CONSP (CAR V1922)))))
    (shen.equal? (CAR (CAR V1921)) (CAR (CAR V1922))))
   (T (shen.f_error 'shen.same_predicate?))))

(DEFUN shen.compile_prolog_procedure (V1924)
  (LET ((F (shen.procedure_name V1924)))
    (LET ((Shen (shen.clauses-to-shen F V1924)))
      Shen)))

(DEFUN shen.procedure_name (V1938)
  (COND
   ((AND (CONSP V1938) (AND (CONSP (CAR V1938)) (CONSP (CAR (CAR V1938)))))
    (CAR (CAR (CAR V1938))))
   (T (shen.f_error 'shen.procedure_name))))

(DEFUN shen.clauses-to-shen (V1941 V1942)
  (LET ((Linear (map #'(LAMBDA (X) (shen.linearise-clause X)) V1942)))
    (LET ((Arity
           (shen.prolog-aritycheck V1941 (map #'(LAMBDA (X) (head X)) V1942))))
      (LET ((Parameters (shen.parameters Arity)))
        (LET ((AUM_instructions
               (map #'(LAMBDA (X) (shen.aum X Parameters)) Linear)))
          (LET ((Code
                 (shen.catch-cut
                  (shen.nest-disjunct
                   (map #'(LAMBDA (X) (shen.aum_to_shen X))
                        AUM_instructions)))))
            (LET ((ShenDef
                   (CONS 'define
                         (CONS V1941
                               (APPEND Parameters
                                       (APPEND
                                        (CONS 'ProcessN
                                              (CONS 'Continuation NIL))
                                        (CONS '-> (CONS Code NIL))))))))
              ShenDef)))))))

(DEFUN shen.catch-cut (V1944)
  (COND ((NOT (shen-cl.true? (shen.occurs? 'cut V1944))) V1944)
        (T
         (CONS 'let
               (CONS 'Throwcontrol
                     (CONS (CONS 'shen.catchpoint NIL)
                           (CONS
                            (CONS 'shen.cutpoint
                                  (CONS 'Throwcontrol (CONS V1944 NIL)))
                            NIL)))))))

(DEFUN shen.catchpoint () (set 'shen.*catch* (shen.add 1 shen.*catch*)))

(DEFUN shen.cutpoint (V1952 V1953)
  (COND ((shen.ABSEQUAL V1953 V1952) 'false) (T V1953)))

(DEFUN shen.nest-disjunct (V1955)
  (COND ((AND (CONSP V1955) (NULL (CDR V1955))) (CAR V1955))
        ((CONSP V1955)
         (shen.lisp-or (CAR V1955) (shen.nest-disjunct (CDR V1955))))
        (T (shen.f_error 'shen.nest-disjunct))))

(DEFUN shen.lisp-or (V1958 V1959)
  (CONS 'let
        (CONS 'Case
              (CONS V1958
                    (CONS
                     (CONS 'if
                           (CONS (CONS '= (CONS 'Case (CONS 'false NIL)))
                                 (CONS V1959 (CONS 'Case NIL))))
                     NIL)))))

(DEFUN shen.prolog-aritycheck (V1964 V1965)
  (COND
   ((AND (CONSP V1965) (NULL (CDR V1965)))
    (shen.subtract (length (CAR V1965)) 1))
   ((AND (CONSP V1965) (CONSP (CDR V1965)))
    (IF (shen.ABSEQUAL (length (CAR V1965)) (length (CAR (CDR V1965))))
        (shen.prolog-aritycheck V1964 (CDR V1965))
        (simple-error
         (cn "arity error in prolog procedure "
             (shen.app (CONS V1964 NIL) "
"
              'shen.a)))))
   (T (shen.f_error 'shen.prolog-aritycheck))))

(DEFUN shen.linearise-clause (V1967)
  (COND
   ((AND (CONSP V1967)
         (AND (CONSP (CDR V1967))
              (AND (EQ '|:-| (CAR (CDR V1967)))
                   (AND (CONSP (CDR (CDR V1967)))
                        (NULL (CDR (CDR (CDR V1967))))))))
    (LET ((Linear (shen.linearise (CONS (CAR V1967) (CDR (CDR V1967))))))
      (shen.clause_form Linear)))
   (T (shen.f_error 'shen.linearise-clause))))

(DEFUN shen.clause_form (V1969)
  (COND
   ((AND (CONSP V1969) (AND (CONSP (CDR V1969)) (NULL (CDR (CDR V1969)))))
    (CONS (shen.explicit_modes (CAR V1969))
          (CONS '|:-| (CONS (shen.cf_help (CAR (CDR V1969))) NIL))))
   (T (shen.f_error 'shen.clause_form))))

(DEFUN shen.explicit_modes (V1971)
  (COND
   ((CONSP V1971)
    (CONS (CAR V1971) (map #'(LAMBDA (X) (shen.em_help X)) (CDR V1971))))
   (T (shen.f_error 'shen.explicit_modes))))

(DEFUN shen.em_help (V1973)
  (COND
   ((AND (CONSP V1973)
         (AND (EQ 'mode (CAR V1973))
              (AND (CONSP (CDR V1973))
                   (AND (CONSP (CDR (CDR V1973)))
                        (NULL (CDR (CDR (CDR V1973))))))))
    V1973)
   (T (CONS 'mode (CONS V1973 (CONS '+ NIL))))))

(DEFUN shen.cf_help (V1975)
  (COND
   ((AND (CONSP V1975)
         (AND (EQ 'where (CAR V1975))
              (AND (CONSP (CDR V1975))
                   (AND (CONSP (CAR (CDR V1975)))
                        (AND (EQ '= (CAR (CAR (CDR V1975))))
                             (AND (CONSP (CDR (CAR (CDR V1975))))
                                  (AND (CONSP (CDR (CDR (CAR (CDR V1975)))))
                                       (AND
                                        (NULL
                                         (CDR (CDR (CDR (CAR (CDR V1975))))))
                                        (AND (CONSP (CDR (CDR V1975)))
                                             (NULL
                                              (CDR (CDR (CDR V1975)))))))))))))
    (CONS
     (CONS
      (IF (shen-cl.true? shen.*occurs*)
          'unify!
          'unify)
      (CDR (CAR (CDR V1975))))
     (shen.cf_help (CAR (CDR (CDR V1975))))))
   (T V1975)))

(DEFUN occurs-check (V1981)
  (COND ((EQ '+ V1981) (set 'shen.*occurs* 'true))
        ((EQ '- V1981) (set 'shen.*occurs* 'false))
        (T
         (simple-error "occurs-check expects + or -
"))))

(DEFUN shen.aum (V1984 V1985)
  (COND
   ((AND (CONSP V1984)
         (AND (CONSP (CAR V1984))
              (AND (CONSP (CDR V1984))
                   (AND (EQ '|:-| (CAR (CDR V1984)))
                        (AND (CONSP (CDR (CDR V1984)))
                             (NULL (CDR (CDR (CDR V1984)))))))))
    (LET ((MuApplication
           (shen.make_mu_application
            (CONS 'shen.mu
                  (CONS (CDR (CAR V1984))
                        (CONS
                         (shen.continuation_call (CDR (CAR V1984))
                          (CAR (CDR (CDR V1984))))
                         NIL)))
            V1985)))
      (shen.mu_reduction MuApplication '+)))
   (T (shen.f_error 'shen.aum))))

(DEFUN shen.continuation_call (V1988 V1989)
  (LET ((VTerms (CONS 'ProcessN (shen.extract_vars V1988))))
    (LET ((VBody (shen.extract_vars V1989)))
      (LET ((Free (remove 'Throwcontrol (difference VBody VTerms))))
        (shen.cc_help Free V1989)))))

(DEFUN remove (V1992 V1993) (shen.remove-h V1992 V1993 NIL))

(DEFUN shen.remove-h (V2000 V2001 V2002)
  (COND ((NULL V2001) (REVERSE V2002))
        ((AND (CONSP V2001) (shen.ABSEQUAL (CAR V2001) V2000))
         (shen.remove-h (CAR V2001) (CDR V2001) V2002))
        ((CONSP V2001)
         (shen.remove-h V2000 (CDR V2001) (CONS (CAR V2001) V2002)))
        (T (shen.f_error 'shen.remove-h))))

(DEFUN shen.cc_help (V2005 V2006)
  (COND
   ((AND (NULL V2005) (NULL V2006))
    (CONS 'shen.pop (CONS 'shen.the (CONS 'shen.stack NIL))))
   ((NULL V2006)
    (CONS 'shen.rename
          (CONS 'shen.the
                (CONS 'shen.variables
                      (CONS 'in
                            (CONS V2005
                                  (CONS 'and
                                        (CONS 'shen.then
                                              (CONS
                                               (CONS 'shen.pop
                                                     (CONS 'shen.the
                                                           (CONS 'shen.stack
                                                                 NIL)))
                                               NIL)))))))))
   ((NULL V2005)
    (CONS 'call (CONS 'shen.the (CONS 'shen.continuation (CONS V2006 NIL)))))
   (T
    (CONS 'shen.rename
          (CONS 'shen.the
                (CONS 'shen.variables
                      (CONS 'in
                            (CONS V2005
                                  (CONS 'and
                                        (CONS 'shen.then
                                              (CONS
                                               (CONS 'call
                                                     (CONS 'shen.the
                                                           (CONS
                                                            'shen.continuation
                                                            (CONS V2006 NIL))))
                                               NIL)))))))))))

(DEFUN shen.make_mu_application (V2009 V2010)
  (COND
   ((AND (CONSP V2009)
         (AND (EQ 'shen.mu (CAR V2009))
              (AND (CONSP (CDR V2009))
                   (AND (NULL (CAR (CDR V2009)))
                        (AND (CONSP (CDR (CDR V2009)))
                             (AND (NULL (CDR (CDR (CDR V2009))))
                                  (NULL V2010)))))))
    (CAR (CDR (CDR V2009))))
   ((AND (CONSP V2009)
         (AND (EQ 'shen.mu (CAR V2009))
              (AND (CONSP (CDR V2009))
                   (AND (CONSP (CAR (CDR V2009)))
                        (AND (CONSP (CDR (CDR V2009)))
                             (AND (NULL (CDR (CDR (CDR V2009))))
                                  (CONSP V2010)))))))
    (CONS
     (CONS 'shen.mu
           (CONS (CAR (CAR (CDR V2009)))
                 (CONS
                  (shen.make_mu_application
                   (CONS 'shen.mu
                         (CONS (CDR (CAR (CDR V2009))) (CDR (CDR V2009))))
                   (CDR V2010))
                  NIL)))
     (CONS (CAR V2010) NIL)))
   (T (shen.f_error 'shen.make_mu_application))))

(DEFUN shen.mu_reduction (V2019 V2020)
  (COND
   ((AND (CONSP V2019)
         (AND (CONSP (CAR V2019))
              (AND (EQ 'shen.mu (CAR (CAR V2019)))
                   (AND (CONSP (CDR (CAR V2019)))
                        (AND (CONSP (CAR (CDR (CAR V2019))))
                             (AND (EQ 'mode (CAR (CAR (CDR (CAR V2019)))))
                                  (AND (CONSP (CDR (CAR (CDR (CAR V2019)))))
                                       (AND
                                        (CONSP
                                         (CDR (CDR (CAR (CDR (CAR V2019))))))
                                        (AND
                                         (NULL
                                          (CDR
                                           (CDR
                                            (CDR (CAR (CDR (CAR V2019)))))))
                                         (AND (CONSP (CDR (CDR (CAR V2019))))
                                              (AND
                                               (NULL
                                                (CDR (CDR (CDR (CAR V2019)))))
                                               (AND (CONSP (CDR V2019))
                                                    (NULL
                                                     (CDR
                                                      (CDR V2019)))))))))))))))
    (shen.mu_reduction
     (CONS
      (CONS 'shen.mu
            (CONS (CAR (CDR (CAR (CDR (CAR V2019))))) (CDR (CDR (CAR V2019)))))
      (CDR V2019))
     (CAR (CDR (CDR (CAR (CDR (CAR V2019))))))))
   ((AND (CONSP V2019)
         (AND (CONSP (CAR V2019))
              (AND (EQ 'shen.mu (CAR (CAR V2019)))
                   (AND (CONSP (CDR (CAR V2019)))
                        (AND (CONSP (CDR (CDR (CAR V2019))))
                             (AND (NULL (CDR (CDR (CDR (CAR V2019)))))
                                  (AND (CONSP (CDR V2019))
                                       (AND (NULL (CDR (CDR V2019)))
                                            (EQ '_
                                                (CAR
                                                 (CDR (CAR V2019))))))))))))
    (shen.mu_reduction (CAR (CDR (CDR (CAR V2019)))) V2020))
   ((AND (CONSP V2019)
         (AND (CONSP (CAR V2019))
              (AND (EQ 'shen.mu (CAR (CAR V2019)))
                   (AND (CONSP (CDR (CAR V2019)))
                        (AND (CONSP (CDR (CDR (CAR V2019))))
                             (AND (NULL (CDR (CDR (CDR (CAR V2019)))))
                                  (AND (CONSP (CDR V2019))
                                       (AND (NULL (CDR (CDR V2019)))
                                            (shen-cl.true?
                                             (shen.ephemeral_variable?
                                              (CAR (CDR (CAR V2019)))
                                              (CAR (CDR V2019))))))))))))
    (subst (CAR (CDR V2019)) (CAR (CDR (CAR V2019)))
           (shen.mu_reduction (CAR (CDR (CDR (CAR V2019)))) V2020)))
   ((AND (CONSP V2019)
         (AND (CONSP (CAR V2019))
              (AND (EQ 'shen.mu (CAR (CAR V2019)))
                   (AND (CONSP (CDR (CAR V2019)))
                        (AND (CONSP (CDR (CDR (CAR V2019))))
                             (AND (NULL (CDR (CDR (CDR (CAR V2019)))))
                                  (AND (CONSP (CDR V2019))
                                       (AND (NULL (CDR (CDR V2019)))
                                            (shen-cl.true?
                                             (variable?
                                              (CAR (CDR (CAR V2019)))))))))))))
    (CONS 'let
          (CONS (CAR (CDR (CAR V2019)))
                (CONS 'shen.be
                      (CONS (CAR (CDR V2019))
                            (CONS 'in
                                  (CONS
                                   (shen.mu_reduction
                                    (CAR (CDR (CDR (CAR V2019)))) V2020)
                                   NIL)))))))
   ((AND (CONSP V2019)
         (AND (CONSP (CAR V2019))
              (AND (EQ 'shen.mu (CAR (CAR V2019)))
                   (AND (CONSP (CDR (CAR V2019)))
                        (AND (CONSP (CDR (CDR (CAR V2019))))
                             (AND (NULL (CDR (CDR (CDR (CAR V2019)))))
                                  (AND (CONSP (CDR V2019))
                                       (AND (NULL (CDR (CDR V2019)))
                                            (AND (EQ '- V2020)
                                                 (shen-cl.true?
                                                  (shen.prolog_constant?
                                                   (CAR
                                                    (CDR
                                                     (CAR V2019))))))))))))))
    (LET ((Z (gensym 'V)))
      (CONS 'let
            (CONS Z
                  (CONS 'shen.be
                        (CONS
                         (CONS 'shen.the
                               (CONS 'shen.result
                                     (CONS 'shen.of
                                           (CONS 'shen.dereferencing
                                                 (CDR V2019)))))
                         (CONS 'in
                               (CONS
                                (CONS 'if
                                      (CONS
                                       (CONS Z
                                             (CONS 'is
                                                   (CONS 'identical
                                                         (CONS 'shen.to
                                                               (CONS
                                                                (CAR
                                                                 (CDR
                                                                  (CAR V2019)))
                                                                NIL)))))
                                       (CONS 'shen.then
                                             (CONS
                                              (shen.mu_reduction
                                               (CAR (CDR (CDR (CAR V2019))))
                                               '-)
                                              (CONS 'shen.else
                                                    (CONS 'shen.failed!
                                                          NIL))))))
                                NIL))))))))
   ((AND (CONSP V2019)
         (AND (CONSP (CAR V2019))
              (AND (EQ 'shen.mu (CAR (CAR V2019)))
                   (AND (CONSP (CDR (CAR V2019)))
                        (AND (CONSP (CDR (CDR (CAR V2019))))
                             (AND (NULL (CDR (CDR (CDR (CAR V2019)))))
                                  (AND (CONSP (CDR V2019))
                                       (AND (NULL (CDR (CDR V2019)))
                                            (AND (EQ '+ V2020)
                                                 (shen-cl.true?
                                                  (shen.prolog_constant?
                                                   (CAR
                                                    (CDR
                                                     (CAR V2019))))))))))))))
    (LET ((Z (gensym 'V)))
      (CONS 'let
            (CONS Z
                  (CONS 'shen.be
                        (CONS
                         (CONS 'shen.the
                               (CONS 'shen.result
                                     (CONS 'shen.of
                                           (CONS 'shen.dereferencing
                                                 (CDR V2019)))))
                         (CONS 'in
                               (CONS
                                (CONS 'if
                                      (CONS
                                       (CONS Z
                                             (CONS 'is
                                                   (CONS 'identical
                                                         (CONS 'shen.to
                                                               (CONS
                                                                (CAR
                                                                 (CDR
                                                                  (CAR V2019)))
                                                                NIL)))))
                                       (CONS 'shen.then
                                             (CONS
                                              (shen.mu_reduction
                                               (CAR (CDR (CDR (CAR V2019))))
                                               '+)
                                              (CONS 'shen.else
                                                    (CONS
                                                     (CONS 'if
                                                           (CONS
                                                            (CONS Z
                                                                  (CONS 'is
                                                                        (CONS
                                                                         'shen.a
                                                                         (CONS
                                                                          'shen.variable
                                                                          NIL))))
                                                            (CONS 'shen.then
                                                                  (CONS
                                                                   (CONS 'bind
                                                                         (CONS
                                                                          Z
                                                                          (CONS
                                                                           'shen.to
                                                                           (CONS
                                                                            (CAR
                                                                             (CDR
                                                                              (CAR
                                                                               V2019)))
                                                                            (CONS
                                                                             'in
                                                                             (CONS
                                                                              (shen.mu_reduction
                                                                               (CAR
                                                                                (CDR
                                                                                 (CDR
                                                                                  (CAR
                                                                                   V2019))))
                                                                               '+)
                                                                              NIL))))))
                                                                   (CONS
                                                                    'shen.else
                                                                    (CONS
                                                                     'shen.failed!
                                                                     NIL))))))
                                                     NIL))))))
                                NIL))))))))
   ((AND (CONSP V2019)
         (AND (CONSP (CAR V2019))
              (AND (EQ 'shen.mu (CAR (CAR V2019)))
                   (AND (CONSP (CDR (CAR V2019)))
                        (AND (CONSP (CAR (CDR (CAR V2019))))
                             (AND (CONSP (CDR (CDR (CAR V2019))))
                                  (AND (NULL (CDR (CDR (CDR (CAR V2019)))))
                                       (AND (CONSP (CDR V2019))
                                            (AND (NULL (CDR (CDR V2019)))
                                                 (EQ '- V2020))))))))))
    (LET ((Z (gensym 'V)))
      (CONS 'let
            (CONS Z
                  (CONS 'shen.be
                        (CONS
                         (CONS 'shen.the
                               (CONS 'shen.result
                                     (CONS 'shen.of
                                           (CONS 'shen.dereferencing
                                                 (CDR V2019)))))
                         (CONS 'in
                               (CONS
                                (CONS 'if
                                      (CONS
                                       (CONS Z
                                             (CONS 'is
                                                   (CONS 'shen.a
                                                         (CONS 'shen.non-empty
                                                               (CONS 'list
                                                                     NIL)))))
                                       (CONS 'shen.then
                                             (CONS
                                              (shen.mu_reduction
                                               (CONS
                                                (CONS 'shen.mu
                                                      (CONS
                                                       (CAR
                                                        (CAR
                                                         (CDR (CAR V2019))))
                                                       (CONS
                                                        (CONS
                                                         (CONS 'shen.mu
                                                               (CONS
                                                                (CDR
                                                                 (CAR
                                                                  (CDR
                                                                   (CAR
                                                                    V2019))))
                                                                (CDR
                                                                 (CDR
                                                                  (CAR
                                                                   V2019)))))
                                                         (CONS
                                                          (CONS 'shen.the
                                                                (CONS 'tail
                                                                      (CONS
                                                                       'shen.of
                                                                       (CONS Z
                                                                             NIL))))
                                                          NIL))
                                                        NIL)))
                                                (CONS
                                                 (CONS 'shen.the
                                                       (CONS 'head
                                                             (CONS 'shen.of
                                                                   (CONS Z
                                                                         NIL))))
                                                 NIL))
                                               '-)
                                              (CONS 'shen.else
                                                    (CONS 'shen.failed!
                                                          NIL))))))
                                NIL))))))))
   ((AND (CONSP V2019)
         (AND (CONSP (CAR V2019))
              (AND (EQ 'shen.mu (CAR (CAR V2019)))
                   (AND (CONSP (CDR (CAR V2019)))
                        (AND (CONSP (CAR (CDR (CAR V2019))))
                             (AND (CONSP (CDR (CDR (CAR V2019))))
                                  (AND (NULL (CDR (CDR (CDR (CAR V2019)))))
                                       (AND (CONSP (CDR V2019))
                                            (AND (NULL (CDR (CDR V2019)))
                                                 (EQ '+ V2020))))))))))
    (LET ((Z (gensym 'V)))
      (CONS 'let
            (CONS Z
                  (CONS 'shen.be
                        (CONS
                         (CONS 'shen.the
                               (CONS 'shen.result
                                     (CONS 'shen.of
                                           (CONS 'shen.dereferencing
                                                 (CDR V2019)))))
                         (CONS 'in
                               (CONS
                                (CONS 'if
                                      (CONS
                                       (CONS Z
                                             (CONS 'is
                                                   (CONS 'shen.a
                                                         (CONS 'shen.non-empty
                                                               (CONS 'list
                                                                     NIL)))))
                                       (CONS 'shen.then
                                             (CONS
                                              (shen.mu_reduction
                                               (CONS
                                                (CONS 'shen.mu
                                                      (CONS
                                                       (CAR
                                                        (CAR
                                                         (CDR (CAR V2019))))
                                                       (CONS
                                                        (CONS
                                                         (CONS 'shen.mu
                                                               (CONS
                                                                (CDR
                                                                 (CAR
                                                                  (CDR
                                                                   (CAR
                                                                    V2019))))
                                                                (CDR
                                                                 (CDR
                                                                  (CAR
                                                                   V2019)))))
                                                         (CONS
                                                          (CONS 'shen.the
                                                                (CONS 'tail
                                                                      (CONS
                                                                       'shen.of
                                                                       (CONS Z
                                                                             NIL))))
                                                          NIL))
                                                        NIL)))
                                                (CONS
                                                 (CONS 'shen.the
                                                       (CONS 'head
                                                             (CONS 'shen.of
                                                                   (CONS Z
                                                                         NIL))))
                                                 NIL))
                                               '+)
                                              (CONS 'shen.else
                                                    (CONS
                                                     (CONS 'if
                                                           (CONS
                                                            (CONS Z
                                                                  (CONS 'is
                                                                        (CONS
                                                                         'shen.a
                                                                         (CONS
                                                                          'shen.variable
                                                                          NIL))))
                                                            (CONS 'shen.then
                                                                  (CONS
                                                                   (CONS
                                                                    'shen.rename
                                                                    (CONS
                                                                     'shen.the
                                                                     (CONS
                                                                      'shen.variables
                                                                      (CONS 'in
                                                                            (CONS
                                                                             (shen.extract_vars
                                                                              (CAR
                                                                               (CDR
                                                                                (CAR
                                                                                 V2019))))
                                                                             (CONS
                                                                              'and
                                                                              (CONS
                                                                               'shen.then
                                                                               (CONS
                                                                                (CONS
                                                                                 'bind
                                                                                 (CONS
                                                                                  Z
                                                                                  (CONS
                                                                                   'shen.to
                                                                                   (CONS
                                                                                    (shen.rcons_form
                                                                                     (shen.remove_modes
                                                                                      (CAR
                                                                                       (CDR
                                                                                        (CAR
                                                                                         V2019)))))
                                                                                    (CONS
                                                                                     'in
                                                                                     (CONS
                                                                                      (shen.mu_reduction
                                                                                       (CAR
                                                                                        (CDR
                                                                                         (CDR
                                                                                          (CAR
                                                                                           V2019))))
                                                                                       '+)
                                                                                      NIL))))))
                                                                                NIL))))))))
                                                                   (CONS
                                                                    'shen.else
                                                                    (CONS
                                                                     'shen.failed!
                                                                     NIL))))))
                                                     NIL))))))
                                NIL))))))))
   (T V2019)))

(DEFUN shen.rcons_form (V2022)
  (COND
   ((CONSP V2022)
    (CONS 'cons
          (CONS (shen.rcons_form (CAR V2022))
                (CONS (shen.rcons_form (CDR V2022)) NIL))))
   (T V2022)))

(DEFUN shen.remove_modes (V2024)
  (COND
   ((AND (CONSP V2024)
         (AND (EQ 'mode (CAR V2024))
              (AND (CONSP (CDR V2024))
                   (AND (CONSP (CDR (CDR V2024)))
                        (AND (EQ '+ (CAR (CDR (CDR V2024))))
                             (NULL (CDR (CDR (CDR V2024)))))))))
    (shen.remove_modes (CAR (CDR V2024))))
   ((AND (CONSP V2024)
         (AND (EQ 'mode (CAR V2024))
              (AND (CONSP (CDR V2024))
                   (AND (CONSP (CDR (CDR V2024)))
                        (AND (EQ '- (CAR (CDR (CDR V2024))))
                             (NULL (CDR (CDR (CDR V2024)))))))))
    (shen.remove_modes (CAR (CDR V2024))))
   ((CONSP V2024)
    (CONS (shen.remove_modes (CAR V2024)) (shen.remove_modes (CDR V2024))))
   (T V2024)))

(DEFUN shen.ephemeral_variable? (V2027 V2028)
  (and (variable? V2027) (variable? V2028)))

(DEFUN shen.prolog_constant? (V2038) (COND ((CONSP V2038) 'false) (T 'true)))

(DEFUN shen.aum_to_shen (V2040)
  (COND
   ((AND (CONSP V2040)
         (AND (EQ 'let (CAR V2040))
              (AND (CONSP (CDR V2040))
                   (AND (CONSP (CDR (CDR V2040)))
                        (AND (EQ 'shen.be (CAR (CDR (CDR V2040))))
                             (AND (CONSP (CDR (CDR (CDR V2040))))
                                  (AND (CONSP (CDR (CDR (CDR (CDR V2040)))))
                                       (AND
                                        (EQ 'in
                                            (CAR
                                             (CDR (CDR (CDR (CDR V2040))))))
                                        (AND
                                         (CONSP
                                          (CDR (CDR (CDR (CDR (CDR V2040))))))
                                         (NULL
                                          (CDR
                                           (CDR
                                            (CDR
                                             (CDR
                                              (CDR (CDR V2040))))))))))))))))
    (CONS 'let
          (CONS (CAR (CDR V2040))
                (CONS (shen.aum_to_shen (CAR (CDR (CDR (CDR V2040)))))
                      (CONS
                       (shen.aum_to_shen
                        (CAR (CDR (CDR (CDR (CDR (CDR V2040)))))))
                       NIL)))))
   ((AND (CONSP V2040)
         (AND (EQ 'shen.the (CAR V2040))
              (AND (CONSP (CDR V2040))
                   (AND (EQ 'shen.result (CAR (CDR V2040)))
                        (AND (CONSP (CDR (CDR V2040)))
                             (AND (EQ 'shen.of (CAR (CDR (CDR V2040))))
                                  (AND (CONSP (CDR (CDR (CDR V2040))))
                                       (AND
                                        (EQ 'shen.dereferencing
                                            (CAR (CDR (CDR (CDR V2040)))))
                                        (AND
                                         (CONSP (CDR (CDR (CDR (CDR V2040)))))
                                         (NULL
                                          (CDR
                                           (CDR
                                            (CDR (CDR (CDR V2040)))))))))))))))
    (CONS 'shen.lazyderef
          (CONS (shen.aum_to_shen (CAR (CDR (CDR (CDR (CDR V2040))))))
                (CONS 'ProcessN NIL))))
   ((AND (CONSP V2040)
         (AND (EQ 'if (CAR V2040))
              (AND (CONSP (CDR V2040))
                   (AND (CONSP (CDR (CDR V2040)))
                        (AND (EQ 'shen.then (CAR (CDR (CDR V2040))))
                             (AND (CONSP (CDR (CDR (CDR V2040))))
                                  (AND (CONSP (CDR (CDR (CDR (CDR V2040)))))
                                       (AND
                                        (EQ 'shen.else
                                            (CAR
                                             (CDR (CDR (CDR (CDR V2040))))))
                                        (AND
                                         (CONSP
                                          (CDR (CDR (CDR (CDR (CDR V2040))))))
                                         (NULL
                                          (CDR
                                           (CDR
                                            (CDR
                                             (CDR
                                              (CDR (CDR V2040))))))))))))))))
    (CONS 'if
          (CONS (shen.aum_to_shen (CAR (CDR V2040)))
                (CONS (shen.aum_to_shen (CAR (CDR (CDR (CDR V2040)))))
                      (CONS
                       (shen.aum_to_shen
                        (CAR (CDR (CDR (CDR (CDR (CDR V2040)))))))
                       NIL)))))
   ((AND (CONSP V2040)
         (AND (CONSP (CDR V2040))
              (AND (EQ 'is (CAR (CDR V2040)))
                   (AND (CONSP (CDR (CDR V2040)))
                        (AND (EQ 'shen.a (CAR (CDR (CDR V2040))))
                             (AND (CONSP (CDR (CDR (CDR V2040))))
                                  (AND
                                   (EQ 'shen.variable
                                       (CAR (CDR (CDR (CDR V2040)))))
                                   (NULL (CDR (CDR (CDR (CDR V2040))))))))))))
    (CONS 'shen.pvar? (CONS (CAR V2040) NIL)))
   ((AND (CONSP V2040)
         (AND (CONSP (CDR V2040))
              (AND (EQ 'is (CAR (CDR V2040)))
                   (AND (CONSP (CDR (CDR V2040)))
                        (AND (EQ 'shen.a (CAR (CDR (CDR V2040))))
                             (AND (CONSP (CDR (CDR (CDR V2040))))
                                  (AND
                                   (EQ 'shen.non-empty
                                       (CAR (CDR (CDR (CDR V2040)))))
                                   (AND (CONSP (CDR (CDR (CDR (CDR V2040)))))
                                        (AND
                                         (EQ 'list
                                             (CAR
                                              (CDR (CDR (CDR (CDR V2040))))))
                                         (NULL
                                          (CDR
                                           (CDR
                                            (CDR (CDR (CDR V2040)))))))))))))))
    (CONS 'cons? (CONS (CAR V2040) NIL)))
   ((AND (CONSP V2040)
         (AND (EQ 'shen.rename (CAR V2040))
              (AND (CONSP (CDR V2040))
                   (AND (EQ 'shen.the (CAR (CDR V2040)))
                        (AND (CONSP (CDR (CDR V2040)))
                             (AND (EQ 'shen.variables (CAR (CDR (CDR V2040))))
                                  (AND (CONSP (CDR (CDR (CDR V2040))))
                                       (AND
                                        (EQ 'in (CAR (CDR (CDR (CDR V2040)))))
                                        (AND
                                         (CONSP (CDR (CDR (CDR (CDR V2040)))))
                                         (AND
                                          (NULL
                                           (CAR (CDR (CDR (CDR (CDR V2040))))))
                                          (AND
                                           (CONSP
                                            (CDR
                                             (CDR (CDR (CDR (CDR V2040))))))
                                           (AND
                                            (EQ 'and
                                                (CAR
                                                 (CDR
                                                  (CDR
                                                   (CDR (CDR (CDR V2040)))))))
                                            (AND
                                             (CONSP
                                              (CDR
                                               (CDR
                                                (CDR
                                                 (CDR (CDR (CDR V2040)))))))
                                             (AND
                                              (EQ 'shen.then
                                                  (CAR
                                                   (CDR
                                                    (CDR
                                                     (CDR
                                                      (CDR
                                                       (CDR (CDR V2040))))))))
                                              (AND
                                               (CONSP
                                                (CDR
                                                 (CDR
                                                  (CDR
                                                   (CDR
                                                    (CDR
                                                     (CDR (CDR V2040))))))))
                                               (NULL
                                                (CDR
                                                 (CDR
                                                  (CDR
                                                   (CDR
                                                    (CDR
                                                     (CDR
                                                      (CDR
                                                       (CDR
                                                        V2040))))))))))))))))))))))))
    (shen.aum_to_shen (CAR (CDR (CDR (CDR (CDR (CDR (CDR (CDR V2040))))))))))
   ((AND (CONSP V2040)
         (AND (EQ 'shen.rename (CAR V2040))
              (AND (CONSP (CDR V2040))
                   (AND (EQ 'shen.the (CAR (CDR V2040)))
                        (AND (CONSP (CDR (CDR V2040)))
                             (AND (EQ 'shen.variables (CAR (CDR (CDR V2040))))
                                  (AND (CONSP (CDR (CDR (CDR V2040))))
                                       (AND
                                        (EQ 'in (CAR (CDR (CDR (CDR V2040)))))
                                        (AND
                                         (CONSP (CDR (CDR (CDR (CDR V2040)))))
                                         (AND
                                          (CONSP
                                           (CAR (CDR (CDR (CDR (CDR V2040))))))
                                          (AND
                                           (CONSP
                                            (CDR
                                             (CDR (CDR (CDR (CDR V2040))))))
                                           (AND
                                            (EQ 'and
                                                (CAR
                                                 (CDR
                                                  (CDR
                                                   (CDR (CDR (CDR V2040)))))))
                                            (AND
                                             (CONSP
                                              (CDR
                                               (CDR
                                                (CDR
                                                 (CDR (CDR (CDR V2040)))))))
                                             (AND
                                              (EQ 'shen.then
                                                  (CAR
                                                   (CDR
                                                    (CDR
                                                     (CDR
                                                      (CDR
                                                       (CDR (CDR V2040))))))))
                                              (AND
                                               (CONSP
                                                (CDR
                                                 (CDR
                                                  (CDR
                                                   (CDR
                                                    (CDR
                                                     (CDR (CDR V2040))))))))
                                               (NULL
                                                (CDR
                                                 (CDR
                                                  (CDR
                                                   (CDR
                                                    (CDR
                                                     (CDR
                                                      (CDR
                                                       (CDR
                                                        V2040))))))))))))))))))))))))
    (CONS 'let
          (CONS (CAR (CAR (CDR (CDR (CDR (CDR V2040))))))
                (CONS (CONS 'shen.newpv (CONS 'ProcessN NIL))
                      (CONS
                       (shen.aum_to_shen
                        (CONS 'shen.rename
                              (CONS 'shen.the
                                    (CONS 'shen.variables
                                          (CONS 'in
                                                (CONS
                                                 (CDR
                                                  (CAR
                                                   (CDR
                                                    (CDR (CDR (CDR V2040))))))
                                                 (CDR
                                                  (CDR
                                                   (CDR
                                                    (CDR (CDR V2040)))))))))))
                       NIL)))))
   ((AND (CONSP V2040)
         (AND (EQ 'bind (CAR V2040))
              (AND (CONSP (CDR V2040))
                   (AND (CONSP (CDR (CDR V2040)))
                        (AND (EQ 'shen.to (CAR (CDR (CDR V2040))))
                             (AND (CONSP (CDR (CDR (CDR V2040))))
                                  (AND (CONSP (CDR (CDR (CDR (CDR V2040)))))
                                       (AND
                                        (EQ 'in
                                            (CAR
                                             (CDR (CDR (CDR (CDR V2040))))))
                                        (AND
                                         (CONSP
                                          (CDR (CDR (CDR (CDR (CDR V2040))))))
                                         (NULL
                                          (CDR
                                           (CDR
                                            (CDR
                                             (CDR
                                              (CDR (CDR V2040))))))))))))))))
    (CONS 'do
          (CONS
           (CONS 'shen.bindv
                 (CONS (CAR (CDR V2040))
                       (CONS (shen.chwild (CAR (CDR (CDR (CDR V2040)))))
                             (CONS 'ProcessN NIL))))
           (CONS
            (CONS 'let
                  (CONS 'Result
                        (CONS
                         (shen.aum_to_shen
                          (CAR (CDR (CDR (CDR (CDR (CDR V2040)))))))
                         (CONS
                          (CONS 'do
                                (CONS
                                 (CONS 'shen.unbindv
                                       (CONS (CAR (CDR V2040))
                                             (CONS 'ProcessN NIL)))
                                 (CONS 'Result NIL)))
                          NIL))))
            NIL))))
   ((AND (CONSP V2040)
         (AND (CONSP (CDR V2040))
              (AND (EQ 'is (CAR (CDR V2040)))
                   (AND (CONSP (CDR (CDR V2040)))
                        (AND (EQ 'identical (CAR (CDR (CDR V2040))))
                             (AND (CONSP (CDR (CDR (CDR V2040))))
                                  (AND
                                   (EQ 'shen.to (CAR (CDR (CDR (CDR V2040)))))
                                   (AND (CONSP (CDR (CDR (CDR (CDR V2040)))))
                                        (NULL
                                         (CDR
                                          (CDR
                                           (CDR (CDR (CDR V2040))))))))))))))
    (CONS '=
          (CONS (CAR (CDR (CDR (CDR (CDR V2040))))) (CONS (CAR V2040) NIL))))
   ((EQ 'shen.failed! V2040) 'false)
   ((AND (CONSP V2040)
         (AND (EQ 'shen.the (CAR V2040))
              (AND (CONSP (CDR V2040))
                   (AND (EQ 'head (CAR (CDR V2040)))
                        (AND (CONSP (CDR (CDR V2040)))
                             (AND (EQ 'shen.of (CAR (CDR (CDR V2040))))
                                  (AND (CONSP (CDR (CDR (CDR V2040))))
                                       (NULL
                                        (CDR (CDR (CDR (CDR V2040))))))))))))
    (CONS 'hd (CDR (CDR (CDR V2040)))))
   ((AND (CONSP V2040)
         (AND (EQ 'shen.the (CAR V2040))
              (AND (CONSP (CDR V2040))
                   (AND (EQ 'tail (CAR (CDR V2040)))
                        (AND (CONSP (CDR (CDR V2040)))
                             (AND (EQ 'shen.of (CAR (CDR (CDR V2040))))
                                  (AND (CONSP (CDR (CDR (CDR V2040))))
                                       (NULL
                                        (CDR (CDR (CDR (CDR V2040))))))))))))
    (CONS 'tl (CDR (CDR (CDR V2040)))))
   ((AND (CONSP V2040)
         (AND (EQ 'shen.pop (CAR V2040))
              (AND (CONSP (CDR V2040))
                   (AND (EQ 'shen.the (CAR (CDR V2040)))
                        (AND (CONSP (CDR (CDR V2040)))
                             (AND (EQ 'shen.stack (CAR (CDR (CDR V2040))))
                                  (NULL (CDR (CDR (CDR V2040))))))))))
    (CONS 'do
          (CONS (CONS 'shen.incinfs NIL)
                (CONS (CONS 'thaw (CONS 'Continuation NIL)) NIL))))
   ((AND (CONSP V2040)
         (AND (EQ 'call (CAR V2040))
              (AND (CONSP (CDR V2040))
                   (AND (EQ 'shen.the (CAR (CDR V2040)))
                        (AND (CONSP (CDR (CDR V2040)))
                             (AND
                              (EQ 'shen.continuation (CAR (CDR (CDR V2040))))
                              (AND (CONSP (CDR (CDR (CDR V2040))))
                                   (NULL (CDR (CDR (CDR (CDR V2040))))))))))))
    (CONS 'do
          (CONS (CONS 'shen.incinfs NIL)
                (CONS
                 (shen.call_the_continuation
                  (shen.chwild (CAR (CDR (CDR (CDR V2040))))) 'ProcessN
                  'Continuation)
                 NIL))))
   (T V2040)))

(DEFUN shen.chwild (V2042)
  (COND ((EQ V2042 '_) (CONS 'shen.newpv (CONS 'ProcessN NIL)))
        ((CONSP V2042) (map #'(LAMBDA (Z) (shen.chwild Z)) V2042)) (T V2042)))

(DEFUN shen.newpv (V2044)
  (LET ((Count+1 (shen.add (<-address shen.*varcounter* V2044) 1)))
    (LET ((IncVar (address-> shen.*varcounter* V2044 Count+1)))
      (LET ((Vector (<-address shen.*prologvectors* V2044)))
        (LET ((ResizeVectorIfNeeded
               (IF (shen.ABSEQUAL Count+1 (limit Vector))
                   (shen.resizeprocessvector V2044 Count+1)
                   'shen.skip)))
          (shen.mk-pvar Count+1))))))

(DEFUN shen.resizeprocessvector (V2047 V2048)
  (LET ((Vector (<-address shen.*prologvectors* V2047)))
    (LET ((BigVector
           (shen.resize-vector Vector (shen.add V2048 V2048) 'shen.-null-)))
      (address-> shen.*prologvectors* V2047 BigVector))))

(DEFUN shen.resize-vector (V2052 V2053 V2054)
  (LET ((BigVector (address-> (absvector (shen.add 1 V2053)) 0 V2053)))
    (shen.copy-vector V2052 BigVector (limit V2052) V2053 V2054)))

(DEFUN shen.copy-vector (V2060 V2061 V2062 V2063 V2064)
  (shen.copy-vector-stage-2 (shen.add 1 V2062) (shen.add V2063 1) V2064
   (shen.copy-vector-stage-1 1 V2060 V2061 (shen.add 1 V2062))))

(DEFUN shen.copy-vector-stage-1 (V2072 V2073 V2074 V2075)
  (COND ((shen.ABSEQUAL V2075 V2072) V2074)
        (T
         (shen.copy-vector-stage-1 (shen.add 1 V2072) V2073
          (address-> V2074 V2072 (<-address V2073 V2072)) V2075))))

(DEFUN shen.copy-vector-stage-2 (V2083 V2084 V2085 V2086)
  (COND ((shen.ABSEQUAL V2084 V2083) V2086)
        (T
         (shen.copy-vector-stage-2 (shen.add V2083 1) V2084 V2085
          (address-> V2086 V2083 V2085)))))

(DEFUN shen.mk-pvar (V2088)
  (address-> (address-> (absvector 2) 0 'shen.pvar) 1 V2088))

(DEFUN shen.pvar? (V2090)
  (and (absvector? V2090)
       (shen.equal? (<-address/or V2090 0 (freeze 'shen.not-pvar)) 'shen.pvar)))

(DEFUN shen.bindv (V2094 V2095 V2096)
  (LET ((Vector (<-address shen.*prologvectors* V2096)))
    (address-> Vector (<-address V2094 1) V2095)))

(DEFUN shen.unbindv (V2099 V2100)
  (LET ((Vector (<-address shen.*prologvectors* V2100)))
    (address-> Vector (<-address V2099 1) 'shen.-null-)))

(DEFUN shen.incinfs () (set 'shen.*infs* (shen.add 1 shen.*infs*)))

(DEFUN shen.call_the_continuation (V2104 V2105 V2106)
  (COND
   ((AND (CONSP V2104) (AND (CONSP (CAR V2104)) (NULL (CDR V2104))))
    (CONS (CAR (CAR V2104))
          (APPEND (CDR (CAR V2104)) (CONS V2105 (CONS V2106 NIL)))))
   ((AND (CONSP V2104) (CONSP (CAR V2104)))
    (LET ((NewContinuation (shen.newcontinuation (CDR V2104) V2105 V2106)))
      (CONS (CAR (CAR V2104))
            (APPEND (CDR (CAR V2104))
                    (CONS V2105 (CONS NewContinuation NIL))))))
   (T (shen.f_error 'shen.call_the_continuation))))

(DEFUN shen.newcontinuation (V2110 V2111 V2112)
  (COND ((NULL V2110) V2112)
        ((AND (CONSP V2110) (CONSP (CAR V2110)))
         (CONS 'freeze
               (CONS
                (CONS (CAR (CAR V2110))
                      (APPEND (CDR (CAR V2110))
                              (CONS V2111
                                    (CONS
                                     (shen.newcontinuation (CDR V2110) V2111
                                      V2112)
                                     NIL))))
                NIL)))
        (T (shen.f_error 'shen.newcontinuation))))

(DEFUN return (V2120 V2121 V2122) (shen.deref V2120 V2121))

(DEFUN shen.measure&return (V2130 V2131 V2132)
  (do
   (shen.prhush
    (shen.app shen.*infs* " inferences
"
     'shen.a)
    (stoutput))
   (shen.deref V2130 V2131)))

(DEFUN unify (V2137 V2138 V2139 V2140)
  (shen.lzy= (shen.lazyderef V2137 V2139) (shen.lazyderef V2138 V2139) V2139
   V2140))

(DEFUN shen.lzy= (V2162 V2163 V2164 V2165)
  (COND ((shen.ABSEQUAL V2163 V2162) (thaw V2165))
        ((shen-cl.true? (shen.pvar? V2162)) (bind V2162 V2163 V2164 V2165))
        ((shen-cl.true? (shen.pvar? V2163)) (bind V2163 V2162 V2164 V2165))
        ((AND (CONSP V2162) (CONSP V2163))
         (shen.lzy= (shen.lazyderef (CAR V2162) V2164)
          (shen.lazyderef (CAR V2163) V2164) V2164
          (freeze
           (shen.lzy= (shen.lazyderef (CDR V2162) V2164)
            (shen.lazyderef (CDR V2163) V2164) V2164 V2165))))
        (T 'false)))

(DEFUN shen.deref (V2168 V2169)
  (COND
   ((CONSP V2168)
    (CONS (shen.deref (CAR V2168) V2169) (shen.deref (CDR V2168) V2169)))
   (T
    (IF (shen-cl.true? (shen.pvar? V2168))
        (LET ((Value (shen.valvector V2168 V2169)))
          (IF (EQ Value 'shen.-null-)
              V2168
              (shen.deref Value V2169)))
        V2168))))

(DEFUN shen.lazyderef (V2172 V2173)
  (IF (shen-cl.true? (shen.pvar? V2172))
      (LET ((Value (shen.valvector V2172 V2173)))
        (IF (EQ Value 'shen.-null-)
            V2172
            (shen.lazyderef Value V2173)))
      V2172))

(DEFUN shen.valvector (V2176 V2177)
  (<-address (<-address shen.*prologvectors* V2177) (<-address V2176 1)))

(DEFUN unify! (V2182 V2183 V2184 V2185)
  (shen.lzy=! (shen.lazyderef V2182 V2184) (shen.lazyderef V2183 V2184) V2184
   V2185))

(DEFUN shen.lzy=! (V2207 V2208 V2209 V2210)
  (COND ((shen.ABSEQUAL V2208 V2207) (thaw V2210))
        ((AND (shen-cl.true? (shen.pvar? V2207))
              (NOT
               (shen-cl.true? (shen.occurs? V2207 (shen.deref V2208 V2209)))))
         (bind V2207 V2208 V2209 V2210))
        ((AND (shen-cl.true? (shen.pvar? V2208))
              (NOT
               (shen-cl.true? (shen.occurs? V2208 (shen.deref V2207 V2209)))))
         (bind V2208 V2207 V2209 V2210))
        ((AND (CONSP V2207) (CONSP V2208))
         (shen.lzy=! (shen.lazyderef (CAR V2207) V2209)
          (shen.lazyderef (CAR V2208) V2209) V2209
          (freeze
           (shen.lzy=! (shen.lazyderef (CDR V2207) V2209)
            (shen.lazyderef (CDR V2208) V2209) V2209 V2210))))
        (T 'false)))

(DEFUN shen.occurs? (V2222 V2223)
  (COND ((shen.ABSEQUAL V2223 V2222) 'true)
        ((CONSP V2223)
         (or (shen.occurs? V2222 (CAR V2223))
             (shen.occurs? V2222 (CDR V2223))))
        (T 'false)))

(DEFUN identical (V2228 V2229 V2230 V2231)
  (shen.lzy== (shen.lazyderef V2228 V2230) (shen.lazyderef V2229 V2230) V2230
   V2231))

(DEFUN shen.lzy== (V2253 V2254 V2255 V2256)
  (COND ((shen.ABSEQUAL V2254 V2253) (thaw V2256))
        ((AND (CONSP V2253) (CONSP V2254))
         (shen.lzy== (shen.lazyderef (CAR V2253) V2255)
          (shen.lazyderef (CAR V2254) V2255) V2255
          (freeze (shen.lzy== (CDR V2253) (CDR V2254) V2255 V2256))))
        (T 'false)))

(DEFUN shen.pvar (V2258) (cn "Var" (shen.app (<-address V2258 1) "" 'shen.a)))

(DEFUN bind (V2263 V2264 V2265 V2266)
  (do (shen.bindv V2263 V2264 V2265)
      (LET ((Result (thaw V2266)))
        (do (shen.unbindv V2263 V2265) Result))))

(DEFUN fwhen (V2284 V2285 V2286)
  (COND ((EQ 'true V2284) (thaw V2286)) ((EQ 'false V2284) 'false)
        (T
         (simple-error
          (cn "fwhen expects a boolean: not " (shen.app V2284 "%" 'shen.s))))))

(DEFUN call (V2302 V2303 V2304)
  (COND
   ((CONSP V2302)
    (shen.call-help (function (shen.lazyderef (CAR V2302) V2303)) (CDR V2302)
     V2303 V2304))
   (T 'false)))

(DEFUN shen.call-help (V2309 V2310 V2311 V2312)
  (COND ((NULL V2310) (shen.apply V2309 (LIST V2311 V2312)))
        ((CONSP V2310)
         (shen.call-help (shen.apply V2309 (LIST (CAR V2310))) (CDR V2310)
          V2311 V2312))
        (T (shen.f_error 'shen.call-help))))

(DEFUN shen.intprolog (V2314)
  (COND
   ((AND (CONSP V2314) (CONSP (CAR V2314)))
    (LET ((ProcessN (shen.start-new-prolog-process)))
      (shen.intprolog-help (CAR (CAR V2314))
       (shen.insert-prolog-variables
        (CONS (CDR (CAR V2314)) (CONS (CDR V2314) NIL)) ProcessN)
       ProcessN)))
   (T (shen.f_error 'shen.intprolog))))

(DEFUN shen.intprolog-help (V2318 V2319 V2320)
  (COND
   ((AND (CONSP V2319) (AND (CONSP (CDR V2319)) (NULL (CDR (CDR V2319)))))
    (shen.intprolog-help-help V2318 (CAR V2319) (CAR (CDR V2319)) V2320))
   (T (shen.f_error 'shen.intprolog-help))))

(DEFUN shen.intprolog-help-help (V2325 V2326 V2327 V2328)
  (COND
   ((NULL V2326)
    (shen.apply V2325 (LIST V2328 (freeze (shen.call-rest V2327 V2328)))))
   ((CONSP V2326)
    (shen.intprolog-help-help (shen.apply V2325 (LIST (CAR V2326))) (CDR V2326)
     V2327 V2328))
   (T (shen.f_error 'shen.intprolog-help-help))))

(DEFUN shen.call-rest (V2333 V2334)
  (COND ((NULL V2333) 'true)
        ((AND (CONSP V2333)
              (AND (CONSP (CAR V2333)) (CONSP (CDR (CAR V2333)))))
         (shen.call-rest
          (CONS
           (CONS (shen.apply (CAR (CAR V2333)) (LIST (CAR (CDR (CAR V2333)))))
                 (CDR (CDR (CAR V2333))))
           (CDR V2333))
          V2334))
        ((AND (CONSP V2333) (AND (CONSP (CAR V2333)) (NULL (CDR (CAR V2333)))))
         (shen.apply (CAR (CAR V2333))
                     (LIST V2334 (freeze (shen.call-rest (CDR V2333) V2334)))))
        (T (shen.f_error 'shen.call-rest))))

(DEFUN shen.start-new-prolog-process ()
  (LET ((IncrementProcessCounter
         (set 'shen.*process-counter* (shen.add 1 shen.*process-counter*))))
    (shen.initialise-prolog IncrementProcessCounter)))

(DEFUN shen.insert-prolog-variables (V2337 V2338)
  (shen.insert-prolog-variables-help V2337 (shen.flatten V2337) V2338))

(DEFUN shen.insert-prolog-variables-help (V2346 V2347 V2348)
  (COND ((NULL V2347) V2346)
        ((AND (CONSP V2347) (shen-cl.true? (variable? (CAR V2347))))
         (LET ((V (shen.newpv V2348)))
           (LET ((XV/Y (subst V (CAR V2347) V2346)))
             (LET ((Z-Y (remove (CAR V2347) (CDR V2347))))
               (shen.insert-prolog-variables-help XV/Y Z-Y V2348)))))
        ((CONSP V2347)
         (shen.insert-prolog-variables-help V2346 (CDR V2347) V2348))
        (T (shen.f_error 'shen.insert-prolog-variables-help))))

(DEFUN shen.initialise-prolog (V2350)
  (LET ((Vector
         (address-> shen.*prologvectors* V2350
                    (shen.fillvector (vector 10) 1 10 'shen.-null-))))
    (LET ((Counter (address-> shen.*varcounter* V2350 1)))
      V2350)))

