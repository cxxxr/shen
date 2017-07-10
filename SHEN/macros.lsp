
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

(DEFUN macroexpand (V1714)
  (LET ((Y (shen.compose *macros* V1714)))
    (IF (shen.ABSEQUAL V1714 Y)
        V1714
        (shen.walk #'(LAMBDA (Z) (macroexpand Z)) Y))))

(DEFUN shen.error-macro (V1716)
  (COND
   ((AND (CONSP V1716) (AND (EQ 'error (CAR V1716)) (CONSP (CDR V1716))))
    (CONS 'simple-error
          (CONS (shen.mkstr (CAR (CDR V1716)) (CDR (CDR V1716))) NIL)))
   (T V1716)))

(DEFUN shen.output-macro (V1718)
  (COND
   ((AND (CONSP V1718) (AND (EQ 'output (CAR V1718)) (CONSP (CDR V1718))))
    (CONS 'shen.prhush
          (CONS (shen.mkstr (CAR (CDR V1718)) (CDR (CDR V1718)))
                (CONS (CONS 'stoutput NIL) NIL))))
   ((AND (CONSP V1718)
         (AND (EQ 'pr (CAR V1718))
              (AND (CONSP (CDR V1718)) (NULL (CDR (CDR V1718))))))
    (CONS 'pr (CONS (CAR (CDR V1718)) (CONS (CONS 'stoutput NIL) NIL))))
   (T V1718)))

(DEFUN shen.make-string-macro (V1720)
  (COND
   ((AND (CONSP V1720) (AND (EQ 'make-string (CAR V1720)) (CONSP (CDR V1720))))
    (shen.mkstr (CAR (CDR V1720)) (CDR (CDR V1720))))
   (T V1720)))

(DEFUN shen.input-macro (V1722)
  (COND
   ((AND (CONSP V1722) (AND (EQ 'lineread (CAR V1722)) (NULL (CDR V1722))))
    (CONS 'lineread (CONS (CONS 'stinput NIL) NIL)))
   ((AND (CONSP V1722) (AND (EQ 'input (CAR V1722)) (NULL (CDR V1722))))
    (CONS 'input (CONS (CONS 'stinput NIL) NIL)))
   ((AND (CONSP V1722) (AND (EQ 'read (CAR V1722)) (NULL (CDR V1722))))
    (CONS 'read (CONS (CONS 'stinput NIL) NIL)))
   ((AND (CONSP V1722)
         (AND (EQ 'input+ (CAR V1722))
              (AND (CONSP (CDR V1722)) (NULL (CDR (CDR V1722))))))
    (CONS 'input+ (CONS (CAR (CDR V1722)) (CONS (CONS 'stinput NIL) NIL))))
   ((AND (CONSP V1722) (AND (EQ 'read-byte (CAR V1722)) (NULL (CDR V1722))))
    (CONS 'read-byte (CONS (CONS 'stinput NIL) NIL)))
   ((AND (CONSP V1722)
         (AND (EQ 'read-char-code (CAR V1722)) (NULL (CDR V1722))))
    (CONS 'read-char-code (CONS (CONS 'stinput NIL) NIL)))
   (T V1722)))

(DEFUN shen.compose (V1725 V1726)
  (COND ((NULL V1725) V1726)
        ((CONSP V1725)
         (shen.compose (CDR V1725) (shen.apply (CAR V1725) (LIST V1726))))
        (T (shen.f_error 'shen.compose))))

(DEFUN shen.compile-macro (V1728)
  (COND
   ((AND (CONSP V1728)
         (AND (EQ 'compile (CAR V1728))
              (AND (CONSP (CDR V1728))
                   (AND (CONSP (CDR (CDR V1728)))
                        (NULL (CDR (CDR (CDR V1728))))))))
    (CONS 'compile
          (CONS (CAR (CDR V1728))
                (CONS (CAR (CDR (CDR V1728)))
                      (CONS
                       (CONS 'lambda
                             (CONS 'E
                                   (CONS
                                    (CONS 'if
                                          (CONS (CONS 'cons? (CONS 'E NIL))
                                                (CONS
                                                 (CONS 'error
                                                       (CONS
                                                        "parse error here: ~S~%"
                                                        (CONS 'E NIL)))
                                                 (CONS
                                                  (CONS 'error
                                                        (CONS "parse error~%"
                                                              NIL))
                                                  NIL))))
                                    NIL)))
                       NIL)))))
   (T V1728)))

(DEFUN shen.prolog-macro (V1730)
  (COND
   ((AND (CONSP V1730) (EQ 'prolog? (CAR V1730)))
    (LET ((F (gensym 'shen.f)))
      (LET ((Receive (shen.receive-terms (CDR V1730))))
        (LET ((PrologDef
               (eval
                (APPEND (CONS 'defprolog (CONS F NIL))
                        (APPEND Receive
                                (APPEND (CONS '<-- NIL)
                                        (APPEND
                                         (shen.pass-literals (CDR V1730))
                                         (CONS '|;| NIL))))))))
          (LET ((Query
                 (CONS F
                       (APPEND Receive
                               (CONS (CONS 'shen.start-new-prolog-process NIL)
                                     (CONS (CONS 'freeze (CONS 'true NIL))
                                           NIL))))))
            Query)))))
   (T V1730)))

(DEFUN shen.receive-terms (V1736)
  (COND ((NULL V1736) NIL)
        ((AND (CONSP V1736)
              (AND (CONSP (CAR V1736))
                   (AND (EQ 'receive (CAR (CAR V1736)))
                        (AND (CONSP (CDR (CAR V1736)))
                             (NULL (CDR (CDR (CAR V1736))))))))
         (CONS (CAR (CDR (CAR V1736))) (shen.receive-terms (CDR V1736))))
        ((CONSP V1736) (shen.receive-terms (CDR V1736)))
        (T (shen.f_error 'shen.receive-terms))))

(DEFUN shen.pass-literals (V1740)
  (COND ((NULL V1740) NIL)
        ((AND (CONSP V1740)
              (AND (CONSP (CAR V1740))
                   (AND (EQ 'receive (CAR (CAR V1740)))
                        (AND (CONSP (CDR (CAR V1740)))
                             (NULL (CDR (CDR (CAR V1740))))))))
         (shen.pass-literals (CDR V1740)))
        ((CONSP V1740) (CONS (CAR V1740) (shen.pass-literals (CDR V1740))))
        (T (shen.f_error 'shen.pass-literals))))

(DEFUN shen.defprolog-macro (V1742)
  (COND
   ((AND (CONSP V1742) (AND (EQ 'defprolog (CAR V1742)) (CONSP (CDR V1742))))
    (compile #'(LAMBDA (Y) (shen.<defprolog> Y)) (CDR V1742)
             #'(LAMBDA (Y) (shen.prolog-error (CAR (CDR V1742)) Y))))
   (T V1742)))

(DEFUN shen.datatype-macro (V1744)
  (COND
   ((AND (CONSP V1744) (AND (EQ 'datatype (CAR V1744)) (CONSP (CDR V1744))))
    (CONS 'shen.process-datatype
          (CONS (shen.intern-type (CAR (CDR V1744)))
                (CONS
                 (CONS 'compile
                       (CONS
                        (CONS 'lambda
                              (CONS 'X
                                    (CONS
                                     (CONS 'shen.<datatype-rules>
                                           (CONS 'X NIL))
                                     NIL)))
                        (CONS (shen.rcons_form (CDR (CDR V1744)))
                              (CONS
                               (CONS 'function (CONS 'shen.datatype-error NIL))
                               NIL))))
                 NIL))))
   (T V1744)))

(DEFUN shen.intern-type (V1746) (intern (cn "type#" (str V1746))))

(DEFUN shen.@s-macro (V1748)
  (COND
   ((AND (CONSP V1748)
         (AND (EQ '@s (CAR V1748))
              (AND (CONSP (CDR V1748))
                   (AND (CONSP (CDR (CDR V1748)))
                        (CONSP (CDR (CDR (CDR V1748))))))))
    (CONS '@s
          (CONS (CAR (CDR V1748))
                (CONS (shen.@s-macro (CONS '@s (CDR (CDR V1748)))) NIL))))
   ((AND (CONSP V1748)
         (AND (EQ '@s (CAR V1748))
              (AND (CONSP (CDR V1748))
                   (AND (CONSP (CDR (CDR V1748)))
                        (AND (NULL (CDR (CDR (CDR V1748))))
                             (STRINGP (CAR (CDR V1748))))))))
    (LET ((E (explode (CAR (CDR V1748)))))
      (IF (> (length E) 1)
          (shen.@s-macro (CONS '@s (APPEND E (CDR (CDR V1748)))))
          V1748)))
   (T V1748)))

(DEFUN shen.synonyms-macro (V1750)
  (COND
   ((AND (CONSP V1750) (EQ 'synonyms (CAR V1750)))
    (CONS 'shen.synonyms-help
          (CONS (shen.rcons_form (shen.curry-synonyms (CDR V1750))) NIL)))
   (T V1750)))

(DEFUN shen.curry-synonyms (V1752)
  (map #'(LAMBDA (X) (shen.curry-type X)) V1752))

(DEFUN shen.nl-macro (V1754)
  (COND
   ((AND (CONSP V1754) (AND (EQ 'nl (CAR V1754)) (NULL (CDR V1754))))
    (CONS 'nl (CONS 1 NIL)))
   (T V1754)))

(DEFUN shen.assoc-macro (V1756)
  (COND
   ((AND (CONSP V1756)
         (AND (CONSP (CDR V1756))
              (AND (CONSP (CDR (CDR V1756)))
                   (AND (CONSP (CDR (CDR (CDR V1756))))
                        (shen-cl.true?
                         (element? (CAR V1756)
                                   (CONS '@p
                                         (CONS '@v
                                               (CONS 'append
                                                     (CONS 'and
                                                           (CONS 'or
                                                                 (CONS '+
                                                                       (CONS '*
                                                                             (CONS
                                                                              'do
                                                                              NIL))))))))))))))
    (CONS (CAR V1756)
          (CONS (CAR (CDR V1756))
                (CONS (shen.assoc-macro (CONS (CAR V1756) (CDR (CDR V1756))))
                      NIL))))
   (T V1756)))

(DEFUN shen.let-macro (V1758)
  (COND
   ((AND (CONSP V1758)
         (AND (EQ 'let (CAR V1758))
              (AND (CONSP (CDR V1758))
                   (AND (CONSP (CDR (CDR V1758)))
                        (AND (CONSP (CDR (CDR (CDR V1758))))
                             (CONSP (CDR (CDR (CDR (CDR V1758))))))))))
    (CONS 'let
          (CONS (CAR (CDR V1758))
                (CONS (CAR (CDR (CDR V1758)))
                      (CONS
                       (shen.let-macro (CONS 'let (CDR (CDR (CDR V1758)))))
                       NIL)))))
   (T V1758)))

(DEFUN shen.abs-macro (V1760)
  (COND
   ((AND (CONSP V1760)
         (AND (EQ '/. (CAR V1760))
              (AND (CONSP (CDR V1760))
                   (AND (CONSP (CDR (CDR V1760)))
                        (CONSP (CDR (CDR (CDR V1760))))))))
    (CONS 'lambda
          (CONS (CAR (CDR V1760))
                (CONS (shen.abs-macro (CONS '/. (CDR (CDR V1760)))) NIL))))
   ((AND (CONSP V1760)
         (AND (EQ '/. (CAR V1760))
              (AND (CONSP (CDR V1760))
                   (AND (CONSP (CDR (CDR V1760)))
                        (NULL (CDR (CDR (CDR V1760))))))))
    (CONS 'lambda (CDR V1760)))
   (T V1760)))

(DEFUN shen.cases-macro (V1764)
  (COND
   ((AND (CONSP V1764)
         (AND (EQ 'cases (CAR V1764))
              (AND (CONSP (CDR V1764))
                   (AND (EQ 'true (CAR (CDR V1764)))
                        (CONSP (CDR (CDR V1764)))))))
    (CAR (CDR (CDR V1764))))
   ((AND (CONSP V1764)
         (AND (EQ 'cases (CAR V1764))
              (AND (CONSP (CDR V1764))
                   (AND (CONSP (CDR (CDR V1764)))
                        (NULL (CDR (CDR (CDR V1764))))))))
    (CONS 'if
          (CONS (CAR (CDR V1764))
                (CONS (CAR (CDR (CDR V1764)))
                      (CONS
                       (CONS 'simple-error (CONS "error: cases exhausted" NIL))
                       NIL)))))
   ((AND (CONSP V1764)
         (AND (EQ 'cases (CAR V1764))
              (AND (CONSP (CDR V1764)) (CONSP (CDR (CDR V1764))))))
    (CONS 'if
          (CONS (CAR (CDR V1764))
                (CONS (CAR (CDR (CDR V1764)))
                      (CONS
                       (shen.cases-macro (CONS 'cases (CDR (CDR (CDR V1764)))))
                       NIL)))))
   ((AND (CONSP V1764)
         (AND (EQ 'cases (CAR V1764))
              (AND (CONSP (CDR V1764)) (NULL (CDR (CDR V1764))))))
    (simple-error "error: odd number of case elements
"))
   (T V1764)))

(DEFUN shen.timer-macro (V1766)
  (COND
   ((AND (CONSP V1766)
         (AND (EQ 'time (CAR V1766))
              (AND (CONSP (CDR V1766)) (NULL (CDR (CDR V1766))))))
    (shen.let-macro
     (CONS 'let
           (CONS 'Start
                 (CONS (CONS 'get-time (CONS 'run NIL))
                       (CONS 'Result
                             (CONS (CAR (CDR V1766))
                                   (CONS 'Finish
                                         (CONS (CONS 'get-time (CONS 'run NIL))
                                               (CONS 'Time
                                                     (CONS
                                                      (CONS '-
                                                            (CONS 'Finish
                                                                  (CONS 'Start
                                                                        NIL)))
                                                      (CONS 'Message
                                                            (CONS
                                                             (CONS 'shen.prhush
                                                                   (CONS
                                                                    (CONS 'cn
                                                                          (CONS
                                                                           "
run time: "
                                                                           (CONS
                                                                            (CONS
                                                                             'cn
                                                                             (CONS
                                                                              (CONS
                                                                               'str
                                                                               (CONS
                                                                                'Time
                                                                                NIL))
                                                                              (CONS
                                                                               " secs
"
                                                                               NIL)))
                                                                            NIL)))
                                                                    (CONS
                                                                     (CONS
                                                                      'stoutput
                                                                      NIL)
                                                                     NIL)))
                                                             (CONS 'Result
                                                                   NIL))))))))))))))
   (T V1766)))

(DEFUN shen.tuple-up (V1768)
  (COND
   ((CONSP V1768)
    (CONS '@p (CONS (CAR V1768) (CONS (shen.tuple-up (CDR V1768)) NIL))))
   (T V1768)))

(DEFUN shen.put/get-macro (V1770)
  (COND
   ((AND (CONSP V1770)
         (AND (EQ 'put (CAR V1770))
              (AND (CONSP (CDR V1770))
                   (AND (CONSP (CDR (CDR V1770)))
                        (AND (CONSP (CDR (CDR (CDR V1770))))
                             (NULL (CDR (CDR (CDR (CDR V1770))))))))))
    (CONS 'put
          (CONS (CAR (CDR V1770))
                (CONS (CAR (CDR (CDR V1770)))
                      (CONS (CAR (CDR (CDR (CDR V1770))))
                            (CONS (CONS 'value (CONS '*property-vector* NIL))
                                  NIL))))))
   ((AND (CONSP V1770)
         (AND (EQ 'get (CAR V1770))
              (AND (CONSP (CDR V1770))
                   (AND (CONSP (CDR (CDR V1770)))
                        (NULL (CDR (CDR (CDR V1770))))))))
    (CONS 'get
          (CONS (CAR (CDR V1770))
                (CONS (CAR (CDR (CDR V1770)))
                      (CONS (CONS 'value (CONS '*property-vector* NIL))
                            NIL)))))
   ((AND (CONSP V1770)
         (AND (EQ 'get/or (CAR V1770))
              (AND (CONSP (CDR V1770))
                   (AND (CONSP (CDR (CDR V1770)))
                        (AND (CONSP (CDR (CDR (CDR V1770))))
                             (NULL (CDR (CDR (CDR (CDR V1770))))))))))
    (CONS 'get/or
          (CONS (CAR (CDR V1770))
                (CONS (CAR (CDR (CDR V1770)))
                      (CONS (CAR (CDR (CDR (CDR V1770))))
                            (CONS (CONS 'value (CONS '*property-vector* NIL))
                                  NIL))))))
   ((AND (CONSP V1770)
         (AND (EQ 'unput (CAR V1770))
              (AND (CONSP (CDR V1770))
                   (AND (CONSP (CDR (CDR V1770)))
                        (NULL (CDR (CDR (CDR V1770))))))))
    (CONS 'unput
          (CONS (CAR (CDR V1770))
                (CONS (CAR (CDR (CDR V1770)))
                      (CONS (CONS 'value (CONS '*property-vector* NIL))
                            NIL)))))
   (T V1770)))

(DEFUN shen.function-macro (V1772)
  (COND
   ((AND (CONSP V1772)
         (AND (EQ 'function (CAR V1772))
              (AND (CONSP (CDR V1772)) (NULL (CDR (CDR V1772))))))
    (shen.function-abstraction (CAR (CDR V1772)) (arity (CAR (CDR V1772)))))
   (T V1772)))

(DEFUN shen.function-abstraction (V1775 V1776)
  (COND
   ((IF (NUMBERP V1776)
        (= V1776 0))
    (simple-error
     (shen.app V1775 " has no lambda form
"
               'shen.a)))
   ((IF (NUMBERP V1776)
        (= V1776 -1))
    (CONS 'function (CONS V1775 NIL)))
   (T (shen.function-abstraction-help V1775 V1776 NIL))))

(DEFUN shen.function-abstraction-help (V1780 V1781 V1782)
  (COND
   ((IF (NUMBERP V1781)
        (= V1781 0))
    (CONS V1780 V1782))
   (T
    (LET ((X (gensym 'V)))
      (CONS '/.
            (CONS X
                  (CONS
                   (shen.function-abstraction-help V1780
                    (shen.subtract V1781 1) (APPEND V1782 (CONS X NIL)))
                   NIL)))))))

(DEFUN undefmacro (V1784)
  (LET ((MacroReg shen.*macroreg*))
    (LET ((Pos (shen.findpos V1784 MacroReg)))
      (LET ((Remove1 (set 'shen.*macroreg* (remove V1784 MacroReg))))
        (LET ((Remove2 (set '*macros* (shen.remove-nth Pos *macros*))))
          V1784)))))

(DEFUN shen.findpos (V1794 V1795)
  (COND
   ((NULL V1795)
    (simple-error
     (shen.app V1794 " is not a macro
"
               'shen.a)))
   ((AND (CONSP V1795) (shen.ABSEQUAL (CAR V1795) V1794)) 1)
   ((CONSP V1795) (shen.add 1 (shen.findpos V1794 (CDR V1795))))
   (T (shen.f_error 'shen.findpos))))

(DEFUN shen.remove-nth (V1800 V1801)
  (COND
   ((AND
     (IF (NUMBERP V1800)
         (= V1800 1))
     (CONSP V1801))
    (CDR V1801))
   ((CONSP V1801)
    (CONS (CAR V1801) (shen.remove-nth (shen.subtract V1800 1) (CDR V1801))))
   (T (shen.f_error 'shen.remove-nth))))

