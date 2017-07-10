
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

(DEFUN load (V1667)
  (LET ((Load
         (LET ((Start (get-time 'run)))
           (LET ((Result (shen.load-help shen.*tc* (read-file V1667))))
             (LET ((Finish (get-time 'run)))
               (LET ((Time (shen.subtract Finish Start)))
                 (LET ((Message
                        (shen.prhush
                         (cn "
run time: "
                             (cn (str Time) " secs
"))
                         (stoutput))))
                   Result)))))))
    (LET ((Infs
           (IF (shen-cl.true? shen.*tc*)
               (shen.prhush
                (cn "
typechecked in "
                    (shen.app (inferences) " inferences
"
                     'shen.a))
                (stoutput))
               'shen.skip)))
      'loaded)))

(DEFUN shen.load-help (V1674 V1675)
  (COND
   ((EQ 'false V1674)
    (for-each
     #'(LAMBDA (X)
         (shen.prhush
          (shen.app (shen.eval-without-macros X) "
"
           'shen.s)
          (stoutput)))
     V1675))
   (T
    (LET ((RemoveSynonyms
           (mapcan #'(LAMBDA (X) (shen.remove-synonyms X)) V1675)))
      (LET ((Table (mapcan #'(LAMBDA (X) (shen.typetable X)) RemoveSynonyms)))
        (LET ((Assume (for-each #'(LAMBDA (X) (shen.assumetype X)) Table)))
          (trap-error
           (for-each #'(LAMBDA (X) (shen.typecheck-and-load X)) RemoveSynonyms)
           #'(LAMBDA (E) (shen.unwind-types E Table)))))))))

(DEFUN shen.remove-synonyms (V1677)
  (COND
   ((AND (CONSP V1677) (EQ 'shen.synonyms-help (CAR V1677)))
    (do (eval V1677) NIL))
   (T (CONS V1677 NIL))))

(DEFUN shen.typecheck-and-load (V1679)
  (do (nl 1) (shen.typecheck-and-evaluate V1679 (gensym 'A))))

(DEFUN shen.typetable (V1685)
  (COND
   ((AND (CONSP V1685) (AND (EQ 'define (CAR V1685)) (CONSP (CDR V1685))))
    (LET ((Sig
           (compile #'(LAMBDA (Y) (shen.<sig+rest> Y)) (CDR (CDR V1685))
                    #'(LAMBDA (E)
                        (simple-error
                         (shen.app (CAR (CDR V1685)) " lacks a proper signature.
"
                          'shen.a))))))
      (CONS (CONS (CAR (CDR V1685)) Sig) NIL)))
   (T NIL)))

(DEFUN shen.assumetype (V1687)
  (COND ((CONSP V1687) (declare (CAR V1687) (CDR V1687)))
        (T (shen.f_error 'shen.assumetype))))

(DEFUN shen.unwind-types (V1694 V1695)
  (COND ((NULL V1695) (simple-error (error-to-string V1694)))
        ((AND (CONSP V1695) (CONSP (CAR V1695)))
         (do (shen.remtype (CAR (CAR V1695)))
             (shen.unwind-types V1694 (CDR V1695))))
        (T (shen.f_error 'shen.unwind-types))))

(DEFUN shen.remtype (V1697)
  (set 'shen.*signedfuncs* (shen.removetype V1697 shen.*signedfuncs*)))

(DEFUN shen.removetype (V1705 V1706)
  (COND ((NULL V1706) NIL)
        ((AND (CONSP V1706)
              (AND (CONSP (CAR V1706))
                   (shen.ABSEQUAL (CAR (CAR V1706)) V1705)))
         (shen.removetype (CAR (CAR V1706)) (CDR V1706)))
        ((CONSP V1706) (CONS (CAR V1706) (shen.removetype V1705 (CDR V1706))))
        (T (shen.f_error 'shen.removetype))))

(DEFUN shen.<sig+rest> (V1708)
  (LET ((Parse_shen.<signature> (shen.<signature> V1708)))
    (IF (NOT (EQ (fail) Parse_shen.<signature>))
        (LET ((Parse_<!> (<!> Parse_shen.<signature>)))
          (IF (NOT (EQ (fail) Parse_<!>))
              (shen.pair (CAR Parse_<!>) (shen.hdtl Parse_shen.<signature>))
              (fail)))
        (fail))))

(DEFUN write-to-file (V1711 V1712)
  (LET ((Stream (open V1711 'out)))
    (LET ((String
           (IF (STRINGP V1712)
               (shen.app V1712 "

"
                'shen.a)
               (shen.app V1712 "

"
                'shen.s))))
      (LET ((Write (pr String Stream)))
        (LET ((Close (close Stream)))
          V1712)))))

