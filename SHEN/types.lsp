
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

(DEFUN declare (V4177 V4178)
  (LET ((Record
         (set 'shen.*signedfuncs*
              (CONS (CONS V4177 V4178) shen.*signedfuncs*))))
    (LET ((Variancy
           (trap-error (shen.variancy-test V4177 V4178)
                       #'(LAMBDA (E) 'shen.skip))))
      (LET ((Type (shen.rcons_form (shen.demodulate V4178))))
        (LET ((F* (concat 'shen.type-signature-of- V4177)))
          (LET ((Parameters (shen.parameters 1)))
            (LET ((Clause
                   (CONS (CONS F* (CONS 'X NIL))
                         (CONS '|:-|
                               (CONS
                                (CONS (CONS 'unify! (CONS 'X (CONS Type NIL)))
                                      NIL)
                                NIL)))))
              (LET ((AUM_instruction (shen.aum Clause Parameters)))
                (LET ((Code (shen.aum_to_shen AUM_instruction)))
                  (LET ((ShenDef
                         (CONS 'define
                               (CONS F*
                                     (APPEND Parameters
                                             (APPEND
                                              (CONS 'ProcessN
                                                    (CONS 'Continuation NIL))
                                              (CONS '-> (CONS Code NIL))))))))
                    (LET ((Eval (shen.eval-without-macros ShenDef)))
                      V4177)))))))))))

(DEFUN shen.demodulate (V4180)
  (LET ((Demod (shen.walk shen.*demodulation-function* V4180)))
    (IF (shen.ABSEQUAL Demod V4180)
        V4180
        (shen.demodulate Demod))))

(DEFUN shen.variancy-test (V4183 V4184)
  (LET ((TypeF (shen.typecheck V4183 'B)))
    (LET ((Check
           (IF (EQ 'symbol TypeF)
               'shen.skip
               (IF (shen-cl.true? (shen.variant? TypeF V4184))
                   'shen.skip
                   (shen.prhush
                    (cn "warning: changing the type of "
                        (shen.app V4183 " may create errors
"
                                  'shen.a))
                    (stoutput))))))
      'shen.skip)))

(DEFUN shen.variant? (V4197 V4198)
  (COND ((shen.ABSEQUAL V4198 V4197) 'true)
        ((AND (CONSP V4197)
              (AND (CONSP V4198) (shen.ABSEQUAL (CAR V4198) (CAR V4197))))
         (shen.variant? (CDR V4197) (CDR V4198)))
        ((AND (CONSP V4197)
              (AND (CONSP V4198)
                   (AND (shen-cl.true? (shen.pvar? (CAR V4197)))
                        (shen-cl.true? (variable? (CAR V4198))))))
         (shen.variant? (subst 'shen.a (CAR V4197) (CDR V4197))
          (subst 'shen.a (CAR V4198) (CDR V4198))))
        ((AND (CONSP V4197)
              (AND (CONSP (CAR V4197))
                   (AND (CONSP V4198) (CONSP (CAR V4198)))))
         (shen.variant? (APPEND (CAR V4197) (CDR V4197))
          (APPEND (CAR V4198) (CDR V4198))))
        (T 'false)))

(declare 'absvector? (CONS 'A (CONS '--> (CONS 'boolean NIL))))

(declare 'adjoin
 (CONS 'A
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS 'A NIL))
                    (CONS '--> (CONS (CONS 'list (CONS 'A NIL)) NIL)))
              NIL))))

(declare 'and
 (CONS 'boolean
       (CONS '--> (CONS (CONS 'boolean (CONS '--> (CONS 'boolean NIL))) NIL))))

(declare 'shen.app
 (CONS 'A
       (CONS '-->
             (CONS
              (CONS 'string
                    (CONS '-->
                          (CONS (CONS 'symbol (CONS '--> (CONS 'string NIL)))
                                NIL)))
              NIL))))

(declare 'append
 (CONS (CONS 'list (CONS 'A NIL))
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS 'A NIL))
                    (CONS '--> (CONS (CONS 'list (CONS 'A NIL)) NIL)))
              NIL))))

(declare 'arity (CONS 'A (CONS '--> (CONS 'number NIL))))

(declare 'assoc
 (CONS 'A
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS (CONS 'list (CONS 'A NIL)) NIL))
                    (CONS '--> (CONS (CONS 'list (CONS 'A NIL)) NIL)))
              NIL))))

(declare 'boolean? (CONS 'A (CONS '--> (CONS 'boolean NIL))))

(declare 'bound? (CONS 'symbol (CONS '--> (CONS 'boolean NIL))))

(declare 'cd (CONS 'string (CONS '--> (CONS 'string NIL))))

(declare 'close
 (CONS (CONS 'stream (CONS 'A NIL))
       (CONS '--> (CONS (CONS 'list (CONS 'B NIL)) NIL))))

(declare 'cn
 (CONS 'string
       (CONS '--> (CONS (CONS 'string (CONS '--> (CONS 'string NIL))) NIL))))

(declare 'command-line (CONS '--> (CONS (CONS 'list (CONS 'string NIL)) NIL)))

(declare 'compile
 (CONS (CONS 'A (CONS 'shen.==> (CONS 'B NIL)))
       (CONS '-->
             (CONS
              (CONS 'A
                    (CONS '-->
                          (CONS
                           (CONS (CONS 'A (CONS '--> (CONS 'B NIL)))
                                 (CONS '--> (CONS 'B NIL)))
                           NIL)))
              NIL))))

(declare 'cons? (CONS 'A (CONS '--> (CONS 'boolean NIL))))

(declare 'destroy
 (CONS (CONS 'A (CONS '--> (CONS 'B NIL))) (CONS '--> (CONS 'symbol NIL))))

(declare 'difference
 (CONS (CONS 'list (CONS 'A NIL))
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS 'A NIL))
                    (CONS '--> (CONS (CONS 'list (CONS 'A NIL)) NIL)))
              NIL))))

(declare 'do
 (CONS 'A (CONS '--> (CONS (CONS 'B (CONS '--> (CONS 'B NIL))) NIL))))

(declare '<e>
 (CONS (CONS 'list (CONS 'A NIL))
       (CONS 'shen.==> (CONS (CONS 'list (CONS 'B NIL)) NIL))))

(declare '<!>
 (CONS (CONS 'list (CONS 'A NIL))
       (CONS 'shen.==> (CONS (CONS 'list (CONS 'A NIL)) NIL))))

(declare 'element?
 (CONS 'A
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS 'A NIL)) (CONS '--> (CONS 'boolean NIL)))
              NIL))))

(declare 'empty? (CONS 'A (CONS '--> (CONS 'boolean NIL))))

(declare 'enable-type-theory (CONS 'symbol (CONS '--> (CONS 'boolean NIL))))

(declare 'external
 (CONS 'symbol (CONS '--> (CONS (CONS 'list (CONS 'symbol NIL)) NIL))))

(declare 'error-to-string (CONS 'exception (CONS '--> (CONS 'string NIL))))

(declare 'explode
 (CONS 'A (CONS '--> (CONS (CONS 'list (CONS 'string NIL)) NIL))))

(declare 'fail (CONS '--> (CONS 'symbol NIL)))

(declare 'fail-if
 (CONS (CONS 'symbol (CONS '--> (CONS 'boolean NIL)))
       (CONS '--> (CONS (CONS 'symbol (CONS '--> (CONS 'symbol NIL))) NIL))))

(declare 'fix
 (CONS (CONS 'A (CONS '--> (CONS 'A NIL)))
       (CONS '--> (CONS (CONS 'A (CONS '--> (CONS 'A NIL))) NIL))))

(declare 'freeze (CONS 'A (CONS '--> (CONS (CONS 'lazy (CONS 'A NIL)) NIL))))

(declare 'fst
 (CONS (CONS 'A (CONS '* (CONS 'B NIL))) (CONS '--> (CONS 'A NIL))))

(declare 'function
 (CONS (CONS 'A (CONS '--> (CONS 'B NIL)))
       (CONS '--> (CONS (CONS 'A (CONS '--> (CONS 'B NIL))) NIL))))

(declare 'gensym (CONS 'symbol (CONS '--> (CONS 'symbol NIL))))

(declare '<-vector
 (CONS (CONS 'vector (CONS 'A NIL))
       (CONS '--> (CONS (CONS 'number (CONS '--> (CONS 'A NIL))) NIL))))

(declare '<-vector/or
 (CONS (CONS 'vector (CONS 'A NIL))
       (CONS '-->
             (CONS
              (CONS 'number
                    (CONS '-->
                          (CONS
                           (CONS (CONS 'lazy (CONS 'A NIL))
                                 (CONS '--> (CONS 'A NIL)))
                           NIL)))
              NIL))))

(declare 'vector->
 (CONS (CONS 'vector (CONS 'A NIL))
       (CONS '-->
             (CONS
              (CONS 'number
                    (CONS '-->
                          (CONS
                           (CONS 'A
                                 (CONS '-->
                                       (CONS (CONS 'vector (CONS 'A NIL))
                                             NIL)))
                           NIL)))
              NIL))))

(declare 'vector
 (CONS 'number (CONS '--> (CONS (CONS 'vector (CONS 'A NIL)) NIL))))

(declare 'dict
 (CONS 'number (CONS '--> (CONS (CONS 'dict (CONS 'K (CONS 'V NIL))) NIL))))

(declare 'dict? (CONS 'A (CONS '--> (CONS 'boolean NIL))))

(declare 'dict-count
 (CONS (CONS 'dict (CONS 'K (CONS 'V NIL))) (CONS '--> (CONS 'number NIL))))

(declare '<-dict
 (CONS (CONS 'dict (CONS 'K (CONS 'V NIL)))
       (CONS '--> (CONS (CONS 'K (CONS '--> (CONS 'V NIL))) NIL))))

(declare '<-dict/or
 (CONS (CONS 'dict (CONS 'K (CONS 'V NIL)))
       (CONS '-->
             (CONS
              (CONS 'K
                    (CONS '-->
                          (CONS
                           (CONS (CONS 'lazy (CONS 'V NIL))
                                 (CONS '--> (CONS 'V NIL)))
                           NIL)))
              NIL))))

(declare 'dict->
 (CONS (CONS 'dict (CONS 'K (CONS 'V NIL)))
       (CONS '-->
             (CONS
              (CONS 'K
                    (CONS '--> (CONS (CONS 'V (CONS '--> (CONS 'V NIL))) NIL)))
              NIL))))

(declare 'dict-rm
 (CONS (CONS 'dict (CONS 'K (CONS 'V NIL)))
       (CONS '--> (CONS (CONS 'K (CONS '--> (CONS 'K NIL))) NIL))))

(declare 'dict-fold
 (CONS
  (CONS 'K
        (CONS '-->
              (CONS
               (CONS 'V
                     (CONS '-->
                           (CONS (CONS 'A (CONS '--> (CONS 'A NIL))) NIL)))
               NIL)))
  (CONS '-->
        (CONS
         (CONS (CONS 'dict (CONS 'K (CONS 'V NIL)))
               (CONS '--> (CONS (CONS 'A (CONS '--> (CONS 'A NIL))) NIL)))
         NIL))))

(declare 'dict-keys
 (CONS (CONS 'dict (CONS 'K (CONS 'V NIL)))
       (CONS '--> (CONS (CONS 'list (CONS 'K NIL)) NIL))))

(declare 'dict-values
 (CONS (CONS 'dict (CONS 'K (CONS 'V NIL)))
       (CONS '--> (CONS (CONS 'list (CONS 'V NIL)) NIL))))

(declare 'exit (CONS 'number (CONS '--> (CONS 'unit NIL))))

(declare 'get-time (CONS 'symbol (CONS '--> (CONS 'number NIL))))

(declare 'hash
 (CONS 'A (CONS '--> (CONS (CONS 'number (CONS '--> (CONS 'number NIL))) NIL))))

(declare 'head (CONS (CONS 'list (CONS 'A NIL)) (CONS '--> (CONS 'A NIL))))

(declare 'hdv (CONS (CONS 'vector (CONS 'A NIL)) (CONS '--> (CONS 'A NIL))))

(declare 'hdstr (CONS 'string (CONS '--> (CONS 'string NIL))))

(declare 'if
 (CONS 'boolean
       (CONS '-->
             (CONS
              (CONS 'A
                    (CONS '--> (CONS (CONS 'A (CONS '--> (CONS 'A NIL))) NIL)))
              NIL))))

(declare 'it (CONS '--> (CONS 'string NIL)))

(declare 'implementation (CONS '--> (CONS 'string NIL)))

(declare 'include
 (CONS (CONS 'list (CONS 'symbol NIL))
       (CONS '--> (CONS (CONS 'list (CONS 'symbol NIL)) NIL))))

(declare 'include-all-but
 (CONS (CONS 'list (CONS 'symbol NIL))
       (CONS '--> (CONS (CONS 'list (CONS 'symbol NIL)) NIL))))

(declare 'inferences (CONS '--> (CONS 'number NIL)))

(declare 'shen.insert
 (CONS 'A (CONS '--> (CONS (CONS 'string (CONS '--> (CONS 'string NIL))) NIL))))

(declare 'integer? (CONS 'A (CONS '--> (CONS 'boolean NIL))))

(declare 'internal
 (CONS 'symbol (CONS '--> (CONS (CONS 'list (CONS 'symbol NIL)) NIL))))

(declare 'intersection
 (CONS (CONS 'list (CONS 'A NIL))
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS 'A NIL))
                    (CONS '--> (CONS (CONS 'list (CONS 'A NIL)) NIL)))
              NIL))))

(declare 'kill (CONS '--> (CONS 'A NIL)))

(declare 'language (CONS '--> (CONS 'string NIL)))

(declare 'length
 (CONS (CONS 'list (CONS 'A NIL)) (CONS '--> (CONS 'number NIL))))

(declare 'limit
 (CONS (CONS 'vector (CONS 'A NIL)) (CONS '--> (CONS 'number NIL))))

(declare 'load (CONS 'string (CONS '--> (CONS 'symbol NIL))))

(declare 'fold-left
 (CONS (CONS 'B (CONS '--> (CONS (CONS 'A (CONS '--> (CONS 'B NIL))) NIL)))
       (CONS '-->
             (CONS
              (CONS 'B
                    (CONS '-->
                          (CONS
                           (CONS (CONS 'list (CONS 'A NIL))
                                 (CONS '--> (CONS 'B NIL)))
                           NIL)))
              NIL))))

(declare 'fold-right
 (CONS (CONS 'A (CONS '--> (CONS (CONS 'B (CONS '--> (CONS 'B NIL))) NIL)))
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS 'A NIL))
                    (CONS '--> (CONS (CONS 'B (CONS '--> (CONS 'B NIL))) NIL)))
              NIL))))

(declare 'for-each
 (CONS (CONS 'A (CONS '--> (CONS 'B NIL)))
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS 'A NIL)) (CONS '--> (CONS 'unit NIL)))
              NIL))))

(declare 'map
 (CONS (CONS 'A (CONS '--> (CONS 'B NIL)))
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS 'A NIL))
                    (CONS '--> (CONS (CONS 'list (CONS 'B NIL)) NIL)))
              NIL))))

(declare 'mapcan
 (CONS (CONS 'A (CONS '--> (CONS (CONS 'list (CONS 'B NIL)) NIL)))
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS 'A NIL))
                    (CONS '--> (CONS (CONS 'list (CONS 'B NIL)) NIL)))
              NIL))))

(declare 'filter
 (CONS (CONS 'A (CONS '--> (CONS 'boolean NIL)))
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS 'A NIL))
                    (CONS '--> (CONS (CONS 'list (CONS 'A NIL)) NIL)))
              NIL))))

(declare 'maxinferences (CONS 'number (CONS '--> (CONS 'number NIL))))

(declare 'n->string (CONS 'number (CONS '--> (CONS 'string NIL))))

(declare 'nl (CONS 'number (CONS '--> (CONS 'number NIL))))

(declare 'not (CONS 'boolean (CONS '--> (CONS 'boolean NIL))))

(declare 'nth
 (CONS 'number
       (CONS '-->
             (CONS (CONS (CONS 'list (CONS 'A NIL)) (CONS '--> (CONS 'A NIL)))
                   NIL))))

(declare 'number? (CONS 'A (CONS '--> (CONS 'boolean NIL))))

(declare 'occurrences
 (CONS 'A (CONS '--> (CONS (CONS 'B (CONS '--> (CONS 'number NIL))) NIL))))

(declare 'occurs-check (CONS 'symbol (CONS '--> (CONS 'boolean NIL))))

(declare 'optimise (CONS 'symbol (CONS '--> (CONS 'boolean NIL))))

(declare 'or
 (CONS 'boolean
       (CONS '--> (CONS (CONS 'boolean (CONS '--> (CONS 'boolean NIL))) NIL))))

(declare 'os (CONS '--> (CONS 'string NIL)))

(declare 'package? (CONS 'symbol (CONS '--> (CONS 'boolean NIL))))

(declare 'port (CONS '--> (CONS 'string NIL)))

(declare 'porters (CONS '--> (CONS 'string NIL)))

(declare 'pos
 (CONS 'string
       (CONS '--> (CONS (CONS 'number (CONS '--> (CONS 'string NIL))) NIL))))

(declare 'pr
 (CONS 'string
       (CONS '-->
             (CONS
              (CONS (CONS 'stream (CONS 'out NIL))
                    (CONS '--> (CONS 'string NIL)))
              NIL))))

(declare 'print (CONS 'A (CONS '--> (CONS 'A NIL))))

(declare 'profile
 (CONS (CONS 'A (CONS '--> (CONS 'B NIL)))
       (CONS '--> (CONS (CONS 'A (CONS '--> (CONS 'B NIL))) NIL))))

(declare 'preclude
 (CONS (CONS 'list (CONS 'symbol NIL))
       (CONS '--> (CONS (CONS 'list (CONS 'symbol NIL)) NIL))))

(declare 'shen.proc-nl (CONS 'string (CONS '--> (CONS 'string NIL))))

(declare 'profile-results
 (CONS (CONS 'A (CONS '--> (CONS 'B NIL)))
       (CONS '-->
             (CONS
              (CONS (CONS 'A (CONS '--> (CONS 'B NIL)))
                    (CONS '* (CONS 'number NIL)))
              NIL))))

(declare 'protect (CONS 'symbol (CONS '--> (CONS 'symbol NIL))))

(declare 'preclude-all-but
 (CONS (CONS 'list (CONS 'symbol NIL))
       (CONS '--> (CONS (CONS 'list (CONS 'symbol NIL)) NIL))))

(declare 'shen.prhush
 (CONS 'string
       (CONS '-->
             (CONS
              (CONS (CONS 'stream (CONS 'out NIL))
                    (CONS '--> (CONS 'string NIL)))
              NIL))))

(declare 'ps
 (CONS 'symbol (CONS '--> (CONS (CONS 'list (CONS 'unit NIL)) NIL))))

(declare 'read
 (CONS (CONS 'stream (CONS 'in NIL)) (CONS '--> (CONS 'unit NIL))))

(declare 'read-byte
 (CONS (CONS 'stream (CONS 'in NIL)) (CONS '--> (CONS 'number NIL))))

(declare 'read-char-code
 (CONS (CONS 'stream (CONS 'in NIL)) (CONS '--> (CONS 'number NIL))))

(declare 'read-file-as-bytelist
 (CONS 'string (CONS '--> (CONS (CONS 'list (CONS 'number NIL)) NIL))))

(declare 'read-file-as-charlist
 (CONS 'string (CONS '--> (CONS (CONS 'list (CONS 'number NIL)) NIL))))

(declare 'read-file-as-string (CONS 'string (CONS '--> (CONS 'string NIL))))

(declare 'read-file
 (CONS 'string (CONS '--> (CONS (CONS 'list (CONS 'unit NIL)) NIL))))

(declare 'read-from-string
 (CONS 'string (CONS '--> (CONS (CONS 'list (CONS 'unit NIL)) NIL))))

(declare 'release (CONS '--> (CONS 'string NIL)))

(declare 'remove
 (CONS 'A
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS 'A NIL))
                    (CONS '--> (CONS (CONS 'list (CONS 'A NIL)) NIL)))
              NIL))))

(declare 'reverse
 (CONS (CONS 'list (CONS 'A NIL))
       (CONS '--> (CONS (CONS 'list (CONS 'A NIL)) NIL))))

(declare 'simple-error (CONS 'string (CONS '--> (CONS 'A NIL))))

(declare 'snd
 (CONS (CONS 'A (CONS '* (CONS 'B NIL))) (CONS '--> (CONS 'B NIL))))

(declare 'specialise (CONS 'symbol (CONS '--> (CONS 'symbol NIL))))

(declare 'spy (CONS 'symbol (CONS '--> (CONS 'boolean NIL))))

(declare 'step (CONS 'symbol (CONS '--> (CONS 'boolean NIL))))

(declare 'stinput (CONS '--> (CONS (CONS 'stream (CONS 'in NIL)) NIL)))

(declare 'sterror (CONS '--> (CONS (CONS 'stream (CONS 'out NIL)) NIL)))

(declare 'stoutput (CONS '--> (CONS (CONS 'stream (CONS 'out NIL)) NIL)))

(declare 'string? (CONS 'A (CONS '--> (CONS 'boolean NIL))))

(declare 'str (CONS 'A (CONS '--> (CONS 'string NIL))))

(declare 'string->n (CONS 'string (CONS '--> (CONS 'number NIL))))

(declare 'string->symbol (CONS 'string (CONS '--> (CONS 'symbol NIL))))

(declare 'sum
 (CONS (CONS 'list (CONS 'number NIL)) (CONS '--> (CONS 'number NIL))))

(declare 'symbol? (CONS 'A (CONS '--> (CONS 'boolean NIL))))

(declare 'systemf (CONS 'symbol (CONS '--> (CONS 'symbol NIL))))

(declare 'tail
 (CONS (CONS 'list (CONS 'A NIL))
       (CONS '--> (CONS (CONS 'list (CONS 'A NIL)) NIL))))

(declare 'tlstr (CONS 'string (CONS '--> (CONS 'string NIL))))

(declare 'tlv
 (CONS (CONS 'vector (CONS 'A NIL))
       (CONS '--> (CONS (CONS 'vector (CONS 'A NIL)) NIL))))

(declare 'tc (CONS 'symbol (CONS '--> (CONS 'boolean NIL))))

(declare 'tc? (CONS '--> (CONS 'boolean NIL)))

(declare 'thaw (CONS (CONS 'lazy (CONS 'A NIL)) (CONS '--> (CONS 'A NIL))))

(declare 'track (CONS 'symbol (CONS '--> (CONS 'symbol NIL))))

(declare 'trap-error
 (CONS 'A
       (CONS '-->
             (CONS
              (CONS (CONS 'exception (CONS '--> (CONS 'A NIL)))
                    (CONS '--> (CONS 'A NIL)))
              NIL))))

(declare 'tuple? (CONS 'A (CONS '--> (CONS 'boolean NIL))))

(declare 'undefmacro (CONS 'symbol (CONS '--> (CONS 'symbol NIL))))

(declare 'union
 (CONS (CONS 'list (CONS 'A NIL))
       (CONS '-->
             (CONS
              (CONS (CONS 'list (CONS 'A NIL))
                    (CONS '--> (CONS (CONS 'list (CONS 'A NIL)) NIL)))
              NIL))))

(declare 'unprofile
 (CONS (CONS 'A (CONS '--> (CONS 'B NIL)))
       (CONS '--> (CONS (CONS 'A (CONS '--> (CONS 'B NIL))) NIL))))

(declare 'untrack (CONS 'symbol (CONS '--> (CONS 'symbol NIL))))

(declare 'unspecialise (CONS 'symbol (CONS '--> (CONS 'symbol NIL))))

(declare 'variable? (CONS 'A (CONS '--> (CONS 'boolean NIL))))

(declare 'vector? (CONS 'A (CONS '--> (CONS 'boolean NIL))))

(declare 'version (CONS '--> (CONS 'string NIL)))

(declare 'write-to-file
 (CONS 'string (CONS '--> (CONS (CONS 'A (CONS '--> (CONS 'A NIL))) NIL))))

(declare 'write-byte
 (CONS 'number
       (CONS '-->
             (CONS
              (CONS (CONS 'stream (CONS 'out NIL))
                    (CONS '--> (CONS 'number NIL)))
              NIL))))

(declare 'y-or-n? (CONS 'string (CONS '--> (CONS 'boolean NIL))))

(declare '>
 (CONS 'number
       (CONS '--> (CONS (CONS 'number (CONS '--> (CONS 'boolean NIL))) NIL))))

(declare '<
 (CONS 'number
       (CONS '--> (CONS (CONS 'number (CONS '--> (CONS 'boolean NIL))) NIL))))

(declare '>=
 (CONS 'number
       (CONS '--> (CONS (CONS 'number (CONS '--> (CONS 'boolean NIL))) NIL))))

(declare '<=
 (CONS 'number
       (CONS '--> (CONS (CONS 'number (CONS '--> (CONS 'boolean NIL))) NIL))))

(declare '=
 (CONS 'A (CONS '--> (CONS (CONS 'A (CONS '--> (CONS 'boolean NIL))) NIL))))

(declare '+
 (CONS 'number
       (CONS '--> (CONS (CONS 'number (CONS '--> (CONS 'number NIL))) NIL))))

(declare '/
 (CONS 'number
       (CONS '--> (CONS (CONS 'number (CONS '--> (CONS 'number NIL))) NIL))))

(declare '-
 (CONS 'number
       (CONS '--> (CONS (CONS 'number (CONS '--> (CONS 'number NIL))) NIL))))

(declare '*
 (CONS 'number
       (CONS '--> (CONS (CONS 'number (CONS '--> (CONS 'number NIL))) NIL))))

(declare '==
 (CONS 'A (CONS '--> (CONS (CONS 'B (CONS '--> (CONS 'boolean NIL))) NIL))))

