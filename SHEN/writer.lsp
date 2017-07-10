
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

(DEFUN pr (V4201 V4202)
  (trap-error (shen.prh V4201 V4202 0) #'(LAMBDA (E) V4201)))

(DEFUN shen.prh (V4206 V4207 V4208)
  (shen.prh V4206 V4207 (shen.write-char-and-inc V4206 V4207 V4208)))

(DEFUN shen.write-char-and-inc (V4212 V4213 V4214)
  (do (write-byte (string->n (pos V4212 V4214)) V4213) (shen.add V4214 1)))

(DEFUN print (V4216)
  (LET ((String (shen.insert V4216 "~S")))
    (LET ((Print (shen.prhush String (stoutput))))
      V4216)))

(DEFUN shen.prhush (V4219 V4220)
  (IF (shen-cl.true? *hush*)
      V4219
      (pr V4219 V4220)))

(DEFUN shen.mkstr (V4223 V4224)
  (COND ((STRINGP V4223) (shen.mkstr-l (shen.proc-nl V4223) V4224))
        (T (shen.mkstr-r (CONS 'shen.proc-nl (CONS V4223 NIL)) V4224))))

(DEFUN shen.mkstr-l (V4227 V4228)
  (COND ((NULL V4228) V4227)
        ((CONSP V4228)
         (shen.mkstr-l (shen.insert-l (CAR V4228) V4227) (CDR V4228)))
        (T (shen.f_error 'shen.mkstr-l))))

(DEFUN shen.insert-l (V4233 V4234)
  (COND ((EQUAL "" V4234) "")
        ((AND (shen-cl.true? (shen.+string? V4234))
              (AND (EQUAL "~" (pos V4234 0))
                   (AND (shen-cl.true? (shen.+string? (tlstr V4234)))
                        (EQUAL "A" (pos (tlstr V4234) 0)))))
         (CONS 'shen.app
               (CONS V4233 (CONS (tlstr (tlstr V4234)) (CONS 'shen.a NIL)))))
        ((AND (shen-cl.true? (shen.+string? V4234))
              (AND (EQUAL "~" (pos V4234 0))
                   (AND (shen-cl.true? (shen.+string? (tlstr V4234)))
                        (EQUAL "R" (pos (tlstr V4234) 0)))))
         (CONS 'shen.app
               (CONS V4233 (CONS (tlstr (tlstr V4234)) (CONS 'shen.r NIL)))))
        ((AND (shen-cl.true? (shen.+string? V4234))
              (AND (EQUAL "~" (pos V4234 0))
                   (AND (shen-cl.true? (shen.+string? (tlstr V4234)))
                        (EQUAL "S" (pos (tlstr V4234) 0)))))
         (CONS 'shen.app
               (CONS V4233 (CONS (tlstr (tlstr V4234)) (CONS 'shen.s NIL)))))
        ((shen-cl.true? (shen.+string? V4234))
         (shen.factor-cn
          (CONS 'cn
                (CONS (pos V4234 0)
                      (CONS (shen.insert-l V4233 (tlstr V4234)) NIL)))))
        ((AND (CONSP V4234)
              (AND (EQ 'cn (CAR V4234))
                   (AND (CONSP (CDR V4234))
                        (AND (CONSP (CDR (CDR V4234)))
                             (NULL (CDR (CDR (CDR V4234))))))))
         (CONS 'cn
               (CONS (CAR (CDR V4234))
                     (CONS (shen.insert-l V4233 (CAR (CDR (CDR V4234))))
                           NIL))))
        ((AND (CONSP V4234)
              (AND (EQ 'shen.app (CAR V4234))
                   (AND (CONSP (CDR V4234))
                        (AND (CONSP (CDR (CDR V4234)))
                             (AND (CONSP (CDR (CDR (CDR V4234))))
                                  (NULL (CDR (CDR (CDR (CDR V4234))))))))))
         (CONS 'shen.app
               (CONS (CAR (CDR V4234))
                     (CONS (shen.insert-l V4233 (CAR (CDR (CDR V4234))))
                           (CDR (CDR (CDR V4234)))))))
        (T (shen.f_error 'shen.insert-l))))

(DEFUN shen.factor-cn (V4236)
  (COND
   ((AND (CONSP V4236)
         (AND (EQ 'cn (CAR V4236))
              (AND (CONSP (CDR V4236))
                   (AND (CONSP (CDR (CDR V4236)))
                        (AND (CONSP (CAR (CDR (CDR V4236))))
                             (AND (EQ 'cn (CAR (CAR (CDR (CDR V4236)))))
                                  (AND (CONSP (CDR (CAR (CDR (CDR V4236)))))
                                       (AND
                                        (CONSP
                                         (CDR (CDR (CAR (CDR (CDR V4236))))))
                                        (AND
                                         (NULL
                                          (CDR
                                           (CDR
                                            (CDR (CAR (CDR (CDR V4236)))))))
                                         (AND (NULL (CDR (CDR (CDR V4236))))
                                              (AND (STRINGP (CAR (CDR V4236)))
                                                   (STRINGP
                                                    (CAR
                                                     (CDR
                                                      (CAR
                                                       (CDR
                                                        (CDR
                                                         V4236)))))))))))))))))
    (CONS 'cn
          (CONS (cn (CAR (CDR V4236)) (CAR (CDR (CAR (CDR (CDR V4236))))))
                (CDR (CDR (CAR (CDR (CDR V4236))))))))
   (T V4236)))

(DEFUN shen.proc-nl (V4238)
  (COND ((EQUAL "" V4238) "")
        ((AND (shen-cl.true? (shen.+string? V4238))
              (AND (EQUAL "~" (pos V4238 0))
                   (AND (shen-cl.true? (shen.+string? (tlstr V4238)))
                        (EQUAL "%" (pos (tlstr V4238) 0)))))
         (cn (n->string 10) (shen.proc-nl (tlstr (tlstr V4238)))))
        ((shen-cl.true? (shen.+string? V4238))
         (cn (pos V4238 0) (shen.proc-nl (tlstr V4238))))
        (T (shen.f_error 'shen.proc-nl))))

(DEFUN shen.mkstr-r (V4241 V4242)
  (COND ((NULL V4242) V4241)
        ((CONSP V4242)
         (shen.mkstr-r (CONS 'shen.insert (CONS (CAR V4242) (CONS V4241 NIL)))
          (CDR V4242)))
        (T (shen.f_error 'shen.mkstr-r))))

(DEFUN shen.insert (V4245 V4246) (shen.insert-h V4245 V4246 ""))

(DEFUN shen.insert-h (V4252 V4253 V4254)
  (COND ((EQUAL "" V4253) V4254)
        ((AND (shen-cl.true? (shen.+string? V4253))
              (AND (EQUAL "~" (pos V4253 0))
                   (AND (shen-cl.true? (shen.+string? (tlstr V4253)))
                        (EQUAL "A" (pos (tlstr V4253) 0)))))
         (cn V4254 (shen.app V4252 (tlstr (tlstr V4253)) 'shen.a)))
        ((AND (shen-cl.true? (shen.+string? V4253))
              (AND (EQUAL "~" (pos V4253 0))
                   (AND (shen-cl.true? (shen.+string? (tlstr V4253)))
                        (EQUAL "R" (pos (tlstr V4253) 0)))))
         (cn V4254 (shen.app V4252 (tlstr (tlstr V4253)) 'shen.r)))
        ((AND (shen-cl.true? (shen.+string? V4253))
              (AND (EQUAL "~" (pos V4253 0))
                   (AND (shen-cl.true? (shen.+string? (tlstr V4253)))
                        (EQUAL "S" (pos (tlstr V4253) 0)))))
         (cn V4254 (shen.app V4252 (tlstr (tlstr V4253)) 'shen.s)))
        ((shen-cl.true? (shen.+string? V4253))
         (shen.insert-h V4252 (tlstr V4253) (cn V4254 (pos V4253 0))))
        (T (shen.f_error 'shen.insert-h))))

(DEFUN shen.app (V4258 V4259 V4260) (cn (shen.arg->str V4258 V4260) V4259))

(DEFUN shen.arg->str (V4268 V4269)
  (COND ((EQ V4268 (fail)) "...")
        ((shen-cl.true? (shen.list? V4268)) (shen.list->str V4268 V4269))
        ((STRINGP V4268) (shen.str->str V4268 V4269))
        ((shen-cl.true? (absvector? V4268)) (shen.vector->str V4268 V4269))
        (T (shen.atom->str V4268))))

(DEFUN shen.list->str (V4272 V4273)
  (COND
   ((EQ 'shen.r V4273)
    (@s "(" (@s (shen.iter-list V4272 'shen.r (shen.maxseq)) ")")))
   (T (@s "[" (@s (shen.iter-list V4272 V4273 (shen.maxseq)) "]")))))

(DEFUN shen.maxseq () *maximum-print-sequence-size*)

(DEFUN shen.iter-list (V4287 V4288 V4289)
  (COND ((NULL V4287) "")
        ((IF (NUMBERP V4289)
             (= V4289 0))
         "... etc")
        ((AND (CONSP V4287) (NULL (CDR V4287)))
         (shen.arg->str (CAR V4287) V4288))
        ((CONSP V4287)
         (@s (shen.arg->str (CAR V4287) V4288)
             (@s " "
                 (shen.iter-list (CDR V4287) V4288 (shen.subtract V4289 1)))))
        (T (@s "|" (@s " " (shen.arg->str V4287 V4288))))))

(DEFUN shen.str->str (V4296 V4297)
  (COND ((EQ 'shen.a V4297) V4296)
        (T (@s (n->string 34) (@s V4296 (n->string 34))))))

(DEFUN shen.vector->str (V4300 V4301)
  (IF (shen-cl.true? (shen.print-vector? V4300))
      (shen.apply (function (<-address V4300 0)) (LIST V4300))
      (IF (shen-cl.true? (vector? V4300))
          (@s "<" (@s (shen.iter-vector V4300 1 V4301 (shen.maxseq)) ">"))
          (@s "<"
              (@s "<"
                  (@s (shen.iter-vector V4300 0 V4301 (shen.maxseq)) ">>"))))))

(DEFUN shen.print-vector? (V4303)
  (LET ((Zero (<-address V4303 0)))
    (IF (EQ Zero 'shen.tuple)
        'true
        (IF (EQ Zero 'shen.pvar)
            'true
            (IF (EQ Zero 'shen.dictionary)
                'true
                (IF (NOT (NUMBERP Zero))
                    (shen.fbound? Zero)
                    'false))))))

(DEFUN shen.fbound? (V4305)
  (trap-error (do (shen.lookup-func V4305) 'true) #'(LAMBDA (E) 'false)))

(DEFUN shen.tuple (V4307)
  (cn "(@p "
      (shen.app (<-address V4307 1)
       (cn " " (shen.app (<-address V4307 2) ")" 'shen.s)) 'shen.s)))

(DEFUN shen.dictionary (V4309) "(dict ...)")

(DEFUN shen.iter-vector (V4320 V4321 V4322 V4323)
  (COND
   ((IF (NUMBERP V4323)
        (= V4323 0))
    "... etc")
   (T
    (LET ((Item (<-address/or V4320 V4321 (freeze 'shen.out-of-bounds))))
      (LET ((Next
             (<-address/or V4320 (shen.add V4321 1)
                           (freeze 'shen.out-of-bounds))))
        (IF (EQ Item 'shen.out-of-bounds)
            ""
            (IF (EQ Next 'shen.out-of-bounds)
                (shen.arg->str Item V4322)
                (@s (shen.arg->str Item V4322)
                    (@s " "
                        (shen.iter-vector V4320 (shen.add V4321 1) V4322
                         (shen.subtract V4323 1)))))))))))

(DEFUN shen.atom->str (V4325)
  (trap-error (str V4325) #'(LAMBDA (E) (shen.funexstring))))

(DEFUN shen.funexstring ()
  (@s ""
      (@s "f"
          (@s "u"
              (@s "n"
                  (@s "e"
                      (@s (shen.arg->str (gensym (intern "x")) 'shen.a)
                          "")))))))

(DEFUN shen.list? (V4327) (or (empty? V4327) (cons? V4327)))

