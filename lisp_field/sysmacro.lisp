;;This is macro for with-open-stream
;;It illustreate how to use the unwind-protect
;;The is a intern varible to indicate whether the execution encounter some error or not.
;;If encounter errors the (setq #:G4403 nil) will not be called so that the
;;Close will close with :abort set to true. 
;;Great. 
(LET ((S (OPEN "dump" :DIRECTION :OUTPUT)) (#:G4403 T))
  (UNWIND-PROTECT
      (MULTIPLE-VALUE-PROG1 (PROGN (PRINC 99 S)) (SETQ #:G4403 NIL))
    (WHEN S (CLOSE S :ABORT #:G4403))))