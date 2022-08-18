(uiop:define-package :coalton-quil/parsed-language
  (:use :coalton :coalton-prelude)
  (:local-nicknames (#:quil :coalton-quil/interface)
                    (#:iter :coalton-library/iterator)
                    (#:list :coalton-library/list)
                    (#:addr :coalton-library/addressable))
  (:export

   #:LogicalQubit
   #:as-logical
   #:unresolved
   #:log-inbounds?

   #:PhysicalQubit
   #:as-physical
   #:resolved
   #:phys-inbounds?

   #:Instruction
   #:Unknown
   #:NoOp
   #:Pragma
   #:Swap
   #:GateApplication
   #:Measure
   #:MeasureDiscard

   #:insn-qubits

   #:parse-insn #:parse-insn-as-logical #:parse-insn-as-physical
   #:unparse-insn #:unparse-insn-as-unresolved #:unparse-insn-as-resolved

   #:apply-rewiring-l2p #:apply-rewiring-p2l))
(cl:in-package :coalton-quil/parsed-language)

(coalton-toplevel
  (define-type LogicalQubit
    "A logical qubit which will appear as an instruction argument before addressing, referring to a conceptual object in a source program.

May be implemented by multiple different `PhysicalQubit's at different points during a program's lifetime."
    (LogicalQubit UFix))

  (define-type PhysicalQubit
    "A physical qubit which will appear as an instruction argument after addressing, referring to a specific `quil:HardwareQubit' on a `quil:ChipSpecification'.

May be used to implement multiple different `LogicalQubit's at different points during a program's lifetime."
    (PhysicalQubit UFix))

  (define-instance (Into LogicalQubit UFix)
    (define (into q)
      (let (LogicalQubit idx) = q)
      idx))
  
  (define-instance (Eq LogicalQubit)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (LogicalQubit a) (LogicalQubit b)) (== a b)))))

  (define-instance (Ord LogicalQubit)
    (define (<=> a b)
      (match (Tuple a b)
        ((Tuple (LogicalQubit a) (LogicalQubit b)) (<=> a b)))))

  (define-instance (Hash LogicalQubit)
    (define (hash q)
      (let (LogicalQubit idx) = q)
      (hash idx)))

  (define-instance (Into PhysicalQubit UFix)
    (define (into q)
      (let (PhysicalQubit idx) = q)
      idx))

  (define-instance (Eq PhysicalQubit)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (PhysicalQubit a) (PhysicalQubit b)) (== a b)))))

  (define-instance (Ord PhysicalQubit)
    (define (<=> a b)
      (match (Tuple a b)
        ((Tuple (PhysicalQubit a) (PhysicalQubit b)) (<=> a b)))))

  (define-instance (Hash PhysicalQubit)
    (define (hash q)
      (let (PhysicalQubit idx) = q)
      (hash idx)))

  (declare phys-inbounds? (UFix -> PhysicalQubit -> Boolean))
  (define (phys-inbounds? num-qubits q)
    "Is Q a valid qubit on a QPU with NUM-QUBITS qubits?"
    (let (PhysicalQubit idx) = q)
    (< idx num-qubits))

  (declare log-inbounds? (UFix -> LogicalQubit -> Boolean))
  (define (log-inbounds? num-qubits q)
    "Is Q a valid qubit on a QPU with NUM-QUBITS qubits?"
    (let (LogicalQubit idx) = q)
    (< idx num-qubits))

  (declare as-logical (quil:Qubit -> LogicalQubit))
  (define (as-logical qu)
    "Treat a CL-native `quil:Qubit' as a `LogicalQubit', not necessarily assigned to any particular `quil:HardwareQubit'."
    (LogicalQubit (quil:qubit-index qu)))

  (declare as-physical (quil:Qubit -> PhysicalQubit))
  (define (as-physical qu)
    "Treat a CL-native `quil:Qubit' as a `PhysicalQubit', corresponding to a specific `quil:HardwareQubit'."
    (PhysicalQubit (quil:qubit-index qu)))

  (declare unresolved (LogicalQubit -> quil:Qubit))
  (define (unresolved qu)
    "Convert a `LogicalQubit' into a CL-native `quil:Qubit', without assigning a particular `PhysicalQubit' to it."
    (match qu
      ((LogicalQubit idx) (quil:make-qubit idx))))

  (declare resolved (PhysicalQubit -> quil:Qubit))
  (define (resolved qu)
    "Convert a `PhysicalQubit' into a CL-native `quil:Qubit', without noting a corresponding `LogicalQubit'."
    (match qu
      ((PhysicalQubit idx) (quil:make-qubit idx))))

  (define-type (Instruction :qubit)
    "An ADT roughly corresponding to `quil:Instruction', the Quil instruction set.

The :QUBIT type parameter should likely be either `PhysicalQubit' or `LogicalQubit'.

Convert from CL-native `quil:Instruction's using `parse-insn', `parse-insn-as-logical' or
`parse-insn-as-physical', and into `quil:Instruction' using `unparse-insn', `unparse-insn-as-unresolved' or
`unparse-insn-as-resolved'."
    ;; Fallback: there are a bunch of Quil instructions that aren't interesting for addressers (e.g. classical
    ;; ops). Stick 'em in this variant and leave 'em alone.
    (Unknown quil:Instruction)

    ;; Should be removed during nativization.
    NoOp

    ;; Some of these should be removed during nativization.
    (Pragma (List String))

    ;; Shorthand for (GateApplication "SWAP" Nil (Cons qa (Cons qb Nil)))
    (Swap :qubit :qubit)

    ;; (GateApplication GATE-NAME PARAMS ARGS) corresponds to a Quil application GATE-NAME(...PARAMS) ...ARGS.
    ;; After nativization, only native gate applications should remain, i.e. 1q and 2q gates on actual
    ;; `HardwareObject's which are `hardware-object-native-instruction?'.
    (GateApplication String (List quil:GateParameter) (List :qubit))

    ;; Measure a qubit, collapsing its state into either zero or one, and also collapsing all the other qubits
    ;; with which it is entangled. If an address is supplied, set a classical bit to the collapsed value.
    (Measure quil:Address :qubit)
    (MeasureDiscard :qubit))

  (define-instance ((Eq :qubit) => (Eq (Instruction :qubit)))
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Unknown a)
                (Unknown b))
         (addr:eq? a b))
        ((Tuple (NoOp)
                (NoOp))
         True)
        ((Tuple (Pragma a)
                (Pragma b))
         (== a b))
        ((Tuple (Swap aa ab)
                (Swap ba bb))
         (and (== aa ba)
              (== ab bb)))
        ((Tuple (GateApplication a-gate a-params a-args)
                (GateApplication b-gate b-params b-args))
         (and (== a-gate b-gate)
              (iter:every! (uncurry addr:eq?)
                           (iter:zip! (iter:list-iter a-params)
                                      (iter:list-iter b-params)))
              (== a-args b-args)))
        ((Tuple (Measure a-addr a-q)
                (Measure b-addr b-q))
         (and (addr:eq? a-addr b-addr)
              (== a-q b-q)))
        ((Tuple (MeasureDiscard a)
                (MeasureDiscard b))
         (== a b))
        (_ False))))

  (define-instance ((Hash :qubit) => (Hash (Instruction :qubit)))
    (define (hash insn)
      (let ((sxhash (fn (a)
                      (lisp UFix (a) (cl:sxhash a))))
            (reduce-hash (fn (hash-elt lst seed)
                           (fold (fn (old new) (combine-hashes old (hash-elt new)))
                                 seed
                                 lst))))
        (match insn
          ((Unknown a) (sxhash a))
          ((NoOp) (hash "NOOP"))
          ((Pragma words) (reduce-hash hash words (hash "PRAGMA")))
          ((Swap a b) (combine-hashes (hash "SWAP")
                                      (combine-hashes (hash a) (hash b))))
          ((GateApplication gat params args)
           (reduce-hash sxhash params
                        (reduce-hash hash args
                                     (hash gat))))
          ((Measure addr q)
           (combine-hashes (hash "MEASURE")
                           (combine-hashes (sxhash addr) (hash q))))
          ((MeasureDiscard q)
           (combine-hashes (hash "MEASUREDISCARD")
                           (hash q)))))))

  (define-instance (Functor Instruction)
    (define (map func insn)
      (match insn
        ((Unknown a) (Unknown a))
        ((NoOp) NoOp)
        ((Pragma words) (Pragma words))
        ((Swap a b) (Swap (func a) (func b)))
        ((GateApplication gat params args) (GateApplication gat
                                                            params
                                                            (map func args)))
        ((Measure addr q) (Measure addr (func q)))
        ((MeasureDiscard q) (MeasureDiscard (func q))))))

  (declare insn-qubits ((Instruction :qubit) -> (List :qubit)))
  (define (insn-qubits insn)
    (match insn
      ((Unknown _) Nil)
      ((NoOp) Nil)
      ((Pragma _) Nil)
      ((Swap qa qb) (make-list qa qb))
      ((GateApplication _ _ qus) qus)
      ((Measure _ q) (make-list q))
      ((MeasureDiscard q) (make-list q))))

  (declare parse-insn ((quil:Qubit -> :qubit) -> quil:Instruction -> (Instruction :qubit)))
  (define (parse-insn parse-qubit insn)
    "Parse a CL-native `quil:Instruction' into a Coalton ADT `Instruction', treating CL-native `quil:Qubit' args according to PARSE-QUBIT."
    (match (the quil:TypedInstruction (into insn))
      ((quil:UnknownInstruction insn) (Unknown insn))

      ((quil:NoOp _) NoOp)

      ((quil:Pragma prag) (Pragma (quil:pragma-words prag)))

      ((quil:GateApplication ga)
       (if (== (quil:gate-application-gate ga) "SWAP")
           (progn
             (let (Cons qa (Cons qb (Nil))) = (quil:gate-application-arguments ga))
             (assert (list:null? (quil:gate-application-parameters ga)))
             (Swap (parse-qubit qa) (parse-qubit qb)))
           (GateApplication (quil:gate-application-gate ga)
                            (quil:gate-application-parameters ga)
                            (map parse-qubit (quil:gate-application-arguments ga)))))

      ((quil:Measure meas)
       (Measure (quil:measure-address meas)
                (parse-qubit (quil:measure-qubit meas))))

      ((quil:MeasureDiscard meas)
       (MeasureDiscard (parse-qubit (quil:measure-discard-qubit meas))))))

  (declare parse-insn-as-logical (quil:Instruction -> (Instruction LogicalQubit)))
  (define parse-insn-as-logical
    "Parse a CL-native `quil:Instruction' into a Coalton ADT `Instruction', treating CL-native `quil:Qubit' args as unassigned `LogicalQubit's."
    (parse-insn as-logical))

  (declare parse-insn-as-physical (quil:Instruction -> (Instruction PhysicalQubit)))
  (define parse-insn-as-physical
    "Parse a CL-native `quil:Instruction' into a Coalton ADT `Instruction', treating CL-native `quil:Qubit' args as assigned `PhysicalQubit's."
    (parse-insn as-physical))

  (declare unparse-insn ((:qubit -> quil:Qubit) -> (Instruction :qubit) -> quil:Instruction))
  (define (unparse-insn unparse-qubit insn)
    "Transform a Coalton ADT `Instruction' into a CL-native `quil:Instruction', treating qubit arguments according to UNPARSE-QUBIT."
    (match insn
      ((Unknown insn) insn)

      ((NoOp) (into (quil:make-no-op)))

      ((Pragma words) (into (quil:make-pragma words)))

      ((GateApplication gate params args)
       (into (quil:make-gate-application gate params (map unparse-qubit args))))

      ((Measure addr qu)
       (into (quil:make-measure (unparse-qubit qu) addr)))

      ((MeasureDiscard qu)
       (into (quil:make-measure-discard (unparse-qubit qu))))))

  (declare unparse-insn-as-unresolved ((Instruction LogicalQubit) -> quil:Instruction))
  (define unparse-insn-as-unresolved
    "Transform a Coalton ADT `Instruction' into a CL-native `quil:Instruction', without resolving `LogicalQubit' args."
    (unparse-insn unresolved))

  (declare unparse-insn-as-resolved ((Instruction PhysicalQubit) -> quil:Instruction))
  (define unparse-insn-as-resolved
    "Transform a Coalton ADT `Instruction' into a CL-native `quil:Instruction', treating `PhysicalQubit' args as resolved."
    (unparse-insn resolved))

  (define-type (NoRewiring :qubit)
    (NoRewiring :qubit))

  (declare apply-rewiring-l2p (quil:Rewiring -> (Instruction LogicalQubit) -> (Result (NoRewiring LogicalQubit)
                                                                                      (Instruction PhysicalQubit))))
  (define (apply-rewiring-l2p rew insn)
    "Apply the rewiring REW to each qubit named in INSN, replacing logical qubits with their physical assignments.

Returns Err if any logical qubit named in INSN does not have a physical assignment, with a `NoRewiring'
referring to the first such unassigned qubit found."
    ;; written in a deliberately verbose style with lots of abstractions and type declarations to get as much
    ;; compile-time type checking as possible
    (let apply-rewiring-or-error =
      (fn (lq)
        (let (LogicalQubit logical-idx) = lq)
        (let physical-idx = (lisp UFix (rew logical-idx)
                              (cl-quil::apply-rewiring-l2p rew logical-idx :assert-wired cl:t)))
        (PhysicalQubit physical-idx)))
    (let map-insn =
      (the (Unit -> (Instruction PhysicalQubit))
           (fn ()
             (map apply-rewiring-or-error insn))))
    (let failure =
      (the (UFix -> (Result (NoRewiring LogicalQubit)
                            :any))
           (fn (logical-idx)
             (Err (NoRewiring (LogicalQubit logical-idx))))))
    (let success =
      (the ((Instruction PhysicalQubit) -> (Result :any (Instruction PhysicalQubit)))
           Ok))
    (lisp (Result (NoRewiring LogicalQubit)
                  (Instruction PhysicalQubit))
        (map-insn failure success)
      (cl:handler-case (call-coalton-function map-insn Unit)
        (cl-quil::missing-rewiring-assignment (e)
          (call-coalton-function failure (cl-quil::missing-rewiring-assignment-wire e)))
        (:no-error (new-insn)
          (call-coalton-function success new-insn)))))

  (declare apply-rewiring-p2l (quil:Rewiring -> (Instruction PhysicalQubit) -> (Result (NoRewiring PhysicalQubit)
                                                                                       (Instruction LogicalQubit))))
  (define (apply-rewiring-p2l rew insn)
    "Apply the rewiring REW to each qubit named in INSN, replacing physical qubits with their logical assignments.

Returns Err if any physical qubit named in INSN does not have a logical assignment, with a `NoRewiring'
referring to the first such unassigned qubit found."
    ;; written in a deliberately verbose style with lots of abstractions and type declarations to get as much
    ;; compile-time type checking as possible
    (let apply-rewiring-or-error =
      (fn (pq)
        (let (PhysicalQubit physical-idx) = pq)
        (let logical-idx = (lisp UFix (rew physical-idx)
                             (cl-quil::apply-rewiring-p2l rew physical-idx :assert-wired cl:t)))
        (LogicalQubit logical-idx)))
    (let map-insn =
      (the (Unit -> (Instruction LogicalQubit))
           (fn ()
             (map apply-rewiring-or-error insn))))
    (let failure =
      (the (UFix -> (Result (NoRewiring PhysicalQubit)
                            :any))
           (fn (physical-idx)
             (Err (NoRewiring (PhysicalQubit physical-idx))))))
    (let success =
      (the ((Instruction LogicalQubit) -> (Result :any (Instruction LogicalQubit)))
           Ok))
    (lisp (Result (NoRewiring PhysicalQubit)
                  (Instruction LogicalQubit))
        (map-insn failure success)
      (cl:handler-case (call-coalton-function map-insn Unit)
        (cl-quil::missing-rewiring-assignment (e)
          (call-coalton-function failure (cl-quil::missing-rewiring-assignment-wire e)))
        (:no-error (logical-insn)
          (call-coalton-function success logical-insn))))))
