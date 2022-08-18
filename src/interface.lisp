;;;; Low-level Coalton wrappers around CL-native Quil data types and functions
(uiop:define-package :coalton-quil/interface
  (:use :coalton :coalton-prelude)
  (:local-nicknames (#:fentry #:coalton-impl/codegen/function-entry)
                    (#:vec :coalton-library/vector)
                    (#:iter :coalton-library/iterator)
                    (#:addr :coalton-library/addressable)
                    (#:ht :coalton-library/hashtable))
  (:export
   ;; chip specifications
   #:ChipSpecification #:chip-specification-num-qubits
   #:chip-specification-hardware-qubits #:chip-specification-hardware-qubit
   #:chip-specification-hardware-links #:chip-specification-hardware-link
   #:build-nq-linear-chip
   #:apply-translation-compilers

   ;; gate bindings
   #:GateBinding
   #:gate-binding-subsumes?

   ;; gate records
   #:GateRecord
   #:gate-record-fidelity

   ;; hardware objects
   #:HardwareObject #:HardwareQubit #:HardwareLink
   #:hardware-object-as-link #:hardware-object-as-qubit
   #:hardware-object-native-gates
   #:qubit-linked-qubits #:link-qubits
   #:lookup-hardware-object #:find-hardware-link
   #:hardware-object-native-instruction?

   ;; rewirings
   #:Rewiring
   #:apply-rewiring-logical-to-physical #:apply-rewiring-physical-to-logical
   #:rewiring-l2p #:rewiring-p2l
   #:make-rewiring #:make-identity-rewiring #:make-empty-rewiring
   #:IntegerOptVector #:integeroptvector-empty? #:integeroptvector-length

   ;; basic blocks
   #:BasicBlock #:basic-block-code #:basic-block-in-rewiring #:basic-block-out-rewiring #:update-basic-block

   ;; instruction fields
   #:Address
   #:GateParameter

   ;; instructions
   #:Instruction
   #:UnknownInstruction
   #:NoOp #:make-no-op
   #:GateApplication #:make-gate-application #:gate-application-gate #:gate-application-parameters #:gate-application-arguments  #:gate-application-binding #:gate-application-fidelity
   #:Pragma #:pragma-words #:make-pragma
   #:Measure #:measure-qubit #:measure-address #:make-measure
   #:MeasureDiscard #:measure-discard-qubit #:make-measure-discard

   ;; qubits
   #:Qubit #:qubit-index #:make-qubit

   ;; `match' dispatch on instructions
   #:TypedInstruction #:UnknownInstruction #:NoOp #:GateApplication #:Pragma #:Measure #:MeasureDiscard))
(cl:in-package :coalton-quil/interface)

(cl:deftype func ()
  "The Common Lisp representation of a Coalton function"
  '(cl:or cl:function fentry::function-entry))

(cl:declaim (cl:ftype (cl:function (Optional) (cl:values (cl:or cl:null cl:t) cl:&optional))
                      optional-to-nullable))
(cl:defun optional-to-nullable (optional)
  "Convert a Coalton (Optional Foo) into a CL (OR NULL FOO), mapping None to NIL and (Some FOO) to FOO."
  (cl:etypecase optional
    (coalton-library/classes::Optional/Some (coalton-library/classes::Optional/Some-_0 optional))
    (coalton-library/classes::Optional/None cl:nil)))

(coalton-toplevel
  (declare unsafe-eq? (:any -> :any -> Boolean))
  (define (unsafe-eq? a b)
    (lisp Boolean (a b) (cl:eq a b)))

  (repr :native cl-quil::chip-specification)
  (define-type ChipSpecification
    "Describes a quantum computer for which Quil code can be compiled.")

  (define-instance (addr:Addressable ChipSpecification)
    (define addr:eq? unsafe-eq?))

  (declare chip-specification-hardware-qubit (ChipSpecification -> UFix -> HardwareQubit))
  (define (chip-specification-hardware-qubit cs idx)
    "Look up the `HardwareQubit' named by IDX on CS."
    (vec:index-unsafe (into idx) (chip-specification-hardware-qubits cs)))

  (declare chip-specification-hardware-links (ChipSpecification -> (Vector HardwareLink)))
  (define (chip-specification-hardware-links cs)
    "Return a `Vector' of all the `HardwareLink's on CS.

This vector should be treated as immutable, as it will share structure with the argument CS object."
    (lisp (Vector HardwareLink) (cs)
      (cl:aref (cl-quil::chip-specification-objects cs) 1)))

  (declare chip-specification-hardware-qubits (ChipSpecification -> (Vector HardwareQubit)))
  (define (chip-specification-hardware-qubits cs)
    "Return a `Vector' of all the `HardwareQubit's on CS.

This vector should be treated as immutable, as it will share structure with the argument CS object."
    (lisp (Vector HardwareQubit) (cs)
      (cl:aref (cl-quil::chip-specification-objects cs) 0)))

  (declare chip-specification-hardware-link (ChipSpecification -> UFix -> HardwareLink))
  (define (chip-specification-hardware-link cs idx)
    "Look up the `HardwareLink' named by IDX on CS."
    (vec:index-unsafe (into idx) (chip-specification-hardware-links cs)))

  (declare chip-specification-num-qubits (ChipSpecification -> UFix))
  (define (chip-specification-num-qubits cs)
    "Count the `HardwareQubit's available on CS."
    (lisp UFix (cs)
      (cl-quil::chip-spec-n-qubits cs)))

  (repr :native cl-quil::hardware-object)
  (define-type HardwareObject
    "A component on a `ChipSpecification', either a `HardwareQubit' or a `HardwareLink'.

Use `hardware-object-as-qubit' or `hardware-object-as-link' to convert into a more specific type.")

  (define-instance (addr:Addressable HardwareObject)
    (define addr:eq? unsafe-eq?))

  (repr :native cl-quil::gate-record)
  (define-type GateRecord)

  (declare gate-record-fidelity (GateRecord -> Single-Float))
  (define (gate-record-fidelity gr)
    (lisp Single-Float (gr)
      (cl:coerce (cl:or (cl-quil::gate-record-fidelity gr)
                        1f0)
                 'cl:single-float)))

  (declare hardware-object-native-gates (HardwareObject -> (iter:Iterator (Tuple GateBinding GateRecord))))
  (define (hardware-object-native-gates obj)
    (iter:list-iter (ht:entries (lisp (ht:HashTable GateBinding GateRecord) (obj)
                                  (cl-quil::hardware-object-gate-information obj)))))

  (declare hardware-object-order (HardwareObject -> UFix))
  (define (hardware-object-order obj)
    "Returns 0 if OBJ is a `HardwareQubit', or 1 if OBJ is a `HardwareLink'.

See the CL docstring on `cl-quil::hardware-object' for a more detailed description."
    (lisp UFix (obj) (cl-quil::hardware-object-order obj)))

  (repr :native cl-quil::hardware-object)
  (define-type HardwareQubit
    "A `HardwareObject' which implements a qubit.")

  (define-instance (addr:Addressable HardwareQubit)
    (define addr:eq? unsafe-eq?))

  (define-instance (Into HardwareQubit HardwareObject)
    (define into unsafe-coerce))

  (repr :native cl-quil::hardware-object)
  (define-type HardwareLink
    "A `HardwareObject' which implements a connection between two `HardwareQubit's.")

  (define-instance (addr:Addressable HardwareLink)
    (define addr:eq? unsafe-eq?))

  (define-instance (Into HardwareLink HardwareObject)
    (define into unsafe-coerce))

  (declare hardware-qubit-link-ids (HardwareQubit -> (Vector UFix)))
  (define (hardware-qubit-link-ids qu)
    (lisp (Vector UFix) (qu)
      (cl:aref (cl-quil::hardware-object-cxns qu) 1)))

  (declare hardware-link-qubit-ids (HardwareLink -> (Vector UFix)))
  (define (hardware-link-qubit-ids lnk)
    (lisp (Vector UFix) (lnk)
      (cl:aref (cl-quil::hardware-object-cxns lnk) 0)))

  (declare hardware-object-as-link (HardwareObject -> (Optional HardwareLink)))
  (define (hardware-object-as-link obj)
    "Treat OBJ as a `HardwareLink'."
    (if (== (hardware-object-order obj) 1)
        (Some (unsafe-coerce obj))
        None))

  (declare hardware-object-as-qubit (HardwareObject -> (Optional HardwareQubit)))
  (define (hardware-object-as-qubit obj)
    "Treat OBJ as a `HardwareQubit'."
    (if (== (hardware-object-order obj) 0)
        (Some (unsafe-coerce obj))
        None))

  (declare qubit-links (ChipSpecification -> Qubit -> (iter:Iterator HardwareLink)))
  (define (qubit-links cs qu)
    "An iterator over all of the `HardwareLink's connected to QU on CS."
    (map (chip-specification-hardware-link cs)
         (iter:vector-iter (hardware-qubit-link-ids (chip-specification-hardware-qubit cs (qubit-index qu))))))

  (declare link-qubits (HardwareLink -> (Tuple Qubit Qubit)))
  (define (link-qubits lnk)
    "An iterator over both of the `Qubit's connected to QU on CS."
    (let ids = (hardware-link-qubit-ids lnk))
    (assert (== 2 (vec:length ids)))
    (Tuple (make-qubit (vec:index-unsafe 0 ids))
           (make-qubit (vec:index-unsafe 1 ids))))

  (declare other-one ((Eq :elt) => :elt -> (Tuple :elt :elt) -> :elt))
  (define (other-one this those)
    (if (== (fst those) this)
        (snd those)
        (fst those)))

  (declare qubit-linked-qubits (ChipSpecification -> Qubit -> (iter:Iterator Qubit)))
  (define (qubit-linked-qubits cs qu)
    "An iterator over all the `Qubit's to which QU is linked on CS."
    (map (fn (lnk)
           (other-one qu (link-qubits lnk)))
         (qubit-links cs qu)))

  (repr :native cl-quil::basic-block)
  (define-type BasicBlock
    "A basic block in a Quil program's control flow graph.

Interesting attributes:
- `basic-block-in-rewiring', the mapping from logical to physical qubits at the basic block's entry.
- `basic-block-out-rewiring', the mapping from logical to physical qubits at the basic block's exit.
- `basic-block-code', a sequence of `Instruction's.")

  (define-instance (addr:Addressable BasicBlock)
    (define addr:eq? unsafe-eq?))

  (repr :native cl-quil::rewiring)
  (define-type Rewiring
    "A CL-native representation of a mapping between physical and logical qubits.")

  (define-instance (addr:Addressable Rewiring)
    (define addr:eq? unsafe-eq?))

  (repr :native (cl:or (cl:and cl:fixnum cl:unsigned-byte) cl:null))
  (define-type IntegerOpt
    "CL-native type corresponding to (Optional UFix)")

  (declare integeropt-to-optional-integer (IntegerOpt -> (Optional UFix)))
  (define (integeropt-to-optional-integer opt)
    (lisp (Optional UFix) (opt)
      (cl:etypecase opt
        ((cl:and cl:fixnum cl:unsigned-byte) (Some opt))
        (cl:null None))))

  (define-instance (Into IntegerOpt (Optional UFix))
    (define into integeropt-to-optional-integer))

  (repr :native cl:vector)
  (define-type IntegerOptVector
    "CL-native type corresponding roughly to (Vector (Optional UFix))")

  (define-instance (addr:Addressable IntegerOptVector)
    (define addr:eq? unsafe-eq?))

  (declare integeroptvector-empty? (IntegerOptVector -> Boolean))
  (define (integeroptvector-empty? vec)
    "Is every element of VEC null?"
    (lisp Boolean (vec)
      (cl:every #'cl:null vec)))

  (declare integeroptvector-length (IntegerOptVector -> UFix))
  (define (integeroptvector-length vec)
    (lisp UFix (vec)
      (cl:length vec)))

  (declare integeroptvector-to-vector-optional-integer (IntegerOptVector -> (Vector (Optional UFix))))
  (define (integeroptvector-to-vector-optional-integer optvec)
    (map integeropt-to-optional-integer
         (lisp (Vector IntegerOpt) (optvec) optvec)))

  (define-instance (Into IntegerOptVector (Vector (Optional UFix)))
    (define into integeroptvector-to-vector-optional-integer))

  (declare try-integeroptvector-to-vector-integer (IntegerOptVector -> (Optional (Vector UFix))))
  (define (try-integeroptvector-to-vector-integer optvec)
    "If every element of OPTVEC is non-null, unwrap them into a (Vector UFix)"
    (if (lisp Boolean (optvec)
          (cl:every #'cl:integerp optvec))
        (Some (lisp (Vector UFix) (optvec)
                (cl:make-array (cl:length optvec)
                               :initial-contents optvec
                               :adjustable cl:t
                               :fill-pointer (cl:length optvec))))
        None))

  (define-instance (Into IntegerOptVector (Optional (Vector UFix)))
    (define into try-integeroptvector-to-vector-integer))

  (declare vector-integer-to-integeroptvec ((Vector UFix) -> IntegerOptVector))
  (define (vector-integer-to-integeroptvec vec)
    (lisp IntegerOptVector (vec)
      (cl:make-array (cl:length vec)
                     :initial-contents vec)))

  (define-instance (Into (Vector UFix) IntegerOptVector)
    (define into vector-integer-to-integeroptvec))

  (declare vector-optional-integer-to-integeroptvec ((Vector (Optional UFix)) -> IntegerOptVector))
  (define (vector-optional-integer-to-integeroptvec vec)
    (lisp IntegerOptVector (vec)
      (cl:map 'cl:vector #'optional-to-nullable vec)))

  (define-instance (Into (Vector (Optional UFix)) IntegerOptVector)
    (define into vector-optional-integer-to-integeroptvec))

  (declare rewiring-l2p (Rewiring -> IntegerOptVector))
  (define (rewiring-l2p rew)
    "A vector which maps logical qubit indices to physical qubit indices."
    (lisp IntegerOptVector (rew)
      (cl-quil::rewiring-l2p rew)))

  (declare rewiring-p2l (Rewiring -> IntegerOptVector))
  (define (rewiring-p2l rew)
    "A vector which maps physical qubit indices to logical qubit indices."
    (lisp IntegerOptVector (rew)
      (cl-quil::rewiring-p2l rew)))

  (declare make-rewiring (IntegerOptVector -> IntegerOptVector -> Rewiring))
  (define (make-rewiring l2p p2l)
    "Build a `Rewiring' from two vectors, one mapping logical qubit indices to physical qubit indices, and the other mapping the other way.

The two vectors must be exact inverse matches. This invariant is unchecked."
    (lisp Rewiring (l2p p2l)
      (cl-quil::init-rewiring :l2p l2p
                          :p2l p2l)))

  (declare make-identity-rewiring (UFix -> Rewiring))
  (define (make-identity-rewiring n-qubits)
    "Construct a `Rewiring' which maps each logical qubit N to the same physical qubit N."
    (lisp Rewiring (n-qubits)
      (cl-quil::make-rewiring n-qubits)))

  (declare make-empty-rewiring (UFix -> Rewiring))
  (define (make-empty-rewiring n-qubits)
    "Construct a `Rewiring' where every qubit is unassigned."
    (lisp Rewiring (n-qubits)
      (cl-quil::make-partial-rewiring n-qubits)))

  (declare basic-block-in-rewiring (BasicBlock -> (Optional Rewiring)))
  (define (basic-block-in-rewiring bb)
    "A mapping between logical and physical qubits at the entry point of BB."
    (lisp (Optional Rewiring) (bb)
      (cl:let ((rew (cl-quil::basic-block-in-rewiring bb)))
        (cl:if rew
               (Some rew)
               None))))

  (declare basic-block-out-rewiring (BasicBlock -> (Optional Rewiring)))
  (define (basic-block-out-rewiring bb)
    "A mapping between logical and physical qubits at the exit point of BB."
    (lisp (Optional Rewiring) (bb)
      (cl:let ((rew (cl-quil::basic-block-out-rewiring bb)))
        (cl:if rew
               (Some rew)
               None))))

  (declare apply-rewiring-logical-to-physical (Rewiring -> Qubit -> (Optional Qubit)))
  (define (apply-rewiring-logical-to-physical rew q)
    (let n = (qubit-index q))
    (lisp (Optional Qubit) (rew n)
      (alexandria:if-let (physical (cl-quil::apply-rewiring-l2p rew n :assert-wired cl:nil))
        (Some physical)
        None)))

  (declare apply-rewiring-physical-to-logical (Rewiring -> Qubit -> (Optional Qubit)))
  (define (apply-rewiring-physical-to-logical rew q)
    (let n = (qubit-index q))
    (lisp (Optional Qubit) (rew n)
      (alexandria:if-let (logical (cl-quil::apply-rewiring-p2l rew n :assert-wired cl:nil))
        (Some (make-qubit logical))
        None)))

  (repr :native cl-quil::instruction)
  (define-type Instruction
    "A CL-native Quil instruction.")

  (define-instance (addr:Addressable Instruction)
    (define addr:eq? unsafe-eq?))

  (declare basic-block-code (BasicBlock -> (Vector Instruction)))
  (define (basic-block-code bb)
    (lisp (Vector Instruction) (bb)
      (cl-quil::basic-block-code bb)))

  (declare update-basic-block (BasicBlock -> (Optional Rewiring) -> (Optional Rewiring) -> (Vector Instruction) -> BasicBlock))
  (define (update-basic-block bb in-rewiring out-rewiring code)
    "Construct a `BasicBlock' like BB, but with a new IN-REWIRING, OUT-REWIRING and CODE.

This is the only way to construct or modify a `BasicBlock' offered by this interface, because
`cl-quil::basic-block' has a bunch of attributes that must be preserved but that aren't relevant to addressing."
    (lisp BasicBlock (bb in-rewiring out-rewiring code)
      (cl-quil::copy-instance bb
                          :in-rewiring (optional-to-nullable in-rewiring)
                          :out-rewiring (optional-to-nullable out-rewiring)
                          :code code)))

  (repr :native cl-quil::qubit)
  (define-type Qubit
    "A CL-native qubit identifier. May represent either a logical or physical qubit.")

  (declare make-qubit (UFix -> Qubit))
  (define (make-qubit idx)
    (lisp Qubit (idx) (cl-quil::qubit idx)))

  (declare qubit-index (Qubit -> UFix))
  (define (qubit-index qu)
    (lisp UFix (qu) (cl-quil::qubit-index qu)))

  (define-instance (Eq Qubit)
    (define (== a b)
      (== (qubit-index a) (qubit-index b))))

  (repr :native cl:t)
  (define-type GateParameter
    "I'm unsure how QuilC represents gate parameters (and too lazy to check), so I'll leave these opaque.")

  (define-instance (addr:Addressable GateParameter)
    (define addr:eq? unsafe-eq?))

  (repr :native cl-quil::gate-application)
  (define-type GateApplication
    "The interesting kind of `Instruction' for addressing: apply a linear operator to the state vector of some qubits.")

  (declare gate-application-gate (GateApplication -> String))
  (define (gate-application-gate ga)
    (lisp String (ga)
      (cl-quil::gate-name
       (cl-quil::gate-application-gate ga))))

  (declare gate-application-parameters (GateApplication -> (List GateParameter)))
  (define (gate-application-parameters ga)
    (lisp (List GateParameter) (ga)
      (cl-quil::application-parameters ga)))

  (declare gate-application-arguments (GateApplication -> (List Qubit)))
  (define (gate-application-arguments ga)
    (lisp (List Qubit) (ga)
      (cl-quil::application-arguments ga)))

  (repr :native cl-quil::gate-binding)
  (define-type GateBinding)

  (define-instance (addr:Addressable GateBinding)
    (define (addr:eq? a b)
      (unsafe-eq? a b)))

  (declare gate-application-binding (GateApplication -> GateBinding))
  (define (gate-application-binding ga)
    (lisp GateBinding (ga)
      (cl-quil::binding-from-instr ga)))

  (declare gate-binding-subsumes? (GateBinding -> GateBinding -> Boolean))
  (define (gate-binding-subsumes? big small)
    (lisp Boolean (big small)
      (cl-quil::binding-subsumes-p big small)))

  (declare make-gate-application (String -> (List GateParameter) -> (List Qubit) -> GateApplication))
  (define (make-gate-application gate-name params args)
    (lisp GateApplication (gate-name params args)
      (cl:apply #'cl-quil::build-gate gate-name params args)))

  (repr :native cl-quil::pragma)
  (define-type Pragma)

  (declare pragma-words (Pragma -> (List String)))
  (define (pragma-words prag)
    (lisp (List String) (prag)
      (cl-quil::pragma-words prag)))

  (declare make-pragma ((List String) -> Pragma))
  (define (make-pragma words)
    (lisp Pragma (words)
      (cl:make-instance 'cl-quil::pragma
                         :words words)))

  (repr :native cl:t)
  (define-type Address
    "I'm unsure how QuilC represents classical memory operands (and too lazy to check), so I'll leave these opaque.")

  (define-instance (addr:Addressable Address)
    (define addr:eq? unsafe-eq?))

  (repr :native cl-quil::measure)
  (define-type Measure
    "Collapse a qubit's value, storing it into a classical bit.")

  (declare measure-qubit (Measure -> Qubit))
  (define (measure-qubit meas)
    (lisp Qubit (meas)
      (cl-quil::measurement-qubit meas)))

  (declare measure-address (Measure -> Address))
  (define (measure-address meas)
    (lisp Address (meas)
      (cl-quil::measure-address meas)))

  (declare make-measure (Qubit -> Address -> Measure))
  (define (make-measure qu addr)
    (lisp Measure (qu addr)
      (cl:make-instance 'cl-quil::measure
                        :address addr
                        :qubit qu)))

  (repr :native cl-quil::measure-discard)
  (define-type MeasureDiscard
    "Collapse a qubit's value without recording it.")

  (declare measure-discard-qubit (MeasureDiscard -> Qubit))
  (define (measure-discard-qubit meas)
    (lisp Qubit (meas)
      (cl-quil::measurement-qubit meas)))

  (declare make-measure-discard (Qubit -> MeasureDiscard))
  (define (make-measure-discard qu)
    (lisp MeasureDiscard (qu)
      (cl:make-instance 'cl-quil::measure-discard
                        :qubit qu)))

  (repr :native cl-quil::no-operation)
  (define-type NoOp
    "Do nothing. Should be removed when nativizing.")

  (declare make-no-op (Unit -> NoOp))
  (define (make-no-op)
    (lisp NoOp ()
      (cl:make-instance 'cl-quil::no-operation)))

  (define-type TypedInstruction
    "An `Instruction' with a tag added; amenable to `match' branching."
    (UnknownInstruction Instruction)
    (NoOp NoOp)
    (GateApplication GateApplication)
    (Pragma Pragma)
    (Measure Measure)
    (MeasureDiscard MeasureDiscard))

  (define-instance (Into Instruction TypedInstruction)
    (define (into insn)
      (lisp TypedInstruction (insn)
        (cl:etypecase insn
          (cl-quil::gate-application (GateApplication insn))
          (cl-quil::pragma (Pragma insn))
          (cl-quil::measure (Measure insn))
          (cl-quil::measure-discard (MeasureDiscard insn))
          (cl-quil::instruction (UnknownInstruction insn))))))

  (declare unsafe-coerce (:any -> :any-other))
  (define (unsafe-coerce thing)
    (lisp :any-other (thing) thing))

  (define-instance (Into TypedInstruction Instruction)
    (define (into insn)
      (match insn
        ((GateApplication insn) (unsafe-coerce insn))
        ((NoOp insn) (unsafe-coerce insn))
        ((Pragma insn) (unsafe-coerce insn))
        ((Measure insn) (unsafe-coerce insn))
        ((MeasureDiscard insn) (unsafe-coerce insn))
        ((UnknownInstruction insn) insn))))

  (define-instance (Into GateApplication Instruction)
    (define into unsafe-coerce))
  (define-instance (Into Measure Instruction)
    (define into unsafe-coerce))
  (define-instance (Into MeasureDiscard Instruction)
    (define into unsafe-coerce))
  (define-instance (Into Pragma Instruction)
    (define into unsafe-coerce))
  (define-instance (Into NoOp Instruction)
    (define into unsafe-coerce))

  (declare lookup-hardware-object (ChipSpecification -> Instruction -> (Optional HardwareObject)))
  (define (lookup-hardware-object cs insn)
    (lisp (Optional HardwareObject) (cs insn)
      (cl:let ((obj (cl-quil::lookup-hardware-object cs insn)))
        (cl:if obj
               (Some obj)
               None))))

  (declare find-hardware-link (ChipSpecification -> Qubit -> Qubit -> (Optional HardwareLink)))
  (define (find-hardware-link chip q0 q1)
    (let arbitrary-instruction = (make-gate-application "SWAP" Nil (make-list q0 q1)))
    (match (lookup-hardware-object chip (into arbitrary-instruction))
      ((None) None)
      ((Some obj) (hardware-object-as-link obj))))

  (declare hardware-object-native-instruction? (HardwareObject -> Instruction -> Boolean))
  (define (hardware-object-native-instruction? obj insn)
    (lisp Boolean (obj insn)
      (cl:not (cl:not (cl-quil::hardware-object-native-instruction-p obj insn)))))

  (declare gate-application-fidelity (ChipSpecification -> GateApplication -> (Optional Single-Float)))
  (define (gate-application-fidelity cs insn)
    (match (lookup-hardware-object cs (into insn))
      ((None) None)
      ((Some obj)
       (let gate = (gate-application-binding insn))
       (match (iter:find! (uncurry (fn (binding _)
                                     (gate-binding-subsumes? binding gate)))
                          (hardware-object-native-gates obj))
         ((None) None)
         ((Some (Tuple _ gate-record)) (Some (gate-record-fidelity gate-record)))))))

  (declare apply-translation-compilers (Instruction -> ChipSpecification -> (Optional HardwareObject) -> (List Instruction)))
  (define (apply-translation-compilers insn cs obj)
    (lisp (List Instruction) (insn cs obj)
      (cl-quil::apply-translation-compilers insn cs (optional-to-nullable obj))))

  (declare build-nq-linear-chip (UFix -> ChipSpecification))
  (define (build-nq-linear-chip nq)
    (lisp ChipSpecification (nq)
      (cl-quil::build-nq-linear-chip nq))))
