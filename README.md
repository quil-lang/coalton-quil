# A Coalton interface for QuilC data structures and operators

## interface.lisp

src/interface.lisp contains a relatively low-level interface for interacting with QuilC
via Coalton. Many (though far from all) QuilC internal data structures have Coalton
wrapper types exposed, and many QuilC functions have Coalton wrapper functions exposed.

The set of wrapped types and operators is rather haphazard, and corresponds to the set of
operations I (Phoebe Goldman) needed during my internship hacking on QuilC's addresser and
backends. In particular, relatively few subclasses of `Instruction` are exposed; classical
instructions are completely ignored, and `GateApplication` parameters are left opaque.

## parsed-language.lisp

src/parsed-language.lisp defines a Coalton ADT representation of (a subset of) the Quil
language. Particular focus is placed on rewiring and addressing applications;
`Instruction` is a parametric type over the `:qubit` field, and defines a `Functor`
instance so rewirings can be mapped over instructions.
