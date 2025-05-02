(in-package #:cl-user)

(defpackage openai-sdk/generics
  (:use #:cl)
  (:shadow #:function #:type))

(in-package #:openai-sdk/generics)

(defmacro dg (generics)
  "Loop GENERICS to define the generic and export it."
  `(progn ,@(loop for g in generics
                  collect `(progn
                             (defgeneric ,g (object))
                             (export ',g 'openai-sdk/generics)))))

(dg (arguments
     audio
     content
     function
     function-call
     id
     name
     refusal
     role
     tool-calls
     type))
