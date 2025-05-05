(in-package #:cl-user)

(defpackage openai-sdk/core
  (:use #:cl)
  (:shadow #:function #:type #:format)
  (:local-nicknames (#:jzon #:com.inuoe.jzon)))

(in-package #:openai-sdk/core)

(defmacro dg (generics)
  "Loop GENERICS to define the generic and export it."
  `(progn ,@(loop for g in generics
                  collect `(progn
                             (defgeneric ,g (object))
                             (export ',g 'openai-sdk/core)))))

(dg (arguments
     audio
     b64encode
     content
     data
     function
     function-call
     format
     id
     image-url
     input-audio
     message
     name
     refusal
     role
     tool-calls
     type
     text
     url))

(defclass openai-request () ()
  (:documentation "Inherit from OPENAI-REQUEST if the child will be serialized to
 json with `jzon'. A `jzon:coerced-fields'-method is specialized on this class to
 ensure slot-names are serialized to snake_case (since OpenAI wants this)."))
(export 'openai-request)

(defmethod jzon:coerced-fields ((element openai-request))
  (macrolet ((%coerced-fields-slots (element)
               `(let ((class (class-of ,element)))
                  (c2mop:ensure-finalized class)
                  (mapcar
                   (lambda (s) (let ((slot-name (c2mop:slot-definition-name s)))
                                 ;; Serialize slot-names as snake_case:
                                 `(,(symbol-munger:lisp->underscores slot-name)
                                   ,(slot-value ,element slot-name)
                                   ,(c2mop:slot-definition-type s))))
                   (remove-if-not (lambda (s) (slot-boundp ,element
                                                           (c2mop:slot-definition-name s)))
                                  (c2mop:class-slots class))))))
    (%coerced-fields-slots element)))
