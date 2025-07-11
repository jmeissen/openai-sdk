(in-package #:cl-user)

(uiop:define-package openai-sdk/platform-apis/models/classes
  (:use #:cl #:openai-sdk/interface)
  (:shadowing-import-from #:openai-sdk/interface #:type #:format #:function #:stream)
  (:export #:model
           #:make-model
           #:models-list
           #:make-models-list))

(in-package #:openai-sdk/platform-apis/models/classes)

(defclass model ()
  ((id :type string :initarg :id :reader id)
   (object :type string :initarg :object)
   (created :type local-time:timestamp :initarg :created)
   (owned-by :type string :initarg :owned-by)
   (deleted :type boolean :initarg :deleted)))

(defun make-model (&key id object created owned-by)
  (make-instance 'model :id id
                        :object object
                        :created (local-time:unix-to-timestamp
                                  created)
                        :owned-by owned-by))

(defclass models-list ()
  ((object :type string :initarg :object)
   (data :type simple-vector :initarg :data :reader data)))

(defun make-models-list (&key object data)
  (make-instance 'models-list :object object
                              :data (map 'simple-vector (lambda (ht) (openai-sdk/util:objectify 'make-model ht))
                                         data)))
