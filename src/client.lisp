(in-package #:cl-user)

(defpackage openai-sdk/client
  (:use #:cl)
  (:export #:*default-base-url*
           #:*default-headers*
           #:*default-model*
           #:*openai*
           #:api-key
           #:base-url
           #:connect-timeout
           #:default-headers
           #:default-model
           #:make-openai
           #:max-retries
           #:openai
           #:organization-id
           #:project-id
           #:read-timeout))

(in-package #:openai-sdk/client)

(defvar *default-base-url* "https://api.openai.com/v1/")
(defvar *default-model* "gpt-4o")
(defvar *default-headers* '(("Content-Type" . "application/json")))
(defvar *openai* nil
  "Default OpenAI instance that will be referenced in other parts of the code.")

(defclass openai ()
  ((base-url :initarg :base-url
             :accessor base-url
             :initform *default-base-url*)
   (api-key :initarg :api-key
            :accessor api-key)
   (organization-id :initarg :organization-id
                    :accessor organization-id
                    :initform nil)
   (project-id :initarg :project-id
               :accessor project-id
               :initform nil)
   (read-timeout :initarg :read-timeout
                 :accessor read-timeout
                 :initform 600)
   (connect-timeout :initarg :connect-timeout
                    :accessor connect-timeout
                    :initform 5.0)
   (max-retries :initarg :max-retries
                :accessor max-retries
                :initform 3)
   (default-headers :initarg :default-headers
                    :accessor default-headers
                    :initform *default-headers*)
   (default-model :initarg :default-model
                  :accessor default-model
                  :initform *default-model*)))

(defun make-openai (api-key &rest args &key base-url connect-timeout default-headers default-model max-retries organization-id project-id read-timeout)
  (declare (ignore base-url connect-timeout default-headers default-model max-retries organization-id project-id read-timeout))
  (setf *openai* (apply #'make-instance 'openai :api-key api-key args)))
