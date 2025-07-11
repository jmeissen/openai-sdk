(in-package #:cl-user)

(defpackage openai-sdk/client
  (:use #:cl #:openai-sdk/util)
  (:import-from #:openai-sdk/interface
                #:base-url
                #:api-key
                #:organization-id
                #:project-id
                #:read-timeout
                #:connect-timeout
                #:max-retries
                #:default-headers
                #:default-model)
  (:export #:*default-base-url*
           #:*default-headers*
           #:*default-model*
           #:*client*
           #:client
           #:make-client

           ;; Message parsing
           #:parse-messages))

(in-package #:openai-sdk/client)

(defvar *default-base-url* "https://api.openai.com/v1/")
(defvar *default-model* "gpt-4o")
(defvar *default-headers* '(("Content-Type" . "application/json")))
(defvar *client* nil
  "Default OpenAI instance that will be referenced in other parts of the code.")

(defclass client ()
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

(defun make-client (api-key &rest args &key base-url connect-timeout default-headers default-model max-retries organization-id project-id read-timeout)
  (declare (ignore base-url connect-timeout default-headers default-model max-retries organization-id project-id read-timeout))
  (setf *client* (apply #'make-instance 'client :api-key api-key args)))
