(in-package #:cl-user)

(uiop:define-package openai-sdk/conditions
  (:use #:cl)
  (:import-from #:openai-sdk/interface
                #:bad-request-code
                #:bad-request-message
                #:bad-request-param
                #:bad-request-type)
  (:export #:openai-sdk-bad-request))

(in-package #:openai-sdk/conditions)

(define-condition openai-sdk-bad-request (error)
  ((message :initarg :message :reader bad-request-message)
   (type :initarg :type :reader bad-request-type)
   (param :initarg :param :reader bad-request-param)
   (code :initarg :code :reader bad-request-code))
  (:report (lambda (condition stream)
             (format stream "~:@(~A~) (in: ~A) (status code: ~A)~%MESSAGE:~% ~@<~A~:>"
                     (bad-request-type condition)
                     (symbol-munger:underscores->keyword (bad-request-param condition))
                     (bad-request-code condition)
                     (str:split #\Space (bad-request-message condition)
                                :omit-nulls t)))))
