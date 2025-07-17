(in-package #:cl-user)

(uiop:define-package openai-sdk/chat-completion/conditions
  (:use #:cl)
  (:nicknames #:oai/cc/conditions)
  (:export #:more-than-one))

(in-package #:openai-sdk/chat-completion/conditions)

(define-condition more-than-one (error) ((what :initarg :what)))
