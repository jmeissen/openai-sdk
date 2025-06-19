(in-package #:cl-user)

(defpackage openai-sdk/structured-output
  (:use #:cl)
  (:import-from #:openai-sdk/chat-completion
                #:make-response-format
                #:make-chat-completion
                #:make-json-schema
                #:system-message)
  (:import-from #:openai-sdk/json #:schema #:parse)
  (:import-from #:openai-sdk/request #:create-chat-completion)
  (:export #:structured-output))

(in-package #:openai-sdk/structured-output)

(defgeneric structured-output (class-symbol system-message &optional messages)
  (:documentation "Shorthand to fill objects with values through structured outputs.
CLASS-SYMBOL is the class that should be instantiated with content
SYSTEM-MESSAGE is the system message to use
MESSAGES are any consequent messages for the message thread that inform the structured output.

Note that currently only STRICT adherence is supported."))

(defmethod structured-output ((class-symbol symbol) (system-message string) &optional messages)
  (structured-output class-symbol (openai-sdk/chat-completion:make-system-message system-message) messages))

(defmethod structured-output ((class-symbol symbol) (system-message system-message) &optional messages)
  (let ((schema-name (format nil "~(~A~)" class-symbol))
        (schema (schema class-symbol)))
    (parse class-symbol
           (com.inuoe.jzon:parse
            (openai-sdk/response:content
             (openai-sdk/core:message
              (aref (openai-sdk/response:choices
                     (create-chat-completion
                      (make-chat-completion
                       (cons system-message messages)
                       :response-format (make-response-format
                                         "json_schema"
                                         :json-schema (make-json-schema schema-name
                                                                        :schema schema
                                                                        :strict t)))))
                    0)))))))
