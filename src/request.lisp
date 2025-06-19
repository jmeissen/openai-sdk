(in-package #:cl-user)

(defpackage openai-sdk/request
  (:use #:cl
        #:openai-sdk/core
        #:openai-sdk/client)
  (:shadowing-import-from #:openai-sdk/core #:function #:type #:format)
  (:import-from #:openai-sdk/util #:objectify)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export #:create-chat-completion))

(in-package #:openai-sdk/request)

(defun request (method base-url path bearer-auth headers max-retries connect-timeout read-timeout content &optional response-object)
  (multiple-value-bind (body code response-headers uri stream)
      (let ((retry-request (dex:retry-request max-retries :interval 2)))
        (handler-bind ((dex:http-request-internal-server-error retry-request)
                       (dex:http-request-not-implemented retry-request)
                       (dex:http-request-service-unavailable retry-request)
                       (dex:http-request-bad-gateway retry-request))
          (dex:request (concatenate 'string base-url path)
                       :method method
                       :bearer-auth bearer-auth
                       :headers headers
                       :connect-timeout connect-timeout
                       :read-timeout read-timeout
                       :content content)))
    (values (let ((parsed-body (jzon:parse body)))
              (if response-object
                  (objectify response-object parsed-body)
                  parsed-body))
            code
            response-headers
            uri
            stream)))

(defgeneric create-chat-completion (chat-completion &optional openai)
  (:documentation "https://platform.openai.com/docs/api-reference/chat/create"))

(defmethod create-chat-completion :around (chat-completion &optional (openai *openai*))
  (request :post (base-url openai) "chat/completions"
           (api-key openai)
           (remove-if #'null (list (car (default-headers openai))
                                   (when (organization-id openai)
                                     (cons "OpenAI-Organization"
                                           (organization-id openai)))
                                   (when (project-id openai)
                                     (cons "OpenAI-Project"
                                           (project-id openai)))))
           (max-retries openai)
           (connect-timeout openai)
           (read-timeout openai)
           (jzon:stringify (call-next-method))
           'openai-sdk/response:make-chat-completion))

(defmethod create-chat-completion (messages &optional (openai *openai*))
  (openai-sdk/chat-completion:make-chat-completion messages :model (default-model openai)))

(defmethod create-chat-completion ((chat-completion openai-sdk/chat-completion:chat-completion) &optional (openai *openai*))
  (unless (slot-boundp chat-completion 'openai-sdk/chat-completion:model)
    (setf (openai-sdk/chat-completion:model chat-completion) (default-model openai)))
  chat-completion)
