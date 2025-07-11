(in-package #:cl-user)

(uiop:define-package openai-sdk/chat-completion/request
  (:use #:cl #:openai-sdk/core #:openai-sdk/interface)
  (:shadow #:stream #:type #:function #:format)
  (:import-from #:openai-sdk/client #:*client*)
  (:import-from #:openai-sdk/chat-completion/classes #:chat-completion))

(in-package #:openai-sdk/chat-completion/request)

(defmethod send ((chat-completion chat-completion) &optional (client *client*))
  ;; If no model is provided, then set the default model for the request
  (unless (slot-boundp chat-completion 'model)
    (setf (oai:model chat-completion) (oai:default-model client)))
  (request :post (oai:base-url client) "chat/completions"
           (oai:api-key client)
           (remove-if #'null (list (car (oai:default-headers client))
                                   (when (oai:organization-id client)
                                     (cons "OpenAI-Organization"
                                           (organization-id client)))
                                   (when (oai:project-id client)
                                     (cons "OpenAI-Project"
                                           (project-id client)))))
           (oai:max-retries client)
           (oai:connect-timeout client)
           (oai:read-timeout client)
           (com.inuoe.jzon:stringify chat-completion)
           'openai-sdk/chat-completion/classes:make-completion))
