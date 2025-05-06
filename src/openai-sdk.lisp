(in-package #:cl-user)

(uiop:define-package openai-sdk
  (:use #:openai-sdk/util
        #:openai-sdk/client)
  (:nicknames #:oai)
  (:import-from #:openai-sdk/core
                #:arguments
                #:audio
                #:content
                #:data
                #:format
                #:function
                #:function-call
                #:id
                #:image-url
                #:input-audio
                #:message
                #:name
                #:refusal
                #:role
                #:tool-calls
                #:text
                #:type
                #:url)
  (:import-from #:openai-sdk/chat-completion
                #:json-schema
                #:description
                #:make-approximate
                #:make-assistant-message
                #:make-assistant-message-audio
                #:make-chat-completion
                #:make-chat-completion-audio
                #:make-chat-completion-function-call
                #:make-developer-message
                #:make-file-data-content-part
                #:make-file-id-content-part
                #:make-function
                #:make-function-call
                #:make-function-message
                #:make-json-schema
                #:make-message-from-keyword
                #:make-prediction
                #:make-prediction-content-part
                #:make-response-format
                #:make-stream-options
                #:make-system-message
                #:make-text-content-part
                #:make-tool-call
                #:make-tool-choice
                #:make-tool
                #:make-tool-message
                #:make-user-location
                #:make-user-message
                #:make-web-search-options
                #:parameters
                #:voice)
  (:import-from #:openai-sdk/request
                #:create-chat-completion)
  (:import-from #:openai-sdk/response
                #:accepted-prediction-tokens
                #:annotations
                #:audio-tokens
                #:bytes
                #:cached-tokens
                #:choice
                #:choices
                #:completion-tokens
                #:completion-tokens-details
                #:created
                #:data
                #:end-index
                #:expires-at
                #:finish-reason
                #:index
                #:logprob
                #:logprobs
                #:message
                #:model
                #:object
                #:prompt-tokens
                #:prompt-tokens-details
                #:reasoning-tokens
                #:rejected-prediction-tokens
                #:service-tier
                #:start-index
                #:system-fingerprint
                #:title
                #:token
                #:top-logprobs
                #:total-tokens
                #:transcript
                #:url
                #:url-citation
                #:usage)
  (:export

   ;; core
   #:arguments
   #:audio
   #:content
   #:data
   #:function
   #:function-call
   #:format
   #:id
   #:image-url
   #:input-audio
   #:name
   #:refusal
   #:role
   #:tool-calls
   #:type
   #:text
   #:url

   ;; client
   #:*default-base-url*
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
   #:read-timeout

   ;; request
   #:create-chat-completion

   ;; chat completion
   #:json-schema
   #:make-assistant-message-tool-call
   #:make-approximate
   #:make-assistant-message
   #:make-assistant-message-audio
   #:make-chat-completion
   #:make-chat-completion-audio
   #:make-chat-completion-function-call
   #:make-developer-message
   #:make-file-data-content-part
   #:make-file-id-content-part
   #:make-function
   #:make-function-call
   #:make-function-message
   #:make-json-schema
   #:make-message-from-keyword
   #:make-prediction
   #:make-prediction-content-part
   #:make-response-format
   #:make-stream-options
   #:make-system-message
   #:make-text-content-part
   #:make-tool
   #:make-tool-call
   #:make-tool-choice
   #:make-tool-message
   #:make-user-location
   #:make-user-message
   #:make-web-search-options
   #:parameters
   #:voice

   ;; response
   #:accepted-prediction-tokens
   #:annotations
   #:audio-tokens
   #:bytes
   #:cached-tokens
   #:choice
   #:choices
   #:completion-tokens
   #:completion-tokens-details
   #:created
   #:data
   #:end-index
   #:expires-at
   #:finish-reason
   #:index
   #:logprob
   #:logprobs
   #:message
   #:model
   #:object
   #:prompt-tokens
   #:prompt-tokens-details
   #:reasoning-tokens
   #:rejected-prediction-tokens
   #:service-tier
   #:start-index
   #:system-fingerprint
   #:title
   #:token
   #:top-logprobs
   #:total-tokens
   #:transcript
   #:url
   #:url-citation
   #:usage))

(in-package #:openai-sdk)
