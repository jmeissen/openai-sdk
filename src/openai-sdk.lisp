(in-package #:cl-user)

(defpackage openai-sdk
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
                #:name
                #:refusal
                #:role
                #:tool-calls
                #:text
                #:type
                #:url)
  (:import-from #:openai-sdk/chat-completion
                #:make-assistant-message
                #:make-developer-message
                #:make-function-message
                #:make-system-message
                #:make-tool-message
                #:make-user-message)
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

  (:export #:*default-base-url*
           #:*default-headers*
           #:*default-model*
           #:api-key
           #:arguments
           #:audio
           #:base-url
           #:connect-timeout
           #:content
           #:choices
           #:choice
           #:create-chat-completion
           #:default-headers
           #:default-model
           #:description
           #:format
           #:function
           #:function-call
           #:id
           #:json-schema
           #:make-assistant-message
           #:make-assistant-message-tool-call
           #:make-chat-completion-audio
           #:make-developer-message
           #:make-function
           #:make-function-message
           #:make-json-schema
           #:make-metadata
           #:make-openai
           #:make-response-format
           #:make-system-message
           #:make-tool-message
           #:make-user-message
           #:message
           #:max-retries
           #:name
           #:openai
           #:organization-id
           #:parameters
           #:project-id
           #:read-timeout
           #:refusal
           #:role
           #:text
           #:tool-calls
           #:type
           #:voice)
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
