(in-package #:cl-user)

(defpackage openai-sdk
  (:use #:cl
        #:openai-sdk/util
        #:openai-sdk/client)
  (:nicknames #:oai)
  (:shadowing-import-from #:openai-sdk/generics #:function #:type)
  (:import-from #:openai-sdk/generics
                #:arguments
                #:audio
                #:content
                #:function-call
                #:id
                #:name
                #:refusal
                #:role
                #:tool-calls)
  (:shadowing-import-from #:openai-sdk/response #:format)
  (:import-from #:openai-sdk/request
                #:create-chat-completion
                #:description
                #:json-schema
                #:make-assistant-message
                #:make-assistant-message-tool-call
                #:make-chat-completion-audio
                #:make-developer-message
                #:make-function
                #:make-function-message
                #:make-json-schema
                #:make-metadata
                #:make-response-format
                #:make-system-message
                #:make-tool-message
                #:make-user-message
                #:parameters
                #:text
                #:voice)
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
           #:voice))

(in-package #:openai-sdk)
