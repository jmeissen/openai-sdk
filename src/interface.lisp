(in-package #:cl-user)

(defpackage openai-sdk/interface
  (:import-from #:cl #:defgeneric #:defclass #:&optional)
  (:shadow #:format #:function #:type)
  (:export #:accepted-prediction-tokens
           #:annotations
           #:api-key
           #:approximate
           #:arguments
           #:audio
           #:audio-tokens
           #:b64encode
           #:bad-request-code
           #:bad-request-message
           #:bad-request-param
           #:bad-request-type
           #:base-url
           #:bytes
           #:cached-tokens
           #:choices
           #:city
           #:completion-tokens
           #:completion-tokens-details
           #:connect-timeout
           #:content
           #:country
           #:created
           #:data
           #:default-headers
           #:default-model
           #:delta
           #:description
           #:detail
           #:end-index
           #:expires-at
           #:file
           #:file-data
           #:file-id
           #:filename
           #:finish-reason
           #:format
           #:frequency-penalty
           #:function
           #:function-call
           #:functions
           #:id
           #:image-url
           #:include-usage
           #:index
           #:input-audio
           #:json-schema
           #:logit-bias
           #:logprob
           #:logprobs
           #:max-completion-tokens
           #:max-retries
           #:max-tokens
           #:message
           #:messages
           #:metadata
           #:modalities
           #:model
           #:n
           #:name
           #:object
           #:openai-json-serializable
           #:organization-id
           #:parallel-tool-calls
           #:parameters
           #:prediction
           #:presence-penalty
           #:project-id
           #:prompt-tokens
           #:prompt-tokens-details
           #:read-timeout
           #:reasoning-effort
           #:reasoning-tokens
           #:refusal
           #:region
           #:rejected-prediction-tokens
           #:response-format
           #:role
           #:schema
           #:search-context-size
           #:seed
           #:send
           #:service-list
           #:service-tier
           #:start-index
           #:stop
           #:store
           #:stream
           #:stream-options
           #:strict
           #:system-fingerprint
           #:temperature
           #:text
           #:timezone
           #:title
           #:token
           #:tool-call-id
           #:tool-calls
           #:tool-choice
           #:tools
           #:top-logprobs
           #:top-p
           #:total-tokens
           #:transcript
           #:type
           #:url
           #:url-citation
           #:usage
           #:user
           #:user-location
           #:voice
           #:web-search-options))

(in-package #:openai-sdk/interface)

(defclass openai-json-serializable () ()
  (:documentation
   "Inherit from OPENAI-JSON-SERIALIZABLE if the child-class slot-names must be
 serialized to snake_case with `com.inuoe.jzon'."))

(defgeneric accepted-prediction-tokens (object))
(defgeneric annotations (object))
(defgeneric api-key (object))
(defgeneric approximate (object))
(defgeneric arguments (object))
(defgeneric audio (object))
(defgeneric audio-tokens (object))
(defgeneric b64encode (object))
(defgeneric base-url (object))
(defgeneric bytes (object))
(defgeneric cached-tokens (object))
(defgeneric choices (object))
(defgeneric city (object))
(defgeneric completion-tokens (object))
(defgeneric completion-tokens-details (object))
(defgeneric connect-timeout (object))
(defgeneric content (object))
(defgeneric country (object))
(defgeneric created (object))
(defgeneric data (object))
(defgeneric default-headers (object))
(defgeneric default-model (object))
(defgeneric delta (object))
(defgeneric description (object))
(defgeneric detail (object))
(defgeneric end-index (object))
(defgeneric expires-at (object))
(defgeneric file (object))
(defgeneric file-data (object))
(defgeneric file-id (object))
(defgeneric filename (object))
(defgeneric finish-reason (object))
(defgeneric format (object))
(defgeneric frequency-penalty (object))
(defgeneric function (object))
(defgeneric function-call (object))
(defgeneric functions (object))
(defgeneric id (object))
(defgeneric image-url (object))
(defgeneric include-usage (object))
(defgeneric index (object))
(defgeneric input-audio (object))
(defgeneric json-schema (object))
(defgeneric logit-bias (object))
(defgeneric logprob (object))
(defgeneric logprobs (object))
(defgeneric max-completion-tokens (object))
(defgeneric max-retries (object))
(defgeneric max-tokens (object))
(defgeneric message (object))
(defgeneric messages (object))
(defgeneric metadata (object))
(defgeneric modalities (object))
(defgeneric model (object))
(defgeneric n (object))
(defgeneric name (object))
(defgeneric object (object))
(defgeneric organization-id (object))
(defgeneric parallel-tool-calls (object))
(defgeneric parameters (object))
(defgeneric prediction (object))
(defgeneric presence-penalty (object))
(defgeneric project-id (object))
(defgeneric prompt-tokens (object))
(defgeneric prompt-tokens-details (object))
(defgeneric read-timeout (object))
(defgeneric reasoning-effort (object))
(defgeneric reasoning-tokens (object))
(defgeneric refusal (object))
(defgeneric region (object))
(defgeneric rejected-prediction-tokens (object))
(defgeneric response-format (object))
(defgeneric role (object))
(defgeneric schema (object))
(defgeneric search-context-size (object))
(defgeneric seed (object))
(defgeneric send (request &optional client))
(defgeneric service-list (object))
(defgeneric service-tier (object))
(defgeneric start-index (object))
(defgeneric stop (object))
(defgeneric store (object))
(defgeneric stream (object))
(defgeneric stream-options (object))
(defgeneric strict (object))
(defgeneric system-fingerprint (object))
(defgeneric temperature (object))
(defgeneric text (object))
(defgeneric timezone (object))
(defgeneric title (object))
(defgeneric token (object))
(defgeneric tool-call-id (object))
(defgeneric tool-calls (object))
(defgeneric tool-choice (object))
(defgeneric tools (object))
(defgeneric top-logprobs (object))
(defgeneric top-p (object))
(defgeneric total-tokens (object))
(defgeneric transcript (object))
(defgeneric type (object))
(defgeneric url (object))
(defgeneric url-citation (object))
(defgeneric usage (object))
(defgeneric user (object))
(defgeneric user-location (object))
(defgeneric voice (object))
(defgeneric web-search-options (object))

(defgeneric bad-request-message (object))
(defgeneric bad-request-type (object))
(defgeneric bad-request-parameter (object))
(defgeneric bad-request-code (object))
