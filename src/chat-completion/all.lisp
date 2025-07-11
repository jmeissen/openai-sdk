(in-package #:cl-user)

(uiop:define-package openai-sdk/chat-completion/all
  (:use #:cl #:openai-sdk/interface)
  (:shadowing-import-from #:openai-sdk/interface
                          #:function
                          #:type
                          #:format
                          #:stream)
  (:nicknames #:openai-sdk/chat-completion
              #:oai/cc)
  (:use-reexport #:openai-sdk/chat-completion/classes)
  (:import-from #:openai-sdk/chat-completion/request))

(in-package #:openai-sdk/chat-completion/all)
