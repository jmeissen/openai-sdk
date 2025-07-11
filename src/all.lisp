(in-package #:cl-user)

(uiop:define-package openai-sdk/all
  (:use #:cl)
  (:nicknames #:openai-sdk
              #:oai)
  (:shadowing-import-from #:openai-sdk/interface #:type #:format #:function #:stream)
  (:use-reexport #:openai-sdk/interface)
  (:use-reexport #:openai-sdk/util)
  (:use-reexport #:openai-sdk/client))

(in-package #:openai-sdk/all)
