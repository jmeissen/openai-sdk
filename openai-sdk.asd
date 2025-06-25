;;;; openai-sdk.asd

(asdf:defsystem #:openai-sdk
  :description "OpenAI SDK API"
  :author "Jeffrey Meissen <jeffrey@meissen.email>"
  :license  "MIT"
  :version "0.5.0"
  :serial t
  :components ((:module "src"
                :components ((:file "core")
                             (:file "util")
                             (:file "json")
                             (:file "client")
                             (:file "response")
                             (:file "chat-completion")
                             (:file "request")
                             (:file "tool-call")
                             (:file "structured-output")
                             (:file "tool-schema-generator")
                             (:file "openai-sdk"))))
  :depends-on (:str
               :dexador
               :alexandria
               :com.inuoe.jzon
               :symbol-munger
               :cl-ppcre
               :log4cl
               :qbase64
               :introspect-environment))
