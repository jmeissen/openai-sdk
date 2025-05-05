;;;; openai-sdk.asd

(asdf:defsystem #:openai-sdk
  :description "OpenAI SDK API"
  :author "Jeffrey Meissen <jeffrey@meissen.email>"
  :license  "MIT"
  :version "0.3.0"
  :serial t
  :components ((:module "src"
                :components ((:file "core")
                             (:file "util")
                             (:file "client")
                             (:file "response")
                             (:file "chat-completion")
                             (:file "request")
                             (:file "openai-sdk"))))
  :depends-on (:dexador
               :alexandria
               :com.inuoe.jzon
               :symbol-munger
               :cl-ppcre
               :log4cl
               :qbase64))
