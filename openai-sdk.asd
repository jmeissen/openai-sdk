;;;; openai-sdk.asd

(asdf:defsystem #:openai-sdk
  :description "OpenAI SDK API"
  :author "Jeffrey Meissen <jeffrey@meissen.email>"
  :license  "MIT"
  :version "0.2.1"
  :serial t
  :components ((:module "src"
                :components ((:file "util")
                             (:file "core")
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
               :log4cl))
