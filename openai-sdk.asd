;;;; openai-sdk.asd
#-asdf3.1 (error "system-name requires ASDF 3.1 or later.")
(asdf:defsystem #:openai-sdk
  :class :package-inferred-system
  :pathname #p"src/"
  :description "OpenAI SDK API"
  :author "Jeffrey Meissen <jeffrey@meissen.email>"
  :license "MIT"
  :version "0.0.1"
  :depends-on (:log4cl
               :str
               :dexador
               :alexandria
               :com.inuoe.jzon
               :symbol-munger
               :cl-ppcre
               :log4cl
               :qbase64
               :openai-sdk/all
               :openai-sdk/chat-completion/all
               :openai-sdk/platform-apis/all))
