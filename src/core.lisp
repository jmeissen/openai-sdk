(in-package #:cl-user)

(uiop:define-package openai-sdk/core
  (:use #:cl)
  (:import-from #:openai-sdk/util #:objectify)
  (:nicknames #:oai/core)
  (:export #:request))

(in-package #:openai-sdk/core)

(defun request (method base-url path bearer-auth headers max-retries connect-timeout read-timeout content &optional response-object)
  (multiple-value-bind (body code response-headers uri stream)
      (let ((retry-request (dex:retry-request max-retries :interval 2)))
        (handler-bind ((dex:http-request-internal-server-error retry-request)
                       (dex:http-request-not-implemented retry-request)
                       (dex:http-request-service-unavailable retry-request)
                       (dex:http-request-bad-gateway retry-request))
          (dex:request (concatenate 'string base-url path)
                       :method method
                       :bearer-auth bearer-auth
                       :headers headers
                       :connect-timeout connect-timeout
                       :read-timeout read-timeout
                       :content content)))
    (values (let ((parsed-body (com.inuoe.jzon:parse body)))
              (if response-object
                  (objectify response-object parsed-body)
                  parsed-body))
            code
            response-headers
            uri
            stream)))
