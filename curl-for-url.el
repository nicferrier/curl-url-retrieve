;;; curl-for-url.el --- use url-retrieve with curl doing the work  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: hypermedia
;; Version: 0.0.1
;; Package-requires: ((noflet "0.0.15"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Sometimes curl is more reliable than Emacs internal url stuff.

;; This provides the `url-retrieve' function but with curl.  This is
;; useful in weird environments where curl does better than
;; `url-retrieve'.


;;; Code:

(require 'noflet)

(defmacro comment (&rest code))

;; this is how url-retrieve works.
(comment
 (url-retrieve
  "http://nic.ferrier.me.uk"
  (lambda (status &rest cbargs)
    (message "url retrieve done")))

 ;; This is how the curl works
 (curl-call
  (url-generic-parse-url (url-encode-url "http://nic.ferrier.me.uk"))
  nil
  (lambda (&rest args)
    (message
     "url retrieve done: [%s] %s"
     url-http-response-status
     (buffer-substring-no-properties
      url-http-end-of-headers (+ url-http-end-of-headers 50))))))

(defun curl-call (url data callback &optional cbargs)
  (let* (connection ; dummy var for url-retrieve interop
         (retry-buffer (generate-new-buffer "http-curl"))
         (url-string (format "%s://%s:%s%s"
                             (url-type url)
                             (url-host url)
                             (url-port url)
                             (url-filename url)))
         (curl-name (format "*curl-%s-%s*" url-string url-request-method))
         (proc
          (start-process
           curl-name curl-name
           "curl" "-i" url-string)))
    (with-current-buffer (process-buffer proc) ; stuff ripped out of url-http
      (mm-disable-multibyte)
      (setq url-current-object url mode-line-format "%b [%s]")
      (dolist (var '(url-http-end-of-headers
                     url-http-content-type
                     url-http-content-length
                     url-http-transfer-encoding
                     url-http-after-change-function
                     url-http-response-version
                     url-http-response-status
                     url-http-chunked-length
                     url-http-chunked-counter
                     url-http-chunked-start
                     url-callback-function
                     url-callback-arguments
                     url-show-status
                     url-http-process
                     url-http-method
                     url-http-extra-headers
                     url-http-data
                     url-http-target-url
                     url-http-no-retry
                     url-http-connection-opened
                     url-http-proxy))
        (set (make-local-variable var) nil))
      (setq url-http-method (or url-request-method "GET")
            url-http-extra-headers url-request-extra-headers
            url-http-data url-request-data
            url-http-process connection
            url-http-chunked-length nil
            url-http-chunked-start nil
            url-http-chunked-counter 0
            url-callback-function callback
            url-callback-arguments cbargs
            url-http-after-change-function 'url-http-wait-for-headers-change-function
            url-http-target-url url-current-object
            url-http-no-retry retry-buffer
            url-http-connection-opened nil
            url-http-proxy url-using-proxy))
    (set-process-sentinel
     proc
     (lambda (proc evt)
       (cl-case (intern (car (split-string evt "\n")))
         ('finished
          (with-current-buffer (process-buffer proc)
            (noflet ((url-http-mark-connection-as-free (&rest params) nil))
              (let ((url-http-end-of-headers
                     (save-excursion
                       (goto-char (point-min))
                       (re-search-forward "\r\n\r\n" nil t))))
                (url-http-end-of-url-request-method
                 document-sentinel proc evt))))))))))

(defun url-http-with-curl (url callback cbargs &optional retry-buffer)
  (curl-call url callback cbargs))


(provide 'curl-for-url)

;;; curl-for-url.el ends here
