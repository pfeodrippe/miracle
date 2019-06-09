;;; -*- indent-tabs-mode: nil -*-
;;; miracle.el --- An Arcadia opiniated fork of monroe, the nREPL client

;; Copyright (c) 2014-2018 Sanel Zukan
;;
;; Author: Jona Ekenberg <saikyun@gmail.com>
;; URL: http://www.github.com/Saikyun/miracle
;; Version: 0.4.0
;; Keywords: languages, clojure, nrepl, lisp

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; An arcadia opiniated fork of monroe, the nREPL client.
;; Much of the code for completion is stolen from CIDER, thank you all cider contributors for this huge sip, long live CIDER!

;;; Installation:

;; Copy it to your load-path and run with:
;; M-: (require 'miracle)

;;; Usage:

;; M-x miracle

;;; Code:

(require 'comint)
(require 'subr-x)
(require 'parseedn)
(eval-when-compile
  (require 'cl))

(defvar miracle-extra-eldoc-commands '("yas-expand")
  "Extra commands to be added to eldoc's safe commands list.")

(defgroup miracle nil
  "Interaction with the nREPL Server."
  :prefix "miracle-"
  :group 'applications)

(defcustom miracle-use-orchardia t
  "When true, uses orchardia."
  :type 'boolean
  :group 'miracle)

(defcustom miracle-repl-prompt-format "%s=> "
  "String used for displaying prompt. '%s' is used as
placeholder for storing current namespace."
  :type 'string
  :group 'miracle)

(defcustom miracle-prompt-regexp "^[^> \n]*>+:? *"
  "Regexp to recognize prompts in Miracle more. The same regexp is
used in inferior-lisp."
  :type 'regexp
  :group 'miracle)

(defcustom miracle-default-host "localhost:3722"
  "Default location where to connect to, unless explicitly given
location and port. Location and port should be delimited with ':'."
  :type 'string
  :group 'miracle)

(defcustom miracle-detail-stacktraces nil
  "If set to true, Miracle will try to get full stacktrace from thrown
exception. Otherwise will just behave as standard REPL version."
  :type 'boolean
  :group 'miracle)

(defcustom miracle-old-style-stacktraces nil
  "If set to true, Miracle will try to emit old style Clojure stacktraces
using 'clojure.stacktrace/print-stack-trace'. This will work on older Clojure versions (e.g. 1.2)
but will NOT work on ClojureScript. This option assumes 'miracle-detail-stacktraces' is true.

DEPRECATED; use miracle-print-stack-trace-function instead."
  :type 'boolean
  :group 'miracle)

(defcustom miracle-print-stack-trace-function nil
  "Set to a clojure-side function in order to override stack-trace printing.

Will be called upon error when `miracle-detail-stacktraces' is non-nil.

e.g. 'clojure.stacktrace/print-stack-trace for old-style stack traces."
  :type 'symbol
  :group 'miracle)

(defvar miracle-version "0.4.1"
  "The current miracle version.")

(defvar miracle-session nil
  "Current nREPL session id.")

(defvar miracle-requests (make-hash-table :test 'equal)
  "Map of requests to be processed.")

(defvar miracle-requests-counter 0
  "Serial number for message.")

(defvar miracle-custom-handlers (make-hash-table :test 'equal)
  "Map of handlers for custom ops.")

(defvar miracle-repl-buffer "*miracle*"
  "Name of nREPL buffer.")

(defvar miracle-buffer-ns "user"
  "Current clojure namespace for this buffer. This namespace
is only advertised until first expression is evaluated, then is updated
to the one used on nrepl side.")

(defvar miracle-nrepl-server-cmd "lein"
  "Command to start nrepl server. Defaults to Leiningen")

(defvar miracle-nrepl-server-cmd-args "trampoline repl :headless"
  "Arguments to pass to the nrepl command. Defaults to 'trampoline repl :headless'")

(defvar miracle-nrepl-server-buffer-name "miracle nrepl server")

(defvar miracle-nrepl-server-project-file "project.clj")

(defvar *miracle-project-path* nil
  "This is set to the running path of the nREPL server
that miracle is is connected to.")

(defvar miracle-repl--root-ns-highlight-template "\\_<\\(%s\\)[^$/: \t\n()]+"
  "Regexp used to highlight root ns in REPL buffers.")

(defvar-local miracle-repl--ns-forms-plist nil
  "Plist holding ns->ns-form mappings within each connection.")

(defvar-local miracle-repl--ns-roots nil
  "List holding all past root namespaces seen during interactive eval.")

(defvar-local miracle-repl--root-ns-regexp nil
  "Cache of root ns regexp in REPLs.")

(make-variable-buffer-local 'miracle-session)
(make-variable-buffer-local 'miracle-requests)
(make-variable-buffer-local 'miracle-requests-counter)
(make-variable-buffer-local 'miracle-buffer-ns)

;;; message stuff

;; Idea for message handling (via callbacks) and destructuring response is shamelessly
;; stolen from nrepl.el.
(defmacro miracle-dbind-response (response keys &rest body)
  "Destructure an nREPL response dict."
  `(let ,(loop for key in keys
               collect `(,key (cdr (assoc ,(format "%s" key) ,response))))
     ,@body))

(defun miracle-eldoc-beginning-of-sexp ()
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Move to the beginning of current sexp.

Return the number of nested sexp the point was over or after.  Return nil
if the maximum number of sexps to skip is exceeded."
  (let ((parse-sexp-ignore-comments t)
        (num-skipped-sexps 0))
    (condition-case _
        (progn
          ;; First account for the case the point is directly over a
          ;; beginning of a nested sexp.
          (condition-case _
              (let ((p (point)))
                (forward-sexp -1)
                (forward-sexp 1)
                (when (< (point) p)
                  (setq num-skipped-sexps 1)))
            (error))
          (while
              (let ((p (point)))
                (forward-sexp -1)
                (when (< (point) p)
                  (setq num-skipped-sexps
                        (unless (and 30
                                     (>= num-skipped-sexps
                                         30))
                          ;; Without the above guard,
                          ;; `miracle-eldoc-beginning-of-sexp' could traverse the
                          ;; whole buffer when the point is not within a
                          ;; list. This behavior is problematic especially with
                          ;; a buffer containing a large number of
                          ;; non-expressions like a REPL buffer.
                          (1+ num-skipped-sexps)))))))
      (error))
    num-skipped-sexps))

(defun miracle-in-comment-p ()
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Return non-nil if point is in a comment."
  (let ((beg (save-excursion (beginning-of-defun) (point))))
    (nth 4 (parse-partial-sexp beg (point)))))

(defun miracle-symbol-at-point (&optional look-back)
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Return the name of the symbol at point, otherwise nil.
Ignores the REPL prompt.  If LOOK-BACK is non-nil, move backwards trying to
find a symbol if there isn't one at point."
  (or (when-let* ((str (thing-at-point 'symbol)))
        (substring-no-properties str))
      (when look-back
        (save-excursion
          (ignore-errors
            (while (not (looking-at "\\sw\\|\\s_\\|\\`"))
              (forward-sexp -1)))
          (miracle-symbol-at-point)))))

(defun miracle-eldoc-info-at-sexp-beginning ()
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES."
  (save-excursion
    (when-let* ((beginning-of-sexp (miracle-eldoc-beginning-of-sexp))
                ;; If we are at the beginning of function name, this will be -1
                (argument-index (max 0 (1- beginning-of-sexp))))
      (unless (or (memq (or (char-before (point)) 0)
                        '(?\" ?\{ ?\[))
                  (miracle-in-comment-p))
        (when-let* ((eldoc-info (miracle-eval-eldoc
                                 (miracle-symbol-at-point))))
          eldoc-info)))))

;;; Bencode
;;; Stolen from nrepl.el which is adapted from http://www.emacswiki.org/emacs-en/bencode.el
(defun miracle-bdecode-buffer ()
  "Decode a bencoded string in the current buffer starting at point."
  (cond
   ((looking-at "i\\([-0-9]+\\)e")
    (goto-char (match-end 0))
    (string-to-number (match-string 1)))
   ((looking-at "\\([0-9]+\\):")
    (goto-char (match-end 0))
    (let* ((start (point))
           (end (byte-to-position (+ (position-bytes start) (string-to-number (match-string 1))))))
      (goto-char end)
      (buffer-substring-no-properties start end)))
   ((looking-at "l")
    (goto-char (match-end 0))
    (let (result item)
      (while (setq item (miracle-bdecode-buffer))
        (setq result (cons item result)))
      (nreverse result)))
   ((looking-at "d")
    (goto-char (match-end 0))
    (let (dict key item)
      (while (setq item (miracle-bdecode-buffer))
        (if key
            (setq dict (cons (cons key item) dict)
                  key nil)
          (unless (stringp item)
            (error "Dictionary keys have to be strings: %s" item))
          (setq key item)))
      (cons 'dict (nreverse dict))))
   ((looking-at "e")
    (goto-char (match-end 0))
    nil)
   (t
    (error "Cannot decode object: %d" (point)))))

(defun miracle-encode (message)
  "Encode message to nrepl format. The message format is
'd<key-len>:key<val-len>:value<key-len>:key<val-len>:valuee', where the message is
starting with 'd' and ending with 'e'."
  (concat "d"
          (apply 'concat
                 (mapcar (lambda (str)
                           (let ((s (if str str "")))
                             (format "%d:%s" (string-bytes s) s)))
                         message))
          "e"))

(defun miracle-decode (str)
  "Decode message using temporary buffer."
  (with-temp-buffer
    (save-excursion (insert str))
    (let ((result '()))
      (while (not (eobp))
        (setq result (cons (miracle-bdecode-buffer) result)))
      (nreverse result))))

(defun miracle-write-message (process message)
  "Send message to given process."
  (process-send-string process message))

(defun miracle-send-request (request callback)
  "Send request as elisp object and assign callback to
be called when reply is received."
  (let* ((id       (number-to-string (incf miracle-requests-counter)))
         (message  (append (list "id" id) request))
         (bmessage (miracle-encode message)))
    (puthash id callback miracle-requests)
    (miracle-write-message "*miracle-connection*" bmessage)))

(defun miracle-clear-request-table ()
  "Erases current request table."
  (clrhash miracle-requests)
  (setq miracle-requests-counter 0))

(defun miracle-current-session ()
  "Returns current session id."
  (with-current-buffer "*miracle-connection*"
    miracle-session))

;;; nrepl messages we knows about

(defun miracle-send-hello (callback)
  "Initiate nREPL session."
  (miracle-send-request '("op" "clone") callback))

(defun miracle-send-describe (callback)
  "Produce a machine- and human-readable directory and documentation for
the operations supported by an nREPL endpoint."
  (miracle-send-request '("op" "describe") callback))

(defun miracle-send-eval-string (str callback)
  "Send code for evaluation on given namespace."
  (miracle--prep-interactive-eval str "*miracle-connection*")
  (miracle-send-request (list "op" "eval"
                              "session" (miracle-current-session)
                              "code" str)
                        callback))

(defun miracle-send-stdin (str callback)
  "Send stdin value."
  (miracle-send-request (list "op" "stdin"
                              "session" (miracle-current-session)
                              "stdin" str)
                        callback))

(defun miracle-send-interrupt (request-id callback)
  "Send interrupt for pending requests."
  (miracle-send-request (list "op" "interrupt"
                              "session" (miracle-current-session)
                              "interrupt-id" request-id)
                        callback))

;;; code

(defconst miracle-namespace-name-regex
  (rx line-start
      "("
      (zero-or-one (group (regexp "clojure.core/")))
      (zero-or-one (submatch "in-"))
      "ns"
      (zero-or-one "+")
      (one-or-more (any whitespace "\n" ""))
      (zero-or-more (or (submatch (zero-or-one "#")
                                  "^{"
                                  (zero-or-more (not (any "}")))
                                  "}")
                        (zero-or-more "^:"
                                      (one-or-more (not (any whitespace)))))
                    (one-or-more (any whitespace "\n" "")))
      (zero-or-one (any ":'")) ;; (in-ns 'foo) or (ns+ :user)
      (group (one-or-more (not (any "()\"" whitespace))) symbol-end)))

(defun miracle-find-ns ()
  "Return the namespace of the current Clojure buffer.
Return the namespace closest to point and above it.  If there are
no namespaces above point, return the first one in the buffer.

This has been taken from `clojure-mode` and modified to handle the
mixed newlines of the clojure core packages."
  (let ((ns (save-excursion
              (save-restriction
                (widen)

                ;; Move to top-level to avoid searching from inside ns
                (ignore-errors (while t (up-list nil t t)))

                ;; The closest ns form above point.
                (when (or (re-search-backward miracle-namespace-name-regex nil t)
                          ;; Or any form at all.
                          (and (goto-char (point-min))
                               (re-search-forward miracle-namespace-name-regex nil t)))
                  (match-string-no-properties 4))))))
    ns))

(defun miracle--text-or-limits (bounds start end)
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Returns the substring or the bounds of text.
If BOUNDS is non-nil, returns the list (START END) of character
positions.  Else returns the substring from START to END."
  (funcall (if bounds #'list #'buffer-substring-no-properties)
           start end))

(defun miracle-defun-at-point (&optional bounds)
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Return the text of the top level sexp at point.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (clojure-backward-logical-sexp 1)
        (miracle--text-or-limits bounds (point) end)))))

(defun miracle-ns-form ()
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Retrieve the ns form."
  (when (miracle-find-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (miracle-defun-at-point))))

(defun miracle-ns-form-p (form)
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Check if FORM is an ns form."
  (string-match-p "^[[:space:]]*\(ns\\([[:space:]]*$\\|[[:space:]]+\\)" form))

(defun miracle-ns-from-form (ns-form)
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Get ns substring from NS-FORM."
  (when (string-match "^[ \t\n]*\(ns[ \t\n]+\\([^][ \t\n(){}]+\\)" ns-form)
    (match-string-no-properties 1 ns-form)))

(defun miracle-repl--ns-form-changed-p (ns-form connection)
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Return non-nil if NS-FORM for CONNECTION changed since last eval."
  (when-let* ((ns (miracle-ns-from-form ns-form)))
    (not (string= ns-form
                  (lax-plist-get
                   (buffer-local-value 'miracle-repl--ns-forms-plist connection)
                   ns)))))

(defun miracle-repl--cache-ns-form (ns-form connection)
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Given NS-FORM cache root ns in CONNECTION."
  (with-current-buffer connection
    (when-let* ((ns (miracle-ns-from-form ns-form)))
      ;; cache ns-form
      (setq miracle-repl--ns-forms-plist
            (lax-plist-put miracle-repl--ns-forms-plist ns ns-form))
      ;; cache ns roots regexp
      (when (string-match "\\([^.]+\\)" ns)
        (let ((root (match-string-no-properties 1 ns)))
          (unless (member root miracle-repl--ns-roots)
            (push root miracle-repl--ns-roots)
            (let ((roots (mapconcat
                          ;; Replace _ or - with regexp pattern to accommodate "raw" namespaces
                          (lambda (r) (replace-regexp-in-string "[_-]+" "[_-]+" r))
                          miracle-repl--ns-roots "\\|")))
              (setq miracle-repl--root-ns-regexp
                    (format miracle-repl--root-ns-highlight-template roots)))))))))

(defun miracle-eldoc--convert-ns-keywords (thing)
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Convert THING values that match ns macro keywords to function names."
  (pcase thing
    (":import" "clojure.core/import")
    (":refer-clojure" "clojure.core/refer-clojure")
    (":use" "clojure.core/use")
    (":refer" "clojure.core/refer")
    (_ thing)))

(defun miracle--prep-interactive-eval (form connection)
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Prepare the environment for an interactive eval of FORM in CONNECTION.
Ensure the current ns declaration has been evaluated (so that the ns
containing FORM exists).  Cache ns-form in the current buffer unless FORM is
ns declaration itself.  Clear any compilation highlights and kill the error
window."
  (let ((cur-ns-form (miracle-ns-form)))
    (when (and cur-ns-form
               (not (miracle-ns-form-p form))
               (miracle-repl--ns-form-changed-p cur-ns-form
                                                (get-buffer
                                                 miracle-repl-buffer)))
      (miracle-send-sync-request (format "(do %s)" cur-ns-form) connection)
      ;; cache at the end, in case of errors
      (miracle-repl--cache-ns-form cur-ns-form connection))))

(defun miracle-make-response-handler ()
  "Returns a function that will be called when event is received."
  (lambda (response)
    (miracle-dbind-response response (id ns value err out ex root-ex status)
                            (let ((output (concat err out
                                                  (if value
                                                      (concat value "\n"))))
                                  (process (get-buffer-process miracle-repl-buffer)))
                              ;; update namespace if needed
                              (if ns (setq miracle-buffer-ns ns))
                              (comint-output-filter process output)
                              ;; now handle status
                              (when status
                                (when (and miracle-detail-stacktraces (member "eval-error" status))
                                  (miracle-get-stacktrace))
                                (when (member "eval-error" status)
                                  (message root-ex))
                                (when (member "interrupted" status)
                                  (message "Evaluation interrupted."))
                                (when (member "need-input" status)
                                  (miracle-handle-input))
                                (when (member "done" status)
                                  (remhash id miracle-requests)))
                              ;; show prompt only when no output is given in any of received vars
                              (unless (or err out value root-ex ex)
                                (comint-output-filter process (format miracle-repl-prompt-format miracle-buffer-ns)))))))

(defun miracle-send-sync-request (str connection &optional abort-on-input)
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES."
  (let* ((time0 (current-time))
         value0
         status0)
    (miracle-send-request (list "op" "eval"
                                "session" (miracle-current-session)
                                "code" (format "(do %s \n)" str))
                          (lambda (resp)
                            (miracle-dbind-response
                             resp (id ns value err out ex root-ex status)
                             (let ((process
                                    (get-buffer-process miracle-repl-buffer)))
                               (when value
                                 (setq value0 value))
                               (when ns
                                 (setq miracle-buffer-ns
                                       ns)
                                 (comint-output-filter
                                  process out)))
                             (setq status0 status))))
    (while (and (not (member "done" status0))
                (not (and abort-on-input
                          (input-pending-p))))
      (if status0
          (print status0))
      ;; If we get a need-input message then the repl probably isn't going
      ;; anywhere, and we'll just timeout. So we forward it to the user.
      (if (member "need-input" status0)
          (progn (miracle-handle-input (current-buffer))
                 ;; If the used took a few seconds to respond, we might
                 ;; unnecessarily timeout, so let's reset the timer.
                 (setq time0 (current-time)))
        ;; break out in case we don't receive a response for a while
        (when (and nrepl-sync-request-timeout
                   (> (cadr (time-subtract (current-time) time0))
                      10))
          (error "Sync nREPL request timed out %s" str)))
      ;; Clean up the response, otherwise we might repeatedly ask for input.
      ;; (nrepl-dict-put response "status" (remove "need-input" status0))
      (accept-process-output nil 0.01))
    (when (member "done" status0)
      value0)))

(defun miracle-input-sender (proc input)
  "Called when user enter data in REPL and when something is received in."
  (miracle-send-eval-string (format "(do %s \n)" input) (miracle-make-response-handler)))

(defun miracle-input-generic (proc input callback)
  "Called when user enter data in REPL and when something is received in."
  (miracle-send-eval-string (format "(do %s \n)" input) callback))

(defun miracle-handle-input ()
  "Called when requested user input."
  (miracle-send-stdin
   (concat (read-from-minibuffer "Stdin: ") "\n")
   (miracle-make-response-handler)))

(defun miracle-sentinel (process message)
  "Called when connection is changed; in out case dropped."
  (message "nREPL connection closed: %s" message)
  (kill-buffer (process-buffer process))
  (miracle-disconnect))

(defun miracle-dispatch (msg)
  "Find associated callback for a message by id or by op."
  (miracle-dbind-response msg (id op)
                          (let ((callback (or (gethash id miracle-requests)
                                              (gethash op miracle-custom-handlers))))
                            (when callback
                              (funcall callback msg)))))

(defun miracle-net-decode ()
  "Decode the data in the current buffer and remove the processed data from the
buffer if the decode successful."
  (let* ((start   (point-min))
         (end     (point-max))
         (data    (buffer-substring start end))
         (decoded (miracle-decode data)))
    (delete-region start end)
    decoded))

(defun miracle-net-filter (process string)
  "Called when the new message is received. Process will redirect
all received output to this function; it will decode it and put in
miracle-repl-buffer."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)
    ;; Stolen from Cider. Assure we have end of the message so decoding can work;
    ;; to make sure we are at the real end (session id can contain 'e' character), we call
    ;; 'accept-process-output' once more.
    ;;
    ;; This 'ignore-errors' is a hard hack here since 'accept-process-output' will call filter
    ;; which will be this function causing Emacs to hit max stack size limit.
    (ignore-errors
      (when (eq ?e (aref string (- (length string) 1)))
        (unless (accept-process-output process 0.01)
          (while (> (buffer-size) 1)
            (dolist (response (miracle-net-decode))
              (miracle-dispatch response))))))))

(defun miracle-new-session-handler (process)
  "Returns callback that is called when new connection is established."
  (lambda (response)
    (miracle-dbind-response response (id new-session)
                            (when new-session
                              (message "Connected.")
                              (setq miracle-session new-session)
                              (remhash id miracle-requests)
                              (sit-for 0.1) ;; Don't ask me, didn't work without it
                              (run-hooks 'miracle-connected-hook)))))

(defun miracle-valid-host-string (str default)
  "Used for getting valid string for host/port part."
  (if (and str (not (string= "" str)))
      str
    default))

(defun miracle-locate-port-file ()
  (locate-dominating-file default-directory ".nrepl-port"))

(defun miracle-locate-running-nrepl-host ()
  "Return host of running nREPL server."
  (let ((dir (miracle-locate-port-file)))
    (when dir
      (with-temp-buffer
        (insert-file-contents (concat dir ".nrepl-port"))
        (concat "localhost:" (buffer-string))))))

(defun miracle-strip-protocol (host)
  "Check if protocol was given and strip it."
  (let ((host (replace-regexp-in-string "[ \t]" "" host)))
    (if (string-match "^nrepl://" host)
        (substring host 8)
      host)))

(defun miracle-connect (host-and-port)
  "Connect to remote endpoint using provided hostname and port."
  (let* ((hp   (split-string (miracle-strip-protocol host-and-port) ":"))
         (host (miracle-valid-host-string (first hp) "localhost"))
         (port (string-to-number
                (miracle-valid-host-string (second hp) "7888"))))
    (message "Connecting to nREPL host on '%s:%d'..." host port)
    (let ((process (open-network-stream "miracle" "*miracle-connection*" host port)))
      (set-process-filter process 'miracle-net-filter)
      (set-process-sentinel process 'miracle-sentinel)
      (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
      (miracle-send-hello (miracle-new-session-handler (process-buffer process)))
      process)))

(defun miracle-disconnect ()
  "Disconnect from current nrepl connection. Calling this function directly
will force connection closing, which will as result call '(miracle-sentinel)'."
  (miracle-clear-request-table)
  (let ((delete-process-safe (lambda (p)
                               (when (and p (process-live-p p))
                                 (delete-process p))))
        ;; 'miracle-repl-buffer' process is actually 'fake-proc'
        (proc1 (get-buffer-process miracle-repl-buffer))
        (proc2 (get-buffer-process "*miracle-connection*")))
    (funcall delete-process-safe proc1)
    (funcall delete-process-safe proc2)))

;;; keys

(defun miracle-eval-region (start end)
  "Evaluate selected region."
  (interactive "r")
  (miracle-input-sender
   (get-buffer-process miracle-repl-buffer)
   (buffer-substring-no-properties start end)))

(defun miracle-eval-buffer ()
  "Evaluate the buffer."
  (interactive)
  (miracle-eval-region (point-min) (point-max)))

(defun miracle-eval-defun ()
  "Figure out top-level expression and send it to evaluation."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (miracle-eval-region (point) end))))

(defun miracle-eval-expression-at-point ()
  "Figure out expression at point and send it for evaluation."
  (interactive)
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (miracle-eval-region (point) end))))

(defun miracle-eval-namespace ()
  "Tries to evaluate Clojure ns form. It does this by matching first
expression at the beginning of the file and evaluating it. Not something
that is 100% accurate, but Clojure practice is to keep ns forms always
at the top of the file."
  (interactive)
  (when (miracle-find-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (miracle-eval-defun))))

(defun miracle-eval-doc (symbol)
  "Internal function to actually ask for symbol documentation via nrepl protocol."
  (miracle-input-sender
   (get-buffer-process miracle-repl-buffer)
   (format "(do (require 'clojure.repl) (clojure.repl/doc %s))" symbol)))

(defun miracle-eval-eldoc (symbol)
  "CODE TAKEN FROM CIDER AND MODIFIED FOR OUR PURPOSES.
Internal function to actually ask for symbol eldoc via nrepl protocol."
  (when miracle-use-orchardia
    (let ((thing (miracle-eldoc--convert-ns-keywords symbol)))
      (when (and thing
                 ;; ignore empty strings
                 (not (string= thing ""))
                 ;; ignore strings
                 (not (string-prefix-p "\"" thing))
                 ;; ignore regular expressions
                 (not (string-prefix-p "#" thing))
                 ;; ignore chars
                 (not (string-prefix-p "\\" thing))
                 ;; ignore numbers
                 (not (string-match-p "^[0-9]" thing)))
        (cond
         ;; handle keywords for map access
         ((string-prefix-p ":" thing) (format "%s %s"
                                              thing
                                              "([map] [map not-found])"))
         ;; handle Classname. by displaying the eldoc for new
         ((string-match-p "^[A-Z].+\\.$" thing) (format "%s %s"
                                                        thing
                                                        "([args*])"))
         (t (let* ((ns (miracle-find-ns))
                   (request (format
                             "(do (require 'nrepl.middleware.info
                     '[clojure.string :as str])
            (let [result (nrepl.middleware.info/info
                          {:ns \"%s\" :symbol \"%s\"})]
              (cond
                (:ns result)
                (-> result
                    (update :ns str)
                    (update :arglists str))

                (:class result)
                (-> result
                    (update :class str)
                    (update :arglists
                            (fn [arglists]
                              (str \"(\"
                                      (->> (mapv (fn [arglists]
                                                   (str \"[\"
                                                           (->> arglists
                                                                (mapv (fn [{:keys [:name :type]}]
                                                                        (str name \" : \" type)) )
                                                                (str/join \", \"))
                                                           \"]\"))
                                                 arglists)
                                           (str/join \", \"))
                                      \")\"))))
                (:candidates result)
                {:candidates
                 (str (:member (first (vals (:candidates result))))
                      \" - \"
                      (str/join \" | \" (keys (:candidates result))))})))"
                             ns
                             symbol))
                   (value (miracle-send-sync-request
                           request
                           "*miracle-connection*")))
              (when-let ((parsed-response (parseedn-read-str
                                           value)))
                (cond
                 ((gethash :ns parsed-response)
                  (format "%s/%s: %s"
                          (gethash :ns parsed-response)
                          (gethash :name parsed-response)
                          (gethash :arglists parsed-response)))

                 ((gethash :class parsed-response)
                  (format "%s/%s: %s"
                          (gethash :class parsed-response)
                          (gethash :member parsed-response)
                          (gethash :arglists parsed-response)))
                 ((gethash :candidates parsed-response)
                  (format "%s"
                          (gethash :candidates parsed-response))))))))))))

(defvar miracle-translate-path-function 'identity
  "This function is called on all paths returned by `miracle-jump'.
You can use it to translate paths if you are running an nrepl server remotely or
inside a container.")

(defun miracle-jump-find-file (file)
  "Internal function to find a file on the disk or inside a jar."
  (if (string-match "^clojure\\|^arcadia" file)
      (find-file (concat *miracle-project-path* "/Assets/Arcadia/Source/" file))
    (find-file file)))

(defun miracle-eval-jump (ns var)
  "Internal function to actually ask for var location via nrepl protocol."
  (lexical-let
      ((ns ns)
       (var var))
    (miracle-send-eval-string
     (format "%s" `((juxt :file :line)
                    (meta ,(if ns `(ns-resolve ',(intern ns) ',(intern var))
                             `(resolve ',(intern var))))))
     (lambda (response)
       (miracle-dbind-response
        response (id value status)
        (when value
          (destructuring-bind (file line)
              (append (car (read-from-string value)) nil)
            (if file
                (progn (miracle-jump-find-file (funcall miracle-translate-path-function file))
                       (when line
                         (goto-char (point-min))
                         (forward-line (1- line))))
              (message "%s"
                       (concat
                        (propertize "Couldn't find symbol: " 'face 'font-lock-warning-face)
                        (propertize (concat var (when ns (concat " (in " ns ")"))) 'face 'font-lock-variable-name-face)))))))))))

(defun miracle-get-stacktrace ()
  "When error happens, print the stack trace"
  (let ((pst (or miracle-print-stack-trace-function
                 (if miracle-old-style-stacktraces
                     'clojure.stacktrace/print-stack-trace
                   'clojure.repl/pst))))
    (miracle-send-eval-string
     (format "(do (require (symbol (namespace '%s))) (%s *e))" pst pst)
     (miracle-make-response-handler))))

(defun miracle-pprint-last-result ()
  "Pretty prints the last result."
  (interactive)
  (comint-delete-output)
  (miracle-send-eval-string
   (format "(do (require 'clojure.pprint) (clojure.pprint/pprint *1))")
   (miracle-make-response-handler)))

(defun miracle-describe (symbol)
  "Ask user about symbol and show symbol documentation if found."
  (interactive
   (list
    (let* ((sym (thing-at-point 'symbol))
           (sym (if sym (substring-no-properties sym)))
           (prompt "Describe")
           (prompt (if sym
                       (format "%s (default %s): " prompt sym)
                     (concat prompt ": "))))
      (read-string prompt nil nil sym))))
  (miracle-eval-doc symbol))

(defun miracle-eldoc ()
  (miracle-eldoc-info-at-sexp-beginning))

(defun miracle-load-file (path)
  "Load file to running process, asking user for alternative path.
This function, contrary to clojure-mode.el, will not use comint-mode for sending files
as path can be remote location. For remote paths, use absolute path."
  (interactive
   (list
    (let ((n (buffer-file-name)))
      (read-file-name "Load file: " nil nil nil
                      (and n (file-name-nondirectory n))))))
  (when-let* ((ns-form  (miracle-ns-form)))
    (miracle-repl--cache-ns-form ns-form (get-buffer "*miracle*")))
  (let ((full-path (convert-standard-filename (expand-file-name path))))
    (miracle-input-sender
     (get-buffer-process miracle-repl-buffer)
     (format "(clojure.core/load-file \"%s\")" full-path))))

(defun miracle-jump (var)
  "Jump to definition of var at point."
  (interactive
   (list (if (thing-at-point 'symbol)
             (substring-no-properties (thing-at-point 'symbol))
           (read-string "Find var: "))))
  (defvar find-tag-marker-ring) ;; etags.el
  (require 'etags)
  (ring-insert find-tag-marker-ring (point-marker))
  (miracle-eval-jump (miracle-find-ns) var))

(defun miracle-jump-pop ()
  "Return point to the position and buffer before running `miracle-jump'."
  (interactive)
  (defvar find-tag-marker-ring) ;; etags.el
  (require 'etags)
  (let ((marker (ring-remove find-tag-marker-ring 0)))
    (switch-to-buffer (marker-buffer marker))
    (goto-char (marker-position marker))))

(defun miracle-switch-to-repl ()
  (interactive)
  (pop-to-buffer miracle-repl-buffer))

(defun miracle-nrepl-server-start ()
  "Starts nrepl server. Uses miracle-nrepl-server-cmd + miracle-nrepl-server-cmd-args as the command. Finds project root by locatin miracle-nrepl-server-project-file"
  (interactive)
  (let* ((nrepl-buf-name (concat "*" miracle-nrepl-server-buffer-name "*"))
         (repl-started-dir (miracle-locate-port-file)))
    (if repl-started-dir
        (message "nREPL server already running in %s" repl-started-dir)
      (progn
        (lexical-let ((default-directory
                        (locate-dominating-file default-directory
                                                miracle-nrepl-server-project-file)))
          (message "Starting nREPL server in %s" default-directory)
          (async-shell-command (concat miracle-nrepl-server-cmd " " miracle-nrepl-server-cmd-args)
                               nrepl-buf-name))))))

(defun miracle-extract-keys (htable)
  "Get all keys from hashtable."
  (let (keys)
    (maphash (lambda (k v) (setq keys (cons k keys))) htable)
    keys))

(defun miracle-interrupt ()
  "Send interrupt to all pending requests."
  (interactive)
  (dolist (id (miracle-extract-keys miracle-requests))
    (miracle-send-interrupt id (miracle-make-response-handler))))

(defun miracle-set-project-path ()
  "Sets *miracle-project-path* to the path of the project
the nREPL server miracle connected to was started in."
  (interactive)
  (miracle-send-eval-string
   (format "%s" `(.. (clojure.clr.io/as-file \".\") FullName))
   (lambda (response)
     (miracle-dbind-response response (id value status)
                             (when (member "done" status)
                               (remhash id miracle-requests))
                             (when value
                               (setq *miracle-project-path* (read value)))))))

(defun miracle-clone-orchadia ()
  (when miracle-use-orchardia
    ;; Clone orchardia repo folders at first source path configured
    ;; at `configuration.edn`
    (when-let* ((dir (parseedn-read-str
                      (miracle-send-sync-request
                       (format
                        "(do (require '[arcadia.internal.config :as config])
      (import '[System.IO Directory])
      (let [dirs (mapv #(str (Directory/GetCurrentDirectory) \"/\" %%)
                       (or (:source-paths (config/config))
                           [\"Assets\"]))
            non-orchadia-dirs (remove #(Directory/Exists (str %% \"/orchard\"))
                                      dirs)]
        (when (= dirs non-orchadia-dirs)
          (str (first dirs) \"/orchardia\"))))")
                       "*miracle-connection*"))))
      (shell-command (format
                      "git clone %s %s"
                      "https://github.com/pfeodrippe/orchardia.git"
                      dir))
      (shell-command (format
                      "mv %s/src/compliment %s/../compliment"
                      dir
                      dir))
      (shell-command (format
                      "mv %s/src/orchard %s/../orchard"
                      dir
                      dir))
      (shell-command (format
                      "mv %s/src/nrepl %s/../nrepl"
                      dir
                      dir))
      (shell-command (format
                      "rm -rf %s"
                      dir))
      (message "Orchadia cloned with success!"))))

;; keys for interacting with Miracle REPL buffer
(defvar miracle-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'miracle-eval-defun)
    (define-key map "\C-c\C-e" 'miracle-eval-expression-at-point)
    (define-key map "\C-c\C-r" 'miracle-eval-region)
    (define-key map "\C-c\C-k" 'miracle-eval-buffer)
    (define-key map "\C-c\C-n" 'miracle-eval-namespace)
    (define-key map "\C-c\C-d" 'miracle-describe)
    (define-key map "\C-c\C-b" 'miracle-interrupt)
    (define-key map "\C-c\C-l" 'miracle-load-file)
    (define-key map "\M-."     'miracle-jump)
    (define-key map "\M-,"     'miracle-jump-pop)
    (define-key map "\C-c\C-z" 'miracle-switch-to-repl)
    map))

;; keys for interacting inside Miracle REPL buffer
(defvar miracle-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-c\C-d" 'miracle-describe)
    (define-key map "\C-c\C-f" 'miracle-pprint-last-result)
    (define-key map "\C-c\C-c" 'miracle-interrupt)
    (define-key map "\M-."     'miracle-jump)
    map))

;;; rest

(define-derived-mode miracle-mode comint-mode "Miracle nREPL"
  "Major mode for evaluating commands over nREPL.

The following keys are available in `miracle-mode':

  \\{miracle-mode-map}"

  :syntax-table lisp-mode-syntax-table
  (setq comint-prompt-regexp miracle-prompt-regexp)
  (setq comint-input-sender 'miracle-input-sender)
  (setq mode-line-process '(":%s"))
                                        ;(set (make-local-variable 'font-lock-defaults) '(clojure-font-lock-keywords t))

  ;; a hack to keep comint happy
  (unless (comint-check-proc (current-buffer))
    (let ((fake-proc (start-process "miracle" (current-buffer) nil)))
      (set-process-query-on-exit-flag fake-proc nil)
      (insert (format ";; Miracle nREPL %s\n" miracle-version))
      (set-marker (process-mark fake-proc) (point))
      (comint-output-filter fake-proc (format miracle-repl-prompt-format miracle-buffer-ns)))))

;;; user command

(defun clojure-enable-miracle ()
  (miracle-interaction-mode t))

;;;###autoload
(define-minor-mode miracle-interaction-mode
  "Minor mode for Miracle interaction from a Clojure buffer.

The following keys are available in `miracle-interaction-mode`:

  \\{miracle-interaction-mode}"

  nil " Miracle" miracle-interaction-mode-map
  (miracle-eldoc-setup))

(add-hook 'miracle-connected-hook 'miracle-set-project-path)
(add-hook 'miracle-connected-hook 'miracle-clone-orchadia)

;;;###autoload
(defun miracle (host-and-port)
  "Load miracle by setting up appropriate mode, asking user for
connection endpoint."
  (interactive
   (let ((host (or (miracle-locate-running-nrepl-host) miracle-default-host)))
     (list
      (read-string (format "Host (default '%s'): " host)
                   nil nil host))))
  (unless (ignore-errors
            (with-current-buffer (get-buffer-create miracle-repl-buffer)
              (prog1
                  (miracle-connect host-and-port)
                (goto-char (point-max))
                (miracle-mode)
                (switch-to-buffer (current-buffer)))))
    (message "Unable to connect to %s" host-and-port)))

(defun miracle-eldoc-setup ()
  "Setup eldoc in the current buffer.
eldoc mode has to be enabled for this to have any effect."
  (setq-local eldoc-documentation-function #'miracle-eldoc)
  (apply #'eldoc-add-command miracle-extra-eldoc-commands))

;; Miracle company backend
(defun company-miracle-backend--make-candidate (candidate)
  (let ((text (car candidate))
        (meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun company-miracle-backend--candidates (items)
  (let (res)
    (dolist (item items)
      (push (company-miracle-backend--make-candidate item) res))
    res))

(defun company-miracle-backend--annotation (candidate)
  (format " %s" (get-text-property 0 'meta candidate)))

(defun company-miracle-backend (command &optional arg &rest ignored)
  (when miracle-use-orchardia
    (case command
      (prefix (and (eq major-mode 'clojure-mode)
                   (company-grab-symbol)))
      (sorted t)
      (candidates
       (company-miracle-backend--candidates
        (parseedn-read-str
         (miracle-send-sync-request
          (format
           " (do (require '[compliment.core :as compliment])
        (let [ns \"%s\"
              prefix \"%s\"]
          (->> (compliment/completions prefix {:ns (symbol ns)})
               (mapv #(update %% :type {:static-field \"<f>\"
                                        :static-method \"<sm>\"
                                        :method \"<mt>\"
                                        :class \"<c>\"
                                        :special-form \"<s>\"
                                        :function \"<f>\"
                                        :var \"<v>\"
                                        :macro \"<m>\"}))
               (mapv (comp seq (juxt :candidate :type)))
               seq)))"
           (miracle-find-ns)
           arg)
          "*miracle-connection*"))))
      (annotation (company-miracle-backend--annotation arg)))))

(add-to-list 'company-backends 'company-miracle-backend)

(provide 'miracle)

;;; miracle.el ends here
