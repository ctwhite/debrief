;;; debrief-log.el --- Logging utilities and UI for Debrief -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides all logging functionalities for the Debrief debugging
;; framework. It includes:
;;
;; - The core `debrief--log` function for recording debug messages.
;; - Configuration options for log levels, destinations (messages, buffer, file),
;;   and log file paths.
;; - A dedicated log buffer (`debrief-log-mode`) with features for viewing,
;;   filtering, and clearing log messages.
;; - Management of log history.
;;
;; This module depends on `debrief-core.el` primarily to access the
;; `debrief--debug-config` hash table for per-target log level overrides.

;;; Code:

(require 'cl-lib)
(require 'dash)       ; For utility functions like --each
(require 's)          ; For string manipulation
(require 'ht)         ; For hash table operations

;; Declare variable from debrief-core.el that debrief--log needs to access.
;; It's defined and initialized in debrief-core.el.
(defvar debrief--debug-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Logging Constants & Variables                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst debrief--log-level-map
  '((:trace . -1) (:debug . 0) (:info . 1) (:warn . 2) (:error . 3) (:fatal . 4)
    (:none . 100)) ; :none has a high value to effectively disable logging below it.
  "Mapping of log level keywords to numeric values for comparison.
Lower numeric values indicate more verbose logging levels.")

(defvar debrief--log-history nil
  "List to store all Debrief log messages, regardless of current filter level.
Each element is a cons cell: (LEVEL . MESSAGE-STRING), where LEVEL is a keyword
like :info, :debug, etc., and MESSAGE-STRING is the fully formatted log message.
The most recent messages are at the head of the list.")

(defcustom debrief-log-history-max-size 1000
  "Maximum number of log messages to keep in `debrief--log-history`.
When the history exceeds this size, older messages are discarded."
  :type 'integer
  :group 'debrief)

(defcustom debrief-log-destination :messages
  "Specifies where Debrief log messages should be sent.
Possible values:
  :messages - Display in the *Messages* buffer (using `message`).
  :buffer   - Display in a dedicated Debrief log buffer (see
              `debrief-log-dedicated-buffer-name`).
  :file     - Append to a log file (see `debrief-log-file-path`)."
  :type '(radio (const :tag "Messages Buffer" :messages)
                (const :tag "Dedicated Buffer" :buffer)
                (const :tag "Log File" :file))
  :group 'debrief)

(defcustom debrief-log-file-path (locate-user-emacs-file "debrief-log.log")
  "Path to the file where Debrief logs are written if `debrief-log-destination`
is set to `:file`."
  :type 'file
  :group 'debrief)

(defcustom debrief-log-dedicated-buffer-name "*Debrief Log*"
  "Name of the dedicated buffer used for Debrief logs when
`debrief-log-destination` is set to `:buffer`."
  :type 'string
  :group 'debrief)

(defcustom debrief-log-level-threshold :debug
  "Global minimum log level to display or record.
Messages with a severity level below this threshold will be ignored, unless
a specific target has its own `:min-log-level` that is lower.
Cannot be set to `:none` globally; use `:fatal` to see almost no messages.
Valid levels (most to least verbose): :trace, :debug, :info, :warn, :error, :fatal."
  :type '(choice (const :tag "Trace (most verbose)" :trace)
                 (const :tag "Debug" :debug)
                 (const :tag "Info" :info)
                 (const :tag "Warn" :warn)
                 (const :tag "Error" :error)
                 (const :tag "Fatal (least verbose)" :fatal))
  :group 'debrief)

(defvar-local debrief-log-buffer-update-hook nil
  "Hook run after a new message is inserted into the Debrief log buffer.
This hook is local to the `debrief-log-mode` buffer. Functions added to
this hook will be called with no arguments.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Core Logging Function                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debrief--log-prefix (level)
  "Return the appropriate styled prefix string for a log LEVEL.
Arguments:
  LEVEL (keyword): The log level (e.g., :info, :warn).
Return:
  (string): A string, possibly with text properties for styling,
            representing the log prefix (e.g., \"[debrief:info]\")."
  (pcase level
    (:trace  (propertize "[debrief:trace]" 'face 'shadow))
    (:debug  (propertize "[debrief:debug]" 'face 'font-lock-comment-face))
    (:info   (propertize "[debrief:info]" 'face 'font-lock-keyword-face))
    (:warn   (propertize "[debrief:warn]" 'face 'warning))
    (:error  (propertize "[debrief:error]" 'face 'error))
    (:fatal  (propertize "[debrief:fatal]" 'face 'error))
    (_       (propertize (format "[debrief:%S]" level)
                         'face 'font-lock-constant-face))))

;;;###autoload
(defun debrief--log (level target-symbol format-string &rest args)
  "Log a message at LEVEL, formatted with FORMAT-STRING and ARGS.
TARGET-SYMBOL is the debug target associated with the message, used for
target-specific log level overrides defined by `:min-log-level` in the
target's configuration. If TARGET-SYMBOL is nil, the global
`debrief-log-level-threshold` is used.
Logging only occurs if the message's LEVEL is at or above the effective
minimum log level.
Arguments:
  LEVEL (keyword): The severity level of the log message (e.g., :info, :debug).
  TARGET-SYMBOL (symbol|nil): The debug target symbol this log pertains to,
                              or nil for general Debrief messages.
  FORMAT-STRING (string): A format control string, as for `format`.
  ARGS (list): Arguments for FORMAT-STRING."
  (let* (;; Get target-specific config if available; `debrief--debug-config`
         ;; is defined in `debrief-core.el`.
         (target-config (when (and target-symbol (boundp 'debrief--debug-config))
                          (ht-get debrief--debug-config target-symbol)))
         (effective-min-level-keyword
          (or (and target-config (plist-get target-config :min-log-level))
              debrief-log-level-threshold))
         (current-level-numeric (cdr (assoc level debrief--log-level-map)))
         (min-level-numeric (cdr (assoc effective-min-level-keyword
                                        debrief--log-level-map))))

    ;; Ensure both levels were found in the map before comparing
    (when (and current-level-numeric min-level-numeric
               (>= current-level-numeric min-level-numeric))
      (let* ((prefix (debrief--log-prefix level))
             (formatted-message (apply #'format
                                       (concat prefix " " format-string)
                                       args)))
        (pcase debrief-log-destination
          (:messages (message "%s" formatted-message))
          (:buffer
           ;; This function is defined below in this file.
           (debrief-log--insert-buffer-message level formatted-message))
          (:file
           (condition-case err
               (with-temp-buffer
                 (when (file-exists-p debrief-log-file-path)
                   (insert-file-contents debrief-log-file-path nil t))
                 (goto-char (point-max))
                 (unless (bolp) (insert "\n")) ; Ensure message is on a new line
                 (insert formatted-message "\n")
                 (write-file debrief-log-file-path nil)) ; nil for no confirm
             (error (message "Debrief: Error writing to log file %s: %S"
                             debrief-log-file-path err)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Dedicated Log Buffer Mode & Functions                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debrief-log--insert-buffer-message (level message-string)
  "Insert MESSAGE-STRING into the dedicated Debrief log buffer and store it in history.
This function is internal to `debrief-log.el` and called by `debrief--log`
when `debrief-log-destination` is set to `:buffer`.
Arguments:
  LEVEL (keyword): The log level of the message (e.g., :info, :debug).
  MESSAGE-STRING (string): The fully formatted log message to insert.
Return:
  nil."
  ;; Add to history first
  (push (cons level message-string) debrief--log-history)
  ;; Trim history if it exceeds max size
  (when (> (length debrief--log-history) debrief-log-history-max-size)
    (setq debrief--log-history (cl-subseq debrief--log-history 0
                                          debrief-log-history-max-size)))

  ;; Attempt to insert into the dedicated log buffer if it exists
  (when-let ((log-buf (get-buffer debrief-log-dedicated-buffer-name)))
    (with-current-buffer log-buf
      ;; Ensure the buffer is in debrief-log-mode
      (unless (eq major-mode 'debrief-log-mode) (debrief-log-mode))

      ;; Check if the message's level meets the current display threshold
      (let* ((current-display-threshold debrief-log-level-threshold)
             (current-msg-level-numeric (cdr (assoc level debrief--log-level-map)))
             (threshold-numeric (cdr (assoc current-display-threshold
                                            debrief--log-level-map))))
        (when (and current-msg-level-numeric threshold-numeric
                   (>= current-msg-level-numeric threshold-numeric))
          (let ((inhibit-read-only t)
                ;; Check if point is at the end to maintain scroll position
                (point-at-end (= (point) (point-max))))
            (goto-char (point-max))
            (unless (bolp) (insert "\n")) ; Ensure new message starts on a new line
            (insert message-string "\n")
            (when point-at-end (goto-char (point-max))) ; Restore point if it was at end
            (run-hooks 'debrief-log-buffer-update-hook))))))
  nil)

;;;###autoload
(define-derived-mode debrief-log-mode special-mode "Debrief Log"
  "Major mode for displaying Debrief log messages interactively.
This mode provides a dedicated buffer for Debrief's log output when
`debrief-log-destination` is set to `:buffer`. It stores a history of
log messages and allows for clearing and filtering the display.

Keybindings:
  `c`: Clear the contents of the log buffer and its history.
  `f`: Filter logs by minimum severity level (prompts for level).
  `g`: Refresh the display / Reapply the current filter."
  :group 'debrief
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)        ; Prefer wrapping long lines
  (setq-local scroll-up-aggressively nil) ; Standard scroll behavior
  (setq-local scroll-down-aggressively nil)
  (setq-local debrief-log-buffer-update-hook nil) ; Initialize local hook

  ;; Define keybindings for this mode
  (define-key debrief-log-mode-map (kbd "c") #'debrief/log-clear-buffer)
  (define-key debrief-log-mode-map (kbd "f") #'debrief/log-filter-by-level)
  (define-key debrief-log-mode-map (kbd "g") #'debrief/log-redisplay-filtered)
  ;; Log that the buffer itself was initialized (using message to avoid recursion issues
  ;; if this log call itself tries to write to this buffer during init)
  (message "Debrief Log buffer initialized in debrief-log-mode."))

(defun debrief/log-clear-buffer ()
  "Clear the contents of the Debrief log buffer and its internal history.
This affects only the buffer in `debrief-log-mode` and the
`debrief--log-history` variable.
Return:
  nil."
  (interactive)
  (when (eq major-mode 'debrief-log-mode)
    (let ((inhibit-read-only t)) ; Allow modification of the read-only buffer
      (erase-buffer))
    (setq debrief--log-history nil) ; Clear the stored history
    (debrief--log :info nil "Debrief Log buffer and history cleared."))
  nil)

(defun debrief/log-redisplay-filtered ()
  "Re-display logs in the Debrief log buffer based on the current filter level.
The current filter level is taken from `debrief-log-level-threshold`.
This function iterates through `debrief--log-history` and inserts messages
that meet the current threshold.
Return:
  nil."
  (interactive)
  (when (eq major-mode 'debrief-log-mode)
    (with-current-buffer (current-buffer)
      (let ((inhibit-read-only t)
            (current-filter-keyword debrief-log-level-threshold)
            (min-level-numeric-val (cdr (assoc debrief-log-level-threshold
                                               debrief--log-level-map))))
        (erase-buffer)
        ;; Iterate through history (which is newest first) and insert if level matches.
        ;; Reverse history to display oldest first.
        (--each (reverse debrief--log-history)
          (let* ((msg-level-keyword (car it))
                 (msg-string (cdr it))
                 (msg-level-numeric-val (cdr (assoc msg-level-keyword
                                                    debrief--log-level-map))))
            (when (and msg-level-numeric-val min-level-numeric-val
                       (>= msg-level-numeric-val min-level-numeric-val))
              (insert msg-string "\n"))))
        (goto-char (point-max)))) ; Move point to the end of the buffer
    (message "Debrief log redisplayed with filter: %S and above."
             debrief-log-level-threshold))
  nil)

(defun debrief/log-filter-by-level (&optional level)
  "Filter logs in the Debrief Log buffer to show only messages at or above LEVEL.

LEVEL can be a symbol like 'info or a keyword like :info. Prompts if LEVEL is nil.
Valid levels: :trace, :debug, :info, :warn, :error, :fatal."
  (interactive
   (list
    (intern (concat ":" (completing-read
                         "Filter log level (trace, debug, info, warn, error, fatal): "
                         '("trace" "debug" "info" "warn" "error" "fatal")
                         nil t nil 'debrief-log-filter-level-history
                         (when (keywordp debrief-log-level-threshold)
                           (substring (symbol-name debrief-log-level-threshold) 1)))))))

  (cl-block debrief/log-filter-by-level
    ;; Ensure level is provided
    (unless level
      (cl-return-from debrief/log-filter-by-level
        (user-error "No log level provided.")))

    ;; Normalize to keyword
    (unless (keywordp level)
      (setq level (intern (concat ":" (symbol-name level)))))

    ;; Validate level
    (unless (assoc level debrief--log-level-map)
      (cl-return-from debrief/log-filter-by-level
        (user-error "Invalid log level: %S. Choose from :trace, :debug, :info, :warn, :error, :fatal." level)))

    ;; Apply filter and redisplay
    (setq debrief-log-level-threshold level)
    (message "Debrief log display threshold set to %S. Re-displaying logs..." level)
    (debrief/log-redisplay-filtered)
    (message "Debrief log filtered to show %S and above." level)))
    
(provide 'debrief-log)
;;; debrief-log.el ends here