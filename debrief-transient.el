;;; debrief-transient.el --- Transient menu for Debrief -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This file defines a transient menu for the Debrief debugging library,
;; providing quick keyboard-driven access to common Debrief commands.
;; The menu is organized into logical sections for managing global debug state,
;; individual debug targets, logging, and persistence.
;;
;; Usage:
;;   Ensure `debrief-transient.el` is loaded (typically via a main `debrief.el`
;;   file that requires all Debrief modules).
;;   Invoke the menu interactively with `M-x debrief-menu`.
;;
;; The transient menu allows users to:
;; - Toggle global debugging on/off.
;; - Refresh and reset all debug configurations.
;; - List, consult, toggle, and unregister debug targets.
;; - Manage target groups and interact with recently used targets.
;; - Control variable watching and break-on-change behavior.
;; - Register new function, variable, hook, and prefix targets.
;; - Perform temporary debugging actions on functions.
;; - Manage argument and return value filters for function advice.
;; - Control the Debrief log buffer (clear, filter, view).
;; - Save and load the Debrief configuration state.

;;; Code:

(require 'transient)
(require 'debrief-core)     ; For debrief-debug-enabled, debrief/reset-all, etc.
(require 'debrief-commands) ; For most action commands
(require 'debrief-consult)  ; For debrief/consult-targets
(require 'debrief-ui)       ; For debrief/list-registered-targets
(require 'debrief-log)      ; For log-related defcustoms and functions
(require 'debrief-persist)  ; For debrief/save-state, debrief/load-state

;; Ensure functions and macros are known at compile-time for transient definitions.
(eval-when-compile
  (require 'transient)
  (require 'debrief-core)
  (require 'debrief-commands)
  (require 'debrief-consult)
  (require 'debrief-ui)
  (require 'debrief-log)
  (require 'debrief-persist))

;; Variables from debrief-log.el needed here
(defvar debrief-log-destination)
(defvar debrief-log-dedicated-buffer-name)
(defvar debrief-log-file-path)
(defvar debrief-debug-enabled) ; from debrief-core.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Internal State                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar debrief--last-target nil
  "Symbol of the most recently interacted-with debug target.
Used by `debrief/toggle-most-recent-target`.")

(defvar debrief--last-group nil
  "Symbol of the most recently interacted-with debug group.
Used by `debrief/toggle-most-recent-group`.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Transient Helper Functions                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debrief/toggle-most-recent-target ()
  "Toggle the enabled state of the most recently interacted Debrief target.
If no recent target is stored in `debrief--last-target`, a message is shown.
This function calls `debrief/toggle-target`.
Return:
  nil."
  (interactive)
  (if debrief--last-target
      (progn
        (debrief--log :info debrief--last-target
                      "Toggling most recent target: %s" debrief--last-target)
        (debrief/toggle-target debrief--last-target))
    (message "No recent target available to toggle.")))

(defun debrief/toggle-most-recent-group ()
  "Toggle the enabled state of all targets in the most recently interacted group.
If no recent group is stored in `debrief--last-group`, a message is shown.
This function calls `debrief/toggle-group`.
Return:
  nil."
  (interactive)
  (if debrief--last-group
      (progn
        (debrief--log :info nil ; Group toggling is a global-ish action
                      "Toggling most recent group: %s" debrief--last-group)
        (debrief/toggle-group debrief--last-group))
    (message "No recent group available to toggle.")))

(defun debrief/reset-all-confirm ()
  "Ask for confirmation before resetting all Debrief configurations.
If confirmed, calls `debrief/reset-all`.
Return:
  nil."
  (interactive)
  (if (yes-or-no-p "Really reset all Debrief state and configurations? This is irreversible. ")
      (progn
        (debrief--log :warn nil "User confirmed reset of all Debrief configurations.")
        (debrief/reset-all))
    (message "Debrief reset cancelled.")))

(defun debrief/view-log ()
  "Open or switch to the current Debrief log destination.
If destination is `:messages`, switches to the *Messages* buffer.
If destination is `:buffer`, switches to the dedicated Debrief log buffer.
If destination is `:file`, opens the configured log file.
Return:
  nil. Side effect is displaying a buffer or opening a file."
  (interactive)
  (pcase debrief-log-destination
    (:messages
     (switch-to-buffer "*Messages*")
     (message "Switched to *Messages* buffer for Debrief logs."))
    (:buffer
     (let ((log-buf (get-buffer-create debrief-log-dedicated-buffer-name)))
       (with-current-buffer log-buf
         (unless (eq major-mode 'debrief-log-mode)
           (debrief-log-mode))) ; Ensure mode is active
       (pop-to-buffer log-buf))
     (message "Switched to Debrief log buffer: %s" debrief-log-dedicated-buffer-name))
    (:file
     (if (and debrief-log-file-path (file-exists-p debrief-log-file-path))
         (progn
           (find-file debrief-log-file-path)
           (message "Opened Debrief log file: %s" debrief-log-file-path))
       (message "Debrief log file not found or path not set: %s"
                (or debrief-log-file-path "Not configured"))))
    (_
     (message "Unknown Debrief log destination: %S" debrief-log-destination))))

(defun debrief-log-next-entry ()
  "Move point to the next log entry in the `*Debrief Log*` buffer.
If the log buffer is not current, this command may not behave as expected.
Return:
  nil."
  (interactive)
  (if-let ((log-buffer (get-buffer debrief-log-dedicated-buffer-name)))
      (with-current-buffer log-buffer
        (forward-line 1))
    (message "Debrief log buffer ('%s') not found."
             debrief-log-dedicated-buffer-name)))

(defun debrief-log-prev-entry ()
  "Move point to the previous log entry in the `*Debrief Log*` buffer.
If the log buffer is not current, this command may not behave as expected.
Return:
  nil."
  (interactive)
  (if-let ((log-buffer (get-buffer debrief-log-dedicated-buffer-name)))
      (with-current-buffer log-buffer
        (forward-line -1))
    (message "Debrief log buffer ('%s') not found."
             debrief-log-dedicated-buffer-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Transient Menu Definition                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(transient-define-prefix debrief-menu ()
  "Transient menu for the Debrief debugging toolkit.
Provides quick access to commands for managing debug targets, logging,
persistence, and global debug state."
  ;; Main transient group definition
  ["Debrief Debug Menu"
   ;; Section: Global Control
   ["🔧 Global Control"
    ("t" (lambda () ; Suffix function to display current global state
           (format "toggle global debug [%s]"
                   (if debrief-debug-enabled "ON" "OFF")))
     debrief/toggle)
    ("r" "refresh all targets" debrief/refresh-all-targets)
    ("R" "reset all (confirm)" debrief/reset-all-confirm)]

   ;; Section: Target Management
   ["🎯 Target Management"
    ("l" "list registered targets" debrief/list-registered-targets)
    ("c" "consult targets" debrief/consult-targets)
    ("T" "toggle target (generic)" debrief/toggle-target)
    ("u" "unregister target" debrief/unregister-target)
    ("g" "toggle group" debrief/toggle-group)
    ("m" "toggle most recent target" debrief/toggle-most-recent-target)
    ("M" "toggle most recent group" debrief/toggle-most-recent-group)
    ("W" "toggle variable watch" debrief/toggle-target-watch)
    ("B" "toggle variable break" debrief/toggle-target-break-on-change)]

   ;; Section: Register New Target
   ["➕ Register New Target"
    ("f" "function" debrief/register-function)
    ("v" "variable" debrief/register-variable)
    ("h" "hook monitor" debrief/register-hook-monitor)
    ("w" "register & watch variable" debrief/watch-variable)
    ("b" "register & break on var change" debrief/break-on-variable-change)]

   ;; Section: Function Actions (for already registered function targets)
   ["⚙️ Function Actions"
    ("C" "call temporarily (debugged)" debrief/call-function-temporarily)
    ("x" "wrap temporarily (debugged)" debrief/wrap-function-temporarily)
    ("a" "add arg filter" debrief/add-arg-filter)
    ("A" "remove arg filter" debrief/remove-arg-filter)
    ("e" "add return filter" debrief/add-return-filter)
    ("E" "remove return filter" debrief/remove-return-filter)]

   ;; Section: Logging (interacts with `debrief-log.el`)
   ["📜 Logging"
    ("V" "view current log" debrief/view-log)
    ("L" "clear log buffer" debrief/log-clear-buffer)
    ("F" "filter log level" debrief/log-filter-by-level)
    ("n" "next log entry (in log buffer)" debrief-log-next-entry)
    ("P" "previous log entry (in log buffer)" debrief-log-prev-entry)]

   ;; Section: Persistence (interacts with `debrief-persist.el`)
   ["💾 Persistence"
    ("s" "save state" debrief/save-state)
    ("o" "load state" debrief/load-state)]
   ])

(provide 'debrief-transient)
;;; debrief-transient.el ends here