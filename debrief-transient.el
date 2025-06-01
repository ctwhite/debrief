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
;; - Control the Debrief log buffer (clear, filter).
;; - Save and load the Debrief configuration state.
;; - Access a simple help screen for Debrief.

;;; Code:

(require 'transient)
(require 'debrief-core)     ; For debrief-debug-enabled, debrief/reset-all, etc.
(require 'debrief-commands) ; For most action commands
(require 'debrief-consult)  ; For debrief/consult-targets
(require 'debrief-ui)       ; For debrief/list-registered-targets
(require 'debrief-log)      ; For debrief-log-clear-buffer, etc.
(require 'debrief-persist)  ; For debrief/save-state, debrief/load-state

;; Ensure functions are known at compile-time for transient definitions.
(eval-when-compile
  (require 'transient)
  (require 'debrief-core)
  (require 'debrief-commands)
  (require 'debrief-consult)
  (require 'debrief-ui)
  (require 'debrief-log)
  (require 'debrief-persist))

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

(defun debrief/show-help ()
  "Display a simple help buffer for the Debrief transient menu.
The help buffer (`*Debrief Help*`) provides a brief overview and workflow tips.
Return:
  nil. Side effect is displaying the help buffer."
  (interactive)
  (with-help-window (help-buffer "*Debrief Help*")
    (princ "Debrief Debugging Toolkit - Transient Menu Help\n\n" help-buffer)
    (princ "This menu provides quick access to Debrief's features.\n\n" help-buffer)
    (princ "Sections:\n" help-buffer)
    (princ "  🔧 Global Control: Manage overall debugging state.\n" help-buffer)
    (princ "  🎯 Target Management: Interact with specific debug targets and groups.\n" help-buffer)
    (princ "  ➕ Register New Target: Define new functions, variables, etc., to debug.\n" help-buffer)
    (princ "  ⚙️ Function Actions: Perform temporary debugging actions on functions.\n" help-buffer)
    (princ "  📜 Logging: Manage the Debrief log buffer.\n" help-buffer)
    (princ "  💾 Persistence: Save or load your Debrief configuration.\n\n" help-buffer)
    (princ "Quick Workflow Example:\n" help-buffer)
    (princ "  1. Register a target: e.g., 'f' for function, 'v' for variable.\n" help-buffer)
    (princ "  2. Toggle its state: 'T' (target) or 'g' (group).\n" help-buffer)
    (princ "  3. Inspect targets: 'l' (list) or 'c' (consult).\n" help-buffer)
    (princ "  4. View logs: (Open log buffer via `debrief-log-destination` setting).\n" help-buffer)
    (princ "  5. Save/Load state: 's' to save, 'o' to load.\n" help-buffer)))

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
    ("p" "prefix (advise all matching)" debrief/register-prefix)
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
    ("L" "clear log buffer" debrief-log-clear-buffer)
    ("F" "filter log level" debrief-log-filter-by-level)
    ("n" "next log entry (in log buffer)" debrief-log-next-entry)
    ("P" "previous log entry (in log buffer)" debrief-log-prev-entry)] ; Changed 'p' to 'P'

   ;; Section: Persistence (interacts with `debrief-persist.el`)
   ["💾 Persistence"
    ("s" "save state" debrief/save-state)
    ("o" "load state" debrief/load-state)]

   ;; Section: Help
   ["❓ Help"
    ("?" "Debrief help" debrief/show-help)]
   ])

(provide 'debrief-transient)
;;; debrief-transient.el ends here