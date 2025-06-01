;;; debrief-ui.el --- UI for listing Debrief debug targets -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides the user interface components for listing and managing
;; registered Debrief debug targets. It primarily defines the
;; `debrief-targets-list-mode` and the command `debrief/list-registered-targets`
;; to display this list.
;;
;; Logging UI (the dedicated log buffer and its mode) has been moved to
;; `debrief-log.el`.
;;
;; This module depends on `debrief-core.el` for accessing target configurations
;; and `debrief-commands.el` for performing actions on targets.

;;; Code:

(require 'cl-lib)
(require 'dash)       ; For utility functions like --map, -sort
(require 's)          ; For string manipulation functions like s-empty?, s-truncate
(require 'debrief-core) ; For core Debrief variables and functions

;; Declare functions from other Debrief modules to satisfy the byte-compiler.
(declare-function debrief/toggle-target "debrief-commands" (target-symbol))
(declare-function debrief/unregister-target "debrief-commands" (target-symbol))
(declare-function debrief--log "debrief-log" (level target-symbol format-string &rest args))
(declare-function debrief--is-entry-active-p "debrief-core" (config-entry))
(declare-function debrief--determine-target-type "debrief-core" (target-symbol config-entry))

;; Variables defined in `debrief-core.el` that are used in this module.
(defvar debrief-list-registered-buffer-name)
(defvar debrief--debug-config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Simple List Mode for Targets                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debrief--list-mode-format-entry (target-symbol config-entry)
  "Format a CONFIG-ENTRY for display in `debrief-targets-list-mode`.
Returns a single string representing one line in the list buffer.
The string includes status indicators, type, name, enabled state, group,
and a truncated description.
Arguments:
  TARGET-SYMBOL (symbol): The symbol of the debug target.
  CONFIG-ENTRY (plist): The configuration plist for TARGET-SYMBOL.
Return:
  (string): A formatted string for display."
  (let* ((is-active (debrief--is-entry-active-p config-entry))
         (type (debrief--determine-target-type target-symbol config-entry))
         (type-str (pcase type    ; Short string representation of the type
                     (:hook-monitor "Hook")
                     (:function-advice "Func")
                     (:variable "Var ") ; Extra space for potential alignment
                     (_ "Unk ")))      ; Unknown type
         (group (or (plist-get config-entry :group) 'none))
         (desc (or (plist-get config-entry :description) ""))
         (enabled-value (plist-get config-entry :enabled))
         ;; String representation of the :enabled property's value
         (enabled-str (cond ((eq enabled-value t) "T")
                            ((eq enabled-value nil) "NIL")
                            ((functionp enabled-value) "Fn") ; If :enabled is a predicate
                            ((null enabled-value) "T(dflt)") ; Default if not specified
                            (t (format "%S" enabled-value)))))
    (format "%s [%s] %-25s State: %-7s (Enabled: %-7s) Group: %-12s Desc: %s"
            (if is-active ; Active status indicator (checkmark or X)
                (propertize "✓" 'face 'success)
              (propertize "✗" 'face 'error))
            type-str
            (propertize (symbol-name target-symbol) 'face 'font-lock-variable-name-face)
            (if is-active "ON" "OFF") ; Current runtime state
            enabled-str               ; Value of the :enabled property
            (propertize (symbol-name group) 'face 'font-lock-constant-face)
            (s-truncate 70 (s-replace "\n" " " desc))))) ; Truncated description

(defun debrief--list-mode-get-target-at-point ()
  "Return the target symbol for the entry at point in `debrief-targets-list-mode`.
Retrieves the symbol from the `debrief-target-symbol` text property on the line.
Return:
  (symbol|nil): The target symbol if found, otherwise nil."
  (when (eq major-mode 'debrief-targets-list-mode)
    (get-text-property (line-beginning-position) 'debrief-target-symbol)))

(defun debrief--list-mode-refresh ()
  "Refresh the contents of the `debrief-targets-list-mode` buffer.
This function is called interactively via 'g' or when the mode is initialized."
  (interactive)
  (when (eq major-mode 'debrief-targets-list-mode)
    (let ((inhibit-read-only t)
          (current-pos (point))) ; Save current point to try and restore it
      (erase-buffer)
      ;; Fetch all targets and their configs from the global hash table
      (let ((targets (--map (list it (ht-get debrief--debug-config it))
                            (ht-keys debrief--debug-config))))
        ;; Sort targets alphabetically by symbol name for consistent display
        (--each (-sort (lambda (entry-a entry-b)
                         (string< (symbol-name (car entry-a))
                                  (symbol-name (car entry-b))))
                       targets)
          (let* ((target-symbol (car it))
                 (config-entry (cadr it))
                 (line-str (debrief--list-mode-format-entry target-symbol config-entry)))
            ;; Insert the formatted line with the target symbol as a text property
            (insert (propertize line-str 'debrief-target-symbol target-symbol) "\n"))))
      (goto-char (min current-pos (point-max))) ; Restore point
      (debrief--log :info nil "Debrief targets list refreshed."))))

(defun debrief--list-mode-toggle-target ()
  "Toggle the :enabled state of the debug target at the current line.
Calls `debrief/toggle-target` and refreshes the list buffer."
  (interactive)
  (when-let ((target-symbol (debrief--list-mode-get-target-at-point)))
    (if (ht-get debrief--debug-config target-symbol) ; Ensure target is known
        (progn
          (debrief/toggle-target target-symbol)
          (debrief--list-mode-refresh)) ; Refresh display to show updated state
      (message "Target %S not found in config (or point not on a target line)."
               target-symbol))))

(defun debrief--list-mode-unregister-target ()
  "Unregister the debug target at the current line after confirmation.
Calls `debrief/unregister-target` and refreshes the list buffer."
  (interactive)
  (when-let ((target-symbol (debrief--list-mode-get-target-at-point)))
    (if (ht-get debrief--debug-config target-symbol) ; Ensure target is known
        (when (y-or-n-p (format "Really unregister target %s? " target-symbol))
          (debrief/unregister-target target-symbol)
          (debrief--list-mode-refresh)) ; Refresh display
      (message "Target %S not found in config (or point not on a target line)."
               target-symbol))))

(defun debrief--list-mode-view-config ()
  "Display the full configuration plist of the target at the current line.
The configuration is printed to the *Messages* buffer."
  (interactive)
  (when-let ((target-symbol (debrief--list-mode-get-target-at-point)))
    (if-let ((config (ht-get debrief--debug-config target-symbol)))
        (message "Config for %s: %S" target-symbol config)
      (message "Target %S not found in config (or point not on a target line)."
               target-symbol))))

(defvar debrief-targets-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'debrief--list-mode-toggle-target)
    (define-key map (kbd "t") #'debrief--list-mode-toggle-target)
    (define-key map (kbd "u") #'debrief--list-mode-unregister-target)
    (define-key map (kbd "v") #'debrief--list-mode-view-config)
    (define-key map (kbd "g") #'debrief--list-mode-refresh)
    map)
  "Keymap for `debrief-targets-list-mode`.")

(define-derived-mode debrief-targets-list-mode special-mode "Debrief Targets"
  "Major mode for displaying and managing registered Debrief debug targets.
This mode provides a read-only list of all targets known to Debrief,
showing their status, type, group, and description.
Keybindings:
  `RET`, `t`: Toggle the enabled state of the target at point.
  `u`: Unregister (remove) the target at point after confirmation.
  `v`: View (print to *Messages*) the full configuration of the target at point.
  `g`: Refresh the list of targets."
  :group 'debrief
  (setq-local buffer-read-only t)
  (setq-local line-spacing 0.2) ; Optional: for better visual separation
  (debrief--list-mode-refresh)  ; Populate buffer on mode activation
  (debrief--log :info nil "Debrief targets list buffer opened."))

;;;###autoload
(defun debrief/list-registered-targets ()
  "Display all registered Debrief debug targets in a dedicated buffer.
The buffer is named according to `debrief-list-registered-buffer-name`
and uses `debrief-targets-list-mode` for interaction.
Return:
  (buffer): The buffer displaying the list of targets."
  (interactive)
  (let ((buffer (get-buffer-create debrief-list-registered-buffer-name)))
    (with-current-buffer buffer
      (debrief-targets-list-mode)) ; Switch to/initialize the mode
    (pop-to-buffer buffer)))       ; Display the buffer

(provide 'debrief-ui)
;;; debrief-ui.el ends here