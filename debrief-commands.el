;;; debrief-commands.el --- Debrief interactive commands -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides the interactive commands for the Debrief debugging
;; framework. These commands allow users to manage debug targets, groups,
;; and the overall debugging state directly from Emacs.
;;
;; Key functionalities include:
;; - **Global Toggle**: `debrief/toggle` to enable or disable all Debrief features.
;; - **Target Management**: Commands to register, unregister, and toggle
;;   individual debug targets.
;; - **Group Management**: Commands to toggle all targets within a specific group.
;; - **Listing Targets**: Displaying all registered targets in a `tabulated-list-mode`
;;   buffer (`debrief/list-registered-targets`).
;; - **Persistence**: Commands to save and load the current Debrief configuration.
;; - **Reset**: A command to clear all Debrief configurations and revert to defaults.
;;
;; This module relies heavily on `debrief-core.el` for the underlying logic
;; and data structures, and `debrief-ui.el` for displaying the tabulated list.

;;; Code:

(require 'cl-lib)     ; For cl-list*, cl-incf, cl-remf
(require 'dash)       ; For --each, --map, --filter, --any?, --count, -sort
(require 'debrief-core) ; For core variables and functions
(require 'debrief-persist) ; For debrief/save-state, debrief/load-state, debrief/reset-all
(require 'subr-x)     ; For when-let, if-let, string-empty-p
(require 's)          ; For s-blank?, s-starts-with?, s-replace, capitalize

;; Declare functions/variables from other modules to satisfy byte-compiler
(declare-function debrief/list-registered-targets "debrief-ui")
(declare-function debrief--get-advice-name "debrief-advice")
(declare-function debrief--generate-advice-function "debrief-advice")

;; Variables from debrief-core.el used here
(defvar debrief--log) ; Function
(defvar debrief--debug-config)
(defvar debrief--debug-groups)
(defvar debrief-apply-entry-config) ; Function
(defvar debrief--register-debug-target) ; Function
(defvar debrief--sanitize-entry-plist) ; Function
(defvar debrief/refresh-all-targets) ; Function
(defvar debrief-debug-enabled)
(defvar debrief-debug-vars)
(defvar debrief--determine-target-type) ; Function
(defvar debrief--symbol-to-keyword) ; Function
(defvar debrief-list-registered-buffer-name) ; From debrief-core
(defvar debrief-register-target-history) ; From debrief-core
(defvar debrief--original-values) ; From debrief-core

;; History variables for interactive commands
(defvar debrief-arg-filter-history nil)
(defvar debrief-return-filter-history nil)
(defvar debrief-group-history nil)
(defvar debrief-values-history nil)
(defvar debrief-unregister-target-history nil)
(defvar debrief-toggle-target-history nil)
(defvar debrief-temp-call-args-history nil)
(defvar debrief-toggle-group-history nil)
(defvar debrief-enabled-lambda-history nil) ; For enabled lambda input
(defvar debrief-if-pred-history nil)      ; For :if predicate lambda input


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Global Commands                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun debrief/toggle ()
  "Toggle the global Debrief debugging state.
This command flips the value of `debrief-debug-enabled`.
It then triggers a refresh of all targets and saves the state via
`debrief-update-debug-vars` (from `debrief-core.el`)."
  (interactive)
  (debrief-update-debug-vars (not debrief-debug-enabled))
  (message "Debrief global debugging toggled to %s."
           (if debrief-debug-enabled "ENABLED" "DISABLED")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Target Management Commands                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debrief--read-lambda-from-minibuffer (prompt &optional default-value history-var)
  "Read a lambda expression from the minibuffer with PROMPT and optional DEFAULT-VALUE.
Uses HISTORY-VAR for input history. Handles basic errors for `read-from-string`."
  (let* ((input (read-string prompt nil history-var default-value))
         (lambda-expr
          (condition-case err
              (car (read-from-string (concat "(" input ")")))
            (error
             (debrief--log 'error nil "Invalid lambda input '%s': %S" input err)
             (user-error "Invalid lambda expression: %s (Error: %s)"
                         input (error-message-string err))))))
    (unless (and (listp lambda-expr) (eq (car lambda-expr) 'lambda))
      (user-error "Input does not appear to be a valid lambda expression: %S"
                  lambda-expr))
    lambda-expr))

;;;###autoload
(defun debrief/register-target (target-symbol &optional initial-meta-plist)
  "Register a new debug target or update an existing one.
`TARGET-SYMBOL` is the variable, function, or hook name.
`INITIAL-META-PLIST` provides properties like `:enabled`, `:group`, etc.

Prompts interactively if arguments are not provided.
Sanitizes input, updates Debrief's internal configuration, applies the
target's state, and saves the overall Debrief configuration."
  (interactive
   (let* ((target-input (read-string "Register/Update target symbol: "
                                     nil 'debrief-register-target-history))
          (target-symbol (intern-soft target-input))
          (config-input (read-string (format "Properties for %S (plist, e.g., \
'(:enabled t :group ui)): " target-symbol)
                                     nil 'debrief-register-target-history nil))
          (parsed-plist nil))
     (unless target-symbol
       (user-error "Invalid target symbol: %S" target-input))
     (if (string-empty-p config-input)
         (setq parsed-plist '(:enabled t)) ; Default if no properties given
       (condition-case err
           (setq parsed-plist (car (read-from-string (concat "(" config-input ")"))))
         (error
          (debrief--log 'error target-symbol "Invalid plist for %S: '%s'. Error: %S"
                        target-symbol config-input err)
          (user-error "Invalid plist format: %s" config-input))))
     (list target-symbol parsed-plist)))

  (unless (symbolp target-symbol)
    (user-error "Target must be a symbol, got: %S" target-symbol))
  (unless (plistp initial-meta-plist)
    (user-error "Properties must be a plist, got: %S" initial-meta-plist))

  (let ((entry-to-register (plist-put (copy-sequence initial-meta-plist)
                                      :target target-symbol)))
    (if-let ((sanitized-entry (debrief--sanitize-entry-plist entry-to-register)))
        (if (debrief--register-debug-target target-symbol sanitized-entry)
            (progn
              (message "Target '%s' registered/updated successfully." target-symbol)
              (when (and (fboundp 'debrief/list-registered-targets)
                         (get-buffer debrief-list-registered-buffer-name))
                (with-current-buffer (get-buffer debrief-list-registered-buffer-name)
                  (when (eq major-mode 'debrief-tabulated-list-mode)
                    (tabulated-list-revert)))))
          (message "Failed to register target '%s'. Check logs." target-symbol))
      (message "Failed to sanitize properties for target '%s'. Check logs."
               target-symbol))))

(cl-defun debrief--build-meta-plist (&key type enabled where timing arg-filter
                                      return-filter values watch break-on-change
                                      group description if-pred min-log-level name)
  "Helper to construct a meta plist for target registration."
  (let (plist)
    (when type (setq plist (plist-put plist :type type)))
    (when enabled (setq plist (plist-put plist :enabled enabled)))
    (when where (setq plist (plist-put plist :where where)))
    (when timing (setq plist (plist-put plist :timing timing)))
    (when arg-filter (setq plist (plist-put plist :arg-filter arg-filter)))
    (when return-filter (setq plist (plist-put plist :return-filter return-filter)))
    (when values (setq plist (plist-put plist :values values)))
    (when watch (setq plist (plist-put plist :watch watch)))
    (when break-on-change (setq plist (plist-put plist :break-on-change break-on-change)))
    (when group (setq plist (plist-put plist :group group)))
    (when description (setq plist (plist-put plist :description description)))
    (when if-pred (setq plist (plist-put plist :if if-pred)))
    (when min-log-level (setq plist (plist-put plist :min-log-level min-log-level)))
    (when name (setq plist (plist-put plist :name name)))
    plist))

;;;###autoload
(defun debrief/register-function (&optional target-symbol where timing arg-filter
                                            return-filter group description
                                            enabled if-pred)
  "Interactively register a FUNCTION as a debug target."
  (interactive
   (let* ((target-input (completing-read "Register function: " obarray #'fboundp
                                         t nil 'debrief-register-target-history))
          (where-choice
           (debrief--symbol-to-keyword
            (intern (completing-read "Advice type (around, before, after, etc.): "
                                     '("around" "before" "after" "override"
                                       "filter-args" "filter-return")
                                     nil t "around"))))
          (timing-choice (if (memq where-choice '(:around :after :before))
                             (y-or-n-p "Enable timing? ") nil))
          (arg-filter-lambda
           (when (memq where-choice '(:around :before :after :filter-args))
             (when (y-or-n-p "Add argument filter? ")
               (debrief--read-lambda-from-minibuffer
                "Arg filter (e.g., '(lambda (args) (car args))'): "
                "'(lambda (args) args)"
                'debrief-arg-filter-history))))
          (return-filter-lambda
           (when (memq where-choice '(:around :after :filter-return))
             (when (y-or-n-p "Add return filter? ")
               (debrief--read-lambda-from-minibuffer
                "Return filter (e.g., '(lambda (ret) (type-of ret))'): "
                "'(lambda (ret) ret)"
                'debrief-return-filter-history))))
          (group-input (read-string "Group (optional): " nil 'debrief-group-history))
          (desc-input (read-string "Description (optional): "))
          (enabled-choice
           (let ((choice (completing-read "Enabled state (t, nil, or lambda string): "
                                          '("t" "nil") nil t "t")))
             (cond ((string= choice "t") t)
                   ((string= choice "nil") nil)
                   (t (debrief--read-lambda-from-minibuffer
                       (format "Enabled lambda for %s: " target-input)
                       choice 'debrief-enabled-lambda-history)))))
          (if-pred-lambda
           (when (y-or-n-p "Add conditional predicate (:if)? ")
             (debrief--read-lambda-from-minibuffer
              "Conditional :if lambda: "
              "'(lambda () t)"
              'debrief-if-pred-history))))
     (list (intern target-input) where-choice timing-choice arg-filter-lambda
           return-filter-lambda
           (unless (string-empty-p group-input) (intern-soft group-input))
           (unless (string-empty-p desc-input) desc-input)
           enabled-choice if-pred-lambda)))

  (let ((meta-plist (debrief--build-meta-plist :type :function-advice
                                               :enabled enabled :if-pred if-pred
                                               :where where :timing timing
                                               :arg-filter arg-filter
                                               :return-filter return-filter
                                               :group group
                                               :description description)))
    (debrief/register-target target-symbol meta-plist)))

;;;###autoload
(defun debrief/register-variable (&optional target-symbol values watch
                                            break-on-change group description
                                            enabled if-pred)
  "Interactively register a VARIABLE as a debug target."
  (interactive
   (let* ((target-input (completing-read "Register variable: " obarray #'boundp
                                         t nil 'debrief-register-target-history))
          (values-str (read-string "Values `(ENABLED DISABLED)` (e.g., '(t nil)): "
                                   nil 'debrief-values-history))
          (parsed-values
           (if (string-empty-p values-str) nil
             (condition-case err (car (read-from-string (concat "(" values-str ")")))
               (error (debrief--log 'warn (intern-soft target-input)
                                    "Invalid :values input '%s': %S" values-str err)
                      nil))))
          (watch-choice (y-or-n-p "Enable watching? "))
          (break-choice (y-or-n-p "Enable break-on-change? "))
          (group-input (read-string "Group (optional): " nil 'debrief-group-history))
          (desc-input (read-string "Description (optional): "))
          (enabled-choice
           (let ((choice (completing-read "Enabled state (t, nil, or lambda string): "
                                          '("t" "nil") nil t "t")))
             (cond ((string= choice "t") t)
                   ((string= choice "nil") nil)
                   (t (debrief--read-lambda-from-minibuffer
                       (format "Enabled lambda for %s: " target-input)
                       choice 'debrief-enabled-lambda-history)))))
          (if-pred-lambda
           (when (y-or-n-p "Add conditional predicate (:if)? ")
             (debrief--read-lambda-from-minibuffer
              "Conditional :if lambda: "
              "'(lambda () t)"
              'debrief-if-pred-history))))
     (list (intern target-input) parsed-values watch-choice break-choice
           (unless (string-empty-p group-input) (intern-soft group-input))
           (unless (string-empty-p desc-input) desc-input)
           enabled-choice if-pred-lambda)))

  (let ((meta-plist (debrief--build-meta-plist :type :variable
                                               :enabled enabled :if-pred if-pred
                                               :values values :watch watch
                                               :break-on-change break-on-change
                                               :group group
                                               :description description)))
    (debrief/register-target target-symbol meta-plist)))

;;;###autoload
(defun debrief/register-hook-monitor (&optional target-symbol group description
                                                enabled if-pred)
  "Interactively register an Emacs HOOK for monitoring."
  (interactive
   (let* ((target-input (completing-read "Register hook to monitor: " obarray #'boundp
                                         t nil 'debrief-register-target-history))
          (group-input (read-string "Group (optional): " nil 'debrief-group-history))
          (desc-input (read-string "Description (optional): "))
          (enabled-choice
           (let ((choice (completing-read "Enabled state (t, nil, or lambda string): "
                                          '("t" "nil") nil t "t")))
             (cond ((string= choice "t") t)
                   ((string= choice "nil") nil)
                   (t (debrief--read-lambda-from-minibuffer
                       (format "Enabled lambda for %s: " target-input)
                       choice 'debrief-enabled-lambda-history)))))
          (if-pred-lambda
           (when (y-or-n-p "Add conditional predicate (:if)? ")
             (debrief--read-lambda-from-minibuffer
              "Conditional :if lambda: "
              "'(lambda () t)"
              'debrief-if-pred-history))))
     (list (intern target-input)
           (unless (string-empty-p group-input) (intern-soft group-input))
           (unless (string-empty-p desc-input) desc-input)
           enabled-choice if-pred-lambda)))

  (let ((meta-plist (debrief--build-meta-plist :type 'hook-monitor
                                               :enabled enabled :if-pred if-pred
                                               :group group
                                               :description description)))
    (debrief/register-target target-symbol meta-plist)))

;;;###autoload
(defun debrief/watch-variable (&optional target-symbol group description)
  "Convenience command to register a variable with `:watch t`."
  (interactive
   (let* ((target-input (completing-read "Watch variable: " obarray #'boundp
                                         t nil 'debrief-register-target-history))
          (group-input (read-string "Group (optional): " nil 'debrief-group-history))
          (desc-input (read-string "Description (optional): ")))
     (list (intern target-input)
           (unless (string-empty-p group-input) (intern-soft group-input))
           (unless (string-empty-p desc-input) desc-input))))
  (debrief/register-variable target-symbol nil t nil group description t nil))

;;;###autoload
(defun debrief/break-on-variable-change (&optional target-symbol group description)
  "Convenience command to register a variable with `:break-on-change t`."
  (interactive
   (let* ((target-input (completing-read "Break on change for variable: "
                                         obarray #'boundp
                                         t nil 'debrief-register-target-history))
          (group-input (read-string "Group (optional): " nil 'debrief-group-history))
          (desc-input (read-string "Description (optional): ")))
     (list (intern target-input)
           (unless (string-empty-p group-input) (intern-soft group-input))
           (unless (string-empty-p desc-input) desc-input))))
  (debrief/register-variable target-symbol nil nil t group description t nil))

;;;###autoload
(defun debrief/unregister-target (&optional target-symbol)
  "Unregister a debug target.
Removes the target from Debrief's management, deactivates its features,
restores original variable states if applicable, and saves configuration."
  (interactive
   (list
    (if (ht-empty-p debrief--debug-config)
        (progn (message "No targets registered.") nil)
      (intern (completing-read "Unregister target: "
                               (--map (symbol-name it)
                                      (-sort (lambda (a b) (string< (symbol-name a) (symbol-name b)))
                                             (ht-keys debrief--debug-config)))
                               nil t nil 'debrief-unregister-target-history)))))
  (cl-block debrief/unregister-target
    (unless target-symbol (cl-return-from debrief/unregister-target nil))

    (unless (ht-get debrief--debug-config target-symbol)
      (user-error "Target '%s' is not registered." target-symbol))

    (let ((config-entry (ht-get debrief--debug-config target-symbol)))
      (debrief-apply-entry-config (plist-put (copy-sequence config-entry)
                                            :enabled nil))
      (ht-remove! debrief--debug-config target-symbol)
      (debrief--log 'info target-symbol "Unregistered debug target: %s" target-symbol)

      (ht-map (lambda (group-key group-targets)
                (let ((new-targets (--filter (lambda (targ) (not (eq targ target-symbol)))
                                            group-targets)))
                  (if (null new-targets)
                      (ht-remove! debrief--debug-groups group-key)
                    (ht-set! debrief--debug-groups group-key new-targets))))
              debrief--debug-groups)

      (when (ht-get debrief--original-values target-symbol)
        (ht-remove! debrief--original-values target-symbol))

      (debrief/save-state)
      (message "Target '%s' unregistered successfully." target-symbol)
      (when (and (fboundp 'debrief/list-registered-targets)
                (get-buffer debrief-list-registered-buffer-name))
        (with-current-buffer (get-buffer debrief-list-registered-buffer-name)
          (when (eq major-mode 'debrief-tabulated-list-mode)
            (tabulated-list-revert)))))))

(defun debrief--toggle-specific-target-state (target-symbol config-entry)
  "Internal helper: Toggle :enabled for TARGET-SYMBOL using CONFIG-ENTRY."
  (unless (and (symbolp target-symbol) (plistp config-entry))
    (user-error "Internal error: Invalid args to debrief--toggle-specific-target-state"))

  (let* ((current-enabled-raw (plist-get config-entry :enabled))
         (current-enabled-effective
          (if (functionp current-enabled-raw)
              (let ((debrief--in-advised-call t)) (funcall current-enabled-raw))
            current-enabled-raw)) ; Respect explicit t/nil. Default to t if missing handled at registration.
         (new-enabled-val (not current-enabled-effective))
         (updated-config (plist-put (copy-sequence config-entry)
                                    :enabled new-enabled-val)))
    (ht-set! debrief--debug-config target-symbol updated-config)
    (debrief-apply-entry-config updated-config)
    (debrief--log 'info target-symbol "Toggled '%s' :enabled to %s."
                  target-symbol new-enabled-val)))

;;;###autoload
(defun debrief/toggle-target (&optional target-symbol)
  "Toggle the :enabled state of a specific debug target.
Prompts if `TARGET-SYMBOL` is nil. Updates, applies, and saves state."
  (interactive
   (list
    (if (ht-empty-p debrief--debug-config)
        (progn (message "No targets registered.") nil)
      (intern (completing-read "Toggle target: "
                               (--map (symbol-name it)
                                      (-sort (lambda (a b) (string< (symbol-name a) (symbol-name b)))
                                             (ht-keys debrief--debug-config)))
                               nil t nil 'debrief-toggle-target-history)))))

  (cl-block debrief/toggle-target
    (unless target-symbol (cl-return-from debrief/toggle-target nil))

    (if-let ((config-entry (ht-get debrief--debug-config target-symbol)))
        (progn
          (debrief--toggle-specific-target-state target-symbol config-entry)
          (debrief/save-state)
          (message "Target '%s' toggled." target-symbol)
          (when (and (fboundp 'debrief/list-registered-targets)
                     (get-buffer debrief-list-registered-buffer-name))
            (with-current-buffer (get-buffer debrief-list-registered-buffer-name)
              (when (eq major-mode 'debrief-tabulated-list-mode)
                (tabulated-list-revert)))))
      (user-error "Target '%s' is not registered." target-symbol))))

(defun debrief--modify-target-property (target-symbol property-key new-value-fn
                                                    prompt-message)
  "Internal helper to modify a boolean property of a target."
  (cl-block debrief--modify-target-property
    (unless target-symbol
      (setq target-symbol
            (intern (completing-read prompt-message
                                    (--map (symbol-name it)
                                            (-sort (lambda (a b) (string< (symbol-name a) (symbol-name b)))
                                                  (--filter (lambda (sym)
                                                              (eq (debrief--determine-target-type
                                                                    sym (ht-get debrief--debug-config sym))
                                                                  :variable))
                                                            (ht-keys debrief--debug-config))))
                                    nil t))))
    (unless target-symbol (cl-return-from debrief--modify-target-property nil))

    (let ((config-entry (ht-get debrief--debug-config target-symbol)))
      (unless (and config-entry (eq (debrief--determine-target-type target-symbol config-entry)
                                    :variable))
        (user-error "Target '%s' is not a registered variable target." target-symbol))

      (let* ((current-value (plist-get config-entry property-key))
            (new-value (funcall new-value-fn current-value))
            (updated-config (plist-put (copy-sequence config-entry)
                                        property-key new-value)))
        (debrief--register-debug-target target-symbol updated-config t)
        (message "%s for '%s' toggled to %s."
                (s-capitalize (symbol-name property-key)) target-symbol new-value)
        (when (and (fboundp 'debrief/list-registered-targets)
                  (get-buffer debrief-list-registered-buffer-name))
          (with-current-buffer (get-buffer debrief-list-registered-buffer-name)
            (when (eq major-mode 'debrief-tabulated-list-mode)
              (tabulated-list-revert))))))))

;;;###autoload
(defun debrief/toggle-target-watch (&optional target-symbol)
  "Toggle the `:watch` property for a variable target."
  (interactive (list nil))
  (debrief--modify-target-property target-symbol :watch #'not
                                 "Toggle watch for variable: "))

;;;###autoload
(defun debrief/toggle-target-break-on-change (&optional target-symbol)
  "Toggle the `:break-on-change` property for a variable target."
  (interactive (list nil))
  (debrief--modify-target-property target-symbol :break-on-change #'not
                                 "Toggle break-on-change for variable: "))

(defun debrief--manage-target-filter (target-symbol filter-key action-add-p
                                                  prompt-message default-lambda)
  "Internal helper to add/remove filters (:arg-filter, :return-filter)."
  (cl-block debrief--manage-target-filter
    (unless target-symbol
      (setq target-symbol
            (intern (completing-read prompt-message
                                    (--map (symbol-name it)
                                            (-sort (lambda (a b) (string< (symbol-name a) (symbol-name b)))
                                                  (--filter (lambda (sym)
                                                              (let ((cfg (ht-get debrief--debug-config sym)))
                                                                (and cfg
                                                                      (eq (debrief--determine-target-type sym cfg)
                                                                          :function-advice)
                                                                      (if action-add-p t
                                                                        (plist-get cfg filter-key)))))
                                                            (ht-keys debrief--debug-config))))
                                    nil t))))
    (unless target-symbol (cl-return-from debrief--manage-target-filter nil))

    (let ((config-entry (ht-get debrief--debug-config target-symbol)))
      (unless (and config-entry (eq (debrief--determine-target-type target-symbol config-entry)
                                    :function-advice))
        (user-error "Target '%s' is not a registered function advice target." target-symbol))

      (let (updated-config filter-fn-new)
        (if action-add-p
            (progn
              (setq filter-fn-new
                    (debrief--read-lambda-from-minibuffer
                    (format "Enter %s lambda for %s (e.g., '%s'): "
                            (s-replace ":" "" (symbol-name filter-key))
                            target-symbol default-lambda)
                    default-lambda
                    (intern (format "debrief-%s-history" (symbol-name filter-key)))))
              (setq updated-config (plist-put (copy-sequence config-entry)
                                              filter-key filter-fn-new)))
          ;; Else, action is remove
          (unless (plist-get config-entry filter-key)
            (user-error "Target '%s' does not have a %s."
                        target-symbol (s-replace ":" "" (symbol-name filter-key))))
          (setq updated-config (cl-remf (copy-sequence config-entry) filter-key)))

        (debrief--register-debug-target target-symbol updated-config t)
        (message "%s %s %s '%s'."
                (s-capitalize (s-replace ":" "" (symbol-name filter-key)))
                (if action-add-p "added to" "removed from")
                target-symbol
                (if action-add-p (format "as %S" filter-fn-new)
                  (format "(was: %S)" (plist-get config-entry filter-key))))
        (when (and (fboundp 'debrief/list-registered-targets)
                  (get-buffer debrief-list-registered-buffer-name))
          (with-current-buffer (get-buffer debrief-list-registered-buffer-name)
            (when (eq major-mode 'debrief-tabulated-list-mode)
              (tabulated-list-revert))))))))

;;;###autoload
(defun debrief/add-arg-filter (&optional target-symbol)
  "Add/Update an argument filter lambda to a function advice target."
  (interactive (list nil))
  (debrief--manage-target-filter target-symbol :arg-filter t
                                 "Add/Update arg filter for function: "
                                 "'(lambda (args) args)"))
;;;###autoload
(defun debrief/remove-arg-filter (&optional target-symbol)
  "Remove the argument filter from a function advice target."
  (interactive (list nil))
  (debrief--manage-target-filter target-symbol :arg-filter nil
                                 "Remove arg filter from function: " nil))
;;;###autoload
(defun debrief/add-return-filter (&optional target-symbol)
  "Add/Update a return value filter lambda to a function advice target."
  (interactive (list nil))
  (debrief--manage-target-filter target-symbol :return-filter t
                                 "Add/Update return filter for function: "
                                 "'(lambda (ret) ret)"))
;;;###autoload
(defun debrief/remove-return-filter (&optional target-symbol)
  "Remove the return value filter from a function advice target."
  (interactive (list nil))
  (debrief--manage-target-filter target-symbol :return-filter nil
                                 "Remove return filter from function: " nil))

;;;###autoload
(defun debrief/call-function-temporarily (target-symbol &rest args)
  "Call a function with a temporary debug wrapper and ARGS.
Applies temporary `:around` advice, executes `(apply TARGET-SYMBOL ARGS)`,
then removes advice. Useful for one-off debug calls."
  (interactive
   (let* ((fn-name (completing-read "Call function temporarily: " obarray #'fboundp
                                    t nil 'debrief-temp-call-args-history))
          (fn-sym (intern fn-name))
          (args-str (read-string (format "Arguments for %S (Lisp form): " fn-sym)
                                 nil 'debrief-temp-call-args-history)))
     (cons fn-sym
           (if (string-empty-p args-str) nil
             (condition-case err (car (read-from-string (format "(list %s)" args-str)))
               (error (user-error "Invalid arguments string '%s': %S"
                                  args-str err)))))))

  (unless (fboundp target-symbol)
    (user-error "Function '%s' is not defined." target-symbol))

  (let* ((temp-advice-name (debrief--get-advice-name target-symbol))
         (temp-advice-fn (debrief--generate-advice-function
                          target-symbol :around t nil nil)) ; Default timing
         result)
    (unless temp-advice-fn
      (user-error "Failed to create temporary advice for %s" target-symbol))
    (unwind-protect
        (progn
          (advice-add target-symbol :around temp-advice-fn
                      (list 'name temp-advice-name)) ; Corrected PROPS
          (debrief--log 'info target-symbol
                        "Temporarily advised %s for call with args %S."
                        target-symbol args)
          (message "Calling %S with temporary debug wrapper (args: %S)..."
                   target-symbol args)
          (setq result (apply target-symbol args))
          (message "Call to %S completed. Result: %S" target-symbol result)
          result)
      (when (advice-member-p temp-advice-name target-symbol)
        (advice-remove target-symbol temp-advice-name)
        (debrief--log 'info target-symbol
                      "Removed temporary advice '%s' from %s."
                      temp-advice-name target-symbol)))))

;;;###autoload
(defun debrief/wrap-function-temporarily (&optional target-symbol where timing
                                                    arg-filter return-filter)
  "Apply a temporary debug wrapper to a function until manually removed."
  (interactive
   (list (intern (completing-read "Wrap function temporarily: " obarray #'fboundp
                                    t nil 'debrief-register-target-history))
         (debrief--symbol-to-keyword
          (intern (completing-read "Advice type (around, before, after): "
                                   '("around" "before" "after") nil t "around")))
         (y-or-n-p "Enable timing? ")
         (when (y-or-n-p "Add argument filter? ")
           (debrief--read-lambda-from-minibuffer "Enter arg filter lambda: "
                                                  "'(lambda (args) args)"
                                                  'debrief-arg-filter-history))
         (when (y-or-n-p "Add return filter? ")
           (debrief--read-lambda-from-minibuffer "Enter return filter lambda: "
                                                  "'(lambda (ret) ret)"
                                                  'debrief-return-filter-history))))

  (unless (fboundp target-symbol)
    (user-error "Function '%s' is not defined." target-symbol))

  (let* ((advice-type (or where :around))
         (enable-timing (or timing t))
         (temp-advice-name (debrief--get-advice-name target-symbol))
         (temp-advice-fn (debrief--generate-advice-function
                          target-symbol advice-type enable-timing
                          arg-filter return-filter)))
    (unless temp-advice-fn
      (user-error "Failed to create temporary advice for %s" target-symbol))

    (if (advice-member-p temp-advice-name target-symbol)
        (message "Function '%s' already has Debrief wrapper '%s'."
                 target-symbol temp-advice-name)
      (advice-add target-symbol advice-type temp-advice-fn
                  (list 'name temp-advice-name)) ; Corrected PROPS
      (debrief--log 'info target-symbol
                    "Applied temporary Debrief advice '%s' to %s (%s)."
                    temp-advice-name target-symbol advice-type)
      (message "Function '%s' temporarily wrapped. Use M-x advice-remove '%s' '%s'."
               target-symbol target-symbol temp-advice-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Group Management Commands                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -*- lexical-binding: t; -*-
;;;###autoload
(defun debrief/toggle-group (&optional group-symbol)
  "Toggle the :enabled state of all targets within a debug group.
If GROUP-SYMBOL is nil, prompt interactively.
This flips the :enabled state of targets, applies changes, and saves."
  (interactive
   (let* ((all-groups (ht-keys debrief--debug-groups)))
     (unless all-groups
       (user-error "No debug groups are currently defined."))
     (let* ((sorted-groups (-sort (lambda (a b) (string< (symbol-name a) (symbol-name b)))
                                  all-groups))
            (annotated-groups
             (--map (let* ((group it)
                           (targets (ht-get debrief--debug-groups group))
                           (active-count
                            (--count (when-let ((cfg (ht-get debrief--debug-config it)))
                                       (debrief--is-entry-active-p cfg))
                                     targets))
                           (status (if (> active-count 0)
                                       (format "Active (%d/%d)" active-count (length targets))
                                     (format "Inactive (0/%d)" (length targets)))))
                      (cons (symbol-name group) status))
                    sorted-groups)))
       (list
        (intern
         (completing-read
          "Toggle group: "
          (lambda (str pred action)
            (if (eq action 'metadata)
                `(metadata
                  (annotation-function
                   . ,(lambda (input)
                        ;; Corrected: Use #'equal for string comparison in assoc
                        (cdr (assoc input annotated-groups #'equal))))
                  (display-sort-function
                   . ,(lambda (completions)
                        (-sort
                         (lambda (a b)
                           (let* ((a-status (cdr (assoc a annotated-groups #'equal)))
                                  (b-status (cdr (assoc b annotated-groups #'equal)))
                                  (a-active (s-starts-with? "Active" a-status))
                                  (b-active (s-starts-with? "Active" b-status)))
                             (if (eq a-active b-active)
                                 (string< a b)
                               a-active)))
                         completions))))
              (complete-with-action action (mapcar #'car annotated-groups) str pred)))
          nil t nil 'debrief-toggle-group-history)))))) ; Added history var

  ;; Sanity check on group
  (unless (symbolp group-symbol)
    (user-error "Invalid group symbol: %S" group-symbol))

  (let ((targets (ht-get debrief--debug-groups group-symbol)))
    (unless targets
      (user-error "Group '%s' not found or is empty." group-symbol))

    (let* ((any-active?
            (--any? (when-let ((cfg (ht-get debrief--debug-config it)))
                      (debrief--is-entry-active-p cfg))
                    targets))
           (new-state (not any-active?)))
      (debrief--log 'info group-symbol "Toggling group '%s'. Setting targets to: %s"
                    group-symbol (if new-state "ENABLED" "DISABLED"))

      (--each targets
        (when-let ((cfg (ht-get debrief--debug-config it)))
          (let ((new-cfg (plist-put (copy-sequence cfg) :enabled new-state)))
            (ht-set! debrief--debug-config it new-cfg)
            (debrief-apply-entry-config new-cfg))))

      (debrief/save-state)
      (message "Group '%s' toggled. Targets set to: %s"
               group-symbol (if new-state "ENABLED" "DISABLED"))

      (when (and (fboundp 'debrief/list-registered-targets)
                 (get-buffer debrief-list-registered-buffer-name))
        (with-current-buffer (get-buffer debrief-list-registered-buffer-name)
          (when (eq major-mode 'debrief-tabulated-list-mode)
            (tabulated-list-revert))))

      (debrief/refresh-all-targets))))

(provide 'debrief-commands)
;;; debrief-commands.el ends here