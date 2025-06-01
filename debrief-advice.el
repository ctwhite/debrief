;;; debrief-advice.el --- Advice and variable watcher management for Debrief -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This file provides the core logic for generating and managing Emacs Lisp
;; advice and variable watchers used by the Debrief debugging framework.
;;
;; It is responsible for:
;; - Dynamically generating advice functions tailored to specific debugging
;;   configurations (e.g., logging, timing, argument/return value filtering).
;; - Providing a generic wrapper for `:around` advice that centralizes the
;;   handling of common debugging tasks like logging function calls, measuring
;;   execution time, applying filters, and managing errors.
;; - Implementing the callback function used by Emacs's variable watcher mechanism
;;   to monitor and log changes to specified variables.
;; - Supplying global advice for monitoring the execution of Emacs hooks, allowing
;;   Debrief to trace hook activities.
;;
;; This module is a foundational component of the Debrief Emacs debugging suite,
;; enabling much of its dynamic introspection capabilities.

;;; Code:

(require 'cl-lib)
(require 'ht)          ; For hash table operations (ht-get, etc.)
(require 'ts)          ; For timestamp functions (ts-now, ts-difference)

;; Declare functions from debrief-core.el to satisfy the byte-compiler
;; and indicate dependencies.
(declare-function debrief--log "debrief-core")
(declare-function debrief--is-entry-active-p "debrief-core")

;; Internal global state variables, primarily defined and managed in `debrief-core.el`,
;; but referenced here.
(defvar debrief--debug-config)           ; Stores all debug configurations.
(defvar debrief--active-monitored-hooks) ; List of hooks currently being monitored.
(defvar debrief-error-handler)           ; User-configurable error handling function.
(defvar debrief--in-advised-call)        ; Guard against re-entrant advised calls.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Function Advice Internals                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debrief--get-advice-name (target-symbol)
  "Generate a unique advice name for TARGET-SYMBOL.
This ensures that advice added by Debrief can be uniquely identified and managed.
Arguments:
  TARGET-SYMBOL (symbol): The symbol (function or variable) being advised.
Return:
  (symbol): A new symbol suitable for use as an advice name."
  (intern (format "debrief--advice-for-%S" target-symbol)))

(defun debrief--default-arg-filter (args)
  "Default argument filter; returns ARGS as is.
This function is used when no custom argument filter is specified in the
debug configuration for a target.
Arguments:
  ARGS (list): The list of arguments passed to the advised function.
Return:
  (list): The original ARGS list, unchanged."
  args)

(defun debrief--default-return-filter (return-val)
  "Default return value filter; returns RETURN-VAL as is.
This function is used when no custom return value filter is specified in the
debug configuration for a target.
Arguments:
  RETURN-VAL (any): The value returned by the advised function.
Return:
  (any): The original RETURN-VAL, unchanged."
  return-val)

(defun debrief--debug-around-wrapper (target-fn orig-fn args
                                                timing-enabled
                                                arg-filter return-filter)
  "Execute ORIG-FN with ARGS for TARGET-FN, providing debug features for :around advice.
This is the primary workhorse for most function advice. It handles:
- Checking if the debug configuration for TARGET-FN is active.
- Optionally timing the execution of ORIG-FN.
- Applying argument and return value filters.
- Logging the call, arguments, return value, and timing.
- Calling a global error handler if an error occurs within ORIG-FN.
Arguments:
  TARGET-FN (symbol): The function symbol being debugged.
  ORIG-FN (function): The original function being advised.
  ARGS (list): The arguments passed to TARGET-FN.
  TIMING-ENABLED (boolean): If non-nil, measure and log execution time.
  ARG-FILTER (function): Function to filter/transform ARGS before logging/calling.
  RETURN-FILTER (function): Function to filter/transform return value before logging.
Return:
  (any): The result of applying ORIG-FN to ARGS (possibly transformed by filters
         if the filters were designed to modify, though default is for logging)."
  (cl-block debrief--debug-around-wrapper
    (debrief--log :trace target-fn
                  "Enter around-wrapper for %s. Args: %S" target-fn args)
    (let ((config (ht-get debrief--debug-config target-fn))
          is-active-check)
      (unless config
        (debrief--log :warn target-fn
                      "No config for %s in around-wrapper. Bypassing." target-fn)
        ;; Directly call original function if no config (should not happen ideally)
        (cl-return-from debrief--debug-around-wrapper (apply orig-fn args)))

      (setq is-active-check (debrief--is-entry-active-p config))
      (debrief--log :trace target-fn "Wrapper for %s: active check: %S"
                    target-fn is-active-check)

      (if is-active-check
          (let ((debrief--in-advised-call t)) ; Prevent re-entrancy into Debrief logic
            (condition-case err
                (let* ((start-time (when timing-enabled (ts-now)))
                      (filtered-args (funcall (or arg-filter #'debrief--default-arg-filter)
                                              args))
                      result)
                  (debrief--log :trace target-fn "Calling original %s with args: %S"
                                target-fn filtered-args)
                  (setq result (apply orig-fn args)) ; Call the original function
                  (debrief--log :trace target-fn "Original %s returned: %S"
                                target-fn result)
                  (let* ((end-time (when timing-enabled (ts-now)))
                        (filtered-result (funcall (or return-filter
                                                      #'debrief--default-return-filter)
                                                  result))
                        (log-message
                          (if timing-enabled
                              (format "Called %s (timed: %.3fms) with args: %S => %S"
                                      target-fn
                                      (* 1000 (ts-difference end-time start-time))
                                      filtered-args
                                      filtered-result)
                            (format "Called %s with args: %S => %S"
                                    target-fn
                                    filtered-args
                                    filtered-result))))
                    (debrief--log :trace target-fn log-message))
                  result) ; Return the original (unfiltered by return-filter here) result
              ((debug error) ; Catch errors from the original function call
              (funcall debrief-error-handler target-fn err)
              (signal (car err) (cdr err))))) ; Re-signal the error
        ;; If not active, just call the original function
        (apply orig-fn args)))))

(defun debrief--debug-before-wrapper (target-fn args arg-filter)
  "Log arguments for `:before` advice if the target is active.
Arguments:
  TARGET-FN (symbol): The function symbol being debugged.
  ARGS (list): The arguments passed to TARGET-FN.
  ARG-FILTER (function): Function to filter/transform ARGS before logging.
Return:
  nil."
  (debrief--log :trace target-fn "Enter before-wrapper for %s. Args: %S" target-fn args)
  (when (debrief--is-entry-active-p (ht-get debrief--debug-config target-fn))
    (let ((debrief--in-advised-call t))
      (let ((filtered-args (funcall (or arg-filter #'debrief--default-arg-filter) args)))
        (debrief--log :trace target-fn "Before %s with args: %S"
                      target-fn filtered-args))))
  nil) ; :before advice return value is usually ignored

(defun debrief--debug-after-wrapper (target-fn args return-val arg-filter return-filter)
  "Log arguments and return value for `:after` advice if the target is active.
Arguments:
  TARGET-FN (symbol): The function symbol being debugged.
  ARGS (list): The arguments originally passed to TARGET-FN.
  RETURN-VAL (any): The value returned by TARGET-FN.
  ARG-FILTER (function): Function to filter/transform ARGS before logging.
  RETURN-FILTER (function): Function to filter/transform RETURN-VAL before logging.
Return:
  nil."
  (debrief--log :trace target-fn "Enter after-wrapper for %s. Args: %S, Return: %S"
                target-fn args return-val)
  (when (debrief--is-entry-active-p (ht-get debrief--debug-config target-fn))
    (let ((debrief--in-advised-call t))
      (let ((filtered-args (funcall (or arg-filter #'debrief--default-arg-filter) args))
            (filtered-return (funcall (or return-filter #'debrief--default-return-filter)
                                      return-val)))
        (debrief--log :trace target-fn "After %s with args: %S => %S"
                      target-fn filtered-args filtered-return))))
  nil) ; :after advice return value is usually ignored

(defun debrief--debug-override-wrapper (target-fn orig-fn args)
  "Log invocation for `:override` advice and pass control to ORIG-FN.
This wrapper ensures that an override advice is logged if active, but still
delegates to the user-provided override function (orig-fn in this context of
how `advice-add` works with :override when the advice IS the override).
Arguments:
  TARGET-FN (symbol): The function symbol being debugged/overridden.
  ORIG-FN (function): The user-supplied function that performs the override.
  ARGS (list): The arguments passed to TARGET-FN.
Return:
  (any): The result of applying the user's ORIG-FN to ARGS."
  (debrief--log :trace target-fn "Enter override-wrapper for %s. Args: %S"
                target-fn args)
  ;; Note: For :override, `orig-fn` is the actual override lambda provided by user.
  ;; The "original" function (the one being overridden) is not directly accessible
  ;; to this simple logging wrapper unless it's captured by the user's override.
  (if (debrief--is-entry-active-p (ht-get debrief--debug-config target-fn))
      (let ((debrief--in-advised-call t))
        (debrief--log :trace target-fn "Overriding %s with args: %S" target-fn args)
        (apply orig-fn args))
    ;; If not active, the override advice should still run as configured by user.
    (apply orig-fn args)))

(defun debrief--debug-filter-args-wrapper (target-fn _orig-fn args arg-filter)
  "Log and filter arguments for `:filter-args` style advice.
This wrapper is intended to be used as part of an advice combination that
modifies arguments before the main function is called.
Arguments:
  TARGET-FN (symbol): The function whose arguments are being filtered.
  _ORIG-FN (function): The original function (unused in this specific wrapper,
                       as it only filters args, but part of advice signature).
  ARGS (list): The original arguments passed to TARGET-FN.
  ARG-FILTER (function): Function to transform ARGS.
Return:
  (list): The filtered list of arguments."
  (debrief--log :trace target-fn "Enter filter-args-wrapper for %s. Args: %S"
                target-fn args)
  (if (debrief--is-entry-active-p (ht-get debrief--debug-config target-fn))
      (let* ((debrief--in-advised-call t)
             (filtered-args (funcall (or arg-filter #'debrief--default-arg-filter) args)))
        (debrief--log :trace target-fn "Filtering args for %s. Original: %S -> Filtered: %S"
                      target-fn args filtered-args)
        filtered-args)
    args)) ; Return original args if not active

(defun debrief--debug-filter-return-wrapper (target-fn _orig-fn return-val return-filter)
  "Log and filter return value for `:filter-return` style advice.
This wrapper is intended to be used as part of an advice combination that
modifies the return value after the main function has executed.
Arguments:
  TARGET-FN (symbol): The function whose return value is being filtered.
  _ORIG-FN (function): The original function (unused here, part of signature).
  RETURN-VAL (any): The original return value from TARGET-FN.
  RETURN-FILTER (function): Function to transform RETURN-VAL.
Return:
  (any): The filtered return value."
  (debrief--log :trace target-fn "Enter filter-return-wrapper for %s. Return: %S"
                target-fn return-val)
  (if (debrief--is-entry-active-p (ht-get debrief--debug-config target-fn))
      (let* ((debrief--in-advised-call t)
             (filtered-return (funcall (or return-filter #'debrief--default-return-filter)
                                       return-val)))
        (debrief--log :trace target-fn "Filtering return for %s. Original: %S -> Filtered: %S"
                      target-fn return-val filtered-return)
        filtered-return)
    return-val)) ; Return original value if not active

(defun debrief--generate-advice-function (target-fn where timing arg-filter return-filter)
  "Generate a Debrief advice function for TARGET-FN based on WHERE, TIMING, etc.
This is used when no custom `:fn` is provided in the debug configuration.
It creates a lambda suitable for `advice-add` that wraps the original function
with appropriate Debrief debugging logic (logging, timing, filtering).
Arguments:
  TARGET-FN (symbol): The function symbol to be advised.
  WHERE (keyword): The advice type (e.g., :around, :before, :after, :filter-args,
                 :filter-return, :override).
  TIMING (boolean): Whether to enable timing for this advice (relevant for :around).
  ARG-FILTER (function): Function to filter arguments for logging/modification.
  RETURN-FILTER (function): Function to filter return values for logging/modification.
Return:
  (function): A lambda expression suitable for `advice-add`, or nil if WHERE is unsupported."
  (pcase where
    (:around
     (lambda (orig-fn &rest args)
       (debrief--debug-around-wrapper target-fn orig-fn args timing
                                      arg-filter return-filter)))
    (:before
     (lambda (&rest args) ; :before advice does not receive orig-fn
       (debrief--debug-before-wrapper target-fn args arg-filter)))
    (:after ; :after advice receives return-val as first arg, then original args
     (lambda (return-val &rest args)
       (debrief--debug-after-wrapper target-fn args return-val
                                     arg-filter return-filter)))
    (:override ; For :override, the advice *is* the new function.
               ; `orig-fn` here is the function provided by the user's override config.
     (lambda (orig-fn &rest args)
       (debrief--debug-override-wrapper target-fn orig-fn args)))
    (:filter-args
     ;; This advice type modifies arguments *before* they reach the original function.
     ;; The advice function itself receives the original function as its first argument.
     (lambda (orig-fn &rest args)
       (apply orig-fn (debrief--debug-filter-args-wrapper target-fn orig-fn args
                                                          arg-filter))))
    (:filter-return
     ;; This advice type modifies the return value *after* the original function runs.
     (lambda (orig-fn &rest args)
       (let ((return-val (apply orig-fn args)))
         (debrief--debug-filter-return-wrapper target-fn orig-fn return-val
                                               return-filter))))
    ;; Advanced advice types from `nadvice.el` (less common for simple logging)
    ((or :before-while :after-while :around-while)
     (debrief--log :warn target-fn "Advanced advice %S for %S. Basic logging wrapper."
               where target-fn)
     (lambda (orig-fn &rest args) ; Generic wrapper for while-style advice
       (when (debrief--is-entry-active-p (ht-get debrief--debug-config target-fn))
         (let ((debrief--in-advised-call t))
           (debrief--log :trace target-fn "Triggered %S advice for %S with args: %S"
                     where target-fn
                     (funcall (or arg-filter #'debrief--default-arg-filter) args))))
       (apply orig-fn args))) ; Always call original for these as a default
    (_
     (debrief--log :warn target-fn "Unsupported advice type %S for %S. No advice generated."
               where target-fn)
     nil))) ; Return nil if no suitable advice function can be generated

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Variable Watchers                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debrief--variable-watcher-function (symbol new-value _operation old-value _where)
  "Function called by `add-variable-watcher` to log variable changes.
This function is installed as a watcher for variables configured with `:watch t`
or `:break-on-change t` in their Debrief configuration.
It logs the change if the variable's Debrief entry is active.
Arguments:
  SYMBOL (symbol): The variable whose value changed.
  NEW-VALUE (any): The new value of the variable.
  _OPERATION (symbol): The operation that changed the value (e.g., `setq`, `let`). Unused.
  OLD-VALUE (any): The value of the variable before the change.
  _WHERE (buffer|nil): Buffer where change occurred, or nil if global. Unused.
Return:
  nil."
  (let ((config (ht-get debrief--debug-config symbol)))
    (when (and config (debrief--is-entry-active-p config))
      (let ((debrief--in-advised-call t)) ; Prevent re-entrancy
        (debrief--log :info symbol
                      "Variable %s changed: %S -> %S"
                      symbol old-value new-value)
        ;; Implement break-on-change logic if configured
        (when (plist-get config :break-on-change)
          (debrief--log :warn symbol "Break on change for variable %s (old: %S, new: %S)"
                    symbol old-value new-value)
          (debug symbol old-value new-value))))) ; Trigger debugger
  nil) ; Watcher functions typically don't return significant values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Hook Monitor Advice                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debrief--global-hook-wrapper (orig-fn hook-name &rest args)
  "Advice for `run-hooks` and similar hook-running functions.
This advice monitors specific hooks if they are registered and active in Debrief.
It logs the hook's start, end, and total execution time.
Arguments:
  ORIG-FN (function): The original hook-running function (e.g., `run-hooks`).
  HOOK-NAME (symbol): The name of the hook variable being executed.
  ARGS (list): Any additional arguments passed to ORIG-FN after HOOK-NAME.
Return:
  (any): The result of calling ORIG-FN."
  (let* ((target-hook-symbol (if (symbolp hook-name) hook-name
                               ;; Attempt to intern if not already a symbol (e.g. from strings)
                               (ignore-errors (intern (symbol-name hook-name)))))
         (config (when target-hook-symbol (ht-get debrief--debug-config target-hook-symbol))))
    (if (and target-hook-symbol
             (memq target-hook-symbol debrief--active-monitored-hooks)
             config
             (eq (plist-get config :type) 'hook-monitor)
             (debrief--is-entry-active-p config))
        ;; Monitored and active hook
        (let ((debrief--in-advised-call t)) ; Prevent re-entrancy
          (let ((start-time (ts-now))
                result)
            (debrief--log :trace target-hook-symbol "Hook '%s' started." target-hook-symbol)
            (setq result (apply orig-fn hook-name args)) ; Call original hook runner
            (let ((end-time (ts-now)))
              (debrief--log :info target-hook-symbol "Hook '%s' finished (timed: %.3fms)."
                            target-hook-symbol
                            (* 1000 (ts-difference end-time start-time))))
            result))
      ;; Not a monitored hook or not active, just run the original function
      (apply orig-fn hook-name args))))

(provide 'debrief-advice)
;;; debrief-advice.el ends here