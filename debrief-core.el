;;; debrief-core.el --- Centralized debug configuration and tooling for Emacs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides the core logic for the Debrief debugging framework.
;; It is responsible for managing the central registry of debug targets,
;; their configurations, and their activation state.
;;
;; Key Responsibilities:
;; - Maintaining the `debrief--debug-config` hash table, which stores the
;;   configuration for all registered debug targets (variables, functions, hooks).
;; - Handling the registration, unregistration, and toggling of debug targets.
;; - Applying debug configurations, which involves:
;;   - Setting variable values or adding watchers for variable targets.
;;   - Adding/removing advice for function targets.
;;   - Managing the monitoring of hook targets.
;; - Providing helper functions for sanitizing and validating debug configurations.
;; - Managing global Debrief settings, such as `debrief-debug-enabled` and
;;   `debrief-error-handler`.
;; - Interfacing with `debrief-advice.el` for generating and applying advice,
;;   `debrief-log.el` for logging, and `debrief-persist.el` for saving/loading state.
;;
;; This file forms the backbone of the Debrief system, coordinating the various
;; debugging mechanisms.

;;; Code:

(require 'cl-lib)
(require 'ht)          ; For hash table operations (ht-create, ht-get, etc.)
(require 'dash)        ; For utility functions like --each
(require 'ts)          ; For timestamp functions (ts-now, ts-difference)
(require 'subr-x)      ; For if-let, when-let, etc.
(require 'debrief-advice) ; For advice generation and management functions
(require 'debrief-log)   ; For logging functionalities

;; Declare functions used from other Debrief modules.
;; These are forward declarations to satisfy the byte-compiler and indicate
;; dependencies between modules.
(declare-function debrief--log "debrief-log" (level target-symbol format-string &rest args))
(declare-function debrief/save-state "debrief-persist" ())
(declare-function debrief/load-state "debrief-persist" ())
(declare-function debrief/list-registered-targets "debrief-ui" ())
(declare-function debrief--get-advice-name "debrief-advice" (target-symbol))
(declare-function debrief--generate-advice-function "debrief-advice"
                  (target-fn where timing arg-filter return-filter))
(declare-function debrief--variable-watcher-function "debrief-advice"
                  (symbol new-value _operation old-value _where))
(declare-function debrief--global-hook-wrapper "debrief-advice"
                  (orig-fn hook-name &rest args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Internal Variables                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst debrief--hook-running-functions
  '(run-hooks run-hook-with-args run-hook-with-args-until-success
    run-hook-wrapped)
  "List of core Emacs functions that execute hooks.
These functions are advised globally by `debrief--global-hook-wrapper`
to monitor hook execution times when a hook is registered as a
`:hook-monitor` type target in Debrief.")

(defvar debrief-list-registered-buffer-name "*debrief-registered*"
  "Default buffer name for the `debrief/list-registered-targets` UI command.")

(defvar debrief-register-target-history nil
  "History variable for the `debrief/register-target` command's minibuffer prompt.")

(defvar debrief--debug-config (ht-create)
  "Internal hash table storing the full debug configuration for each target.
Keys are target symbols (e.g., a function name, variable name, or hook name).
Values are plists containing the sanitized and processed configuration
options for that target, derived from `debrief-debug-vars` or dynamic
registrations.")

(defvar debrief--debug-groups (ht-create)
  "Internal hash table mapping group symbols to a list of target symbols.
This allows organizing debug targets into logical groups for easier management.
Groups are defined via the `:group` key in `debrief-debug-vars`.")

(defvar debrief--active-monitored-hooks nil
  "A list of hook symbols that are currently being actively monitored by Debrief.
This list is populated based on targets of `:type :hook-monitor` that are
currently enabled. It's used by `debrief--global-hook-wrapper` to decide
whether to log a particular hook's execution.")

(defvar debrief--global-hook-monitor-advice-active-p nil
  "Non-nil if the global advice for hook monitoring (on functions like `run-hooks`)
is currently active. This is managed by `debrief--ensure-global-hook-advice`.")

(defvar debrief--original-values (make-hash-table :test 'eq)
  "Internal hash table to store original values of debugged variables.
This is used when a variable target has `:values` specified or is a boolean
that Debrief toggles. When Debrief disables debugging for such a variable,
it attempts to restore its original value from this table.
Key: variable symbol. Value: (original-value . t), where `t` indicates
Debrief stored it (to differentiate from a stored value of `nil`).")

(defvar debrief--active-advice (ht-create)
  "Hash table to store active advice names for functions.
Key: advised function symbol.
Value: list of advice names (symbols) applied by Debrief to that function.
This helps in correctly removing Debrief's advice when a target is disabled
or unregistered.")

(defvar debrief--in-advised-call nil
  "Dynamically scoped variable to prevent re-entrant calls to Debrief's advice/logic.
It is bound to `t` when Debrief's internal advice wrappers or watcher functions
are executing. This helps avoid infinite loops if, for example, a logging
function itself is being debugged by Debrief, or if an `:if` predicate
in a debug configuration calls a function that is also being debugged.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Error Handling                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debrief--default-error-handler (target-fn err)
  "Default error handler for errors caught by Debrief's advice wrappers.
Logs the error using `debrief--log`. This function is the default value for
`debrief-error-handler`.
Arguments:
  TARGET-FN (symbol): The function symbol where the error occurred.
  ERR (error): The error condition signaled."
  (debrief--log :error target-fn "Error in advised function %s: %S" target-fn err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Helper Functions (for config sanitization)              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debrief--validate-boolean-prop (val target-symbol key)
  "Validate VAL is a boolean for property KEY of TARGET-SYMBOL.
Returns VAL if boolean, otherwise logs a warning and returns nil.
Arguments:
  VAL (any): The value to validate.
  TARGET-SYMBOL (symbol): The target symbol for context in warnings.
  KEY (keyword): The property key being validated (e.g., :enabled).
Return:
  (boolean|nil): VAL if valid, else nil."
  (unless (booleanp val)
    (debrief--log :warn target-symbol
                  "Invalid %S for %S: Expected boolean, got %S. Defaulting to nil."
                  key target-symbol val)
    (setq val nil))
  val)

(defun debrief--validate-function-prop (val target-symbol key)
  "Validate VAL is a function for property KEY of TARGET-SYMBOL.
Returns VAL if a function, otherwise logs a warning and returns nil.
Arguments:
  VAL (any): The value to validate.
  TARGET-SYMBOL (symbol): The target symbol for context in warnings.
  KEY (keyword): The property key being validated (e.g., :if, :fn).
Return:
  (function|nil): VAL if valid, else nil."
  (unless (functionp val)
    (debrief--log :warn target-symbol
                  "Invalid %S for %S: Expected function, got %S. Removing key."
                  key target-symbol val)
    (setq val nil))
  val)

(defun debrief--validate-keyword-or-symbol-prop (val target-symbol key
                                                 valid-keywords default-val)
  "Validate VAL is a keyword or symbol and is in VALID-KEYWORDS (list of keywords).
Converts symbol VAL to a keyword before checking.
Returns the validated keyword, or DEFAULT-VAL with a warning if invalid.
Arguments:
  VAL (any): The value to validate.
  TARGET-SYMBOL (symbol): The target symbol for context in warnings.
  KEY (keyword): The property key being validated (e.g., :type, :where).
  VALID-KEYWORDS (list keyword): A list of valid keyword values.
  DEFAULT-VAL (keyword|nil): The default keyword to return if VAL is invalid.
Return:
  (keyword|nil): The validated keyword or DEFAULT-VAL."
  (let ((kw-val (if (keywordp val)
                    val ; Already a keyword, use as is
                  (if (symbolp val)
                      (intern (format ":%s" (symbol-name val))) ; Convert symbol
                    val)))) ; Not a symbol or keyword, pass for memq check
    (unless (memq kw-val valid-keywords)
      (debrief--log :warn target-symbol
                    "Invalid %S for %S: got %S (processed as %S). Defaulting to %S."
                    key target-symbol val kw-val default-val)
      (setq kw-val default-val))
    kw-val))

(defun debrief--validate-list-of-length-prop (val target-symbol key
                                              expected-length)
  "Validate VAL is a list of EXPECTED-LENGTH for property KEY of TARGET-SYMBOL.
Returns VAL if valid, otherwise logs a warning and returns nil.
Arguments:
  VAL (any): The value to validate.
  TARGET-SYMBOL (symbol): The target symbol for context in warnings.
  KEY (keyword): The property key being validated (e.g., :values).
  EXPECTED-LENGTH (integer): The required length of the list.
Return:
  (list|nil): VAL if valid, else nil."
  (unless (and (listp val) (= (length val) expected-length))
    (debrief--log :warn target-symbol
                  "Invalid %S for %S: Expected list of %d elements, got %S. Removing key."
                  key target-symbol expected-length val)
    (setq val nil))
  val)

(defun debrief--sanitize-entry-plist (raw-plist)
  "Sanitize and validate a raw debug entry RAW-PLIST.
Converts string targets to symbols, validates property types, and ensures
essential keys are present or defaulted.
Arguments:
  RAW-PLIST (plist): The raw property list from `debrief-debug-vars` or user input.
Return:
  (plist|nil): A new, sanitized plist, or nil if the :target is invalid."
  (unless (plistp raw-plist)
    (debrief--log :error nil "Invalid raw plist: %S. Aborting sanitization." raw-plist)
    (cl-return-from debrief--sanitize-entry-plist nil))

  (let* ((target-val (plist-get raw-plist :target))
         (target-symbol nil) ; This will hold the validated symbol for :target
         (sanitized-plist '()))

    ;; Validate and process :target first
    (cond
     ((symbolp target-val)
      (setq target-symbol target-val))
     ((stringp target-val) ; Convert string target to symbol
      (setq target-symbol (intern target-val))
      (debrief--log :warn target-symbol ; Log with the symbol for consistency
                    "Sanitized :target string '%S' to symbol '%S'."
                    target-val target-symbol))
     (t
      (debrief--log :error nil "Invalid :target value: %S in plist %S. Dropping entry."
                    target-val raw-plist)
      (cl-return-from debrief--sanitize-entry-plist nil)))

    (setq sanitized-plist (plist-put sanitized-plist :target target-symbol))

    ;; Iterate over the rest of the raw plist to sanitize other keys
    (cl-loop for (key val) on raw-plist by #'cddr
             unless (eq key :target) ; Skip :target as it's already processed
             do
             (let ((validated-val :skip)) ; :skip means key might be removed if val invalid
               (pcase key
                 (:type
                  (setq validated-val
                        (debrief--validate-keyword-or-symbol-prop
                         val target-symbol key
                         '(:hook-monitor :function-advice :variable) ; Valid types
                         nil))) ; No default, type is inferred later if nil
                 (:enabled
                  (setq validated-val
                        (if (functionp val) val ; Store function as is
                          (debrief--validate-boolean-prop val target-symbol key))))
                 ((or :if :arg-filter :return-filter :fn) ; Function properties
                  (setq validated-val
                        (debrief--validate-function-prop val target-symbol key)))
                 (:where ; Advice position
                  (setq validated-val
                        (debrief--validate-keyword-or-symbol-prop
                         val target-symbol key
                         '(:around :before :after :override
                           :filter-args :filter-return
                           :before-while :after-while :around-while)
                         :around))) ; Default advice position
                 (:values ; For variable toggling
                  (setq validated-val
                        (debrief--validate-list-of-length-prop
                         val target-symbol key 2)))
                 ((or :timing :watch :break-on-change) ; Boolean properties
                  (setq validated-val
                        (debrief--validate-boolean-prop val target-symbol key)))
                 (:group ; Target group
                  (cond ((and (listp val) (= (length val) 2) (eq (car val) 'quote)
                              (symbolp (cadr val)))
                         (setq validated-val (cadr val))) ; Handle ('some-symbol)
                        ((symbolp val)
                         (setq validated-val val))      ; Handle some-symbol
                        (t
                         (debrief--log :warn target-symbol
                                       "Invalid :group value %S for %S. Expected symbol or ('symbol). Defaulting to 'default."
                                       val target-symbol)
                         (setq validated-val 'default))))
                 (:min-log-level
                  (setq validated-val
                        (debrief--validate-keyword-or-symbol-prop
                         val target-symbol key
                         ;; Ensure `debrief--log-level-map` is accessible if needed here
                         ;; or use a static list of valid log level keywords.
                         (mapcar #'car debrief--log-level-map) ; Get keys from map
                         nil))) ; No default, global threshold will be used
                 (_ ; Fallback for any other unhandled keys (e.g., :description)
                  (setq validated-val val))) ; Keep original value for these
               ;; Add to sanitized plist if validation didn't result in :skip
               ;; (or if it's a key we just copy, like :description)
               (unless (eq validated-val :skip)
                 (setq sanitized-plist (plist-put sanitized-plist key validated-val))))))
    sanitized-plist))

(defun debrief--determine-target-type (target-symbol config-entry)
  "Determine the type of the debug TARGET-SYMBOL based on its CONFIG-ENTRY.
Infers type if not explicitly set in CONFIG-ENTRY.
Arguments:
  TARGET-SYMBOL (symbol): The target symbol.
  CONFIG-ENTRY (plist): The sanitized configuration plist for the target.
Return:
  (keyword): One of :hook-monitor, :function-advice, or :variable."
  (let ((type-prop (plist-get config-entry :type)))
    (cond
      ;; Explicit type takes precedence
      ((eq type-prop :hook-monitor) :hook-monitor)
      ((eq type-prop :function-advice) :function-advice)
      ((eq type-prop :variable) :variable)
      ;; Infer type if not explicitly set
      ((or (plist-get config-entry :where) (plist-get config-entry :fn))
       :function-advice) ; Presence of advice-specific keys implies function
      ((or (plist-get config-entry :values) (plist-get config-entry :watch)
           (plist-get config-entry :break-on-change))
       :variable) ; Presence of variable-specific keys implies variable
      ;; Further inference based on symbol properties
      ((and (symbolp target-symbol) (boundp target-symbol)
            (not (fboundp target-symbol))) ; Bound but not a function -> variable
       :variable)
      ((fboundp target-symbol) :function-advice) ; Function-bound -> function advice
      (t :variable)))) ; Default to variable if unsure (e.g., unbound symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Custom Variables                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup debrief nil
  "Centralized debug configuration and tooling for Emacs."
  :group 'tools
  :prefix "debrief-")

(eval-when-compile
  (deftype debrief-debug-entry-type ()
    "Custom type specification for an entry in `debrief-debug-vars`.
This is a plist defining a single debug target."
    '(plist :tag "Debug Target Entry"
            :options-mutually-exclusive nil ; Allow combinations
            :key-type keyword
            :value-type any ; Individual keys have more specific types below
            :validation-function
            (lambda (plist) ; Basic validation: :target must be a symbol or string
              (let ((target (plist-get plist :target)))
                (or (symbolp target)
                    (and (stringp target) (not (string-empty-p target))))))
            ;; The :elements below are for documentation and `customize` interface.
            ;; Actual validation and sanitization is done by `debrief--sanitize-entry-plist`.
            :elements (
                       (target :tag "Target Symbol/Function"
                               :type (choice symbol string))
                       (enabled :tag "Enabled (t/nil or predicate function)"
                                :type (choice boolean function) :value-default t)
                       (if :tag "Conditional Predicate (0-arg function)"
                           :type (choice function (const :none)))
                       (description :tag "Description of the target" :type string)
                       (group :tag "Group for organization" :type symbol :value-default default)
                       (min-log-level :tag "Minimum Log Level for this target"
                                      :type (choice (const :trace) (const :debug)
                                                    (const :info)  (const :warn)
                                                    (const :error) (const :fatal)
                                                    (const :none)))
                       (type :tag "Target Type (inferred if omitted)"
                             :type (choice (const :hook-monitor)
                                           (const :function-advice)
                                           (const :variable)
                                           (const :inferred) ; Placeholder if not specified
                                           (const :none)))
                       ;; Variable-specific keys
                       (values :tag "Values for Enabled/Disabled states (list of 2)"
                               :type (list sexp sexp))
                       (watch :tag "Watch Variable Changes (log)" :type boolean)
                       (break-on-change :tag "Break on Variable Change (debugger)"
                                        :type boolean)
                       ;; Function/Advice-specific keys
                       (where :tag "Advice Position (e.g., :around, :before)"
                              :type (choice (const :around) (const :before)
                                            (const :after) (const :override)
                                            (const :filter-args)
                                            (const :filter-return)
                                            (const :before-while)
                                            (const :after-while)
                                            (const :around-while)
                                            (const :none)) :value-default :around)
                       (timing :tag "Measure Execution Timing (for :around advice)"
                               :type boolean :value-default t)
                       (fn :tag "Custom Advice Function/Lambda"
                           :type (choice function (const :none)))
                       (name :tag "Specific Advice Name (symbol or :auto)"
                             :type (choice symbol (const :auto)) :value-default :auto)
                       (arg-filter :tag "Argument Filter Function (for logging)"
                                   :type (choice function (const :none)))
                       (return-filter :tag "Return Value Filter Function (for logging)"
                                      :type (choice function (const :none)))
                       ))))

(defcustom debrief-debug-vars
  '(;; Core Emacs Debugging Variables
    (:target debug-on-error :enabled t
     :description "Enable debug on error globally (enters debugger on Lisp errors)."
     :group 'core)
    (:target debug-on-quit :enabled nil
     :description "Enable debug on quit (C-g enters debugger)."
     :group 'core)
    (:target debug-on-signal :enabled nil
     :description "Enter debugger on signals (e.g., from external processes)."
     :group 'core)
    (:target debug-on-next-call :enabled nil
     :description "Enter debugger on the next function call."
     :group 'core)
    (:target current-buffer :enabled nil :watch t :break-on-change nil
     :description "Watch 'current-buffer' for changes."
     :group 'core)
    (:target warning-minimum-level :enabled nil :values '(debug warn)
     :description "Set minimum warning level (e.g., 'debug to see all warnings)."
     :group 'core)

    ;; Startup & Initialization Hooks (Monitored)
    (:target emacs-startup-hook :type :hook-monitor :enabled t
     :description "Monitor Emacs startup hook execution time." :group 'init)
    (:target after-init-hook :type :hook-monitor :enabled t
     :description "Monitor after-init hook execution time." :group 'init)
    (:target initial-window-setup-hook :type :hook-monitor :enabled t
     :description "Monitor initial window setup hook execution time." :group 'init)
    (:target after-load-hook :type :hook-monitor :enabled nil
     :description "Monitor `after-load-hook` for package loading times." :group 'init)

    ;; Performance & Garbage Collection
    (:target gcmh-verbose :enabled t
     :description "Verbose GC management (if `gcmh` package is installed)."
     :group 'performance)
    (:target gc-cons-threshold :enabled nil :values (16777216 800000)
     :description "Adjust GC threshold (smaller for more frequent GC for debugging)."
     :group 'performance)
    (:target profiler-start :enabled nil :where :around :timing t
     :description "Wrap `profiler-start` to log its calls." :group 'performance)
    (:target profiler-report :enabled nil :where :around :timing t
     :description "Wrap `profiler-report` to log its calls." :group 'performance)
    (:target fill-column :enabled nil :watch t
     :description "Watch 'fill-column' for changes." :group 'performance)

    ;; UI & Minibuffer
    (:target vertico--exhibit :enabled t :where :around :timing t
     :if (lambda () (bound-and-true-p vertico-mode)) ; Only if vertico-mode is active
     :description "Wrap vertico--exhibit for debugging with timing (if vertico active)."
     :group 'ui)
    (:target eldoc-debug-p :enabled nil
     :description "Enable verbose ElDoc logging." :group 'ui)
    (:target minibuffer-setup-hook :type :hook-monitor :enabled nil
     :description "Monitor minibuffer setup hook execution time." :group 'ui)
    (:target post-command-hook :type :hook-monitor :enabled nil
     :description "Monitor `post-command-hook` (runs after every command)." :group 'ui)
    (:target pre-command-hook :type :hook-monitor :enabled nil
     :description "Monitor `pre-command-hook` (runs before every command)." :group 'ui)
    (:target command-history :enabled nil :watch t
     :description "Watch `command-history` to see commands as they're added." :group 'ui)

    ;; File & Buffer Operations
    (:target init-file-debug :enabled t
     :description "Init file debug mode (verbose messages during init file loading)."
     :group 'file)
    (:target before-save-hook :type :hook-monitor :enabled nil
     :description "Monitor before-save hook execution time." :group 'file)
    (:target after-save-hook :type :hook-monitor :enabled nil
     :description "Monitor after-save-hook execution time." :group 'file)
    (:target find-file-hook :type :hook-monitor :enabled t
     :description "Monitor find-file hook execution time." :group 'file)
    (:target buffer-list-update-hook :type :hook-monitor :enabled nil
     :description "Monitor hook for when the buffer list is updated." :group 'file)
    (:target kill-buffer-hook :type :hook-monitor :enabled nil
     :description "Monitor kill-buffer hook execution time." :group 'file)

    ;; Async & Processes
    (:target async-debug :enabled t
     :description "Enable async debugging (verbose output from `async` package)."
     :group 'async)
    (:target async-inject-variables :enabled t :where :around :timing nil
     :description "Wrap async-inject-vars (no timing) to see calls." :group 'async)
    (:target process-connection-type :enabled nil :watch t
     :description "Watch `process-connection-type`." :group 'async)

    ;; Network
    (:target url-debug :enabled t
     :description "Enable URL debugging (verbose output from `url` library)."
     :group 'network)
    (:target url-http-debug :enabled nil
     :description "Enable detailed HTTP request/response debugging." :group 'network)
    (:target network-stream-debug :enabled nil
     :description "Enable debugging for network streams." :group 'network)

    ;; Package Management
    (:target use-package-minimum-reported-time :enabled t :values (0.1 1)
     :description "Set `use-package` min reported time (0.1s when active)."
     :group 'package)
    (:target use-package-verbose :enabled t
     :description "Verbose `use-package` output." :group 'package)
    (:target load-path :enabled nil :watch t
     :description "Watch `load-path` for changes." :group 'package)
    (:target package-load-list :enabled nil :watch t
     :description "Watch `package-load-list` for changes." :group 'package)
    (:target byte-compile-warnings :enabled nil :values '(t nil)
     :description "Enable verbose byte-compiler warnings when active." :group 'package)

    ;; Major/Minor Mode Changes
    (:target after-change-major-mode-hook :type :hook-monitor :enabled nil
     :description "Monitor major mode change hook execution time." :group 'mode)
    (:target auto-mode-alist :enabled nil :watch t
     :description "Watch `auto-mode-alist` for changes." :group 'mode))
  "List of debug variables and function advice configurations managed by Debrief.
Each entry is a plist that defines a debug target. See the documentation for
`debrief-debug-entry-type` (via `C-h v debrief-debug-entry-type`) or the
Debrief manual for detailed explanation of all supported keys.

Key properties include:
  - `:target` (symbol): The variable, function, or hook to manage. (Required)
  - `:enabled` (boolean or function): Default activation state for this entry.
    If a function, it's called to determine state. Defaults to `t`.
  - `:if` (function): Predicate; target active only if this returns non-nil.
  - `:description` (string): Human-readable description.
  - `:group` (symbol): Organizational group (e.g., 'core, 'ui).
  - `:min-log-level` (keyword): Target-specific log level (e.g., :trace, :debug).
  - `:type` (keyword): Explicitly `:hook-monitor`, `:function-advice`, or `:variable`.
    Often inferred if omitted.

For variable targets:
  - `:values (ENABLED-VAL DISABLED-VAL)`: Values to set when active/inactive.
  - `:watch t`: Log changes to this variable.
  - `:break-on-change t`: Enter debugger when this variable changes.

For function/advice targets:
  - `:where` (keyword): Advice type (e.g., :around, :before). Default :around.
  - `:timing t`: Measure and log execution time (for :around). Default t.
  - `:fn` (function): Custom advice lambda.
  - `:arg-filter` (function): Filter arguments for logging.
  - `:return-filter` (function): Filter return value for logging."
  :type '(repeat debrief-debug-entry-type) ; Uses the custom type for validation
  :group 'debrief)

(defcustom debrief-debug-enabled nil
  "Global toggle for enabling or disabling all Debrief debugging features.
When set, Debrief will attempt to apply the configurations defined in
`debrief-debug-vars` or registered dynamically. When unset, Debrief will
attempt to revert these changes and disable its debugging mechanisms."
  :type 'boolean
  :group 'debrief
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Ensure debrief-update-debug-vars is called after this var is set.
         ;; This is crucial for initialization and state changes.
         (when (fboundp 'debrief-update-debug-vars)
           (debrief-update-debug-vars value))))

(defcustom debrief-error-handler #'debrief--default-error-handler
  "Handler function called when an error occurs within Debrief-advised code.
The function should accept two arguments: the TARGET-FN (symbol) where the
error occurred, and ERR (the error condition signaled)."
  :type 'function
  :group 'debrief)

(defvar debrief-persist-file (locate-user-emacs-file "debrief-config.el")
  "File path used by `debrief-persist.el` to save and load Debrief's
active debug configuration states across Emacs sessions.")

;; Logging-related defcustoms moved to `debrief-log.el`:
;; - debrief-log-destination
;; - debrief-log-file-path
;; - debrief-log-dedicated-buffer-name
;; - debrief-log-level-threshold

(defcustom debrief-hook-monitor-enabled t
  "Global toggle for enabling or disabling Emacs hook monitoring features.
If nil, no hooks will be monitored even if they are configured as
`:hook-monitor` type targets and are individually enabled. This allows a
quick way to turn off all hook monitoring overhead."
  :type 'boolean
  :group 'debrief
  :set (lambda (symbol value) ; Custom setter to update global advice
         (set-default symbol value)
         (debrief--ensure-global-hook-advice value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Core Logic & Initialization                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debrief--ensure-global-hook-advice (activate-p)
  "Activate or deactivate the global advice on hook-running functions.
This function manages the advice placed on functions listed in
`debrief--hook-running-functions`. The advice is only active if
`debrief-hook-monitor-enabled` is true AND there are hooks in
`debrief--active-monitored-hooks`.
Arguments:
  ACTIVATE-P (boolean): If non-nil, attempt to activate the advice if conditions
                        are met. If nil, attempt to deactivate it."
  (if activate-p
      ;; Attempt to activate
      (when (and debrief-hook-monitor-enabled          ; Global toggle must be on
                 (not debrief--global-hook-monitor-advice-active-p) ; Not already active
                 debrief--active-monitored-hooks)      ; At least one hook to monitor
        (--each debrief--hook-running-functions
          (lambda (fn)
            (unless (advice-member-p 'debrief--global-hook-advice fn)
              (advice-add fn :around #'debrief--global-hook-wrapper
                          ;; Provide a name for the advice piece
                          '(name . debrief--global-hook-advice)))))
        (setq debrief--global-hook-monitor-advice-active-p t)
        (debrief--log :info nil "Global hook monitor advice activated."))
    ;; Attempt to deactivate
    (when (and debrief--global-hook-monitor-advice-active-p ; Currently active
               (or (not debrief-hook-monitor-enabled)    ; Global toggle off OR
                   (null debrief--active-monitored-hooks))) ; No hooks to monitor
      (--each debrief--hook-running-functions
        (lambda (fn) (advice-remove fn 'debrief--global-hook-advice)))
      (setq debrief--global-hook-monitor-advice-active-p nil)
      (debrief--log :info nil "Global hook monitor advice deactivated."))))

(defun debrief--is-entry-active-p (config-entry)
  "Return non-nil if the debug entry CONFIG-ENTRY should be considered active.
This checks `debrief--in-advised-call` to prevent re-entrancy.
Then, it evaluates:
1. Global `debrief-debug-enabled`.
2. The entry's own `:enabled` property (which can be a boolean or a function).
3. The entry's `:if` predicate function (if provided).
All conditions must be true for the entry to be considered active.
Arguments:
  CONFIG-ENTRY (plist): The sanitized configuration plist for a debug target.
Return:
  (boolean): t if the entry is active, nil otherwise."
  (if debrief--in-advised-call
      (progn
        (when config-entry ; Log only if there's a config to reference
          (debrief--log :trace (plist-get config-entry :target)
                        "Re-entrant call to debrief--is-entry-active-p, returning nil."))
        nil) ; Short-circuit to prevent re-entrancy
    (unless config-entry
      (debrief--log :warn nil "Called debrief--is-entry-active-p with nil config.")
      (cl-return-from debrief--is-entry-active-p nil))

    (let* ((target (plist-get config-entry :target))
           (entry-enabled-prop (plist-get config-entry :enabled))
           ;; Evaluate :enabled if it's a function, otherwise use its boolean value
           (entry-enabled-val
            (if (functionp entry-enabled-prop)
                ;; Protect this specific function call from re-entrancy
                (let ((debrief--in-advised-call t)) (funcall entry-enabled-prop))
              entry-enabled-prop))
           (if-predicate (plist-get config-entry :if))
           ;; Evaluate :if predicate if it's a function, default to t if not
           (predicate-eval
            (if (functionp if-predicate)
                ;; Protect this specific function call from re-entrancy
                (let ((debrief--in-advised-call t)) (funcall if-predicate))
              t)) ; If no :if predicate, it doesn't block activation
           (is-active (and debrief-debug-enabled ; Global switch
                             entry-enabled-val   ; Entry's own :enabled state
                             predicate-eval)))   ; Entry's :if condition
      (debrief--log :trace target
                    "Active check for %s: global=%S, entry_enabled_val=%S (prop: %S), if_pred_val=%S -> active=%S"
                    target debrief-debug-enabled entry-enabled-val
                    entry-enabled-prop predicate-eval
                    is-active)
      is-active)))

(defun debrief--handle-variable-target (target-symbol config-entry is-active)
  "Activate or deactivate debugging for variable TARGET-SYMBOL based on IS-ACTIVE.
Handles setting values based on `:values`, storing/restoring original values,
and adding/removing variable watchers and `debug-on-variable-change`.
Arguments:
  TARGET-SYMBOL (symbol): The variable to manage.
  CONFIG-ENTRY (plist): The sanitized configuration for TARGET-SYMBOL.
  IS-ACTIVE (boolean): Whether the debugging for this variable should be active."
  (cl-block debrief--handle-variable-target
    (unless (boundp target-symbol)
      (debrief--log :warn target-symbol "Variable %s is not bound. Cannot manage."
                    target-symbol)
      (cl-return-from debrief--handle-variable-target nil))

    (let* ((values-prop (plist-get config-entry :values))
           (watch-prop (plist-get config-entry :watch))
           (break-prop (plist-get config-entry :break-on-change))
           (current-val (symbol-value target-symbol)))
      (if is-active
          ;; Activation logic
          (progn
            ;; Store original value if not already stored by Debrief
            (unless (ht-get debrief--original-values target-symbol)
              (ht-set! debrief--original-values target-symbol (cons current-val t))
              (debrief--log :debug target-symbol "Stored original value for %s: %S"
                            target-symbol current-val))
            ;; Set variable value based on :values or boolean nature
            (cond (values-prop
                   (let ((val-to-set (nth 0 values-prop)))
                     (set target-symbol (if (functionp val-to-set) (funcall val-to-set)
                                          val-to-set))
                     (debrief--log :info target-symbol "Set %s to active value: %S"
                                   target-symbol (symbol-value target-symbol))))
                  ((booleanp current-val) ; If it's a boolean and no :values
                   (set target-symbol t)
                   ;; Special case for debug-on-next-call to avoid immediate trigger
                   (if (eq target-symbol 'debug-on-next-call)
                       (let ((debug-on-next-call nil)) ; Temporarily nil to log
                         (debrief--log :info target-symbol
                                       "Set boolean %s to active state: t" target-symbol))
                     (debrief--log :info target-symbol
                                   "Set boolean %s to active state: t" target-symbol)))
                  (t (debrief--log :debug target-symbol
                                   "Variable %s (non-boolean) has no :values. Value not changed by Debrief."
                                   target-symbol)))
            ;; Add watcher if configured
            (when watch-prop
              (add-variable-watcher target-symbol #'debrief--variable-watcher-function)
              (debrief--log :info target-symbol "Added variable watcher for %s."
                            target-symbol))
            ;; Enable break-on-change if configured
            (when break-prop
              (debug-on-variable-change target-symbol)
              (debrief--log :info target-symbol "Enabled break-on-change for %s."
                            target-symbol)))
        ;; Deactivation logic
        (progn
          ;; Restore original value if stored by Debrief
          (let ((original-data (ht-get debrief--original-values target-symbol)))
            (if (and original-data (cdr original-data)) ; Check if Debrief stored it
                (progn
                  (set target-symbol (car original-data))
                  (ht-remove! debrief--original-values target-symbol)
                  (debrief--log :info target-symbol "Restored %s to original value: %S"
                                target-symbol (symbol-value target-symbol)))
              ;; No original value stored by Debrief, apply deactivation logic
              (cond (values-prop
                     (let ((val-to-set (nth 1 values-prop)))
                       (set target-symbol (if (functionp val-to-set) (funcall val-to-set)
                                            val-to-set))
                       (debrief--log :info target-symbol
                                     "Set %s to inactive value (no original stored): %S"
                                     target-symbol (symbol-value target-symbol))))
                    ((booleanp (symbol-value current-val)) ; Check current type
                     (set target-symbol nil)
                     (debrief--log :info target-symbol
                                   "Set boolean %s to inactive state (no original stored): nil"
                                   target-symbol))
                    (t (debrief--log :debug target-symbol
                                     "Variable %s (non-boolean) has no :values. Value not changed (no original stored)."
                                     target-symbol)))))
          ;; Remove watcher/break regardless of value restoration
          (when watch-prop
            (remove-variable-watcher target-symbol #'debrief--variable-watcher-function)
            (debrief--log :info target-symbol "Removed variable watcher for %s."
                          target-symbol))
          (when break-prop
            (cancel-debug-on-variable-change target-symbol)
            (debrief--log :info target-symbol "Disabled break-on-change for %s."
                          target-symbol)))))
    ;; Special handling for `debug-on-quit` as it's a global C variable
    (when (eq target-symbol 'debug-on-quit)
      (let ((should-be-active (if is-active t nil)))
        (unless (eq debug-on-quit should-be-active) ; Only set if different
          (setq debug-on-quit should-be-active)
          (debrief--log :info target-symbol "`debug-on-quit` set to %s"
                        should-be-active))))))

(defun debrief--handle-function-advice-target (target-symbol config-entry
                                               _is-active-at-setup-arg)
  "Manage advice for function TARGET-SYMBOL based on current Debrief state and CONFIG-ENTRY.
Advice is physically added or removed based on the global `debrief-debug-enabled`
flag and the static `:enabled` property of the CONFIG-ENTRY.
The dynamic `:if` predicate and dynamic `:enabled` function (if any) are checked
at runtime by the advice wrapper itself via `debrief--is-entry-active-p`.
Arguments:
  TARGET-SYMBOL (symbol): The function to advise.
  CONFIG-ENTRY (plist): The sanitized configuration for TARGET-SYMBOL.
  _IS-ACTIVE-AT-SETUP-ARG (boolean): This argument reflects the active state at the
                                     time of setup/refresh, but for function advice,
                                     the physical adding/removing of advice is based
                                     on simpler static checks. The runtime check is
                                     done inside the advice wrapper. (Currently unused).
Return:
  nil."
  (cl-block debrief--handle-function-advice-target
    (unless (fboundp target-symbol)
      (debrief--log :warn target-symbol "Function %s is not fbound. Cannot advise."
                    target-symbol)
      (cl-return-from debrief--handle-function-advice-target nil))

    (let* ((adv-name-prop (plist-get config-entry :name))
           (adv-name (if (and adv-name-prop (symbolp adv-name-prop)
                              (not (eq adv-name-prop :auto)))
                         adv-name-prop ; Use user-specified name
                       (debrief--get-advice-name target-symbol))) ; Auto-generate
           (where (or (plist-get config-entry :where) :around))
           (custom-fn (plist-get config-entry :fn))
           (timing (plist-get config-entry :timing))
           (arg-filter (plist-get config-entry :arg-filter))
           (ret-filter (plist-get config-entry :return-filter))
           (adv-fn (if custom-fn custom-fn ; User-provided advice function
                     (debrief--generate-advice-function ; Or generate one
                      target-symbol where timing arg-filter ret-filter)))
           (entry-enabled-prop (plist-get config-entry :enabled))
           ;; Determine if advice should be *physically* on the function.
           ;; Runtime activation is handled by `debrief--is-entry-active-p`
           ;; inside the wrapper.
           (should-be-physically-adviced
            (and debrief-debug-enabled ; Global switch must be on
                 ;; Entry's static :enabled (if boolean) or assume true if :enabled is a function
                 (if (functionp entry-enabled-prop) t entry-enabled-prop))))

      (unless adv-fn
        (debrief--log :error target-symbol
                      "Could not generate or find advice function for %s (where: %S)."
                      target-symbol where)
        (cl-return-from debrief--handle-function-advice-target nil))

      (debrief--log :debug target-symbol
                    "Handling advice for %s. Name: '%s'. Should be physically advised: %S. (Global: %S, Entry :enabled: %S)"
                    target-symbol adv-name should-be-physically-adviced
                    debrief-debug-enabled entry-enabled-prop)

      (if should-be-physically-adviced
          ;; Attempt to add advice if not already present
          (unless (advice-member-p adv-name target-symbol)
            (debrief--log :info target-symbol "Adding advice '%s' to %s (%s)."
                          adv-name target-symbol where)
            (condition-case err
                (progn
                  (advice-add target-symbol where adv-fn `((name . ,adv-name)))
                  (ht-set! debrief--active-advice target-symbol
                           (cons adv-name (ht-get debrief--active-advice
                                                  target-symbol)))
                  (debrief--log :info target-symbol
                                "Added advice '%s' to %s. Runtime behavior depends on full active check."
                                adv-name target-symbol))
              (error (debrief--log :error target-symbol
                                   "Failed to add advice '%s' to %s: %S"
                                   adv-name target-symbol err))))
        ;; Attempt to remove advice if it's present but shouldn't be
        (when (advice-member-p adv-name target-symbol)
          (debrief--log :info target-symbol "Removing advice '%s' from %s."
                        adv-name target-symbol)
          (condition-case err
              (progn
                (advice-remove target-symbol adv-name)
                (ht-set! debrief--active-advice target-symbol
                         (remove adv-name (ht-get debrief--active-advice
                                                  target-symbol)))
                (debrief--log :info target-symbol "Removed advice '%s' from %s."
                              adv-name target-symbol))
            (error (debrief--log :error target-symbol
                                 "Failed to remove advice '%s' from %s: %S"
                                 adv-name target-symbol err))))))))

(defun debrief--handle-hook-monitor-target (target-symbol config-entry is-active)
  "Manage monitoring of a hook TARGET-SYMBOL based on IS-ACTIVE.
Adds or removes the hook from `debrief--active-monitored-hooks` and ensures
the global hook advice state is updated accordingly.
Arguments:
  TARGET-SYMBOL (symbol): The hook variable symbol.
  CONFIG-ENTRY (plist): The sanitized configuration for TARGET-SYMBOL.
  IS-ACTIVE (boolean): Whether monitoring for this hook should be active."
  (cl-block debrief--handle-hook-monitor-target
    (unless (boundp target-symbol)
      (debrief--log :warn target-symbol "Hook variable %S is not bound. Cannot monitor."
                    target-symbol)
      (cl-return-from debrief--handle-hook-monitor-target nil))

    (if is-active ; This `is-active` considers the :if predicate from setup time
        (progn
          (unless (memq target-symbol debrief--active-monitored-hooks)
            (push target-symbol debrief--active-monitored-hooks)
            (debrief--log :info target-symbol "Added hook %s to active monitors."
                          target-symbol))
          ;; Ensure global advice is active if there are monitored hooks and enabled globally
          (debrief--ensure-global-hook-advice t))
      (progn
        (when (memq target-symbol debrief--active-monitored-hooks)
          (setq debrief--active-monitored-hooks
                (cl-remove target-symbol debrief--active-monitored-hooks))
          (debrief--log :info target-symbol "Removed hook %s from active monitors."
                        target-symbol))
        ;; Re-evaluate global advice state (might deactivate if no hooks left)
        (debrief--ensure-global-hook-advice t)))))

(defun debrief-apply-entry-config (config-entry)
  "Apply or deactivate the debug configuration for a single CONFIG-ENTRY.
This function determines the target type and calls the appropriate handler.
The active state of the entry is determined by `debrief--is-entry-active-p`.
Arguments:
  CONFIG-ENTRY (plist): A sanitized configuration plist for one debug target."
  (cl-block debrief-apply-entry-config
    (unless config-entry
      (debrief--log :warn nil "Attempted to apply a nil config entry.")
      (cl-return-from debrief-apply-entry-config nil))

    (let* ((target (plist-get config-entry :target))
           ;; is-active-at-this-moment reflects the full check including dynamic :if
           (is-active-at-this-moment (debrief--is-entry-active-p config-entry))
           (type (debrief--determine-target-type target config-entry)))
      (unless target
        (debrief--log :error nil "Configuration entry missing :target key: %S"
                      config-entry)
        (cl-return-from debrief-apply-entry-config nil))

      (debrief--log :debug target
                    "Applying config for '%s' (type: %s, active check: %s)"
                    target type is-active-at-this-moment)
      (pcase type
        (:variable
         (debrief--handle-variable-target target config-entry is-active-at-this-moment))
        (:function-advice
         (debrief--handle-function-advice-target target config-entry
                                                 is-active-at-this-moment))
        (:hook-monitor
         (debrief--handle-hook-monitor-target target config-entry
                                              is-active-at-this-moment))
        (_ (debrief--log :warn target "Unknown target type '%s' for %s. Config: %S"
                         type target config-entry))))))

(cl-defun debrief--register-debug-target (target-symbol config-plist
                                         &optional (save-state-p t))
  "Core logic to register or update debug TARGET-SYMBOL with CONFIG-PLIST.
This function stores the configuration, associates it with a group if specified,
applies the configuration, and optionally saves the state.
Arguments:
  TARGET-SYMBOL (symbol): The symbol for the debug target.
  CONFIG-PLIST (plist): The sanitized configuration plist for the target.
  SAVE-STATE-P (boolean, optional): If non-nil (default), save the Debrief state
                                   after registering/updating.
Return:
  (symbol|nil): TARGET-SYMBOL on success, nil on failure (e.g., target mismatch)."
  (let* ((plist-target (plist-get config-plist :target))
         (group (plist-get config-plist :group))
         ;; Ensure :enabled key exists, defaulting to t if not specified.
         ;; This is a static default for the entry; runtime checks are separate.
         (final-config (if (plist-member config-plist :enabled)
                           (copy-sequence config-plist)
                         (plist-put (copy-sequence config-plist) :enabled t))))
    ;; Sanity check: target in plist should match the key symbol
    (unless (eq plist-target target-symbol)
      (debrief--log :error target-symbol
                    "Target mismatch in registration: key '%s' vs plist :target '%s'. Config: %S"
                    target-symbol plist-target config-plist)
      (cl-return-from debrief--register-debug-target nil))

    (ht-set! debrief--debug-config target-symbol final-config)
    (debrief--log :debug target-symbol "Registered/Updated '%s'. Config: %S"
                  target-symbol final-config)
    ;; Manage group association
    (when group
      (unless (symbolp group) ; Ensure group is a symbol
        (debrief--log :warn target-symbol
                      "Invalid :group value %S for %s. Using 'default'."
                      target-symbol group)
        (setq group 'default))
      (let ((targets-in-group (ht-get debrief--debug-groups group)))
        (unless (memq target-symbol targets-in-group)
          (ht-set! debrief--debug-groups group
                   (cons target-symbol targets-in-group)))))

    (debrief-apply-entry-config final-config) ; Apply the new/updated config

    (when save-state-p
      (if (fboundp 'debrief/save-state) (debrief/save-state)
        (debrief--log :warn target-symbol
                      "`debrief/save-state` function not found. State not persisted.")))
    target-symbol)) ; Return target symbol on success

;;;###autoload
(defmacro debrief-define-debug (target &rest plist)
  "Define and register a single debug TARGET with properties PLIST.
This macro sanitizes the provided PLIST and registers the target with Debrief.
It's a convenient way to declare debug targets in user configuration.
Arguments:
  TARGET (symbol): The symbol of the variable, function, or hook to debug.
  PLIST (plist): Property list specifying the debug configuration for TARGET.
                 See `debrief-debug-vars` for available keys.
Example:
  (debrief-define-debug my-function :where :around :timing t)
  (debrief-define-debug my-variable :watch t :group 'my-vars)"
  (declare (indent 1) (debug (symbolp &rest plist-form)))
  `(let* ((raw-config-data (list :target ',target ,@plist))
          (sanitized-config (debrief--sanitize-entry-plist raw-config-data)))
     (if sanitized-config
         (progn
           (debrief--register-debug-target (plist-get sanitized-config :target)
                                           sanitized-config)
           (debrief--log :info ',target "Defined debug target %S via macro." ',target)
           ',target) ; Return the target symbol
       (user-error "[Debrief] Invalid debug definition for %S. Raw plist: %S"
                   ',target raw-config-data))))

;;;###autoload
(defmacro debrief-define-debug-group (group-symbol &rest entries)
  "Define multiple debug targets, all associated with GROUP-SYMBOL.
Each entry in ENTRIES is a list of the form (TARGET-SYMBOL &rest PLIST),
where PLIST is the configuration for that TARGET-SYMBOL (without needing
to specify `:group` again).
Arguments:
  GROUP-SYMBOL (symbol): The symbol to use as the group name.
  ENTRIES (list): A list of debug target definitions. Each definition is
                  a list: (TARGET-SYMBOL . PLIST-FOR-TARGET).
Example:
  (debrief-define-debug-group 'my-project-debug
    (some-function :where :before)
    (another-variable :watch t :min-log-level :trace))"
  (declare (indent 1) (debug (symbolp &rest (&rest form))))
  `(progn
     ,@(mapcar (lambda (entry)
                 (unless (and (consp entry) (symbolp (car entry)))
                   (error "[Debrief] Invalid entry in debug group %S: %S"
                          group-symbol entry))
                 ;; Construct a call to debrief-define-debug for each entry,
                 ;; automatically adding the :group property.
                 `(debrief-define-debug ,(car entry) :group ',group-symbol
                                      ,@(cdr entry)))
               entries)
     (debrief--log :info nil "Defined debug group %s with %d entries."
                   ',group-symbol (length entries))
     ',group-symbol)) ; Return the group symbol

(defun debrief--initialize-targets-from-custom-vars ()
  "Initialize all debug targets from the `debrief-debug-vars` customizable list.
This function clears existing Debrief configurations and re-populates them
based on the current value of `debrief-debug-vars`. It's typically called
during Debrief's startup or when `debrief-debug-vars` is changed."
  (interactive) ; Allow manual re-initialization if needed
  ;; Clear existing internal state
  (ht-clear! debrief--debug-config)
  (ht-clear! debrief--debug-groups)
  (clrhash debrief--original-values) ; `clrhash` for hash-table.el compatibility
  (setq debrief--active-monitored-hooks nil)
  (debrief--ensure-global-hook-advice nil) ; Deactivate global hook advice first

  (debrief--log :info nil "Initializing all targets from `debrief-debug-vars`...")
  (--each debrief-debug-vars
    (lambda (raw-entry-plist)
      (if-let ((sanitized-entry (debrief--sanitize-entry-plist raw-entry-plist)))
          (let ((target (plist-get sanitized-entry :target)))
            ;; Ensure :enabled key exists, defaulting to t if not specified in var
            (unless (plist-member sanitized-entry :enabled)
              (setq sanitized-entry (plist-put sanitized-entry :enabled t)))
            ;; Register without saving state for each; save once at the end if needed
            (debrief--register-debug-target target sanitized-entry nil))
        (debrief--log :error nil "Invalid entry in `debrief-debug-vars` (skipped): %S."
                      raw-entry-plist))))
  (debrief--log :info nil "Targets initialized from `debrief-debug-vars`.")
  ;; After all targets are registered, ensure global hook advice state is correct
  (debrief--ensure-global-hook-advice t))

(defun debrief--symbol-to-keyword (sym)
  "Convert a SYMBOL to a keyword symbol.
Arguments:
  SYM (symbol): The symbol to convert.
Return:
  (keyword): The keyword symbol (e.g., 'foo -> :foo)."
  (intern (format ":%s" (symbol-name sym))))

;;;###autoload
(defun debrief-update-debug-vars (enabled)
  "Enable or disable debugging globally, refreshing all registered debug entries.
This function is typically called by the setter of `debrief-debug-enabled`.
It sets the global `debrief-debug-enabled` state, re-applies configurations
for all known targets, and persists the state.
Arguments:
  ENABLED (boolean): The new global enabled state for Debrief."
  (debrief--log :debug nil "debrief-update-debug-vars: initial global enabled: %S, new value: %S"
                debrief-debug-enabled enabled)
  (setq debrief-debug-enabled enabled) ; Set the global state
  (debrief--log :info nil "Global debugging has been %s."
                (if debrief-debug-enabled "ENABLED" "DISABLED"))

  ;; If this is the first time (e.g., config hash is empty), ensure
  ;; debrief-debug-vars are loaded into the config.
  (when (and (zerop (ht-size debrief--debug-config)) debrief-debug-vars)
    (debrief--log :info nil
                  "Config empty, initializing from `debrief-debug-vars` during update.")
    (--each debrief-debug-vars
      (lambda (raw-entry-plist)
        (if (plistp raw-entry-plist)
            (when-let ((sane (debrief--sanitize-entry-plist raw-entry-plist)))
              (when-let ((target (plist-get sane :target)))
                ;; Register without saving state for each; save once at the end
                (debrief--register-debug-target target sane nil)))
          (debrief--log :error nil
                        "Invalid non-plist entry in `debrief-debug-vars`: %S."
                        raw-entry-plist)))))

  (debrief--log :info nil "Refreshing all %d registered targets due to global state change."
                (ht-size debrief--debug-config))
  (debrief/refresh-all-targets) ; Re-apply config for all targets based on new state

  (when (fboundp 'debrief/save-state) (debrief/save-state)) ; Persist the new state
  (debrief--log :info nil "Debrief state saved. Debugging is now %S."
                (if debrief-debug-enabled "ENABLED" "DISABLED"))

  ;; Refresh UI if it's open
  (when (get-buffer debrief-list-registered-buffer-name)
    (if (fboundp 'debrief/list-registered-targets)
        (debrief/list-registered-targets)
      (debrief--log :warn nil "`debrief/list-registered-targets` not found for UI refresh."))))

;;;###autoload
(defun debrief/refresh-all-targets ()
  "Re-evaluate and reapply all registered debug target configurations.
This iterates through all targets in `debrief--debug-config` and calls
`debrief-apply-entry-config` for each one. It also ensures the global
hook advice state is correctly set."
  (interactive)
  (debrief--log :info nil "Refreshing all Debrief targets...")
  (ht-each (lambda (_target config-entry) ; _target is key, config-entry is value
             (debrief-apply-entry-config config-entry))
           debrief--debug-config)
  ;; After processing all individual hook monitors, ensure the global advice
  ;; for hook running functions is correctly activated or deactivated.
  (debrief--ensure-global-hook-advice t)
  (debrief--log :info nil "All Debrief targets refreshed."))

;; Initial processing of `debrief-debug-vars` and loading persisted state.
;; This ensures that configurations defined by the user are loaded when
;; `debrief-core.el` is loaded.
(debrief--initialize-targets-from-custom-vars)

;; Load persisted state after Emacs has fully started.
;; The priority 100 ensures it runs fairly late in the startup sequence.
(add-hook 'emacs-startup-hook #'debrief/load-state 100)

(provide 'debrief-core)
;;; debrief-core.el ends here