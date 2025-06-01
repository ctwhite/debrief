;;; debrief.el --- Dynamic Debug Variable and Function Registry -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This library provides a centralized system for managing dynamic debugging
;; configurations in Emacs. It allows users to declaratively define, activate,
;; and deactivate various debug targets, such as Emacs variables, functions
;; (via advice), and built-in hooks.
;;
;; This file serves as the main entry point for the Debrief library. It defines
;; global custom variables, sets up initial hooks, and loads all other
;; Debrief modules. The core logic, specific functionalities (like advice,
;; variable management, hook monitoring), user interface, commands, and
;; persistence are now split into separate files for better organization and
;; maintainability.
;;
;; Optional UI integrations like `debrief-consult.el` and `debrief-transient.el`
;; are loaded conditionally based on the availability of `consult.el` and
;; `transient.el` respectively.
;;
;; Features:
;; - **Declarative Configuration:** Define debug targets in `debrief-debug-vars`
;;   or dynamically using interactive commands.
;; - **Global & Per-Target Toggling:** Control debugging globally or for
;;   individual variables, functions, or groups of targets.
;; - **Function Advice:** Apply `:around` or `:before` advice to functions to
;;   log calls, arguments, return values, and execution time. Supports optional
;;   `:arg-filter` and `:return-filter` functions to reduce log verbosity.
;;   Automatically defers advice application for functions not yet loaded,
;;   applying it when the function becomes available via `eval-after-load`.
;; - **Variable Management:** Set variables to specific values based on the
;;   debug state.
;; - **Variable Watching:** Monitor variable changes, logging old and new values.
;; - **Break on Variable Change:** Automatically invoke the debugger when a
;;   watched variable's value changes.
;; - **Hook Monitoring:** Time and log the execution of specific Emacs built-in
;;   hooks for performance analysis.
;; - **Conditional Activation:** Use `:if` predicates to activate targets only
;;   when certain conditions are met (e.g., in a specific major mode).
;; - **Grouping:** Organize related debug targets into groups for batch operations.
;; - **Persistence:** Save and load the entire debug configuration state across
;;   Emacs sessions. Runtime-specific data (like lambdas for filters) are
;;   excluded from persistence.
;; - **Interactive Interface:** Comprehensive commands for registering,
;;   unregistering, toggling, and listing targets, including:
;;   - `tabulated-list-mode` buffer for an overview and quick actions (via `debrief-ui.el`).
;;   - `consult` integration for interactive filtering and actions (if `consult.el` is present).
;;   - `transient` menu for quick access to common commands (if `transient.el` is present).
;; - **Dedicated Log Buffer:** A specialized buffer (`*Debrief Log*`) for
;;   viewing, clearing, and filtering debug messages (via `debrief-log.el`).
;; - **Robust Input Validation:** Ensures that debug configurations are well-formed,
;;   preventing issues like byte-compiled function objects appearing where symbols
;;   are expected.
;;
;; Note on potential issues with byte-compiled functions in configuration:
;; If you observe byte-compiled function objects (e.g., `#[...]`) appearing
;; in log messages where symbols or keywords are expected (e.g., for target
;; names or types), it indicates a corruption in the configuration data.
;; This can sometimes happen if function objects are directly serialized to
;; a persistence file and then read back into contexts expecting symbols.
;; The `debrief/reset-all` command can help clear such corrupted states.
;; The input sanitization (`debrief--sanitize-entry-plist`) aims to prevent
;; these issues from occurring in the first place.

;;; Code:

(require 'debrief-core)
(require 'debrief-log)      ; Logging must be loaded early
(require 'debrief-advice)   ; Advice mechanisms
(require 'debrief-persist)  ; Persistence needs core and log
(require 'debrief-ui)       ; Basic UI for listing targets
(require 'debrief-commands) ; Commands depend on core, ui, log, persist, advice

;; Conditionally load UI enhancement modules
(when (featurep 'transient)
  (require 'debrief-transient))

(when (featurep 'consult)
  (require 'debrief-consult))

(provide 'debrief)
;;; debrief.el ends here