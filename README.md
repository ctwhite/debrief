# Debrief: Dynamic Debugging Toolkit for Emacs

**Debrief is an Emacs-based debugging toolkit designed to help you dynamically trace, inspect, and manage the behavior of functions, variables, and hooks for more effective troubleshooting.**

It provides a centralized system for managing dynamic debugging configurations, allowing users to declaratively define, activate, and deactivate various debug targets.

## Key Features

* **Declarative Configuration**: Define debug targets in the `debrief-debug-vars` customizable variable or dynamically using interactive commands.
* **Global & Per-Target Toggling**: Control debugging globally or for individual variables, functions, or groups of targets.
* **Function Advice**: Apply `:around` or `:before` advice to functions to log calls, arguments, return values, and execution time. Supports optional `:arg-filter` and `:return-filter` functions to reduce log verbosity.
* **Variable Management & Watching**: Set variables to specific values based on the debug state, monitor variable changes (logging old and new values), and optionally break into the debugger when a watched variable's value changes.
* **Hook Monitoring**: Time and log the execution of specific Emacs built-in hooks for performance analysis.
* **Conditional Activation**: Use `:if` predicates to activate targets only when certain conditions are met (e.g., in a specific major mode).
* **Grouping**: Organize related debug targets into groups for easier management.
* **Persistence**: Save and load the entire debug configuration state across Emacs sessions.
* **Interactive Interfaces**:
  * A list of registered targets in a dedicated buffer (`debrief/list-registered-targets`).
  * Optional integration with [Consult](https://github.com/minad/consult) for interactive filtering and actions (`debrief/consult-targets`).
  * Optional [Transient](https://github.com/magit/transient) menu (`debrief-menu`) for quick access to common commands.
* **Dedicated Log Buffer**: A specialized buffer (`*Debrief Log*`) for viewing, clearing, and filtering debug messages, with configurable log levels and destinations.
* **Input Validation**: Helps ensure debug configurations are well-formed.

## Modules

The Debrief toolkit is organized into several modules:

* `debrief.el`: Main entry point, loads other modules.
* `debrief-core.el`: Core logic for managing debug targets, configurations, and activation state.
* `debrief-log.el`: Handles all logging functionalities, including the dedicated log buffer and UI.
* `debrief-advice.el`: Manages the dynamic generation and application of advice to functions.
* `debrief-commands.el`: Provides user-facing interactive commands for managing Debrief.
* `debrief-persist.el`: Handles saving and loading of the Debrief configuration state.
* `debrief-ui.el`: Provides the basic UI for listing registered targets.
* `debrief-consult.el` (optional): Integrates Debrief with the Consult framework.
* `debrief-transient.el` (optional): Provides a Transient menu for Debrief commands.

## Installation

1. Clone the Debrief repository (or place all `debrief-*.el` files) into a directory in your Emacs `load-path`.
2. Ensure dependencies are installed (see below).
3. In your Emacs configuration (`init.el` or equivalent), add:

    ```emacs-lisp
    (require 'debrief)
    ```

## Basic Usage Example

You can define debug targets directly in your Emacs configuration using the `debrief-debug-vars` customizable variable or the `debrief-define-debug` and `debrief-define-debug-group` macros.

```emacs-lisp
;; In your init.el or a dedicated configuration file:

;; Example using the custom variable (after Debrief is loaded)
(setq debrief-debug-vars
      '(;; Target a specific function
        (:target my-buggy-function
         :enabled t                ; Start with this target active
         :where :around            ; Use :around advice
         :timing t                 ; Log execution time
         :description "Debug calls to my-buggy-function"
         :group 'my-project)

        ;; Target a variable
        (:target my-important-variable
         :watch t                  ; Log changes to this variable
         :break-on-change nil      ; Don't break into debugger on change (yet)
         :description "Monitor changes to my-important-variable"
         :group 'my-project)

        ;; Monitor a hook
        (:target 'text-mode-hook   ; Note the quote for hook symbols
         :type :hook-monitor
         :enabled t
         :description "Time the execution of text-mode-hook"
         :group 'hooks)))

;; Or using the macros (can be placed anywhere after (require 'debrief))
(debrief-define-debug magit-status
  :where :before
  :description "Log before magit-status is called.")

(debrief-define-debug-group 'ui-vars
  (cursor-type :watch t)
  (select-enable-clipboard :enabled nil :watch t))
```

## Key Commands

(Assuming `transient.el` and `consult.el` are installed for the best experience)

* `M-x debrief-menu`: Opens the main Transient menu for Debrief.
  * **t**: Toggle global debugging ON/OFF.
  * **l**: List all registered debug targets.
  * **c**: Consult Debrief targets (interactive search and actions).
  * **f**: Register a new function target.
  * **v**: Register a new variable target.
  * **s**: Save current Debrief state.
  * **o**: Load saved Debrief state.
  * **R**: Reset all Debrief configurations (with confirmation).
* `M-x debrief/consult-targets`: Interactively search and manage targets using Consult.
* `M-x debrief/list-registered-targets`: Show a buffer listing all targets and their status.
* `M-x debrief/toggle-target`: Interactively toggle a specific target's active state.
* `M-x debrief-log-filter-by-level`: Change the visible log level in the `*Debrief Log*` buffer.

## Dependencies

* Emacs (version compatible with `lexical-binding` and modern Elisp features)
* `cl-lib` (usually built-in)
* `dash.el`
* `ht.el` (for hash tables)
* `s.el` (string manipulation)
* `ts.el` (timestamps)
* `subr-x.el` (for `if-let`, `when-let`)

**Optional (for enhanced UI):**

* [Consult](https://github.com/minad/consult)
* [Marginalia](https://github.com/minad/marginalia) (highly recommended with Consult)
* [Transient](https://github.com/magit/transient)

## Contributing

Contributions, bug reports, and feature requests are welcome! Please open an issue or pull request on the GitHub repository.
