;;; debrief.el --- Dynamic Debug Variable and Function Registry -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This library provides a centralized system for managing dynamic debugging
;; configurations in Emacs. It allows users to declaratively define, activate,
;; and deactivate various debug targets, such as Emacs variables, functions
;; (via advice), and built-in hooks.
;;
;; This file serves as the main entry point for the Debrief library. It
;; requires all other Debrief modules.
;;
;; Initialization of targets from `debrief-debug-vars` and loading of
;; persisted state are both handled via `emacs-startup-hook` to ensure
;; they run after all modules are loaded and Emacs customization is processed.
;;
;; Optional UI integrations like `debrief-consult.el` and `debrief-transient.el`
;; are loaded conditionally based on the availability of `consult.el` and
;; `transient.el` respectively.

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