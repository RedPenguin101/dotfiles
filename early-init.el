;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 50 1024 1024)) ;; Set runtime GC to 50MB
(setopt inhibit-splash-screen t)
(setq read-process-output-max (* 1024 1024)) ;; 1MB buffer instead of 4KB
;; Silently log native compilation warnings without popping up the buffer window
(setq native-comp-async-report-warnings-errors 'silent)
;; Set the absolute minimum log display level to errors only (ignores stylistic warnings)
(setq warning-minimum-level :error)

;; Remove noise at startup
(setopt initial-scratch-message nil)

;; lets frames/windows resize smoothly at sub-character increments
(setq frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode) ; When in a GUI, disable tool bar;
  (tool-bar-mode -1))          ; all these tools are in the menu-bar anyway

;; start fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
