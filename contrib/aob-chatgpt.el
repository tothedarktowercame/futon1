;; -*- lexical-binding: t; -*-
;; This shim keeps futon1 in sync with the canonical aob-chatgpt integration
;; that now lives in futon3/contrib.  Loading this file simply delegates to the
;; shared implementation so we avoid code drift between futons.

(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name default-directory)))
       (shared-path (expand-file-name "../../futon3/contrib/aob-chatgpt.el" this-dir)))
  (unless (file-readable-p shared-path)
    (error "Cannot locate futon3 aob-chatgpt layer at %s" shared-path))
  (load-file shared-path))
