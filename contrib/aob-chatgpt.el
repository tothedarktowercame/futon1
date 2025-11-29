;; -*- lexical-binding: t; -*-

(defvar my-futon-prompt-directory "~/.emacs-graph/aob/"
  "Directory where complex system prompts are stored as text files.")

(defun my-futon-read-prompt (name)
  "Read a prompt file NAME from `my-futon-prompt-directory', or nil if missing.
NAME should be a bare name like \"par-shell\"; \".prompt\" is added automatically."
  (let* ((file (expand-file-name (concat name ".prompt")
                                 my-futon-prompt-directory)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

;;; ChatGPT profiles

(defvar-local my-chatgpt-shell-profile "General"
  "Name of the current futon prompt profile for this chatgpt-shell buffer.
Used to look up a system prompt in `~/.emacs-graph/aob/'.")

(defun my-chatgpt-shell-set-profile (profile)
  "Set `my-chatgpt-shell-profile' in the current buffer."
  (setq my-chatgpt-shell-profile profile
        my-chatgpt-shell--tatami-disabled nil)
  (force-mode-line-update))

(defun futon-set-general-profile ()
  "Restore the chat buffer to the General futon profile."
  (interactive)
  (my-chatgpt-shell-set-profile "General")
  (message "Futon profile set to General"))

(defun my-chatgpt-shell--insert-prompt (name &optional profile temporary)
  "Insert prompt NAME from `my-futon-read-prompt'.
When PROFILE is non-nil, switch the current buffer to that profile first.
If TEMPORARY is non-nil, skip Tatami ingestion for the next turn."
  (unless (derived-mode-p 'chatgpt-shell-mode)
    (user-error "Prompt insertion only works inside chatgpt-shell buffers"))
  (let ((body (my-futon-read-prompt name)))
    (unless body
      (user-error "Prompt %s.prompt was not found" name))
    (when profile
      (my-chatgpt-shell-set-profile profile))
    (when temporary
      (setq my-chatgpt-shell--tatami-disabled t))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert body)
    (unless (string-suffix-p "\n" body)
      (insert "\n"))
    (message "Inserted %s prompt" name)))


;;; Set up tatami — my custom integration layer between local futon stack and ChatGPT

(require 'url)
(load-file "~/code/futon1/contrib/tatami.el")
(require 'tatami)

(setq tatami-data-directory "/home/joe/code/futon1/data/")
(setq tatami-start-directory "/home/joe/code/futon1/")
(setq tatami-base-url "http://localhost:8080")
(defvar my-tatami--clojure (or (executable-find "clojure") "clojure"))
(unless (and my-tatami--clojure (file-executable-p my-tatami--clojure))
  (message "Warning: could not locate a runnable clojure executable; tatami auto-start may fail."))
(setq tatami-verbose nil)
(setq tatami-start-command (list my-tatami--clojure "-M:server"))
(setq tatami-startup-wait 20)

;;; Futon3 (MUSN) orchestration ------------------------------------------------

(defvar my-futon3-start-directory "/home/joe/code/futon3/"
  "Where to launch the Futon3 MUSN sandbox from.")

(defvar my-futon3-start-command (list my-tatami--clojure "-M:dev")
  "Command vector used to start Futon3.")

(defvar my-futon3-server-buffer "*Futon3*"
  "Buffer used to collect Futon3 stdout/stderr.")

(defvar my-futon3-process nil)
(defvar my-futon3-ui-base-url "http://localhost:6060")
(defvar my-futon3-last-status nil)
(defvar my-futon3-tatami-session-id nil)
(defvar my-futon3-tatami-default-prototypes '("f0/p0" "f3/p0"))
(defvar my-futon3-tatami-default-intent "chatgpt-shell block")
(defvar my-futon3-devmap-directory "/home/joe/code/futon3/holes")

(defun my-futon3-running-p ()
  (and my-futon3-process (process-live-p my-futon3-process)))

(defun my-futon3-start (&optional interactive)
  "Start Futon3 (MUSN sandbox) if needed."
  (interactive "p")
  (if (my-futon3-running-p)
      (when interactive (message "Futon3 already running."))
    (let ((default-directory my-futon3-start-directory))
      (setq my-futon3-process
            (apply #'start-process "futon3-server" my-futon3-server-buffer my-futon3-start-command))
      (set-process-query-on-exit-flag my-futon3-process nil)
      (set-process-sentinel my-futon3-process
                            (lambda (_proc event)
                              (when interactive
                                (message "Futon3 server event: %s" (string-trim event)))))
      (my-futon3-sync-selection)
      (when interactive
        (message "Starting Futon3 (ui=%s)" my-futon3-ui-base-url))))
  my-futon3-process)

(defun my-futon3-stop ()
  "Stop the Futon3 sandbox if it is running."
  (interactive)
  (when (my-futon3-running-p)
    (kill-process my-futon3-process))
  (setq my-futon3-process nil)
  (message "Stopped Futon3."))

(defun my-futon3-ensure-running ()
  (unless (my-futon3-running-p)
    (my-futon3-start)))

(defun my-futon3--request-json (path)
  (let ((url-request-method "GET")
        (url-request-extra-headers nil)
        (url (concat (string-remove-suffix "/" my-futon3-ui-base-url) path)))
    (condition-case err
        (let ((buffer (url-retrieve-synchronously url t t 1.5)))
          (unless buffer (error "No response"))
          (unwind-protect
              (with-current-buffer buffer
                (goto-char (point-min))
                (if (re-search-forward "\n\n" nil t)
                    (let ((body (buffer-substring-no-properties (point) (point-max))))
                      (json-parse-string body :object-type 'plist :array-type 'list))
                  (error "Malformed response")))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))
      (error
       (setq my-futon3-last-status (list :error (error-message-string err)))
       nil))))

(defun my-futon3-refresh-status ()
  "Fetch the latest tatami status from Futon3."
  (setq my-futon3-last-status (my-futon3--request-json "/musn/tatami/status")))

(defun my-futon3--tatami-url (path)
  (concat (string-remove-suffix "/" my-futon3-ui-base-url) path))

(defun my-futon3--tatami-request (method path payload)
  (my-futon3-ensure-running)
  (let* ((url-request-method method)
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (and payload
                                (encode-coding-string (json-encode payload) 'utf-8)))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (buffer (url-retrieve-synchronously (my-futon3--tatami-url path) t t 2)))
    (unless buffer
      (error "No response from Futon3"))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (let ((status (or url-http-response-status 0)))
            (search-forward "\n\n" nil 'move)
            (let ((body (buffer-substring-no-properties (point) (point-max))))
              (if (/= status 200)
                  (error "Futon3 %s failed (%s): %s" path status body)
                (when (and body (not (string-empty-p (string-trim body))))
                  (json-parse-string body :object-type 'plist :array-type 'list))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun my-futon3--encode-prototypes ()
  (mapcar (lambda (sym)
            (cond
             ((symbolp sym) (symbol-name sym))
             ((and (stringp sym)
                   (> (length sym) 0)
                   (char-equal (aref sym 0) ?:)) (substring sym 1))
             (t (format "%s" sym))))
          my-futon3-tatami-default-prototypes))

(defun my-futon3--read-devmap-prototypes ()
  (when (file-directory-p my-futon3-devmap-directory)
    (let (acc)
      (dolist (name (directory-files my-futon3-devmap-directory nil "^futon[0-9]+\\.devmap$"))
        (let* ((file (expand-file-name name my-futon3-devmap-directory))
               (futon-num (and (string-match "futon\\([0-9]+\\)" name)
                               (string-to-number (match-string 1 name)))))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let ((title (and (re-search-forward "^@title \\(.*\\)$" nil t)
                               (match-string 1))))
              (while (re-search-forward "^! instantiated-by: Prototype \\([0-9]+\\) — \\(.*\\)$" nil t)
                (let* ((proto-num (string-to-number (match-string 1)))
                       (label (match-string 2))
                       (proto-sym (format "f%s/p%s" futon-num proto-num))
                       (display (format "FUTON%s — Prototype %s — %s"
                                        futon-num proto-num
                                        (or label (or title (format "Futon %d" futon-num))))))
                  (push (list futon-num proto-num display proto-sym) acc)))))))
      (mapcar (lambda (entry)
                (cons (nth 2 entry) (nth 3 entry)))
              (sort acc (lambda (a b)
                          (if (= (car a) (car b))
                              (< (cadr a) (cadr b))
                            (< (car a) (car b)))))))))

(defun my-futon3--parse-prototype-string (text)
  (let ((parts (split-string (or text "") "[, ]" t)))
    (or (mapcar (lambda (tok)
                  (let* ((trim (string-trim tok))
                         (clean (if (and (> (length trim) 0)
                                         (char-equal (aref trim 0) ?:))
                                    (substring trim 1)
                                  trim)))
                    clean))
                parts)
        my-futon3-tatami-default-prototypes)))

(defun my-futon3--prototype-list (value)
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun my-futon3--prototype-string (proto)
  (cond
   ((null proto) nil)
   ((symbolp proto)
    (let ((name (symbol-name proto)))
      (if (and (> (length name) 0)
               (char-equal (aref name 0) ?:))
          (substring name 1)
        name)))
   ((stringp proto)
    (if (and (> (length proto) 0)
             (char-equal (aref proto 0) ?:))
        (substring proto 1)
      proto))
   (t (format "%s" proto))))

(defun my-futon3--display-prototypes (value)
  (let* ((raw (my-futon3--prototype-list value))
         (strings (delq nil (mapcar #'my-futon3--prototype-string raw))))
    (cond
     ((and strings
           (seq-some (lambda (s) (string-match-p "/" s)) strings)) strings)
     (my-futon3-tatami-default-prototypes my-futon3-tatami-default-prototypes)
     (t strings))))

(defun my-futon3-sync-selection ()
  (condition-case err
      (my-futon3--tatami-request
       "POST" "/musn/tatami/select"
       `(("prototypes" . ,(my-futon3--encode-prototypes))
         ("intent" . ,my-futon3-tatami-default-intent)))
    (error
     (message "Futon3 select failed: %s" (error-message-string err))
     nil)))

(defun my-futon3-ensure-tatami-session ()
  (unless my-futon3-tatami-session-id
    (let ((resp (my-futon3--tatami-request
                 "POST" "/musn/tatami/start"
                 `(("prototypes" . ,(my-futon3--encode-prototypes))
                   ("intent" . ,my-futon3-tatami-default-intent)))))
      (setq my-futon3-tatami-session-id (plist-get resp :session-id)))))

(defun my-futon3--truncate (text max-len)
  (let ((trimmed (string-trim (or text ""))))
    (if (<= (length trimmed) max-len)
        trimmed
      (concat (substring trimmed 0 max-len) "…"))))

(defun my-futon3-set-tatami-target (prototypes intent)
  "Interactive helper to change Futon3 tatami target defaults."
  (interactive
   (let* ((candidates (or (my-futon3--read-devmap-prototypes)
                          '(("FUTON0 — Prototype 0" . :f0/p0))))
          (names (mapcar #'car candidates))
          (table (lambda (string pred action)
                   (if (eq action 'metadata)
                       '(metadata
                         (display-sort-function . identity)
                         (cycle-sort-function . identity))
                     (complete-with-action action names string pred))))
          (display (let ((completion-ignore-case t))
                     (completing-read "Tatami target: " table nil t nil nil nil)))
          (proto (cdr (assoc display candidates)))
          (intent-input (read-string "Tatami intent: " my-futon3-tatami-default-intent)))
     (list (cond
            ((null proto) display)
            ((symbolp proto) (symbol-name proto))
            ((stringp proto) proto)
            (t (format "%s" proto)))
           intent-input)))
  (setq my-futon3-tatami-default-prototypes (my-futon3--parse-prototype-string prototypes)
        my-futon3-tatami-default-intent intent)
  (my-futon3-sync-selection)
  (my-chatgpt-shell--refresh-context-all)
  (message "Set Futon3 tatami target to %s (intent %s)"
           my-futon3-tatami-default-prototypes my-futon3-tatami-default-intent))

(defun my-futon3-log-chatgpt-turn (text)
  (when (and text (my-futon3-running-p))
    (let ((attempt 0)
          (done nil))
      (while (and (< attempt 2) (not done))
        (setq attempt (1+ attempt))
        (condition-case err
            (progn
              (my-futon3-ensure-tatami-session)
              (my-futon3--tatami-request
               "POST" "/musn/tatami/log"
               `(("session-id" . ,my-futon3-tatami-session-id)
                 ("activity" . "agent-work")
                 ("performed?" . t)
                 ("felt-state" . "ok")
                 ("notes" . ,(my-futon3--truncate text 800))))
              (setq done t))
          (error
           (let ((msg (error-message-string err)))
             (if (and (= attempt 1)
                      (string-match-p "unknown-session" msg))
                 (progn
                   (setq my-futon3-tatami-session-id nil)
                   (message "Futon3 tatami session expired; retrying."))
               (message "Futon3 tatami log failed: %s" msg)
               (setq attempt 2)))))))))

(defun my-futon3-close-tatami-session (&optional summary)
  (when my-futon3-tatami-session-id
    (ignore-errors
      (my-futon3--tatami-request
       "POST" "/musn/tatami/close"
       `(("session-id" . ,my-futon3-tatami-session-id)
         ("summary" . ,(or summary "chatgpt-shell session")))))
    (setq my-futon3-tatami-session-id nil)))

(defun my-futon3--format-counts (data)
  (let ((fruit-map
         '(("indicator" . "🍌")
             ("obligation" . "🍐")
             ("joy" . "🍒")
             ("insight" . "🍓")
             ("sleep" . "💤")
             ("stretch" . "🍊")
             ("baseline" . "🍈")
             ("rocket" . "🚀")
             ("bell" . "🔔")
             ("ghost" . "👻"))))
    (cond
     ((and (listp data) (keywordp (car data)))
      (let (parts p)
        (setq p data)
        (while (and p (cdr p))
          (let ((key (pop p))
                (val (pop p)))
            (let* ((name (if (keywordp key) (substring (symbol-name key) 1) key))
                   (icon (or (cdr (assoc name fruit-map)) name)))
              (push (format "%s=%s" icon val) parts))))
        (string-join (nreverse parts) ", ")))
     ((and (listp data) (consp (car data)))
      (let (parts)
        (dolist (pair data)
          (let ((key (car pair))
                (val (cdr pair)))
            (let* ((name (if (keywordp key) (substring (symbol-name key) 1) key))
                   (icon (or (cdr (assoc name fruit-map)) name)))
              (push (format "%s=%s" icon val) parts))))
        (string-join (nreverse parts) ", "))))))

(defun my-futon3-open-dashboard ()
  "Open the Futon3 sessions dashboard in a browser."
  (interactive)
  (browse-url (concat (string-remove-suffix "/" my-futon3-ui-base-url) "/musn/sessions")))

;;; Set up chatgpt-shell

(require 'chatgpt-shell)
(require 'subr-x)

(setq chatgpt-shell-model-version "gpt-5")
(setq chatgpt-shell-streaming t)

(unless (getenv "OPENAI_API_KEY")
  (setenv "OPENAI_API_KEY"
          (string-trim
           (with-temp-buffer
             (insert-file-contents "~/.openai-key")
             (buffer-string)))))

(unless (getenv "GEMINI_API_KEY")
  (setenv "GEMINI_API_KEY"
          (string-trim
           (with-temp-buffer
             (insert-file-contents "~/.gemini-key")
             (buffer-string)))))

(setq chatgpt-shell-openai-key (lambda () (getenv "OPENAI_API_KEY")))

(defvar-local my-chatgpt-shell--seeded-context nil
  "Non-nil after this chatgpt-shell buffer has sent the initial summary.")
(defvar-local my-chatgpt-shell-last-summary nil
  "Latest Tatami :me summary captured for this buffer.")
(defvar-local my-chatgpt-shell-last-focus nil
  "Latest Tatami focus header snippet captured for this buffer.")
(defvar-local my-chatgpt-shell--tatami-disabled nil
  "When non-nil, skip Tatami ingestion for the next message only.")
(defconst my-chatgpt-shell-context-buffer-name "*Tatami Context*")
(defconst my-chatgpt-shell-tatami-in-marker "FROM-TATAMI-EDN")
(defconst my-chatgpt-shell-tatami-out-marker "FROM-CHATGPT-EDN")
(defvar-local my-chatgpt-shell-last-inbound-edn nil
  "Most recent FROM-TATAMI-EDN payload prepared for ChatGPT.")
(defvar-local my-chatgpt-shell-last-edn nil
  "Most recent FROM-CHATGPT-EDN payload captured from ChatGPT.")
(defvar-local my-chatgpt-shell-last-validation-warning nil
  "Latest warning emitted when FROM-CHATGPT-EDN misses required pattern events.")
(defvar my-chatgpt-shell-debug nil
  "When non-nil, emit debug messages for Tatami prompts/after-hook.")

(defun my-chatgpt-shell--init-context ()
  (setq my-chatgpt-shell--seeded-context nil))

(add-hook 'chatgpt-shell-mode-hook #'my-chatgpt-shell--init-context)

(defun my-chatgpt-shell--close-futon3-on-kill ()
  (my-futon3-close-tatami-session
   (format "chatgpt-shell %s closed" (buffer-name))))

(add-hook 'chatgpt-shell-mode-hook
          (lambda ()
            (add-hook 'kill-buffer-hook #'my-chatgpt-shell--close-futon3-on-kill nil t)))

;;; Further configuration of Tatami for interactive use

(require 'cl-lib)
(require 'seq)

(defun my-chatgpt-shell--format-focus (fh)
  (let* ((ts (alist-get 'generated_at fh))
         (current (alist-get 'current fh))
         (history (alist-get 'history fh))
         (intent  (alist-get 'intent fh)))
    (when (and (listp current) current)
      (let* ((usable (seq-remove (lambda (entry)
                                   (let ((label (alist-get 'label entry)))
                                     (or (null label)
                                         (string= label "I"))))
                                 current))
             (primary (or (car usable) (car history)))
             (current-labels (delq nil (mapcar (lambda (entry)
                                                 (alist-get 'label entry))
                                               (or usable current))))
             (intent-type (when (listp intent)
                            (alist-get 'type intent)))
             (primary-score (and (listp primary)
                                 (alist-get 'score primary)))
             (parts (delq nil
                          (list (when ts
                                  (format "Generated at %s"
                                          (format-time-string "%Y-%m-%d %H:%M:%S (%Z)"
                                                              (seconds-to-time (/ ts 1000.0)))))
                                (when current-labels
                                  (format "Current focus: %s"
                                          (string-join current-labels ", ")))
                                (when intent-type
                                  (format "Intent: %s" intent-type))
                                (when primary
                                  (format "Top anchor: %s%s"
                                          (alist-get 'label primary)
                                          (if (numberp primary-score)
                                              (format " (score %.2f)" (float primary-score))
                                            "")))))))
        (when parts
          (setq my-chatgpt-shell-last-focus (string-join parts " · "))
          (my-chatgpt-shell--maybe-render-context)
          my-chatgpt-shell-last-focus)))))

(defun my-chatgpt-shell--focus-snippet ()
  (let* ((resp    (tatami-focus-header))
         (fh      (alist-get 'focus_header resp)))
    (my-chatgpt-shell--format-focus fh)))

;;;; --- Tatami availability + safe call --------------------------------------

(defgroup tatami-integration nil
  "Resilient helpers for the Tatami headless API."
  :group 'external)

(defcustom tatami-status-url nil
  "Optional health/ready URL for the headless API (e.g., http://127.0.0.1:5100/healthz).
 set, we’ll do a quick HTTP 200 check. If nil, we fall back to checking
r a live process in the *headless-api-server* buffer."
  :type '(choice (const :tag "No HTTP probe (buffer/process fallback)" nil)
		 (string :tag "Health URL")))

(defun tatami--http-200-p (url &optional timeout silent)
  "Return non-nil iff URL responds with HTTP 200 within TIMEOUT seconds."
  (when (and url (stringp url) (not (string-empty-p url)))
    (let ((url-request-method "GET")
          (url-show-status nil)
          (url-automatic-caching nil))
      (condition-case err
          (with-timeout ((or timeout 0.6) nil)
	    (when-let* ((buf (url-retrieve-synchronously url t (or timeout 0.6))))
              (unwind-protect
                  (with-current-buffer buf
		      (goto-char (point-min))
		      (when (re-search-forward "^HTTP/[^ ]+ \\([0-9][0-9][0-9]\\)" nil t)
                        (string= (match-string 1) "200")))
                (kill-buffer buf))))
        (error
         (unless silent
           (setq tatami--last-error (error-message-string err)))
         nil)))))

(defun tatami-available-p ()
  "Return non-nil if the headless API looks reachable."
  (or (tatami--server-running-p)
      (let ((buf (get-buffer "*headless-api-server*")))
	(when buf
          (when-let* ((proc (get-buffer-process buf)))
            (process-live-p proc))))))

(defun my-tatami--server-running-p (&rest _ignored)
  "Stricter probe that only treats HTTP 200 responses as healthy."
  (let* ((base (and tatami-base-url
                    (replace-regexp-in-string "/\\'" "" tatami-base-url)))
         (health-url
          (cond ((and (stringp tatami-status-url)
                      (not (string-empty-p tatami-status-url)))
                 tatami-status-url)
                ((and (stringp tatami-health-path)
                      (not (string-empty-p tatami-health-path))
                      base)
                 (concat base tatami-health-path))))
         (fallback (when base (concat base "/")))
         (candidates (delq nil (list health-url fallback))))
    (or (seq-some (lambda (url)
                    (when (tatami--http-200-p url 0.8 t)
                      (setq tatami--last-error nil)
                      t))
                  candidates)
        (progn
          (setq tatami--last-error
                (format "No HTTP 200 response from %s"
                        (if candidates
                            (string-join candidates ", ")
                          "the configured Tatami endpoints")))
          nil))))

(advice-add 'tatami--server-running-p :override #'my-tatami--server-running-p)

(defun tatami-me-summary-safe ()
  "Call `tatami-me-summary' only if available; never signal."
  (when (tatami-available-p)
    (condition-case _e
	(with-timeout (0.8 nil)       ;; belt-and-braces: don’t hang if it wedges
          (tatami-me-summary))
      (error nil))))

;;; --- Tatami EDN plumbing ----------------------------------------------------

(defun my-chatgpt-shell--debug (fmt &rest args)
  (when my-chatgpt-shell-debug
    (apply #'message (concat "[tatami] " fmt) args)))

(defun my-chatgpt-shell--encoded-prototypes ()
  (or (my-futon3--encode-prototypes) '()))

(defun my-chatgpt-shell--prototype-keywords ()
  (mapcar (lambda (proto)
            (intern (concat ":" proto)))
          (my-chatgpt-shell--encoded-prototypes)))

(defun my-chatgpt-shell--current-futons ()
  (let (acc)
    (dolist (proto (my-chatgpt-shell--encoded-prototypes))
      (when (string-match "^\\([^/]+\\)/" proto)
        (let ((futon (intern (concat ":" (match-string 1 proto)))))
          (cl-pushnew futon acc :test #'eq))))
    (nreverse acc)))

(defun my-chatgpt-shell--now-iso ()
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))

(defun my-chatgpt-shell--vector (items)
  (apply #'vector items))

(defun my-chatgpt-shell--encode-sigils (pairs)
  (my-chatgpt-shell--vector
   (mapcar (lambda (pair)
             (list (cons "emoji" (plist-get pair :emoji))
                   (cons "hanzi" (plist-get pair :hanzi))))
           pairs)))

(defun my-chatgpt-shell--plist-p (value)
  (and (listp value)
       (cl-evenp (length value))))

(defun my-chatgpt-shell--edn-encode (value)
  (cond
   ((null value) "nil")
   ((eq value t) "true")
   ((eq value :false) "false")
   ((stringp value) (prin1-to-string value))
   ((keywordp value)
    (let* ((name (symbol-name value))
           (trim (if (and (> (length name) 0)
                           (char-equal (aref name 0) ?:))
                      (substring name 1)
                    name)))
      (concat ":" trim)))
   ((symbolp value) (symbol-name value))
   ((integerp value) (number-to-string value))
   ((floatp value) (format "%S" value))
   ((vectorp value)
    (concat "[" (mapconcat #'my-chatgpt-shell--edn-encode (append value nil) " ") "]"))
   ((my-chatgpt-shell--plist-p value)
    (let (parts)
      (while value
        (let ((k (pop value))
              (v (pop value)))
          (push (concat (my-chatgpt-shell--edn-encode k)
                        " "
                        (my-chatgpt-shell--edn-encode v))
                parts)))
      (concat "{" (mapconcat #'identity (nreverse parts) " ") "}")))
   ((listp value)
    (concat "(" (mapconcat #'my-chatgpt-shell--edn-encode value " ") ")"))
   (t (prin1-to-string value))))

(defun my-chatgpt-shell--format-edn-block (marker payload)
  (format "---%s---\n%s\n---END-%s---"
          marker (my-chatgpt-shell--edn-encode payload) marker))

(defun my-chatgpt-shell--extract-edn-block (text marker)
  (when (and text marker)
    (let* ((start-marker (format "---%s---" marker))
           (end-marker (format "---END-%s---" marker))
           (start (string-match (regexp-quote start-marker) text)))
      (when start
        (let ((end (string-match (regexp-quote end-marker) text start)))
          (when end
            (substring text (+ start (length start-marker)) end)))))))

(defun my-chatgpt-shell--parse-edn-string (raw)
  (when raw
    (let* ((converted (replace-regexp-in-string
                       "}" ")"
                       (replace-regexp-in-string "{" "(" raw nil 'literal)
                       nil 'literal))
           (value (condition-case err
                      (car (read-from-string converted))
                    (error
                     (message "Failed to parse EDN: %s"
                              (error-message-string err))
                     nil))))
      (when (and value (listp value))
        value))))

(defun my-chatgpt-shell--extract-chatgpt-edn (text)
  (when-let* ((raw (my-chatgpt-shell--extract-edn-block
                    text my-chatgpt-shell-tatami-out-marker)))
    (my-chatgpt-shell--parse-edn-string raw)))

(defun my-chatgpt-shell--coerce-seq (value)
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun my-chatgpt-shell--events-seq (events)
  (my-chatgpt-shell--coerce-seq events))

(defun my-chatgpt-shell--valid-pattern-value-p (value)
  (and value
       (or (and (stringp value) (> (length value) 0))
           (keywordp value)
           (symbolp value))))

(defun my-chatgpt-shell--pattern-event-p (event)
  (when (listp event)
    (let ((pattern (plist-get event :pattern))
          (notes (plist-get event :notes)))
      (and (my-chatgpt-shell--valid-pattern-value-p pattern)
           (stringp notes)
           (> (length notes) 0)))))

(defun my-chatgpt-shell--warn-missing-pattern-event ()
  (let ((msg "FROM-CHATGPT-EDN missing required :pattern event; remind the model to log {:pattern ... :notes ...} or {:pattern :none ...}."))
    (unless (equal my-chatgpt-shell-last-validation-warning msg)
      (message "%s" msg))
    (setq my-chatgpt-shell-last-validation-warning msg)))

(defun my-chatgpt-shell--validate-pattern-events (edn)
  (let* ((events (my-chatgpt-shell--events-seq (plist-get edn :events)))
         (valid (cl-some #'my-chatgpt-shell--pattern-event-p events)))
    (if valid
        (setq my-chatgpt-shell-last-validation-warning nil)
      (my-chatgpt-shell--warn-missing-pattern-event))
    valid))

(defun my-chatgpt-shell--maybe-update-intent (edn)
  (let* ((incoming (plist-get edn :intent))
         (trimmed (and incoming (string-trim incoming))))
    (when (and trimmed
               (> (length trimmed) 0)
               (not (string= trimmed my-futon3-tatami-default-intent)))
      (setq my-futon3-tatami-default-intent trimmed)
      (when my-chatgpt-shell-last-inbound-edn
        (setq my-chatgpt-shell-last-inbound-edn
              (plist-put (copy-sequence my-chatgpt-shell-last-inbound-edn)
                         :intent trimmed)))
      (when my-chatgpt-shell-last-edn
        (setq my-chatgpt-shell-last-edn
              (plist-put (copy-sequence my-chatgpt-shell-last-edn)
                         :intent trimmed)))
      (my-futon3-sync-selection)

      (my-chatgpt-shell--render-context t))))

(defun my-chatgpt-shell--pattern->string (pattern)
  (cond
   ((null pattern) "")
   ((stringp pattern) pattern)
   ((keywordp pattern)
    (let ((name (symbol-name pattern)))
      (if (string-prefix-p ":" name)
          (substring name 1)
        name)))
   ((symbolp pattern) (symbol-name pattern))
   (t (format "%s" pattern))))

(defun my-chatgpt-shell--truncate (text limit)
  (if (and text (> (length text) limit))
      (concat (substring text 0 limit) "…")
    text))

(defun my-chatgpt-shell--take-last (items limit)
  (let ((len (length items)))
    (cond
     ((or (<= len limit) (<= limit 0)) items)
     (t (nthcdr (- len limit) items)))))

(defun my-chatgpt-shell--format-pattern-line (pattern)
  (let* ((id (or (plist-get pattern :id)
                 (plist-get pattern :title)
                 "(unknown)"))
         (title (plist-get pattern :title))
         (summary (string-trim (or (plist-get pattern :summary) "")))
         (score (plist-get pattern :score))
         (headline (string-trim
                    (or (and title
                             (not (string= title id))
                             (format "%s (%s)" title id))
                        title
                        id))))
    (concat "• " headline
            (if (> (length summary) 0)
                (format " – %s" (my-chatgpt-shell--truncate summary 140))
              "")
            (if (numberp score)
                (format " [d=%.2f]" score)
              ""))))

(defun my-chatgpt-shell--format-support-line (label entries emoji-key id-key)
  (let ((parts
         (cl-loop for entry in entries
                  for emoji = (plist-get entry emoji-key)
                  for ident = (or (plist-get entry id-key)
                                  (plist-get entry :title)
                                  (plist-get entry :name))
                  for token = (string-trim
                               (concat (or emoji "")
                                       (when (and emoji ident) " ")
                                       (or ident "")))
                  when (> (length token) 0)
                  collect token)))
    (when parts
      (format "%s: %s" label (string-join parts ", ")))))

(defun my-chatgpt-shell--format-event-line (event)
  (let* ((pattern (my-chatgpt-shell--pattern->string (plist-get event :pattern)))
         (kind (my-chatgpt-shell--pattern->string (plist-get event :kind)))
         (notes (string-trim (format "%s" (or (plist-get event :notes) ""))))
         (pattern-text (if (and pattern (> (length pattern) 0)
                                (not (string= pattern "none")))
                           (format "Pattern %s" pattern)
                         "No pattern"))
         (kind-text (if (> (length kind) 0)
                        (format " (%s)" kind)
                      ""))
         (notes-text (if (> (length notes) 0)
                         (my-chatgpt-shell--truncate notes 160)
                       "No notes supplied.")))
    (format "• %s%s – %s" pattern-text kind-text notes-text)))

(defun my-chatgpt-shell--prompt-pattern-summary (edn)
  (let* ((intent (plist-get edn :intent))
         (patterns (my-chatgpt-shell--coerce-seq (plist-get edn :patterns)))
         (fruits (my-chatgpt-shell--coerce-seq (plist-get edn :fruits)))
         (paramitas (my-chatgpt-shell--coerce-seq (plist-get edn :paramitas)))
         (parts nil))
    (when (and intent
               (not (string-empty-p (string-trim intent))))
      (push (format "Current intent: %s" intent) parts))
    (when patterns
      (let ((lines (cl-loop for pattern in patterns
                            for idx from 0 below 4
                            for pid = (or (plist-get pattern :id)
                                          (plist-get pattern :title)
                                          "(unknown)")
                            for summary = (string-trim (or (plist-get pattern :summary)
                                                          "No summary provided."))
                            for score = (plist-get pattern :score)
                            collect (concat pid " → " summary
                                            (if (numberp score)
                                                (format " [d=%.2f]" score)
                                              "")))) )
        (push (concat "Patterns: " (string-join lines " | ")) parts)))
    (when fruits
      (let ((tokens (cl-loop for fruit in fruits
                             for emoji = (plist-get fruit :emoji)
                             for name = (or (plist-get fruit :fruit/id)
                                            (plist-get fruit :title)
                                            "fruit")
                             for score = (plist-get fruit :score)
                             collect (string-trim (concat (or emoji "")
                                                     (when (and emoji name) " ")
                                                     name
                                                     (if (numberp score)
                                                         (format " [d=%.2f]" score)
                                                       ""))))))
        (when tokens
          (push (concat "Fruits: " (string-join tokens ", ")) parts))))
    (when paramitas
      (let ((tokens (cl-loop for paramita in paramitas
                             for emoji = (plist-get paramita :emoji)
                             for name = (or (plist-get paramita :paramita/id)
                                            (plist-get paramita :title)
                                            "paramita")
                             for score = (plist-get paramita :score)
                             collect (string-trim (concat (or emoji "")
                                                     (when (and emoji name) " ")
                                                     name
                                                     (if (numberp score)
                                                         (format " [d=%.2f]" score)
                                                       ""))))))
        (when tokens
          (push (concat "Pāramitās: " (string-join tokens ", ")) parts))))
    (when parts
      (string-join (nreverse parts) "\n"))))

(defun my-chatgpt-shell--insert-inbound-edn (edn)
  (insert (propertize "Latest FROM-TATAMI-EDN block" 'face 'bold) "\n")
  (insert (my-chatgpt-shell--format-edn-block my-chatgpt-shell-tatami-in-marker edn) "\n\n"))

(defun my-chatgpt-shell--insert-edn-summary (edn)
  (let* ((session (or (plist-get edn :session-id) "?"))
         (mode (my-chatgpt-shell--pattern->string (plist-get edn :mode)))
         (clock (or (plist-get edn :clock) "?"))
         (patterns (my-chatgpt-shell--coerce-seq (plist-get edn :patterns)))
         (fruits (my-chatgpt-shell--coerce-seq (plist-get edn :fruits)))
         (paramitas (my-chatgpt-shell--coerce-seq (plist-get edn :paramitas)))
         (events (my-chatgpt-shell--events-seq (plist-get edn :events)))
         (pattern-limit 3)
         (event-limit 3))
    (insert (propertize "Latest FROM-CHATGPT state" 'face 'bold) "\n")
    (insert (format "Session %s | Mode %s | Clock %s\n"
                    session (or mode "?") clock))
    (insert (format "Intent: %s\n\n"
                    (or (and (plist-get edn :intent)
                             (stringp (plist-get edn :intent))
                             (plist-get edn :intent))
                        my-futon3-tatami-default-intent
                        "unspecified")))
    (if patterns
        (progn
          (insert (propertize "Candidate clauses" 'face 'bold)
                  (if (> (length patterns) pattern-limit)
                      (format " (top %d of %d)" pattern-limit (length patterns))
                    "")
                  "\n")
          (cl-loop for pattern in patterns
                   for idx from 0
                   while (< idx pattern-limit)
                   do (insert (my-chatgpt-shell--format-pattern-line pattern) "\n"))
          (insert "\n"))
      (insert "No candidate clauses available.\n\n"))
    (let ((fruit-line (my-chatgpt-shell--format-support-line "Fruits" fruits :emoji :fruit/id))
          (param-line (my-chatgpt-shell--format-support-line "Pāramitās" paramitas :emoji :paramita/id)))
      (when fruit-line
        (insert fruit-line "\n"))
      (when param-line
        (insert param-line "\n"))
      (when (or fruit-line param-line)
        (insert "\n")))
    (insert (propertize "Reasoning trace" 'face 'bold)
            (if (> (length events) event-limit)
                (format " (latest %d of %d)" event-limit (length events))
              "")
            "\n")
    (let ((recent (my-chatgpt-shell--take-last events event-limit)))
      (if recent
          (dolist (event recent)
            (insert (my-chatgpt-shell--format-event-line event) "\n"))
        (insert "No reasoning events recorded yet.\n")))
    (insert "\n")))


(defun my-chatgpt-shell--strip-edn-block (text)
  (let* ((str (or text ""))
         (start-marker (format "---%s---" my-chatgpt-shell-tatami-out-marker))
         (end-marker (format "---END-%s---" my-chatgpt-shell-tatami-out-marker))
         (start (string-match (regexp-quote start-marker) str)))
    (when start
      (let ((end (string-match (regexp-quote end-marker) str start)))
        (when end
          (setq str (concat (substring str 0 start)
                            (substring str (+ end (length end-marker))))))))
    (string-trim str)))

(defun my-chatgpt-shell--remove-edn-from-buffer ()
  (let ((start-marker (format "---%s---" my-chatgpt-shell-tatami-out-marker))
        (end-marker (format "---END-%s---" my-chatgpt-shell-tatami-out-marker)))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (when (search-backward end-marker nil t)
          (let ((end (match-end 0)))
            (when (search-backward start-marker nil t)
              (delete-region (match-beginning 0) end))))))))

(defun my-chatgpt-shell--buffer-edn-string ()
  (let ((start-marker (format "---%s---" my-chatgpt-shell-tatami-out-marker))
        (end-marker (format "---END-%s---" my-chatgpt-shell-tatami-out-marker)))
    (save-excursion
      (goto-char (point-max))
      (when (search-backward end-marker nil t)
        (let ((end (match-beginning 0)))
          (when (search-backward start-marker nil t)
            (let ((start (match-end 0)))
              (buffer-substring-no-properties start end))))))))

(defun my-chatgpt-shell--process-buffer-edn (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (boundp 'my-chatgpt-shell-profile)
                 (string= my-chatgpt-shell-profile "General"))
        (when-let ((raw (my-chatgpt-shell--buffer-edn-string)))
          (my-chatgpt-shell--remove-edn-from-buffer)
          (when-let ((edn (my-chatgpt-shell--parse-edn-string raw)))
            (my-chatgpt-shell--debug "Captured FROM-CHATGPT-EDN: %s" (prin1-to-string edn))
            (my-chatgpt-shell--apply-chatgpt-edn edn)))))))

(defun my-chatgpt-shell--refresh-context-buffer ()
  (when (derived-mode-p 'chatgpt-shell-mode)
    (condition-case err
        (progn
          (setq my-chatgpt-shell-last-inbound-edn nil)
          (my-chatgpt-shell--build-inbound-edn))
      (error
       (message "Tatami hint refresh failed: %s" (error-message-string err))))))

(defun my-chatgpt-shell--refresh-context-all ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'chatgpt-shell-mode)
        (my-chatgpt-shell--refresh-context-buffer)))))

(defun my-chatgpt-shell-persist-edn (_payload)
  "Placeholder for future futon1 persistence wiring."
  nil)

(defun my-futon3-fetch-hints (&optional sigils)
  "Fetch nearest LDTS/fruit/paramita hints from Futon3."
  (condition-case err
      (let ((payload nil)
            (encoded (my-chatgpt-shell--encoded-prototypes)))
        (when encoded
          (push (cons "prototypes" (my-chatgpt-shell--vector encoded)) payload))
        (when sigils
          (push (cons "sigils" (my-chatgpt-shell--encode-sigils sigils)) payload))
        (my-futon3--tatami-request "POST" "/musn/hints" payload))
    (error
     (message "Futon3 hints failed: %s" (error-message-string err))
     nil)))

(defun my-chatgpt-shell--build-inbound-edn ()
  (my-futon3-ensure-tatami-session)
  (let* ((existing (copy-sequence my-chatgpt-shell-last-edn))
         (session (or (plist-get existing :session-id)
                      my-futon3-tatami-session-id))
         (hints (my-futon3-fetch-hints))
         (edn (or existing '())))
    (setq edn (plist-put edn :session-id session))
    (setq edn (plist-put edn :mode (or (plist-get edn :mode) :ambient)))
    (setq edn (plist-put edn :clock (my-chatgpt-shell--now-iso)))
    (setq edn (plist-put edn :futons (my-chatgpt-shell--current-futons)))
    (setq edn (plist-put edn :prototypes (my-chatgpt-shell--prototype-keywords)))
    (setq edn (plist-put edn :intent (or (plist-get edn :intent)
                                         my-futon3-tatami-default-intent
                                         "unspecified")))
    (setq edn (plist-put edn :patterns (or (plist-get hints :patterns) [])))
    (setq edn (plist-put edn :fruits (or (plist-get hints :fruits) [])))
    (setq edn (plist-put edn :paramitas (or (plist-get hints :paramitas) [])))
    (setq edn (plist-put edn :events (or (plist-get edn :events) [])))
    (setq my-chatgpt-shell-last-inbound-edn edn)
    (my-chatgpt-shell--render-context)
    edn))

;; --- Prompt builder for futon1 ----------------------------------------

(defun set-user-system-prompt (_command)
  "Build a context-aware system prompt for chatgpt-shell.
Includes date/time, tatami summary or focus, and an optional
profile-specific prompt loaded from `my-futon-prompt-directory`."
  (let* ((now   (current-time))
         (date  (format-time-string "%A, %B %d, %Y" now))
         (time  (format-time-string "%H:%M" now))
         (context
          (if my-chatgpt-shell--seeded-context
              ;; 2nd+ runs: prefer current focus snippet
              (or (my-chatgpt-shell--focus-snippet)
                  "Current focus unavailable.")
            ;; 1st run: mark seeded, then (only if up) try Tatami summary
            (setq my-chatgpt-shell--seeded-context t)
            (or (when-let* ((sum (tatami-me-summary-safe)))
                  (setq my-chatgpt-shell-last-summary (my-chatgpt-shell--format-summary sum))
                  (my-chatgpt-shell--maybe-render-context)
                  sum)
                "Current profile summary unavailable.")))
         ;; profile-specific prompt body from ~/.emacs-graph/aob/<profile>.prompt
         (profile (or my-chatgpt-shell-profile "General"))
         (profile-body (my-futon-read-prompt profile))
         (tatami-edn (my-chatgpt-shell--build-inbound-edn))
         (tatami-block (my-chatgpt-shell--format-edn-block
                        my-chatgpt-shell-tatami-in-marker tatami-edn))
         (adjacency-brief (or (my-chatgpt-shell--prompt-pattern-summary tatami-edn)
                              "No adjacency cues supplied."))
         (prompt (concat
                  "You use markdown liberally to structure responses. "
                  "Always show code snippets in markdown blocks with language labels. "
                  "The user’s most recent query has been submitted at the following date: "
                  date " and the local time (in the London timezone) is " time
                  ". Please preface all your replies with the metadata ["
                  date "/" time "].\n"
                  "When giving answers related to time, do not use any other cached time values.\n\n"
                  "Current working intent: " (or my-futon3-tatami-default-intent "unspecified") "\n"
                  "Infer or refine this intent (<= 10 words) each turn and write it to the FROM-CHATGPT-EDN :intent key so Futon3 can retarget you.\n"
                  "If you are unsure, describe the **next best question or artifact** that would advance the proof.\n\n"
                  "Primary directive: perform pattern-theoretic inference (deduction, abduction, induction) over the supplied Tatami hints. Keep any direct response to the user's explicit question to roughly 100 words, then devote the rest of the turn to analysing patterns and adjacent structures.\n"
                  "Treat all candidate clauses as proof obligations: compare them, combine them creatively, and identify missing premises so Futon3 can learn from gaps.\n\n"
                  "Pattern reasoning checklist:\n"
                  "1. Inspect every entry in :patterns (and supporting fruits/pāramitās) from the inbound EDN.\n"
                  "2. Decide whether one or more clauses apply to the user's latest move.\n"
                  "3. If a clause applies, cite its ID in markdown and justify the match.\n"
                  "4. If none apply, explicitly say so and name the blocking condition.\n"
                  "5. Always append an event map shaped {:kind :note :pattern \"fX/pY\" :notes \"Justification\"}. When nothing applies, record {:pattern :none :notes \"Reason\"}.\n\n"
                  "Markdown reply template:\n"
                  "- Pattern verdict (e.g., \"Pattern f2/p7 applies\" or \"No candidate applies because ...\").\n"
                  "- Reasoning (premises checked, conflicts, or follow-up obligations).\n"
                  "- Next step suggestion (if helpful).\n\n"
                  "Adjacency briefing (top hints):\n" adjacency-brief "\n\n"
                  "After answering the user in clear markdown, append a block delimited by ---"
                  my-chatgpt-shell-tatami-out-marker
                  "--- and ---END-" my-chatgpt-shell-tatami-out-marker
                  "--- containing valid EDN that mirrors and updates the keys shown below. "
                  "Only include keys whose values changed this turn (e.g., :intent, updated :events); do NOT echo the inbound :patterns/fruits etc. unless you are altering them. "
                  "Do not include commentary or markdown inside that EDN block.\n\n"
                  "Here is the current FROM-TATAMI-EDN block for this turn. These patterns are the candidate clauses you must reason about. Follow the checklist/template above, then append the required :events entry (e.g., {:kind :note :pattern \"t4r/rationale\" :notes \"Matched because...\"}).\nDo not mirror the block blindly—use these hints to guide your reasoning and report the result.\nHere is the block:\n"
                  tatami-block "\n\n"
                  context "\n\n"
                  (or profile-body
                      "No profile-specific system prompt was found; respond helpfully and clearly."))))
    (setq chatgpt-shell-system-prompts
          `(("tl;dr" . "Be as succinct but informative as possible and respond in tl;dr form to my queries")
            ("General" . ,prompt)))
    (setq-local chatgpt-shell-system-prompt prompt)))


;;; Launchers

(defun my-chatgpt-shell--spawn (profile buffer-name)
  "Open a new chatgpt-shell BUFFER-NAME configured for PROFILE."
  (let ((buf (chatgpt-shell)))
    (with-current-buffer buf
      (rename-buffer (generate-new-buffer-name buffer-name) t)
      (my-chatgpt-shell-set-profile profile))
    buf))

(defun par-case-shell ()
  "Open a chatgpt-shell configured for PAR → Case File work."
  (interactive)
  (my-chatgpt-shell--spawn "par-case-shell" "*PAR Case ChatGPT*"))

(defalias 'par-shell #'par-case-shell)

(defun paramita-shell ()
  "Open a chatgpt-shell configured for Pāramitā signature work."
  (interactive)
  (my-chatgpt-shell--spawn "paramita-shell" "*Paramita ChatGPT*"))

(global-set-key (kbd "C-c P") #'par-shell)
(global-set-key (kbd "C-c M") #'paramita-shell)

(defun futon0-insert-clock-out-prompt ()
  "Insert the futon0 clock-out prompt into the current chatgpt-shell buffer."
  (interactive)
  (my-chatgpt-shell--insert-prompt "futon0-clock-out" "clock-out" t))

(defun par-case-insert-prompt ()
  "Insert the PAR → Case File helper prompt into the current chat buffer."
  (interactive)
  (my-chatgpt-shell--insert-prompt "par-case-shell" "par-case-shell" t))

(defun paramita-insert-prompt ()
  "Insert the Pāramitā signature prompt for a single turn."
  (interactive)
  (my-chatgpt-shell--insert-prompt "paramita-shell" "paramita-shell" t))


;;; Advice

(defun my-chatgpt-shell-before-command (command)
  "Wrapper for `chatgpt-shell-before-command-functions`.

Always sets the system prompt. Tatami ingestion now occurs via
`my-chatgpt-shell-after-command` so the LLM response is not blocked."
  (set-user-system-prompt command))

(defun my-chatgpt-shell--context-buffer (&optional ensure)
  (if ensure
      (get-buffer-create my-chatgpt-shell-context-buffer-name)
    (get-buffer my-chatgpt-shell-context-buffer-name)))

(defun my-chatgpt-shell--render-context (&optional ensure)
  (let ((summary my-chatgpt-shell-last-summary)
        (focus my-chatgpt-shell-last-focus)
        (inbound my-chatgpt-shell-last-inbound-edn))
    (when-let ((buf (my-chatgpt-shell--context-buffer ensure)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (unless (derived-mode-p 'special-mode)
            (special-mode))
          (setq-local truncate-lines t)
          (let ((status (and (my-futon3-running-p)
                             (or (my-futon3-refresh-status)
                                 my-futon3-last-status))))
            (when (and status (plist-get status :events))
              (let* ((selection (plist-get status :selection))
                     (sel-protos (my-futon3--display-prototypes (plist-get selection :prototypes)))
                     (sel-intent (or (plist-get selection :intent)
                                     my-futon3-tatami-default-intent
                                     "unspecified"))
                     (sel-line (when sel-protos
                                 (format "Active: %s | Intent: %s"
                                         (string-join sel-protos ", ")
                                         sel-intent))))
                (insert (propertize "Tatami / Futon3 status (24h)" 'face 'bold) "\n")
                (when sel-line
                  (insert sel-line "\n"))
                (insert (format "Events: %s\nProof summaries today: %s\n\n"
                                (plist-get status :events)
                                (if (plist-get status :proofs?) "yes" "no"))))))
          (insert (propertize "Latest focus header" 'face 'bold) "\n")
          (if focus
              (insert focus "\n\n")
            (insert "No Tatami focus header captured yet." "\n\n"))
          (when inbound
            (my-chatgpt-shell--insert-inbound-edn inbound))
          (when summary
            (insert (propertize "Latest :me summary" 'face 'bold) "\n"
                    summary "\n"))
          (when my-chatgpt-shell-last-edn
            (my-chatgpt-shell--insert-edn-summary my-chatgpt-shell-last-edn))
          (goto-char (point-min)))))))

(defun my-chatgpt-shell--maybe-render-context ()
  (let ((buf (my-chatgpt-shell--context-buffer t)))
    (my-chatgpt-shell--render-context t)
    (unless (get-buffer-window buf)
      (let ((window (display-buffer-in-side-window
                     buf '((side . bottom)
                           (slot . 0)
                           (window-height . 0.25)))))
        (set-window-dedicated-p window t)))))

(defun my-chatgpt-shell-toggle-context ()
  "Toggle display of the Tatami context HUD at the bottom of the frame."
  (interactive)
  (let ((buf (my-chatgpt-shell--context-buffer t)))
    (if-let ((win (get-buffer-window buf)))
        (delete-window win)
      (if (and (null my-chatgpt-shell-last-summary)
               (null my-chatgpt-shell-last-focus))
          (message "No Tatami context captured yet.")
        (my-chatgpt-shell--render-context t)
        (let ((window (display-buffer-in-side-window
                       buf '((side . bottom)
                             (slot . 0)
                             (window-height . 0.25)))))
          (set-window-dedicated-p window t))))))

(defun my-chatgpt-shell--ingest (text)
  "Safely send TEXT to Tatami without surfacing hard errors."
  (unless (and my-chatgpt-shell--tatami-disabled
               (progn (setq my-chatgpt-shell--tatami-disabled nil) nil))
    (let ((waited 0)
        (max-wait 3)
        (sleep 0.3))
    (while (and (< waited max-wait)
                (not (tatami-available-p)))
      (sleep-for sleep)
      (setq waited (+ waited sleep)))
    (if (tatami--server-running-p)
        (condition-case err
            (let ((tatami-startup-wait 20))
              (when-let ((fh (tatami-send-sentences text)))
                (my-chatgpt-shell--format-focus fh)))
          (error
           (message "Tatami ingestion skipped: %s (%s)"
                    (error-message-string err)
                    (or (tatami-last-error) "no tatami-last-error"))))
      (message "Tatami ingestion skipped: headless API unavailable (waited %.1fs)." waited)))))

(defun my-chatgpt-shell-after-command (command output _success)
  "Ingest COMMAND text into Tatami and capture FROM-CHATGPT-EDN payloads."
  (message "DEBUG: [tatami] after-command profile=%s output=%s"
           (and (boundp 'my-chatgpt-shell-profile) my-chatgpt-shell-profile)
           (and output (substring output 0 (min 40 (length output)))))
  (when (and (boundp 'my-chatgpt-shell-profile)
             (string= my-chatgpt-shell-profile "General"))
    ;; BUG NOTE: chatgpt-shell-after-command currently hands us only the
    ;; *timestamp header* (see https://github.com/xenodium/chatgpt-shell/issues/412),
    ;; so we cannot rely on OUTPUT to contain the FROM-CHATGPT-EDN block. Instead
    ;; we grab the block directly from the buffer, process it asynchronously, and
    ;; then strip it from the visible chat. Once the upstream bug is fixed we can
    ;; simplify this collector.
    (let ((buffer (current-buffer)))
      (run-at-time 0 nil #'my-chatgpt-shell--process-buffer-edn buffer))
    (my-chatgpt-shell--ingest command)
    (my-futon3-log-chatgpt-turn command)))

(advice-add 'chatgpt-shell :before
            (lambda (&rest _ignore)
              (my-futon3-ensure-running)
              ;; Force interactive start so we surface failures in the minibuffer.
              (setq tatami--last-command tatami-start-command)
              (when (tatami--launch-server t)
                (run-at-time 0.2 nil #'my-tatami--notify-ready))))

(defun my-tatami--notify-ready ()
  "Emit a message when the headless server becomes reachable."
  (let ((start (float-time))
        (initial-delay 0.5)
        timer)
    (setq timer
          (run-with-timer
           initial-delay 0.5
           (lambda ()
             (if (tatami--server-running-p)
                 (progn
                   (cancel-timer timer)
                   (message "Headless server available after %.1fs"
                            (- (float-time) start)))
               (when (> (- (float-time) start) 10)
                 (cancel-timer timer)
                 (message "Headless server still unavailable after %.1fs"
                          (- (float-time) start)))))))))

(add-hook 'chatgpt-shell-before-command-functions #'my-chatgpt-shell-before-command)
(add-hook 'chatgpt-shell-after-command-functions #'my-chatgpt-shell-after-command)

;;; Minor mode with lighters

;; --- Mode-line lighter for futon/chatgpt shells --------------------------

(defvar-local my-chatgpt-shell-profile "General"
  "Name of the current futon prompt profile for this chatgpt-shell buffer.
Used both for selecting the system prompt and for the mode-line lighter.")

(defun futon-profile-lighter ()
  "Return a short mode-line lighter based on `my-chatgpt-shell-profile`."
  (pcase my-chatgpt-shell-profile
    ("par-shell"      " PAR")
    ("par-case-shell" " PC")
    ("paramita-shell" " Π")
    ("clock-out"      " CO")
    ("General"        " F0")
    (_                (concat " " my-chatgpt-shell-profile))))

(define-minor-mode futon-profile-mode
  "Minor mode to show the futon/chatgpt profile in the mode line."
  :init-value t
  :lighter '(:eval (futon-profile-lighter)))

(add-hook 'chatgpt-shell-mode-hook #'futon-profile-mode)

(define-key chatgpt-shell-mode-map (kbd "C-c C-t") #'my-chatgpt-shell-toggle-context)
(define-key chatgpt-shell-mode-map (kbd "C-c C-o") #'futon0-insert-clock-out-prompt)
(define-key chatgpt-shell-mode-map (kbd "C-c C-p") #'par-case-insert-prompt)
(define-key chatgpt-shell-mode-map (kbd "C-c C-m") #'paramita-insert-prompt)
(define-key chatgpt-shell-mode-map (kbd "C-c C-g") #'futon-set-general-profile)

(defun my-chatgpt-shell--format-summary (text)
  (when text
    (if (string-match "Generated at: \([0-9]+\)" text)
        (let* ((ms (string-to-number (match-string 1 text)))
               (ts (format-time-string "%Y-%m-%d %H:%M:%S (%Z)"
                                       (seconds-to-time (/ ms 1000.0)))))
          (replace-match (format "Generated at: %s" ts) t t text))
      text)))
(defun my-chatgpt-shell--apply-chatgpt-edn (edn)
  (setq my-chatgpt-shell-last-edn edn)
  (my-chatgpt-shell-persist-edn edn)
  (my-chatgpt-shell--validate-pattern-events edn)
  (my-chatgpt-shell--maybe-update-intent edn)
  (my-chatgpt-shell--maybe-render-context))

(defun my-chatgpt-shell-inject-chatgpt-edn (text)
  "Manually feed a FROM-CHATGPT-EDN payload, bypassing the LLM."
  (interactive "sFROM-CHATGPT-EDN payload: ")
  (let ((edn (my-chatgpt-shell--parse-edn-string text)))
    (unless edn
      (user-error "Could not parse EDN payload"))
    (my-chatgpt-shell--apply-chatgpt-edn edn)
    (message "Injected FROM-CHATGPT-EDN payload.")))
