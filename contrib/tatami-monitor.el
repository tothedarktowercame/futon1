;;; tatami-monitor.el  -*- lexical-binding: t; -*-

(defgroup tatami-monitor nil
  "Minimal monitoring for Tatami ⇄ Futon1."
  :group 'external)

(defcustom tatami-monitor-server-buffer "*headless-api-server*"
  "Buffer where the Futon1 server writes logs/output."
  :type 'string)

(defcustom tatami-monitor-timeout-sec 12
  "How long to wait for server buffer growth before flagging a timeout."
  :type 'integer)

(defcustom tatami-monitor-log-file (expand-file-name "~/.cache/tatami-monitor.log")
  "Where to append monitoring events."
  :type 'file)

(defcustom tatami-monitor-log-max-lines 5000
  "Rudimentary rotation: trim file to this many lines periodically."
  :type 'integer)

(defvar tatami-monitor--last-status "ok")

(defun tatami-monitor--now () (format-time-string "%Y-%m-%d %H:%M:%S%z"))
(defun tatami-monitor--log (fmt &rest args)
  (let* ((line (apply #'format fmt args))
         (msg  (format "[%s] %s\n" (tatami-monitor--now) line)))
    (make-directory (file-name-directory tatami-monitor-log-file) t)
    (with-temp-buffer
      (when (file-exists-p tatami-monitor-log-file)
        (insert-file-contents tatami-monitor-log-file))
      (goto-char (point-max))
      (insert msg)
      ;; naive rotation
      (let ((lines (count-lines (point-min) (point-max))))
        (when (> lines tatami-monitor-log-max-lines)
          (goto-char (point-min))
          (forward-line (- lines tatami-monitor-log-max-lines))
          (write-region (point) (point-max) tatami-monitor-log-file nil 'silent)))
      (unless (file-exists-p tatami-monitor-log-file)
        (write-region (point-min) (point-max) tatami-monitor-log-file nil 'silent))
      (write-region (point-min) (point-max) tatami-monitor-log-file nil 'silent)))
  ;; also echo in *Messages*
  (apply #'message (concat "[tatami] " fmt) args))

(defun tatami/monitor-send (send-fn line &optional context)
  "Send LINE via SEND-FN while monitoring server responsiveness.

SEND-FN is a function of one arg (string) that actually sends the text
(e.g., your Tatami/Futon1 sender). CONTEXT is any short tag you want in logs.

We mark current size of `tatami-monitor-server-buffer` and check
for growth within `tatami-monitor-timeout-sec` seconds."
  (let* ((ctx (or context "tatami"))
         (start (float-time))
         (buf (get-buffer tatami-monitor-server-buffer))
         (pos0 (and buf (with-current-buffer buf (buffer-size))))
         (ticket (format "%.6f" start)))
    (tatami-monitor--log "▶ send ticket=%s ctx=%s bytes0=%s text=%s"
                         ticket ctx (or pos0 -1) (string-trim line))
    (condition-case err
        (progn
          (funcall send-fn line)
          ;; schedule a one-shot check
          (run-at-time tatami-monitor-timeout-sec nil
                       (lambda ()
                         (let* ((buf* (get-buffer tatami-monitor-server-buffer))
                                (bytes1 (and buf* (with-current-buffer buf* (buffer-size))))
                                (elapsed (- (float-time) start)))
                           (cond
                            ((and bytes1 pos0 (> bytes1 pos0))
                             (setq tatami-monitor--last-status "ok")
                             (tatami-monitor--log "✓ recv  ticket=%s ctx=%s Δ=%.2fs bytes=%s→%s"
                                                  ticket ctx elapsed pos0 bytes1))
                            (t
                             (setq tatami-monitor--last-status "timeout")
                             (tatami-monitor--log "✗ timeout ticket=%s ctx=%s Δ=%.2fs (no buffer growth)"
                                                  ticket ctx elapsed))))))
          t)
      (error
       (setq tatami-monitor--last-status "error")
       (tatami-monitor--log "☠ error ticket=%s ctx=%s err=%s"
                            ticket ctx (error-message-string err))
       nil))))

(defun tatami/monitor-health ()
  "Quick health snapshot: process live?, server buffer size, last status."
  (interactive)
  (let* ((buf (get-buffer tatami-monitor-server-buffer))
         (bytes (and buf (with-current-buffer buf (buffer-size))))
         ;; Heuristic: look for a Futon1 process by name; adapt if you use a different launcher.
         (procs (seq-filter (lambda (p) (string-match-p "futon1\\|clojure" (process-name p)))
                            (process-list)))
         (live (seq-some #'process-live-p procs)))
    (tatami-monitor--log "ℹ health live=%s bytes=%s last=%s procs=%s"
                         (if live "yes" "no") (or bytes -1) tatami-monitor--last-status
                         (mapcar #'process-name procs))
    (list :live live :bytes bytes :last tatami-monitor--last-status
          :procs (mapcar #'process-name procs))))

;;; ---- API-side tracing ----------------------------------------------

(defvar tatami-monitor--ticket-seq 0)
(defun tatami-monitor--ticket () (setq tatami-monitor--ticket-seq (1+ tatami-monitor--ticket-seq)))

(defun tatami-monitor--trace-request (orig-fn method path payload as-json)
  (let* ((t0 (float-time))
         (ticket (format "r%06d" (tatami-monitor--ticket))))
    (tatami-monitor--log "▶ api  ticket=%s %s %s as-json=%s"
                         ticket method path (if as-json "t" "nil"))
    (condition-case err
        (let* ((res (funcall orig-fn method path payload as-json))
               (dt  (- (float-time) t0)))
          (tatami-monitor--log "✓ api  ticket=%s %s %s Δ=%.2fs"
                               ticket method path dt)
          res)
      (error
       (let ((dt (- (float-time) t0)))
         (tatami-monitor--log "✗ api  ticket=%s %s %s Δ=%.2fs err=%s"
                              ticket method path dt (string-trim (error-message-string err))))
       (signal (car err) (cdr err))))))

(with-eval-after-load 'tatami
  (advice-add 'tatami--request :around #'tatami-monitor--trace-request))

;;; ---- Auto-recover on consecutive timeouts --------------------------

(defcustom tatami-monitor-timeout-streak-threshold 2
  "How many consecutive timeouts trigger an auto-restart."
  :type 'integer)

(defvar tatami-monitor--timeout-streak 0)

(defun tatami-monitor--on-timeout (ticket ctx elapsed)
  (setq tatami-monitor--timeout-streak (1+ tatami-monitor--timeout-streak))
  (tatami-monitor--log "✗ timeout ticket=%s ctx=%s Δ=%.2fs (streak=%d)"
                       ticket ctx elapsed tatami-monitor--timeout-streak)
  (when (>= tatami-monitor--timeout-streak tatami-monitor-timeout-streak-threshold)
    (tatami-monitor--log "↻ auto-restart: timeout streak=%d ≥ %d"
                         tatami-monitor--timeout-streak tatami-monitor-timeout-streak-threshold)
    ;; swap these for your actual start/stop helpers if named differently
    (ignore-errors (tatami--stop-server))
    (ignore-errors (tatami--start-server))
    (setq tatami-monitor--timeout-streak 0)))

(defun tatami-monitor--on-success (_ticket _ctx _elapsed)
  (setq tatami-monitor--timeout-streak 0))

;; Patch your existing monitor’s timeout/success branches:
;; In the run-at-time callback where you currently log success/timeout,
;; call these helpers accordingly:
;;   (tatami-monitor--on-success ticket ctx elapsed)
;;   (tatami-monitor--on-timeout ticket ctx elapsed)
