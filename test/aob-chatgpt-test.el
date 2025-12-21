(require 'ert)
(require 'cl-lib)
(defvar chatgpt-shell-streaming t)
(defvar chatgpt-shell-model-version "gpt-4o")
(defvar chatgpt-shell-mode-map (make-sparse-keymap))
(defun chatgpt-shell-crop-context (&rest _) nil)
(defun chatgpt-shell-mode () nil)
(provide 'chatgpt-shell)
(load-file "/home/joe/code/futon1/contrib/aob-chatgpt.el")

(ert-deftest my-chatgpt-shell-edn-roundtrip ()
  (let* ((fake-edn "---FROM-CHATGPT-EDN---\n{:session-id \"test\" :clock \"2025\" :patterns ({:title \"Sample\" :summary \"Short\" :score 0.1}) :fruits ({:fruit/id \"doable\" :emoji \"🍒\" :score 0.2}) :paramitas ({:paramita/id \"truth\" :orb \"🟣\" :emoji \"🟣\"}) :events ({:activity :coding :text \"did a thing\"})}\n---END-FROM-CHATGPT-EDN---")
         (text (concat "Response body\n" fake-edn))
         (parsed (my-chatgpt-shell--extract-chatgpt-edn text)))
    (should (equal (plist-get parsed :session-id) "test"))
    (should (string-match "^Response body" (my-chatgpt-shell--strip-edn-block text)))))

(ert-deftest my-chatgpt-shell-validate-pattern-events-ok ()
  (setq my-chatgpt-shell-last-validation-warning nil)
  (let ((payload '(:events ((:pattern "f2/p7" :notes "Matched clause")))))
    (should (my-chatgpt-shell--validate-pattern-events payload))
    (should (null my-chatgpt-shell-last-validation-warning))))

(ert-deftest my-chatgpt-shell-validate-pattern-events-missing ()
  (setq my-chatgpt-shell-last-validation-warning nil)
  (let ((payload '(:events [])))
    (should-not (my-chatgpt-shell--validate-pattern-events payload))
    (should (stringp my-chatgpt-shell-last-validation-warning))))

(ert-deftest my-chatgpt-shell-insert-inbound-edn-block ()
  (with-temp-buffer
    (my-chatgpt-shell--insert-inbound-edn '(:session-id "s1" :patterns []))
    (let ((output (buffer-string)))
      (should (string-match "Latest FROM-TATAMI-EDN block" output))
      (should (string-match "---FROM-TATAMI-EDN---" output)))))

(ert-deftest my-chatgpt-shell-prompt-pattern-summary ()
  (let* ((edn '(:patterns ((:id "f2/p7" :summary "Clause" :score 0.02)
                          (:id "f3/p1" :summary "Alt" :score 0.5))
                :fruits ((:fruit/id "doable" :emoji "🍒" :score 0.1))
                :paramitas ((:paramita/id "truth" :emoji "🟣" :score 0.2))))
         (summary (my-chatgpt-shell--prompt-pattern-summary edn)))
    (should (stringp summary))
    (should (string-match "f2/p7" summary))
    (should (string-match "Fruits" summary))
    (should (string-match "Pāramitās" summary))))

(ert-deftest my-chatgpt-shell-insert-edn-summary-trace ()
  (with-temp-buffer
    (let ((edn '(:session-id "s1"
                :mode :ambient
                :clock "2025"
                :patterns ((:id "f2/p7" :title "Clause" :summary "Check obligation" :score 0.05))
                :fruits ((:fruit/id "doable" :emoji "🍒"))
                :paramitas ((:paramita/id "truth" :emoji "🟣"))
                :events ((:kind :note :pattern "f2/p7" :notes "Matched because...")))))
      (my-chatgpt-shell--insert-edn-summary edn)
      (let ((output (buffer-string)))
        (should (string-match "Candidate clauses" output))
        (should (string-match "Reasoning trace" output))
        (should (string-match "Pattern f2/p7" output))))))

(ert-deftest my-chatgpt-shell-format-edn-block-braces ()
  (let* ((payload '(:session-id "x" :foo (:bar 1 2)))
         (block (my-chatgpt-shell--format-edn-block "FROM" payload)))
    (should (string-match "---FROM---" block))
    (should (string-match (regexp-quote "{:session-id") block))
    (should (string-match (regexp-quote ":foo") block))))

; (ert-deftest my-chatgpt-shell-edn-encode-keywords ()
;   (should (equal (my-chatgpt-shell--edn-encode :foo) ":foo"))
;   (should (equal (my-chatgpt-shell--edn-encode '::bar) "::bar"))
;   (should (equal (my-chatgpt-shell--edn-encode '(:foo 1)) "(:foo 1)")))

(ert-deftest my-chatgpt-shell-context-shows-intent ()
  (let* ((my-chatgpt-shell-context-buffer-name "*Tatami Context Test*")
         (buf (get-buffer-create my-chatgpt-shell-context-buffer-name))
         (my-chatgpt-shell-last-inbound-edn '(:session-id "s1"
                                            :intent "initial"
                                            :patterns []
                                            :fruits []
                                            :paramitas []
                                            :events []))
         (my-chatgpt-shell-last-edn nil)
         (my-chatgpt-shell-last-summary nil)
         (my-chatgpt-shell-last-focus nil)
         (my-futon3-tatami-default-intent "initial")
         (calls nil))
    (cl-letf (((symbol-function 'my-futon3-running-p) (lambda () nil))
              ((symbol-function 'my-futon3-refresh-status) (lambda () nil))
              ((symbol-function 'my-futon3-sync-selection)
               (lambda () (push :sync calls))))
      (unwind-protect
          (progn
            (setq my-chatgpt-shell-last-edn '(:intent "initial"))
            (my-chatgpt-shell--render-context t)
            (with-current-buffer buf
              (should (string-match "Intent: initial" (buffer-string))))
            (my-chatgpt-shell--maybe-update-intent '(:intent "new intent"))
            (my-chatgpt-shell--render-context t)
            (with-current-buffer buf
              (should (string-match "Intent: new intent" (buffer-string))))
            (should (member :sync calls)))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest my-chatgpt-shell-maybe-update-intent ()
  (let ((my-futon3-tatami-default-intent "old")
        (calls nil))
    (cl-letf (((symbol-function 'my-futon3-sync-selection)
               (lambda () (push :sync calls)))
              ((symbol-function 'my-chatgpt-shell--render-context)
               (lambda (&rest _) (push :render calls))))
      (my-chatgpt-shell--maybe-update-intent '(:intent "new intent"))
      (should (equal my-futon3-tatami-default-intent "new intent"))
      (should (equal calls '(:render :sync))))
    (setq calls nil my-futon3-tatami-default-intent "stay")
    (cl-letf (((symbol-function 'my-futon3-sync-selection)
               (lambda () (push :sync calls)))
              ((symbol-function 'my-chatgpt-shell--render-context)
               (lambda (&rest _) (push :render calls))))
      (my-chatgpt-shell--maybe-update-intent '(:intent "   "))
      (should (equal my-futon3-tatami-default-intent "stay"))
      (should (null calls)))))

(provide 'aob-chatgpt-test)
