# Emacs utilities

This directory contains optional editor integrations for interacting with the
Futon headless API.

## `tatami1.el`

The `tatami1.el` package provides a thin Emacs client for the headless API.  It
can start the server (via `clojure -M:run-m` by default), retrieve the current
`:me` summary, and post turns to obtain the updated focus header.

### Quick start

1. Ensure you can start the headless API from the repository root with:

   ```bash
   clojure -M:run-m
   ```

2. Copy `contrib/tatami1.el` somewhere on your `load-path`, or add this
   repository directly:

   ```elisp
   (add-to-list 'load-path "/path/to/futon1/contrib")
   (require 'tatami1)
   ```

3. (Optional) Configure the startup command, working directory, base URL, or
   profile:

   ```elisp
   (setq tatami1-start-directory "/path/to/futon1/apps/api")
   (setq tatami1-start-command '("clojure" "-M:run-m"))
   (setq tatami1-base-url "http://localhost:8080")
   (setq tatami1-profile "default")
   ;; Inspect the last recorded error, if any
   (tatami1-last-error)
   ```

4. Use the interactive commands:

   - `M-x tatami1-open-summary-buffer` – start the server if needed and
     display the current `:me` summary in a temporary buffer.
   - `M-x tatami1-focus-header` – fetch the latest focus header as an alist
     (printed in the echo area by default).
   - `M-x tatami1-send-turn` – prompt for a text block, post it to the
     `/api/α/turns` endpoint, and return the new focus header.  When invoked with
     a prefix argument (`C-u`), the commands prompt for an explicit profile
     override.
   - `M-x tatami1-send-sentences` – send the active region (or prompted text)
     sentence by sentence, returning the focus header produced by the final
     turn.  `tatami1-send-lines` remains as an alias for convenience.
   - `M-x tatami1-send-buffer-sentences` – send every sentence in the current
     buffer to the API in order.
   - `M-x tatami1-send-next-sentence` – send the sentence at point and advance.

If a server launch fails (e.g., because the port is already in use), inspect the
`*headless-api-server*` buffer or call `(tatami1-last-error)` for the detailed
message.
