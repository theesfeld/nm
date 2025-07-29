;;; test-keybindings.el --- Test NetworkManager keybindings  -*- lexical-binding: t; -*-

;; Test script to verify keybindings are properly set up

(require 'nm)
(require 'nm-ui)

(defun test-global-keybindings ()
  "Test global keybindings are set correctly."
  (let ((binding (lookup-key global-map (kbd "C-c N"))))
    (if (eq binding nm-prefix-map)
        (message "✓ Global keybinding C-c N is correctly set")
      (error "✗ Global keybinding C-c N is not set correctly")))
  
  ;; Test prefix map bindings
  (dolist (test '(("s" . nm-status)
                  ("n" . nm-toggle-networking)
                  ("w" . nm-toggle-wireless)
                  ("u" . nm-ui)
                  ("W" . nm-ui-wifi)
                  ("E" . nm-ui-ethernet)
                  ("d" . nm-ui-devices)
                  ("c" . nm-ui-connections)
                  ("v" . nm-vpn-activate)
                  ("V" . nm-vpn-deactivate-all)
                  ("r" . nm-reload)
                  ("?" . nm-show-help)))
    (let ((key (car test))
          (expected-command (cdr test)))
      (let ((actual-command (lookup-key nm-prefix-map key)))
        (if (eq actual-command expected-command)
            (message "✓ C-c N %s → %s" key expected-command)
          (error "✗ C-c N %s → expected %s, got %s" key expected-command actual-command))))))

(defun test-mode-keybindings ()
  "Test mode-specific keybindings."
  ;; Test nm-ui-mode
  (with-temp-buffer
    (nm-ui-mode)
    (message "\nTesting nm-ui-mode keybindings:")
    (dolist (test '(("g" . nm-ui-refresh)
                    ("q" . quit-window)
                    ("n" . nm-toggle-networking)
                    ("w" . nm-toggle-wireless)
                    ("s" . nm-status)
                    ("u" . nm-ui)
                    ("W" . nm-ui-wifi)
                    ("E" . nm-ui-ethernet)
                    ("d" . nm-ui-devices)
                    ("c" . nm-ui-connections)))
      (let ((key (car test))
            (expected-command (cdr test)))
        (let ((actual-command (lookup-key (current-local-map) key)))
          (if (eq actual-command expected-command)
              (message "✓ %s → %s" key expected-command)
            (error "✗ %s → expected %s, got %s" key expected-command actual-command))))))
  
  ;; Test nm-ui-wifi-mode
  (with-temp-buffer
    (nm-ui-wifi-mode)
    (message "\nTesting nm-ui-wifi-mode keybindings:")
    (dolist (test '(("g" . nm-ui-refresh)
                    ("s" . nm-ui-scan-wifi)
                    ("S" . nm-status)
                    ("d" . nm-ui-disconnect)
                    ("f" . nm-ui-forget-network)
                    ("l" . nm-ui-devices)))
      (let ((key (car test))
            (expected-command (cdr test)))
        (let ((actual-command (lookup-key (current-local-map) key)))
          (if (eq actual-command expected-command)
              (message "✓ %s → %s" key expected-command)
            (error "✗ %s → expected %s, got %s" key expected-command actual-command))))))
  
  ;; Test connections mode map
  (message "\nTesting nm-ui-connections-mode-map keybindings:")
  (dolist (test '(("g" . nm-ui-refresh)
                  ("a" . nm-ui-activate-connection)
                  ("d" . nm-ui-deactivate-connection)
                  ("e" . nm-ui-edit-connection)
                  ("D" . nm-ui-delete-connection)
                  ("l" . nm-ui-devices)))
    (let ((key (car test))
          (expected-command (cdr test)))
      (let ((actual-command (lookup-key nm-ui-connections-mode-map key)))
        (if (eq actual-command expected-command)
            (message "✓ %s → %s" key expected-command)
          (error "✗ %s → expected %s, got %s" key expected-command actual-command))))))

;; Run tests
(test-global-keybindings)
(test-mode-keybindings)
(message "\nAll keybinding tests passed!")

;;; test-keybindings.el ends here