;;; nm-ui.el --- User interface for NetworkManager  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 William Theesfeld <william@theesfeld.net>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; User interface components for NetworkManager.

;;; Code:

(require 'nm)
(require 'nm-dbus)
(require 'nm-device)
(require 'nm-connection)
(require 'nm-wifi)
(require 'nm-vpn)
(require 'tabulated-list)
(require 'widget)
(require 'wid-edit)

(defvar nm-ui-buffer-name "*NetworkManager*"
  "Name of the NetworkManager UI buffer.")

(defvar nm-ui-wifi-buffer-name "*NetworkManager WiFi*"
  "Name of the NetworkManager WiFi buffer.")

(defvar nm-ui-connection-buffer-name "*NetworkManager Connection*"
  "Name of the NetworkManager connection editor buffer.")

(defvar nm-ui-refresh-timer nil
  "Timer for automatic refresh.")

(defvar nm-ui-selected-device nil
  "Currently selected device.")

(defvar nm-ui-selected-connection nil
  "Currently selected connection.")

(defvar nm-ui-password-callback nil
  "Callback for password input.")

(defvar-local nm-ui-widgets nil
  "List of widgets in current buffer.")

(defun nm-ui-insert-header (title)
  "Insert section TITLE with separator."
  (insert (propertize title 'face 'bold))
  (insert "\n" (make-string 50 ?─) "\n"))

(defun nm-ui-format-status-line (label value)
  "Format status line with LABEL and VALUE."
  (format "%s: %s\n" label value))

(defun nm-ui-insert-status-info ()
  "Insert NetworkManager status information."
  (let ((state (nm-get-state))
        (connectivity (nm-get-connectivity))
        (version (nm-get-version))
        (networking (nm-networking-enabled-p))
        (wireless (nm-wireless-enabled-p)))
    (insert (nm-ui-format-status-line "Version" version))
    (insert (nm-ui-format-status-line "State" (nm-state-to-string state)))
    (insert (nm-ui-format-status-line "Connectivity" (nm-connectivity-to-string connectivity)))
    (insert (nm-ui-format-status-line "Networking" (if networking "Enabled" "Disabled")))
    (insert (nm-ui-format-status-line "Wireless" (if wireless "Enabled" "Disabled")))
    (insert "\n")))

(defun nm-ui-format-connection-line (conn)
  "Format connection CONN as display line."
  (format "• %s (%s) - %s\n"
          (cdr (assoc 'id conn))
          (cdr (assoc 'type conn))
          (cdr (assoc 'state conn))))

(defun nm-ui-insert-active-connections ()
  "Insert active connections list."
  (nm-ui-insert-header "Active Connections")
  (let ((active-conns (nm-active-connections-info)))
    (if active-conns
        (dolist (conn active-conns)
          (insert (nm-ui-format-connection-line conn)))
      (insert "No active connections\n"))))

(defun nm-ui-format-device-line (device)
  "Format DEVICE as display line."
  (format "• %s (%s) - %s %s\n"
          (cdr (assoc 'interface device))
          (cdr (assoc 'type device))
          (cdr (assoc 'state device))
          (if (cdr (assoc 'managed device)) "" "[unmanaged]")))

(defun nm-ui-insert-devices ()
  "Insert devices list."
  (insert "\n")
  (nm-ui-insert-header "Devices")
  (let ((devices (nm-devices-info)))
    (dolist (device devices)
      (insert (nm-ui-format-device-line device)))))

(defun nm-ui-render-status ()
  "Render NetworkManager status in current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (nm-ui-insert-header "NetworkManager Status")
    (insert "\n")
    (nm-ui-insert-status-info)
    (nm-ui-insert-active-connections)
    (nm-ui-insert-devices)))

(defun nm-ui-render-connections ()
  "Render connections list."
  (let ((inhibit-read-only t)
        (connections (nm-connections-info)))
    (erase-buffer)
    (insert (propertize "Network Connections\n" 'face 'bold))
    (insert (make-string 50 ?─) "\n\n")
    
    (dolist (conn connections)
      (let ((start (point)))
        (insert (format "• %s (%s)"
                        (cdr (assoc 'id conn))
                        (cdr (assoc 'type conn))))
        (when (cdr (assoc 'autoconnect conn))
          (insert " [auto]"))
        (insert "\n")
        (put-text-property start (point) 'nm-connection conn)))))

(defun nm-ui-insert-current-wifi ()
  "Insert current WiFi connection info."
  (let ((current-conn (nm-wifi-get-current-connection)))
    (when current-conn
      (insert (format "Connected to: %s\n\n" (cdr (assoc 'id current-conn)))))))

(defun nm-ui-deduplicate-networks (networks)
  "Remove duplicate NETWORKS by SSID."
  (seq-uniq networks
            (lambda (a b)
              (equal (cdr (assoc 'ssid a))
                     (cdr (assoc 'ssid b))))))

(defun nm-ui-sort-networks-by-strength (networks)
  "Sort NETWORKS by signal strength descending."
  (seq-sort (lambda (a b)
              (> (cdr (assoc 'strength a))
                 (cdr (assoc 'strength b))))
            networks))

(defun nm-ui-valid-network-p (network)
  "Return t if NETWORK has valid SSID."
  (let ((ssid (cdr (assoc 'ssid network))))
    (and ssid (not (string-empty-p ssid)))))

(defun nm-ui-format-network-line (network)
  "Format NETWORK as display line."
  (format "%s %3d%% %s %s\n"
          (nm-wifi-strength-bars (cdr (assoc 'strength network)))
          (cdr (assoc 'strength network))
          (cdr (assoc 'ssid network))
          (cdr (assoc 'security network))))

(defun nm-ui-insert-network (network)
  "Insert NETWORK with text properties."
  (let ((start (point)))
    (when (nm-ui-valid-network-p network)
      (insert (nm-ui-format-network-line network))
      (put-text-property start (point) 'nm-network network))))

(defun nm-ui-render-wifi-networks ()
  "Render WiFi networks list."
  (let ((inhibit-read-only t)
        (networks (nm-wifi-get-all-access-points)))
    (erase-buffer)
    (nm-ui-insert-header "WiFi Networks")
    (insert "\n")
    (nm-ui-insert-current-wifi)
    (dolist (network (nm-ui-sort-networks-by-strength
                      (nm-ui-deduplicate-networks networks)))
      (nm-ui-insert-network network))))

(defun nm-ui-get-connection-settings (connection)
  "Get settings alist from CONNECTION."
  (when connection
    (nm-dbus-connection-settings-to-alist
     (nm-connection-get-settings (cdr (assoc 'path connection))))))

(defun nm-ui-get-setting-value (settings group key default)
  "Get value from SETTINGS for GROUP and KEY with DEFAULT."
  (or (when settings
        (cdr (assoc key (cdr (assoc group settings)))))
      default))

(defun nm-ui-create-name-widget (settings)
  "Create name widget with value from SETTINGS."
  (widget-create 'editable-field
                 :size 30
                 :format "Name: %v\n"
                 :value (nm-ui-get-setting-value settings "connection" "id" "")))

(defun nm-ui-create-type-widget (settings)
  "Create connection type widget with value from SETTINGS."
  (widget-create 'menu-choice
                 :tag "Type"
                 :value (nm-ui-get-setting-value settings "connection" "type" "802-3-ethernet")
                 :notify (lambda (widget &rest ignore)
                           (nm-ui-update-connection-form
                            (widget-value widget)))
                 '(item :tag "Ethernet" :value "802-3-ethernet")
                 '(item :tag "WiFi" :value "802-11-wireless")
                 '(item :tag "VPN" :value "vpn")
                 '(item :tag "WireGuard" :value "wireguard")))

(defun nm-ui-create-autoconnect-widget (settings)
  "Create autoconnect checkbox with value from SETTINGS."
  (widget-create 'checkbox
                 :value (nm-ui-get-setting-value settings "connection" "autoconnect" t)))

(defun nm-ui-create-ipv4-method-widget (settings)
  "Create IPv4 method widget with value from SETTINGS."
  (widget-create 'menu-choice
                 :tag "Method"
                 :value (nm-ui-get-setting-value settings "ipv4" "method" "auto")
                 '(item :tag "Automatic (DHCP)" :value "auto")
                 '(item :tag "Manual" :value "manual")
                 '(item :tag "Link-Local" :value "link-local")
                 '(item :tag "Disabled" :value "disabled")))

(defun nm-ui-create-button (label action)
  "Create push button with LABEL and ACTION."
  (widget-create 'push-button :notify action label))

(defun nm-ui-insert-form-header ()
  "Insert connection form header."
  (widget-insert (propertize "Connection Editor\n" 'face 'bold))
  (widget-insert (make-string 50 ?─) "\n\n"))

(defun nm-ui-insert-form-widgets (settings)
  "Insert form widgets with values from SETTINGS."
  (push (nm-ui-create-name-widget settings) nm-ui-widgets)
  (push (nm-ui-create-type-widget settings) nm-ui-widgets)
  (widget-insert "\n")
  (push (nm-ui-create-autoconnect-widget settings) nm-ui-widgets)
  (widget-insert " Autoconnect\n\n")
  (widget-insert (propertize "IPv4 Settings\n" 'face 'bold))
  (push (nm-ui-create-ipv4-method-widget settings) nm-ui-widgets)
  (widget-insert "\n\n")
  (push (nm-ui-create-button "Save" (lambda (&rest ignore)
                                       (nm-ui-save-connection)))
        nm-ui-widgets)
  (widget-insert " ")
  (push (nm-ui-create-button "Cancel" (lambda (&rest ignore)
                                         (kill-buffer)))
        nm-ui-widgets))

(defun nm-ui-create-connection-form (connection)
  "Create connection editor form for CONNECTION."
  (let ((inhibit-read-only t)
        (settings (nm-ui-get-connection-settings connection)))
    (erase-buffer)
    (remove-overlays)
    (setq nm-ui-widgets nil)
    (nm-ui-insert-form-header)
    (nm-ui-insert-form-widgets settings)
    (widget-setup)
    (goto-char (point-min))
    (widget-forward 1)))

(defun nm-ui-update-connection-form (type)
  "Update connection form based on TYPE."
  (message "Connection type changed to: %s" type))

(defun nm-ui-save-connection ()
  "Save connection from form."
  (let* ((widgets (reverse nm-ui-widgets))
         (name (widget-value (nth 0 widgets)))
         (type (widget-value (nth 1 widgets)))
         (autoconnect (widget-value (nth 2 widgets)))
         (ipv4-method (widget-value (nth 3 widgets))))
    (message "Saving connection: %s" name)))

(defun nm-ui-password-prompt (prompt callback)
  "Prompt for password with PROMPT and call CALLBACK."
  (setq nm-ui-password-callback callback)
  (let ((password (read-passwd prompt)))
    (when nm-ui-password-callback
      (funcall nm-ui-password-callback password)
      (setq nm-ui-password-callback nil))))

(defun nm-ui-connect-to-network ()
  "Connect to network at point."
  (interactive)
  (let ((network (get-text-property (point) 'nm-network)))
    (when network
      (let ((ssid (cdr (assoc 'ssid network)))
            (security (cdr (assoc 'security network))))
        (if (string= security "Open")
            (nm-wifi-connect-to-ap network)
          (nm-ui-password-prompt
           (format "Password for %s: " ssid)
           (lambda (password)
             (nm-wifi-connect-to-ap network password))))))))

(defun nm-ui-disconnect ()
  "Disconnect current connection."
  (interactive)
  (nm-wifi-disconnect))

(defun nm-ui-forget-network ()
  "Forget network at point."
  (interactive)
  (let ((network (get-text-property (point) 'nm-network)))
    (when network
      (nm-wifi-forget-network (cdr (assoc 'ssid network))))))

(defun nm-ui-activate-connection ()
  "Activate connection at point."
  (interactive)
  (let ((connection (get-text-property (point) 'nm-connection)))
    (when connection
      (let ((devices (nm-get-devices)))
        (nm-activate-connection (cdr (assoc 'path connection))
                                (or (car devices) "/")
                                "/")))))

(defun nm-ui-deactivate-connection ()
  "Deactivate connection at point."
  (interactive)
  (let ((connection (get-text-property (point) 'nm-connection)))
    (when connection
      (let ((active-conns (nm-active-connections-info)))
        (when-let ((active (seq-find (lambda (conn)
                                       (equal (cdr (assoc 'uuid conn))
                                              (cdr (assoc 'uuid connection))))
                                     active-conns)))
          (nm-deactivate-connection (cdr (assoc 'path active))))))))

(defun nm-ui-edit-connection ()
  "Edit connection at point."
  (interactive)
  (let ((connection (get-text-property (point) 'nm-connection)))
    (when connection
      (let ((buffer (get-buffer-create nm-ui-connection-buffer-name)))
        (with-current-buffer buffer
          (nm-ui-connection-mode)
          (nm-ui-create-connection-form connection))
        (switch-to-buffer buffer)))))

(defun nm-ui-delete-connection ()
  "Delete connection at point."
  (interactive)
  (let ((connection (get-text-property (point) 'nm-connection)))
    (when (and connection
               (yes-or-no-p (format "Delete connection %s? "
                                    (cdr (assoc 'id connection)))))
      (nm-connection-delete (cdr (assoc 'path connection)))
      (nm-ui-refresh))))

(defun nm-ui-scan-wifi ()
  "Scan for WiFi networks."
  (interactive)
  (nm-wifi-scan-all
   (lambda () (with-current-buffer nm-ui-wifi-buffer-name
                (nm-ui-refresh))))
  (message "Scanning for WiFi networks..."))

(defun nm-ui-refresh ()
  "Refresh current buffer."
  (interactive)
  (cond
   ((equal (buffer-name) nm-ui-buffer-name)
    (nm-ui-render-status))
   ((equal (buffer-name) nm-ui-wifi-buffer-name)
    (nm-ui-render-wifi-networks))
   ((string-match-p "Connections" (buffer-name))
    (nm-ui-render-connections))))

(defun nm-ui-start-refresh-timer ()
  "Start automatic refresh timer."
  (when (and nm-auto-refresh (not nm-ui-refresh-timer))
    (setq nm-ui-refresh-timer
          (run-with-timer nm-refresh-interval nm-refresh-interval
                          #'nm-ui-refresh))))

(defun nm-ui-stop-refresh-timer ()
  "Stop automatic refresh timer."
  (when nm-ui-refresh-timer
    (cancel-timer nm-ui-refresh-timer)
    (setq nm-ui-refresh-timer nil)))

(defun nm-ui-setup-which-key-for-mode ()
  "Setup which-key descriptions for current mode."
  (when (fboundp 'which-key-add-major-mode-key-based-replacements)
    (cond
     ((eq major-mode 'nm-ui-mode)
      (which-key-add-major-mode-key-based-replacements 'nm-ui-mode
        "g" "refresh"
        "q" "quit"
        "n" "toggle networking"
        "w" "toggle wireless"
        "W" "WiFi browser"
        "C" "connections"))
     ((eq major-mode 'nm-ui-wifi-mode)
      (which-key-add-major-mode-key-based-replacements 'nm-ui-wifi-mode
        "g" "refresh"
        "s" "scan WiFi"
        "q" "quit"
        "RET" "connect"
        "d" "disconnect"
        "f" "forget network")))))

(define-derived-mode nm-ui-mode special-mode "NetworkManager"
  "Major mode for NetworkManager interface."
  (setq truncate-lines t)
  (nm-ui-start-refresh-timer)
  (nm-ui-setup-which-key-for-mode))

(define-derived-mode nm-ui-wifi-mode special-mode "NM-WiFi"
  "Major mode for NetworkManager WiFi browser."
  (setq truncate-lines t)
  (nm-ui-setup-which-key-for-mode))

(define-derived-mode nm-ui-connection-mode special-mode "NM-Connection"
  "Major mode for NetworkManager connection editor.")

(defvar nm-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'nm-ui-refresh)
    (define-key map "q" #'quit-window)
    (define-key map "n" #'nm-toggle-networking)
    (define-key map "w" #'nm-toggle-wireless)
    (define-key map "W" #'nm-ui-wifi)
    (define-key map "C" #'nm-ui-connections)
    map)
  "Keymap for nm-ui-mode.")

(defvar nm-ui-wifi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'nm-ui-refresh)
    (define-key map "s" #'nm-ui-scan-wifi)
    (define-key map "q" #'quit-window)
    (define-key map (kbd "RET") #'nm-ui-connect-to-network)
    (define-key map "d" #'nm-ui-disconnect)
    (define-key map "f" #'nm-ui-forget-network)
    map)
  "Keymap for nm-ui-wifi-mode.")

(defvar nm-ui-connections-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'nm-ui-refresh)
    (define-key map "q" #'quit-window)
    (define-key map (kbd "RET") #'nm-ui-activate-connection)
    (define-key map "a" #'nm-ui-activate-connection)
    (define-key map "d" #'nm-ui-deactivate-connection)
    (define-key map "e" #'nm-ui-edit-connection)
    (define-key map "D" #'nm-ui-delete-connection)
    map)
  "Keymap for connections list.")

;;;###autoload
(defun nm-ui ()
  "Open NetworkManager user interface."
  (interactive)
  (unless (nm-available-p)
    (error "NetworkManager service not available"))
  (let ((buffer (get-buffer-create nm-ui-buffer-name)))
    (with-current-buffer buffer
      (nm-ui-mode)
      (nm-ui-render-status))
    (switch-to-buffer buffer)))

;;;###autoload
(defun nm-ui-wifi ()
  "Open NetworkManager WiFi browser."
  (interactive)
  (unless (nm-available-p)
    (error "NetworkManager service not available"))
  (let ((buffer (get-buffer-create nm-ui-wifi-buffer-name)))
    (with-current-buffer buffer
      (nm-ui-wifi-mode)
      (nm-ui-render-wifi-networks)
      (nm-ui-scan-wifi))
    (switch-to-buffer buffer)))

;;;###autoload
(defun nm-ui-connections ()
  "Open NetworkManager connections list."
  (interactive)
  (unless (nm-available-p)
    (error "NetworkManager service not available"))
  (let ((buffer (get-buffer-create "*NetworkManager Connections*")))
    (with-current-buffer buffer
      (nm-ui-mode)
      (use-local-map nm-ui-connections-mode-map)
      (nm-ui-render-connections))
    (switch-to-buffer buffer)))

(provide 'nm-ui)
;;; nm-ui.el ends here