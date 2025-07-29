;;; nm-ui-tabulated.el --- Tabulated list UI for NetworkManager  -*- lexical-binding: t; -*-

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

;; Tabulated list modes for NetworkManager UI.

;;; Code:

(require 'tabulated-list)
(require 'nm)
(require 'nm-wifi)
(require 'nm-device)
(require 'nm-connection)
(require 'nm-ui)

(defvar nm-ui-wifi-list-format
  [("" 5 nil) ;; Signal bars
   ("SSID" 25 t)
   ("Channel" 8 t :right-align t)
   ("Security" 15 t)
   ("Strength" 10 t :right-align t)
   ("BSSID" 18 t)]
  "Format for WiFi list columns.")

(defvar nm-ui-connections-list-format
  [("Name" 25 t)
   ("Type" 15 t)
   ("Status" 12 t)
   ("Auto" 6 t)
   ("Device" 15 t)]
  "Format for connections list columns.")

(defvar nm-ui-devices-list-format
  [("Interface" 15 t)
   ("Type" 12 t)
   ("State" 15 t)
   ("Driver" 15 t)
   ("Managed" 8 t)]
  "Format for devices list columns.")

(define-derived-mode nm-ui-wifi-list-mode tabulated-list-mode "NM-WiFi"
  "Major mode for WiFi network list."
  (setq tabulated-list-format nm-ui-wifi-list-format)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Strength" . t))
  (add-hook 'tabulated-list-revert-hook #'nm-ui-wifi-list-refresh nil t)
  (tabulated-list-init-header))

(defvar nm-ui-wifi-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'nm-ui-wifi-list-connect)
    (define-key map "c" #'nm-ui-wifi-list-connect)
    (define-key map "d" #'nm-ui-disconnect)
    (define-key map "f" #'nm-ui-wifi-list-forget)
    (define-key map "s" #'nm-ui-scan-wifi)
    (define-key map "g" #'nm-ui-wifi-list-refresh)
    (define-key map "n" #'nm-toggle-networking)
    (define-key map "w" #'nm-toggle-wireless)
    (define-key map "?" #'nm-show-help)
    map)
  "Keymap for WiFi list mode.")

(defun nm-ui-wifi-list-refresh ()
  "Refresh WiFi network list."
  (let ((networks (nm-wifi-get-all-access-points))
        (current-conn (nm-wifi-get-current-connection))
        (current-ssid (when current-conn (cdr (assoc 'id current-conn))))
        entries)
    (dolist (network networks)
      (when-let ((ssid (cdr (assoc 'ssid network))))
        (unless (string-empty-p ssid)
          (let* ((strength (cdr (assoc 'strength network)))
                 (security (cdr (assoc 'security network)))
                 (channel (cdr (assoc 'channel network)))
                 (bssid (cdr (assoc 'bssid network)))
                 (bars (nm-wifi-strength-bars strength))
                 (id network)
                 (ssid-display (if (equal ssid current-ssid)
                                   (propertize ssid 'face 'success)
                                 ssid)))
            (push (list id
                        (vector bars
                                ssid-display
                                (format "%d" channel)
                                security
                                (format "%d%%" strength)
                                bssid))
                  entries)))))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-print t)))

(defun nm-ui-wifi-list-connect ()
  "Connect to WiFi network at point."
  (interactive)
  (when-let ((network (tabulated-list-get-id)))
    (nm-ui-connect-to-network-internal network)))

(defun nm-ui-connect-to-network-internal (network)
  "Internal function to connect to NETWORK."
  (let ((ssid (cdr (assoc 'ssid network)))
        (security (cdr (assoc 'security network))))
    (if (string= security "Open")
        (nm-wifi-connect-to-ap network)
      (require 'nm-secrets)
      (let ((saved-password (nm-secrets-get-from-auth-source ssid "psk")))
        (if saved-password
            (progn
              (nm-wifi-connect-to-ap network saved-password)
              (nm-secrets-clear-string saved-password))
          (nm-ui-password-prompt
           (format "Password for %s: " ssid)
           (lambda (password)
             (nm-wifi-connect-to-ap network password)
             (when (y-or-n-p "Save password? ")
               (nm-secrets-save-to-auth-source ssid "psk" password))
             (nm-secrets-clear-string password))))))))

(defun nm-ui-wifi-list-forget ()
  "Forget WiFi network at point."
  (interactive)
  (when-let* ((network (tabulated-list-get-id))
              (ssid (cdr (assoc 'ssid network))))
    (when (yes-or-no-p (format "Forget network '%s'? " ssid))
      (nm-wifi-forget-network ssid)
      (nm-ui-wifi-list-refresh))))

;;;###autoload
(defun nm-ui-wifi-list ()
  "Open WiFi network list in tabulated mode."
  (interactive)
  (unless (nm-available-p)
    (error "NetworkManager service not available"))
  (let ((buffer (get-buffer-create "*NetworkManager WiFi List*")))
    (with-current-buffer buffer
      (nm-ui-wifi-list-mode)
      (nm-ui-wifi-list-refresh)
      (nm-ui-scan-wifi))
    (switch-to-buffer buffer)))

(define-derived-mode nm-ui-connections-list-mode tabulated-list-mode "NM-Connections"
  "Major mode for connection list."
  (setq tabulated-list-format nm-ui-connections-list-format)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Name" . nil))
  (add-hook 'tabulated-list-revert-hook #'nm-ui-connections-list-refresh nil t)
  (tabulated-list-init-header))

(defvar nm-ui-connections-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'nm-ui-connections-list-activate)
    (define-key map "a" #'nm-ui-connections-list-activate)
    (define-key map "d" #'nm-ui-connections-list-deactivate)
    (define-key map "e" #'nm-ui-connections-list-edit)
    (define-key map "D" #'nm-ui-connections-list-delete)
    (define-key map "+" #'nm-ui-new-connection)
    (define-key map "g" #'nm-ui-connections-list-refresh)
    (define-key map "?" #'nm-show-help)
    map)
  "Keymap for connections list mode.")

(defun nm-ui-connections-list-refresh ()
  "Refresh connections list."
  (let ((connections (nm-connections-info))
        (active-conns (nm-active-connections-info))
        entries)
    (dolist (conn connections)
      (let* ((id (cdr (assoc 'id conn)))
             (uuid (cdr (assoc 'uuid conn)))
             (type (cdr (assoc 'type conn)))
             (auto (if (cdr (assoc 'autoconnect conn)) "Yes" "No"))
             (active (seq-find (lambda (ac)
                                 (equal (cdr (assoc 'uuid ac)) uuid))
                               active-conns))
             (status (if active "Active" ""))
             (device (if active
                         (let ((devices (cdr (assoc 'devices active))))
                           (if devices
                               (nm-device-get-interface (car devices))
                             ""))
                       "")))
        (push (list conn
                    (vector (if active
                                (propertize id 'face 'success)
                              id)
                            type
                            status
                            auto
                            device))
              entries)))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-print t)))

(defun nm-ui-connections-list-activate ()
  "Activate connection at point."
  (interactive)
  (when-let ((conn (tabulated-list-get-id)))
    (nm-activate-connection (cdr (assoc 'path conn)) "/" "/")
    (message "Activating connection: %s" (cdr (assoc 'id conn)))
    (run-at-time 1 nil #'nm-ui-connections-list-refresh)))

(defun nm-ui-connections-list-deactivate ()
  "Deactivate connection at point."
  (interactive)
  (when-let ((conn (tabulated-list-get-id)))
    (nm-ui-deactivate-connection-internal conn)
    (run-at-time 1 nil #'nm-ui-connections-list-refresh)))

(defun nm-ui-deactivate-connection-internal (connection)
  "Internal function to deactivate CONNECTION."
  (let ((active-conns (nm-active-connections-info)))
    (when-let ((active (seq-find (lambda (ac)
                                   (equal (cdr (assoc 'uuid ac))
                                          (cdr (assoc 'uuid connection))))
                                 active-conns)))
      (nm-deactivate-connection (cdr (assoc 'path active)))
      (message "Deactivated connection: %s" (cdr (assoc 'id connection))))))

(defun nm-ui-connections-list-edit ()
  "Edit connection at point."
  (interactive)
  (when-let ((conn (tabulated-list-get-id)))
    (nm-ui-edit-connection-form conn)))

(defun nm-ui-connections-list-delete ()
  "Delete connection at point."
  (interactive)
  (when-let ((conn (tabulated-list-get-id)))
    (when (yes-or-no-p (format "Delete connection '%s'? " (cdr (assoc 'id conn))))
      (nm-connection-delete (cdr (assoc 'path conn)))
      (message "Deleted connection: %s" (cdr (assoc 'id conn)))
      (nm-ui-connections-list-refresh))))

;;;###autoload
(defun nm-ui-connections-list ()
  "Open connections list in tabulated mode."
  (interactive)
  (unless (nm-available-p)
    (error "NetworkManager service not available"))
  (let ((buffer (get-buffer-create "*NetworkManager Connections List*")))
    (with-current-buffer buffer
      (nm-ui-connections-list-mode)
      (nm-ui-connections-list-refresh))
    (switch-to-buffer buffer)))

(define-derived-mode nm-ui-devices-list-mode tabulated-list-mode "NM-Devices"
  "Major mode for device list."
  (setq tabulated-list-format nm-ui-devices-list-format)
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'nm-ui-devices-list-refresh nil t)
  (tabulated-list-init-header))

(defvar nm-ui-devices-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'nm-ui-devices-list-show-details)
    (define-key map "m" #'nm-ui-devices-list-toggle-managed)
    (define-key map "a" #'nm-ui-devices-list-toggle-autoconnect)
    (define-key map "d" #'nm-ui-devices-list-disconnect)
    (define-key map "g" #'nm-ui-devices-list-refresh)
    (define-key map "?" #'nm-show-help)
    map)
  "Keymap for devices list mode.")

(defun nm-ui-devices-list-refresh ()
  "Refresh devices list."
  (let ((devices (nm-devices-info))
        entries)
    (dolist (device devices)
      (let* ((interface (cdr (assoc 'interface device)))
             (type (cdr (assoc 'type device)))
             (state (cdr (assoc 'state device)))
             (driver (or (cdr (assoc 'driver device)) "N/A"))
             (managed (if (cdr (assoc 'managed device)) "Yes" "No")))
        (push (list device
                    (vector interface
                            type
                            state
                            driver
                            managed))
              entries)))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-print t)))

(defun nm-ui-devices-list-show-details ()
  "Show details for device at point."
  (interactive)
  (when-let ((device (tabulated-list-get-id)))
    (message "Device: %s | Type: %s | State: %s | Driver: %s | Managed: %s"
             (cdr (assoc 'interface device))
             (cdr (assoc 'type device))
             (cdr (assoc 'state device))
             (or (cdr (assoc 'driver device)) "N/A")
             (if (cdr (assoc 'managed device)) "Yes" "No"))))

(defun nm-ui-devices-list-toggle-managed ()
  "Toggle managed state for device at point."
  (interactive)
  (when-let ((device (tabulated-list-get-id)))
    (let ((current (cdr (assoc 'managed device))))
      (nm-device-set-managed (cdr (assoc 'path device)) (not current))
      (message "Device %s is now %s"
               (cdr (assoc 'interface device))
               (if current "unmanaged" "managed"))
      (nm-ui-devices-list-refresh))))

(defun nm-ui-devices-list-toggle-autoconnect ()
  "Toggle autoconnect for device at point."
  (interactive)
  (when-let ((device (tabulated-list-get-id)))
    (let ((current (cdr (assoc 'autoconnect device))))
      (nm-device-set-autoconnect (cdr (assoc 'path device)) (not current))
      (message "Device %s autoconnect %s"
               (cdr (assoc 'interface device))
               (if current "disabled" "enabled"))
      (nm-ui-devices-list-refresh))))

(defun nm-ui-devices-list-disconnect ()
  "Disconnect device at point."
  (interactive)
  (when-let ((device (tabulated-list-get-id)))
    (condition-case err
        (progn
          (nm-device-disconnect (cdr (assoc 'path device)))
          (message "Device disconnected")
          (run-at-time 1 nil #'nm-ui-devices-list-refresh))
      (error (message "Error disconnecting device: %s" (error-message-string err))))))

;;;###autoload
(defun nm-ui-devices-list ()
  "Open devices list in tabulated mode."
  (interactive)
  (unless (nm-available-p)
    (error "NetworkManager service not available"))
  (let ((buffer (get-buffer-create "*NetworkManager Devices List*")))
    (with-current-buffer buffer
      (nm-ui-devices-list-mode)
      (nm-ui-devices-list-refresh))
    (switch-to-buffer buffer)))

(provide 'nm-ui-tabulated)
;;; nm-ui-tabulated.el ends here