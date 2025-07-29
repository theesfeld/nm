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
(require 'widget)
(require 'wid-edit)
(require 'tabulated-list)

(declare-function nm-show-help "nm" ())
(declare-function nm-reload "nm" (&optional flags))
(declare-function nm-generate-uuid "nm-connection" ())

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



(defun nm-ui-scan-wifi ()
  "Scan for WiFi networks."
  (interactive)
  (nm-wifi-scan-all
   (lambda () (with-current-buffer nm-ui-wifi-buffer-name
                (nm-ui-refresh))))
  (message "Scanning for WiFi networks..."))


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


(define-derived-mode nm-ui-connection-mode special-mode "NM-Connection"
  "Major mode for NetworkManager connection editor.")





(defun nm-ui-new-connection ()
  "Create a new connection."
  (interactive)
  (nm-ui-edit-connection-form nil))

(defun nm-ui-create-form-widgets (settings)
  "Create basic form widgets for connection SETTINGS."
  (let* ((conn-settings (cdr (assoc "connection" settings)))
         (name-widget (widget-create 'editable-field
                                     :size 30
                                     :format "Name: %v\n"
                                     :value (or (cdr (assoc "id" conn-settings)) "")))
         (type-widget (widget-create 'menu-choice
                                     :tag "Type"
                                     :value (or (cdr (assoc "type" conn-settings)) "802-3-ethernet")
                                     :notify (lambda (widget &rest ignore)
                                               (nm-ui-update-connection-form widget))
                                     '(item :tag "Ethernet" :value "802-3-ethernet")
                                     '(item :tag "WiFi" :value "802-11-wireless")
                                     '(item :tag "VPN (OpenVPN)" :value "vpn")))
         (autoconnect-widget (widget-create 'checkbox
                                            :value (if conn-settings
                                                       (cdr (assoc "autoconnect" conn-settings))
                                                     t))))
    (list name-widget type-widget autoconnect-widget)))

(defun nm-ui-create-wifi-widgets (settings)
  "Create WiFi-specific widgets for SETTINGS."
  (let ((wifi-settings (cdr (assoc "802-11-wireless" settings))))
    (list (cons 'ssid (widget-create 'editable-field
                                     :size 30
                                     :format "SSID: %v\n"
                                     :value (or (when wifi-settings
                                                  (nm-dbus-ssid-to-string
                                                   (cdr (assoc "ssid" wifi-settings))))
                                                "")))
          (cons 'password (widget-create 'password-field
                                         :size 30
                                         :format "Password: %v\n"
                                         :value "")))))

(defun nm-ui-create-vpn-widgets ()
  "Create VPN-specific widgets."
  (list (cons 'vpn-type (widget-create 'menu-choice
                                       :tag "VPN Type"
                                       :value "openvpn"
                                       '(item :tag "OpenVPN" :value "openvpn")
                                       '(item :tag "WireGuard" :value "wireguard")
                                       '(item :tag "L2TP" :value "l2tp")))))

(defun nm-ui-setup-connection-form-buffer (connection)
  "Setup connection form buffer for CONNECTION."
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert (if connection
                     (format "Edit Connection: %s\n\n" (cdr (assoc 'id connection)))
                   "New Connection\n\n")))

(defun nm-ui-edit-connection-form (connection)
  "Edit CONNECTION or create new if nil."
  (let ((buffer (get-buffer-create "*NetworkManager Connection Editor*")))
    (with-current-buffer buffer
      (nm-ui-setup-connection-form-buffer connection)
      
      (let* ((settings (when connection
                         (nm-dbus-connection-settings-to-alist
                          (nm-connection-get-settings (cdr (assoc 'path connection))))))
             (basic-widgets (nm-ui-create-form-widgets settings))
             (name-widget (nth 0 basic-widgets))
             (type-widget (nth 1 basic-widgets))
             (autoconnect-widget (nth 2 basic-widgets))
             extra-widgets)
        
        (widget-insert "\n")
        (widget-insert "Autoconnect: ")
        (widget-insert "\n\n")
        
        (cond
         ((equal (widget-value type-widget) "802-11-wireless")
          (setq extra-widgets (nm-ui-create-wifi-widgets settings)))
         ((equal (widget-value type-widget) "vpn")
          (setq extra-widgets (nm-ui-create-vpn-widgets))))
        
        (widget-insert "\n")
        (widget-create 'push-button
                       :notify (lambda (&rest ignore)
                                 (nm-ui-save-connection connection
                                                        name-widget
                                                        type-widget
                                                        autoconnect-widget
                                                        extra-widgets))
                       "Save")
        (widget-insert " ")
        (widget-create 'push-button
                       :notify (lambda (&rest ignore)
                                 (kill-buffer))
                       "Cancel")
        
        (use-local-map widget-keymap)
        (widget-setup)
        (goto-char (point-min))
        (widget-forward 1)))
    (switch-to-buffer buffer)))

(defun nm-ui-extract-widget-values (name-widget type-widget autoconnect-widget extra-widgets)
  "Extract values from connection form widgets."
  (list :name (widget-value name-widget)
        :type (widget-value type-widget)
        :autoconnect (widget-value autoconnect-widget)
        :extra extra-widgets))

(defun nm-ui-build-connection-settings (values uuid)
  "Build connection settings from VALUES and UUID."
  (let ((settings `(("connection" . (("id" . ,(plist-get values :name))
                                     ("uuid" . ,uuid)
                                     ("type" . ,(plist-get values :type))
                                     ("autoconnect" . ,(plist-get values :autoconnect)))))))
    (nm-ui-add-type-specific-settings settings values)
    (push '("ipv4" . (("method" . "auto"))) settings)
    (push '("ipv6" . (("method" . "auto"))) settings)
    settings))

(defun nm-ui-add-type-specific-settings (settings values)
  "Add type-specific settings to SETTINGS based on VALUES."
  (let ((type (plist-get values :type))
        (extra-widgets (plist-get values :extra)))
    (cond
     ((equal type "802-11-wireless")
      (nm-ui-add-wifi-settings settings extra-widgets (plist-get values :name)))
     ((equal type "vpn")
      (nm-ui-add-vpn-settings settings extra-widgets)))))

(defun nm-ui-add-wifi-settings (settings extra-widgets connection-name)
  "Add WiFi settings to SETTINGS from EXTRA-WIDGETS for CONNECTION-NAME."
  (let ((ssid (widget-value (cdr (assoc 'ssid extra-widgets))))
        (password (widget-value (cdr (assoc 'password extra-widgets)))))
    (push `("802-11-wireless" . (("ssid" . ,(nm-dbus-string-to-ssid ssid))
                                  ("mode" . "infrastructure")))
          settings)
    (when (and password (> (length password) 0))
      (push `("802-11-wireless-security" . (("key-mgmt" . "wpa-psk")))
            settings)
      (nm-ui-handle-wifi-password connection-name password extra-widgets))))

(defun nm-ui-handle-wifi-password (connection-name password extra-widgets)
  "Handle WiFi PASSWORD for CONNECTION-NAME and clear from EXTRA-WIDGETS."
  (require 'nm-secrets)
  (when nm-secrets-use-auth-source
    (nm-secrets-save-to-auth-source connection-name "psk" password))
  (widget-value-set (cdr (assoc 'password extra-widgets)) "")
  (nm-secrets-clear-string password))

(defun nm-ui-add-vpn-settings (settings extra-widgets)
  "Add VPN settings to SETTINGS from EXTRA-WIDGETS."
  (let ((vpn-type (widget-value (cdr (assoc 'vpn-type extra-widgets)))))
    (push `("vpn" . (("service-type" . ,(format "org.freedesktop.NetworkManager.%s" vpn-type))))
          settings)))

(defun nm-ui-save-connection (connection name-widget type-widget autoconnect-widget extra-widgets)
  "Save connection with values from widgets."
  (let* ((values (nm-ui-extract-widget-values name-widget type-widget autoconnect-widget extra-widgets))
         (uuid (or (when connection (cdr (assoc 'uuid connection)))
                   (nm-generate-uuid)))
         (settings (nm-ui-build-connection-settings values uuid))
         (name (plist-get values :name)))
    (condition-case err
        (if connection
            (progn
              (nm-connection-update (cdr (assoc 'path connection))
                                    (nm-dbus-alist-to-connection-settings settings))
              (message "Updated connection: %s" name))
          (nm-add-connection (nm-dbus-alist-to-connection-settings settings))
          (message "Created connection: %s" name))
      (error (message "Error saving connection: %s" (error-message-string err))))
    (kill-buffer)
    (nm-ui-refresh)))

(defun nm-ui-update-connection-form (widget)
  "Update connection form based on type selection."
  (let ((type (widget-value widget)))
    (message "Connection type changed to: %s" type)))


(provide 'nm-ui)
;;; nm-ui.el ends here