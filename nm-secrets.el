;;; nm-secrets.el --- NetworkManager secrets management  -*- lexical-binding: t; -*-

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

;; Secure handling of NetworkManager secrets and authentication.
;; This module ensures passwords are never stored in Emacs memory
;; longer than necessary and uses NetworkManager's secret agent API.

;;; Code:

(require 'nm)
(require 'nm-dbus)
(require 'auth-source)

(defgroup nm-secrets nil
  "NetworkManager secrets management."
  :group 'nm)

(defcustom nm-secrets-use-auth-source t
  "Whether to use auth-source for storing secrets."
  :type 'boolean
  :group 'nm-secrets)

(defcustom nm-secrets-auth-source-host "NetworkManager"
  "Host name to use in auth-source entries."
  :type 'string
  :group 'nm-secrets)

(defvar nm-secrets-agent-registered nil
  "Whether we've registered as a secret agent.")

(defconst nm-dbus-interface-secret-agent
  "org.freedesktop.NetworkManager.SecretAgent"
  "D-Bus interface for secret agent.")

(defun nm-secrets-clear-string (string)
  "Clear STRING from memory by overwriting it."
  (when (stringp string)
    (dotimes (i (length string))
      (aset string i ?\0))))

(defun nm-secrets-get-from-auth-source (connection-id setting-name)
  "Get secret from auth-source for CONNECTION-ID and SETTING-NAME."
  (when nm-secrets-use-auth-source
    (let ((result (auth-source-search
                   :host nm-secrets-auth-source-host
                   :user connection-id
                   :port setting-name
                   :max 1)))
      (when result
        (let ((secret (plist-get (car result) :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))))))

(defun nm-secrets-save-to-auth-source (connection-id setting-name secret)
  "Save SECRET to auth-source for CONNECTION-ID and SETTING-NAME."
  (when nm-secrets-use-auth-source
    (auth-source-forget-all-cached)
    (let ((auth-source-creation-prompts
           '((secret . "Password: "))))
      (auth-source-search
       :host nm-secrets-auth-source-host
       :user connection-id
       :port setting-name
       :secret secret
       :create t))))

(defun nm-secrets-get-secrets (connection-path connection-settings hints)
  "Get secrets for CONNECTION-PATH with SETTINGS and HINTS.
This is called by NetworkManager when it needs authentication."
  (let* ((conn-data (cdr (assoc "connection" connection-settings)))
         (conn-id (cdr (assoc "id" conn-data)))
         (conn-type (cdr (assoc "type" conn-data)))
         secrets)
    
    ;; Check auth-source first
    (pcase conn-type
      ("802-11-wireless"
       (let ((saved-psk (nm-secrets-get-from-auth-source conn-id "psk")))
         (if saved-psk
             (setq secrets `(("802-11-wireless-security" . (("psk" . ,saved-psk)))))
           ;; Prompt for password
           (let ((psk (read-passwd (format "WiFi password for %s: " conn-id))))
             (when (and psk (> (length psk) 0))
               (setq secrets `(("802-11-wireless-security" . (("psk" . ,psk)))))
               ;; Optionally save to auth-source
               (when (y-or-n-p "Save password? ")
                 (nm-secrets-save-to-auth-source conn-id "psk" psk)))
             ;; Clear password from memory
             (nm-secrets-clear-string psk)))))
      
      ("vpn"
       (let ((vpn-secrets (nm-secrets-get-vpn-secrets conn-id hints)))
         (when vpn-secrets
           (setq secrets `(("vpn" . ,vpn-secrets))))))
      
      (_
       (message "Unknown connection type for secrets: %s" conn-type)))
    
    secrets))

(defun nm-secrets-get-vpn-secrets (connection-id hints)
  "Get VPN secrets for CONNECTION-ID with HINTS."
  (let (secrets)
    (dolist (hint hints)
      (let* ((secret-name (if (string-match "\\." hint)
                              (substring hint (1+ (string-match "\\." hint)))
                            hint))
             (prompt (format "%s for %s: " secret-name connection-id))
             (saved (nm-secrets-get-from-auth-source connection-id hint)))
        (if saved
            (push (cons hint saved) secrets)
          (let ((value (read-passwd prompt)))
            (when (and value (> (length value) 0))
              (push (cons hint value) secrets)
              (when (y-or-n-p (format "Save %s? " secret-name))
                (nm-secrets-save-to-auth-source connection-id hint value)))
            (nm-secrets-clear-string value)))))
    secrets))

(defun nm-secrets-register-agent ()
  "Register as NetworkManager secret agent."
  (unless nm-secrets-agent-registered
    (condition-case err
        (progn
          (nm-dbus-call-method
           nm-dbus-agent-manager-path
           nm-dbus-interface-agent-manager
           "Register"
           "org.freedesktop.nm-applet")
          (setq nm-secrets-agent-registered t)
          (message "Registered as NetworkManager secret agent"))
      (error
       (message "Failed to register secret agent: %s" err)))))

(defun nm-secrets-unregister-agent ()
  "Unregister as NetworkManager secret agent."
  (when nm-secrets-agent-registered
    (condition-case nil
        (nm-dbus-call-method
         nm-dbus-agent-manager-path
         nm-dbus-interface-agent-manager
         "Unregister")
      (error nil))
    (setq nm-secrets-agent-registered nil)))

(defun nm-secrets-strip-from-settings (settings)
  "Remove all secrets from SETTINGS for safe storage."
  (let ((clean-settings (copy-tree settings)))
    ;; Remove WiFi passwords
    (when-let ((wifi-sec (assoc "802-11-wireless-security" clean-settings)))
      (setcdr (assoc "psk" (cdr wifi-sec)) nil)
      (setcdr (assoc "wep-key0" (cdr wifi-sec)) nil)
      (setcdr (assoc "wep-key1" (cdr wifi-sec)) nil)
      (setcdr (assoc "wep-key2" (cdr wifi-sec)) nil)
      (setcdr (assoc "wep-key3" (cdr wifi-sec)) nil))
    
    ;; Remove VPN secrets
    (when-let ((vpn (assoc "vpn" clean-settings)))
      (let ((vpn-data (cdr vpn)))
        (dolist (item vpn-data)
          (when (and (consp item)
                     (stringp (car item))
                     (or (string-match "-password$" (car item))
                         (string-match "-secret$" (car item))
                         (string-match "-key$" (car item))))
            (setcdr item nil)))))
    
    ;; Remove 802.1x secrets
    (when-let ((ieee (assoc "802-1x" clean-settings)))
      (setcdr (assoc "password" (cdr ieee)) nil)
      (setcdr (assoc "private-key-password" (cdr ieee)) nil))
    
    clean-settings))

(defun nm-secrets-prompt-if-needed (connection-settings)
  "Prompt for secrets if needed for CONNECTION-SETTINGS.
Returns settings with secrets included temporarily."
  (let* ((settings (copy-tree connection-settings))
         (conn-type (cdr (assoc "type" (cdr (assoc "connection" settings))))))
    
    (pcase conn-type
      ("802-11-wireless"
       (when-let ((sec (assoc "802-11-wireless-security" settings)))
         (unless (cdr (assoc "psk" (cdr sec)))
           (let ((psk (read-passwd "WiFi password: ")))
             (setcdr (assoc "psk" (cdr sec)) psk)))))
      
      ("vpn"
       ;; VPN secrets are handled by the secret agent
       nil)
      
      (_
       nil))
    
    settings))

(provide 'nm-secrets)
;;; nm-secrets.el ends here