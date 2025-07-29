;;; nm-connection.el --- Connection management for NetworkManager  -*- lexical-binding: t; -*-

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

;; Connection profile management functions for NetworkManager.

;;; Code:

(require 'nm)
(require 'nm-dbus)

(defun nm-list-connections ()
  "List all connection object paths."
  (nm-dbus-call-method nm-settings-path nm-dbus-interface-settings "ListConnections"))

(defun nm-get-connection-by-uuid (uuid)
  "Get connection object path by UUID."
  (nm-dbus-call-method nm-settings-path nm-dbus-interface-settings "GetConnectionByUuid" uuid))

(defun nm-reload-connections ()
  "Reload all connection files from disk."
  (nm-dbus-call-method nm-settings-path nm-dbus-interface-settings "ReloadConnections"))

(defun nm-save-hostname (hostname)
  "Save system HOSTNAME."
  (nm-dbus-call-method nm-settings-path nm-dbus-interface-settings "SaveHostname" hostname))

(defun nm-can-modify-p ()
  "Return t if connections can be modified."
  (nm-dbus-get-property nm-settings-path nm-dbus-interface-settings "CanModify"))

(defun nm-get-hostname ()
  "Get system hostname."
  (nm-dbus-get-property nm-settings-path nm-dbus-interface-settings "Hostname"))

(defun nm-add-connection (settings)
  "Add new connection with SETTINGS."
  (nm-dbus-call-method nm-settings-path nm-dbus-interface-settings "AddConnection" settings))

(defun nm-add-connection-unsaved (settings)
  "Add new unsaved connection with SETTINGS."
  (nm-dbus-call-method nm-settings-path nm-dbus-interface-settings "AddConnectionUnsaved" settings))

(defun nm-load-connections (filenames)
  "Load connections from FILENAMES."
  (nm-dbus-call-method nm-settings-path nm-dbus-interface-settings "LoadConnections" filenames))

(defun nm-connection-get-settings (connection-path)
  "Get settings for CONNECTION-PATH."
  (nm-dbus-call-method connection-path nm-dbus-interface-connection "GetSettings"))

(defun nm-connection-get-secrets (connection-path setting-name)
  "Get secrets for SETTING-NAME in CONNECTION-PATH."
  (nm-dbus-call-method connection-path nm-dbus-interface-connection "GetSecrets" setting-name))

(defun nm-connection-update (connection-path settings)
  "Update CONNECTION-PATH with new SETTINGS."
  (nm-dbus-call-method connection-path nm-dbus-interface-connection "Update" settings))

(defun nm-connection-update-unsaved (connection-path settings)
  "Update CONNECTION-PATH with new SETTINGS without saving."
  (nm-dbus-call-method connection-path nm-dbus-interface-connection "UpdateUnsaved" settings))

(defun nm-connection-save (connection-path)
  "Save CONNECTION-PATH to disk."
  (nm-dbus-call-method connection-path nm-dbus-interface-connection "Save"))

(defun nm-connection-delete (connection-path)
  "Delete CONNECTION-PATH."
  (nm-dbus-call-method connection-path nm-dbus-interface-connection "Delete"))

(defun nm-connection-clear-secrets (connection-path)
  "Clear secrets for CONNECTION-PATH."
  (nm-dbus-call-method connection-path nm-dbus-interface-connection "ClearSecrets"))

(defun nm-connection-get-filename (connection-path)
  "Get filename for CONNECTION-PATH."
  (nm-dbus-get-property connection-path nm-dbus-interface-connection "Filename"))

(defun nm-connection-get-flags (connection-path)
  "Get flags for CONNECTION-PATH."
  (nm-dbus-get-property connection-path nm-dbus-interface-connection "Flags"))

(defun nm-connection-unsaved-p (connection-path)
  "Return t if CONNECTION-PATH has unsaved changes."
  (nm-dbus-get-property connection-path nm-dbus-interface-connection "Unsaved"))

(defun nm-activate-connection (connection-path device-path specific-object)
  "Activate CONNECTION-PATH on DEVICE-PATH with SPECIFIC-OBJECT."
  (nm-dbus-call-method nm-path nm-dbus-interface-manager "ActivateConnection"
                       connection-path device-path specific-object))

(defun nm-add-and-activate-connection (settings device-path specific-object)
  "Add and activate connection with SETTINGS on DEVICE-PATH with SPECIFIC-OBJECT."
  (nm-dbus-call-method nm-path nm-dbus-interface-manager "AddAndActivateConnection"
                       settings device-path specific-object))

(defun nm-deactivate-connection (active-connection-path)
  "Deactivate ACTIVE-CONNECTION-PATH."
  (nm-dbus-call-method nm-path nm-dbus-interface-manager "DeactivateConnection"
                       active-connection-path))

(defun nm-get-active-connections ()
  "Get list of active connection paths."
  (nm-dbus-get-property nm-path nm-dbus-interface-manager "ActiveConnections"))

(defun nm-active-connection-get-connection (active-path)
  "Get connection path for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Connection"))

(defun nm-active-connection-get-specific-object (active-path)
  "Get specific object for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "SpecificObject"))

(defun nm-active-connection-get-id (active-path)
  "Get connection ID for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Id"))

(defun nm-active-connection-get-uuid (active-path)
  "Get connection UUID for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Uuid"))

(defun nm-active-connection-get-type (active-path)
  "Get connection type for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Type"))

(defun nm-active-connection-get-devices (active-path)
  "Get devices for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Devices"))

(defun nm-active-connection-get-state (active-path)
  "Get state for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "State"))

(defun nm-active-connection-get-state-flags (active-path)
  "Get state flags for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "StateFlags"))

(defun nm-active-connection-default-p (active-path)
  "Return t if ACTIVE-PATH is default for IPv4."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Default"))

(defun nm-active-connection-get-ip4-config (active-path)
  "Get IPv4 config path for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Ip4Config"))

(defun nm-active-connection-get-dhcp4-config (active-path)
  "Get DHCP4 config path for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Dhcp4Config"))

(defun nm-active-connection-default6-p (active-path)
  "Return t if ACTIVE-PATH is default for IPv6."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Default6"))

(defun nm-active-connection-get-ip6-config (active-path)
  "Get IPv6 config path for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Ip6Config"))

(defun nm-active-connection-get-dhcp6-config (active-path)
  "Get DHCP6 config path for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Dhcp6Config"))

(defun nm-active-connection-vpn-p (active-path)
  "Return t if ACTIVE-PATH is a VPN connection."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Vpn"))

(defun nm-active-connection-get-master (active-path)
  "Get master device path for ACTIVE-PATH."
  (nm-dbus-get-property active-path nm-dbus-interface-active "Master"))

(defun nm-connection-info (connection-path)
  "Get connection information for CONNECTION-PATH as alist."
  (let* ((settings (nm-connection-get-settings connection-path))
         (parsed (nm-dbus-connection-settings-to-alist settings))
         (connection (cdr (assoc "connection" parsed))))
    (list (cons 'path connection-path)
          (cons 'id (cdr (assoc "id" connection)))
          (cons 'uuid (cdr (assoc "uuid" connection)))
          (cons 'type (cdr (assoc "type" connection)))
          (cons 'autoconnect (cdr (assoc "autoconnect" connection)))
          (cons 'timestamp (cdr (assoc "timestamp" connection)))
          (cons 'filename (nm-connection-get-filename connection-path))
          (cons 'unsaved (nm-connection-unsaved-p connection-path)))))

(defun nm-connections-info ()
  "Get information for all connections."
  (mapcar #'nm-connection-info (nm-list-connections)))

(defun nm-active-connection-info (active-path)
  "Get active connection information for ACTIVE-PATH as alist."
  (list (cons 'path active-path)
        (cons 'connection (nm-active-connection-get-connection active-path))
        (cons 'id (nm-active-connection-get-id active-path))
        (cons 'uuid (nm-active-connection-get-uuid active-path))
        (cons 'type (nm-active-connection-get-type active-path))
        (cons 'state (nm-dbus-active-connection-state-to-string
                      (nm-active-connection-get-state active-path)))
        (cons 'devices (nm-active-connection-get-devices active-path))
        (cons 'default (nm-active-connection-default-p active-path))
        (cons 'default6 (nm-active-connection-default6-p active-path))
        (cons 'vpn (nm-active-connection-vpn-p active-path))))

(defun nm-active-connections-info ()
  "Get information for all active connections."
  (mapcar #'nm-active-connection-info (nm-get-active-connections)))

(defun nm-create-wifi-connection (ssid password &optional hidden)
  "Create WiFi connection settings for SSID with PASSWORD and optional HIDDEN."
  (let ((uuid (nm-generate-uuid)))
    (nm-dbus-alist-to-connection-settings
     `(("connection" . (("id" . ,ssid)
                        ("uuid" . ,uuid)
                        ("type" . "802-11-wireless")
                        ("autoconnect" . t)))
       ("802-11-wireless" . (("ssid" . ,(nm-dbus-string-to-ssid ssid))
                             ("mode" . "infrastructure")
                             ("hidden" . ,(if hidden t nil))))
       ("802-11-wireless-security" . (("key-mgmt" . "wpa-psk")
                                      ("psk" . ,password)))
       ("ipv4" . (("method" . "auto")))
       ("ipv6" . (("method" . "auto")))))))

(defun nm-create-ethernet-connection (name &optional static-ip)
  "Create ethernet connection settings with NAME and optional STATIC-IP."
  (let ((uuid (nm-generate-uuid)))
    (nm-dbus-alist-to-connection-settings
     `(("connection" . (("id" . ,name)
                        ("uuid" . ,uuid)
                        ("type" . "802-3-ethernet")
                        ("autoconnect" . t)))
       ("802-3-ethernet" . ())
       ("ipv4" . ,(if static-ip
                      `(("method" . "manual")
                        ("addresses" . ,(list static-ip)))
                    '(("method" . "auto"))))
       ("ipv6" . (("method" . "auto")))))))

(defun nm-generate-uuid ()
  "Generate UUID for connection."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior (random 16384) 16384)
          (logior (random 16384) 32768)
          (random 65536) (random 65536) (random 65536)))

(defun nm-connection-register-updated (connection-path handler)
  "Register HANDLER for updates on CONNECTION-PATH."
  (nm-dbus-register-signal connection-path nm-dbus-interface-connection "Updated" handler))

(defun nm-connection-register-removed (connection-path handler)
  "Register HANDLER for removal of CONNECTION-PATH."
  (nm-dbus-register-signal connection-path nm-dbus-interface-connection "Removed" handler))

(defun nm-connections-register-new (handler)
  "Register HANDLER for new connections."
  (nm-dbus-register-signal nm-settings-path nm-dbus-interface-settings "NewConnection" handler))

(defun nm-connections-register-removed (handler)
  "Register HANDLER for removed connections."
  (nm-dbus-register-signal nm-settings-path nm-dbus-interface-settings "ConnectionRemoved" handler))

(defun nm-connection-export (connection-path file)
  "Export connection at CONNECTION-PATH to FILE."
  (let ((settings (nm-connection-get-settings connection-path)))
    (with-temp-file file
      (insert ";; NetworkManager connection export\n")
      (insert ";; Created: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
      (pp settings (current-buffer)))
    (message "Exported connection to %s" file)))

(defun nm-connection-export-all (directory)
  "Export all connections to DIRECTORY."
  (interactive "DExport connections to directory: ")
  (let ((connections (nm-list-connections))
        (count 0))
    (dolist (conn-path connections)
      (let* ((settings (nm-connection-get-settings conn-path))
             (parsed (nm-dbus-connection-settings-to-alist settings))
             (conn-settings (cdr (assoc "connection" parsed)))
             (id (cdr (assoc "id" conn-settings)))
             (uuid (cdr (assoc "uuid" conn-settings)))
             (filename (expand-file-name
                        (format "%s_%s.nmconnection" id uuid)
                        directory)))
        (nm-connection-export conn-path filename)
        (setq count (1+ count))))
    (message "Exported %d connections to %s" count directory)))

(defun nm-connection-import (file)
  "Import connection from FILE."
  (interactive "fImport connection from file: ")
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (looking-at "^;;")
          (forward-line))
        (let ((settings (read (current-buffer))))
          (nm-add-connection settings)
          (message "Imported connection from %s" file)))
    (error
     (message "Failed to import connection: %s" (error-message-string err)))))

(defun nm-connection-import-directory (directory)
  "Import all connection files from DIRECTORY."
  (interactive "DImport connections from directory: ")
  (let ((files (directory-files directory t "\\.nmconnection$"))
        (count 0)
        (failed 0))
    (dolist (file files)
      (condition-case nil
          (progn
            (nm-connection-import file)
            (setq count (1+ count)))
        (error (setq failed (1+ failed)))))
    (message "Imported %d connections, %d failed" count failed)))

(defun nm-connection-to-keyfile (connection-path)
  "Convert connection at CONNECTION-PATH to NetworkManager keyfile format."
  (let* ((settings (nm-connection-get-settings connection-path))
         (parsed (nm-dbus-connection-settings-to-alist settings))
         (lines '()))
    (dolist (group parsed)
      (let ((group-name (car group))
            (group-settings (cdr group)))
        (push (format "[%s]" group-name) lines)
        (dolist (setting group-settings)
          (let ((key (car setting))
                (value (cdr setting)))
            (cond
             ((stringp value)
              (push (format "%s=%s" key value) lines))
             ((numberp value)
              (push (format "%s=%s" key value) lines))
             ((booleanp value)
              (push (format "%s=%s" key (if value "true" "false")) lines))
             ((listp value)
              (when value
                (push (format "%s=%s" key (mapconcat #'prin1-to-string value ";")) lines))))))
        (push "" lines)))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun nm-connection-export-keyfile (connection-path file)
  "Export connection at CONNECTION-PATH to FILE in keyfile format."
  (let ((content (nm-connection-to-keyfile connection-path)))
    (with-temp-file file
      (insert content))
    (message "Exported connection to %s (keyfile format)" file)))

(provide 'nm-connection)
;;; nm-connection.el ends here