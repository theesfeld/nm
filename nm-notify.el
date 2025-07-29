;;; nm-notify.el --- NetworkManager notifications  -*- lexical-binding: t; -*-

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

;; Notification support for NetworkManager state changes.

;;; Code:

(require 'nm)
(require 'nm-dbus)
(require 'notifications nil t)

(defgroup nm-notify nil
  "NetworkManager notifications."
  :group 'nm)

(defcustom nm-notify-enabled t
  "Whether to show NetworkManager notifications."
  :type 'boolean
  :group 'nm-notify)

(defcustom nm-notify-use-notifications-lib t
  "Use notifications library for desktop notifications."
  :type 'boolean
  :group 'nm-notify)

(defcustom nm-notify-connect-message "Connected to %s"
  "Message format for connection notifications."
  :type 'string
  :group 'nm-notify)

(defcustom nm-notify-disconnect-message "Disconnected from %s"
  "Message format for disconnection notifications."
  :type 'string
  :group 'nm-notify)

(defcustom nm-notify-state-change-message "Network state: %s"
  "Message format for state change notifications."
  :type 'string
  :group 'nm-notify)

(defcustom nm-notify-vpn-connect-message "VPN connected: %s"
  "Message format for VPN connection notifications."
  :type 'string
  :group 'nm-notify)

(defcustom nm-notify-vpn-disconnect-message "VPN disconnected: %s"
  "Message format for VPN disconnection notifications."
  :type 'string
  :group 'nm-notify)

(defvar nm-notify-handlers nil
  "List of registered notification handlers.")

(defvar nm-notify-last-state nil
  "Last known NetworkManager state.")

(defvar nm-notify-last-connectivity nil
  "Last known connectivity state.")

(defun nm-notify-show (title body &optional urgency)
  "Show notification with TITLE and BODY at URGENCY level."
  (when nm-notify-enabled
    (cond
     ((and nm-notify-use-notifications-lib
           (fboundp 'notifications-notify))
      (notifications-notify
       :title title
       :body body
       :urgency (or urgency 'normal)
       :app-name "NetworkManager"))
     (t
      (message "%s: %s" title body)))))

(defun nm-notify-state-changed (new-state old-state reason)
  "Handle state change from OLD-STATE to NEW-STATE with REASON."
  (when (and nm-notify-enabled
             (not (equal new-state old-state)))
    (let ((new-state-str (nm-state-to-string new-state))
          (old-state-str (nm-state-to-string old-state)))
      (nm-notify-show "NetworkManager"
                      (format nm-notify-state-change-message new-state-str)
                      (if (< new-state old-state) 'critical 'normal))
      (setq nm-notify-last-state new-state))))

(defun nm-notify-connectivity-changed (connectivity)
  "Handle connectivity change to CONNECTIVITY."
  (when (and nm-notify-enabled
             (not (equal connectivity nm-notify-last-connectivity)))
    (let ((conn-str (nm-connectivity-to-string connectivity))
          (urgency (if (< connectivity 4) 'critical 'normal)))
      (nm-notify-show "Network Connectivity"
                      conn-str
                      urgency)
      (setq nm-notify-last-connectivity connectivity))))

(defun nm-notify-connection-activated (connection-path)
  "Handle connection activation for CONNECTION-PATH."
  (when nm-notify-enabled
    (condition-case nil
        (let* ((active-info (nm-active-connection-info connection-path))
               (id (cdr (assoc 'id active-info)))
               (type (cdr (assoc 'type active-info)))
               (vpn-p (cdr (assoc 'vpn active-info))))
          (if vpn-p
              (nm-notify-show "VPN Connected"
                              (format nm-notify-vpn-connect-message id))
            (nm-notify-show "Network Connected"
                            (format nm-notify-connect-message id))))
      (error nil))))

(defun nm-notify-connection-deactivated (connection-path)
  "Handle connection deactivation for CONNECTION-PATH."
  (when nm-notify-enabled
    (condition-case nil
        (let* ((conn-info (nm-connection-info connection-path))
               (id (cdr (assoc 'id conn-info)))
               (type (cdr (assoc 'type conn-info))))
          (if (string-match "vpn" type)
              (nm-notify-show "VPN Disconnected"
                              (format nm-notify-vpn-disconnect-message id))
            (nm-notify-show "Network Disconnected"
                            (format nm-notify-disconnect-message id))))
      (error nil))))

(defun nm-notify-device-state-changed (device-path new-state old-state reason)
  "Handle device state change for DEVICE-PATH from OLD-STATE to NEW-STATE with REASON."
  (when (and nm-notify-enabled
             (not (equal new-state old-state)))
    (condition-case nil
        (let* ((device-info (nm-device-info device-path))
               (interface (cdr (assoc 'interface device-info)))
               (type (cdr (assoc 'type device-info)))
               (new-state-str (nm-dbus-device-state-to-string new-state))
               (old-state-str (nm-dbus-device-state-to-string old-state)))
          (when (or (and (= old-state 30) (= new-state 100))
                    (and (= old-state 100) (= new-state 30)))
            (nm-notify-show (format "%s %s" (capitalize type) interface)
                            (format "State: %s" new-state-str)
                            (if (< new-state old-state) 'critical 'normal))))
      (error nil))))

(defun nm-notify-start ()
  "Start NetworkManager notifications."
  (interactive)
  (setq nm-notify-enabled t)
  (nm-notify-register-handlers)
  (setq nm-notify-last-state (nm-get-state))
  (setq nm-notify-last-connectivity (nm-get-connectivity))
  (message "NetworkManager notifications enabled"))

(defun nm-notify-stop ()
  "Stop NetworkManager notifications."
  (interactive)
  (setq nm-notify-enabled nil)
  (nm-notify-unregister-handlers)
  (message "NetworkManager notifications disabled"))

(defun nm-notify-register-handlers ()
  "Register D-Bus signal handlers for notifications."
  (unless nm-notify-handlers
    (push (nm-dbus-register-signal nm-path nm-dbus-interface-manager
                                   "StateChanged"
                                   #'nm-notify-state-changed)
          nm-notify-handlers)
    (push (nm-dbus-register-signal nm-path nm-dbus-interface-manager
                                   "CheckPermissions"
                                   (lambda () (nm-notify-connectivity-changed
                                               (nm-get-connectivity))))
          nm-notify-handlers)))

(defun nm-notify-unregister-handlers ()
  "Unregister D-Bus signal handlers."
  (dolist (handler nm-notify-handlers)
    (dbus-unregister-object handler))
  (setq nm-notify-handlers nil))

;;;###autoload
(define-minor-mode nm-notify-mode
  "Toggle NetworkManager notifications."
  :global t
  :group 'nm-notify
  (if nm-notify-mode
      (nm-notify-start)
    (nm-notify-stop)))

(provide 'nm-notify)
;;; nm-notify.el ends here