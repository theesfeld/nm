;;; nm.el --- NetworkManager interface for Emacs  -*- lexical-binding: t; -*-

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

;; NetworkManager interface for Emacs providing complete control over
;; network connections via D-Bus API.

;;; Code:

(require 'dbus)
(declare-function nm-ui-ethernet-internal "nm-ui" ())
(declare-function nm-ui-devices-internal "nm-ui" ())
(declare-function nm-vpn-activate "nm-vpn" (vpn-name))
(declare-function nm-vpn-deactivate-all "nm-vpn" ())

(defgroup nm nil
  "NetworkManager interface for Emacs."
  :group 'comm
  :prefix "nm-")

(defcustom nm-service "org.freedesktop.NetworkManager"
  "D-Bus service name for NetworkManager."
  :type 'string
  :group 'nm)

(defcustom nm-path "/org/freedesktop/NetworkManager"
  "D-Bus object path for NetworkManager."
  :type 'string
  :group 'nm)

(defcustom nm-settings-path "/org/freedesktop/NetworkManager/Settings"
  "D-Bus object path for NetworkManager Settings."
  :type 'string
  :group 'nm)

(defcustom nm-auto-refresh t
  "Whether to automatically refresh network status."
  :type 'boolean
  :group 'nm)

(defcustom nm-refresh-interval 5
  "Interval in seconds for automatic refresh."
  :type 'integer
  :group 'nm)

(defvar nm-version nil
  "NetworkManager version string.")

(defvar nm-capabilities nil
  "NetworkManager capabilities list.")

(defun nm-available-p ()
  "Return t if NetworkManager service is available."
  (member nm-service (dbus-list-known-names :system)))

(defun nm-get-version ()
  "Get NetworkManager version."
  (when (nm-available-p)
    (dbus-get-property :system nm-service nm-path nm-service "Version")))

(defun nm-get-state ()
  "Get NetworkManager global state."
  (when (nm-available-p)
    (dbus-get-property :system nm-service nm-path nm-service "State")))

(defun nm-state-to-string (state)
  "Convert numeric STATE to human-readable string."
  (pcase state
    (10 "asleep")
    (20 "disconnected")
    (30 "disconnecting")
    (40 "connecting")
    (50 "connected-local")
    (60 "connected-site")
    (70 "connected-global")
    (_ "unknown")))

(defun nm-get-connectivity ()
  "Get NetworkManager connectivity state."
  (when (nm-available-p)
    (dbus-get-property :system nm-service nm-path nm-service "Connectivity")))

(defun nm-connectivity-to-string (connectivity)
  "Convert numeric CONNECTIVITY to human-readable string."
  (pcase connectivity
    (0 "unknown")
    (1 "none")
    (2 "portal")
    (3 "limited")
    (4 "full")
    (_ "unknown")))

(defun nm-networking-enabled-p ()
  "Return t if networking is enabled."
  (when (nm-available-p)
    (dbus-get-property :system nm-service nm-path nm-service "NetworkingEnabled")))

(defun nm-enable-networking (enable)
  "Enable or disable networking based on ENABLE."
  (when (nm-available-p)
    (dbus-call-method :system nm-service nm-path nm-service "Enable" enable)))

(defun nm-wireless-enabled-p ()
  "Return t if wireless is enabled."
  (when (nm-available-p)
    (dbus-get-property :system nm-service nm-path nm-service "WirelessEnabled")))

(defun nm-set-wireless-enabled (enable)
  "Enable or disable wireless based on ENABLE."
  (when (nm-available-p)
    (dbus-set-property :system nm-service nm-path nm-service "WirelessEnabled" enable)))

(defun nm-wireless-hardware-enabled-p ()
  "Return t if wireless hardware is enabled."
  (when (nm-available-p)
    (dbus-get-property :system nm-service nm-path nm-service "WirelessHardwareEnabled")))

(defun nm-check-connectivity ()
  "Check network connectivity."
  (when (nm-available-p)
    (dbus-call-method :system nm-service nm-path nm-service "CheckConnectivity")))

(defun nm-get-permissions ()
  "Get NetworkManager permissions for current user."
  (when (nm-available-p)
    (dbus-call-method :system nm-service nm-path nm-service "GetPermissions")))

(defun nm-sleep (sleep)
  "Put NetworkManager to sleep if SLEEP is non-nil."
  (when (nm-available-p)
    (dbus-call-method :system nm-service nm-path nm-service "Sleep" sleep)))

(defun nm-get-primary-connection ()
  "Get primary connection object path."
  (when (nm-available-p)
    (dbus-get-property :system nm-service nm-path nm-service "PrimaryConnection")))

(defun nm-get-activating-connection ()
  "Get activating connection object path."
  (when (nm-available-p)
    (dbus-get-property :system nm-service nm-path nm-service "ActivatingConnection")))

(defun nm-reload (&optional flags)
  "Reload NetworkManager configuration with optional FLAGS."
  (when (nm-available-p)
    (dbus-call-method :system nm-service nm-path nm-service "Reload" (or flags 0))))

;;;###autoload
(defun nm-status ()
  "Display NetworkManager status."
  (interactive)
  (if (nm-available-p)
      (let ((state (nm-get-state))
            (connectivity (nm-get-connectivity))
            (version (nm-get-version))
            (networking (nm-networking-enabled-p))
            (wireless (nm-wireless-enabled-p)))
        (message "NetworkManager %s: %s (%s) | Networking: %s | Wireless: %s"
                 version
                 (nm-state-to-string state)
                 (nm-connectivity-to-string connectivity)
                 (if networking "enabled" "disabled")
                 (if wireless "enabled" "disabled")))
    (message "NetworkManager service not available")))

;;;###autoload
(defun nm-toggle-networking ()
  "Toggle networking on/off."
  (interactive)
  (when (nm-available-p)
    (let ((current (nm-networking-enabled-p)))
      (nm-enable-networking (not current))
      (message "Networking %s" (if current "disabled" "enabled")))))

;;;###autoload
(defun nm-toggle-wireless ()
  "Toggle wireless on/off."
  (interactive)
  (when (nm-available-p)
    (let ((current (nm-wireless-enabled-p)))
      (if (nm-wireless-hardware-enabled-p)
          (progn
            (nm-set-wireless-enabled (not current))
            (message "Wireless %s" (if current "disabled" "enabled")))
        (message "Wireless hardware is disabled")))))

(defun nm-initialize ()
  "Initialize NetworkManager interface."
  (when (nm-available-p)
    (setq nm-version (nm-get-version))
    (setq nm-capabilities (nm-get-permissions))
    (message "NetworkManager %s initialized" nm-version)))

(defvar nm-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'nm-status)
    (define-key map "n" #'nm-toggle-networking)
    (define-key map "w" #'nm-toggle-wireless)
    (define-key map "u" #'nm-ui)
    (define-key map "W" #'nm-ui-wifi)
    (define-key map "E" #'nm-ui-ethernet)
    (define-key map "d" #'nm-ui-devices)
    (define-key map "c" #'nm-ui-connections)
    (define-key map "v" #'nm-vpn-activate)
    (define-key map "V" #'nm-vpn-deactivate-all)
    (define-key map "r" #'nm-reload)
    (define-key map "?" #'nm-show-help)
    map)
  "Keymap for NetworkManager commands.")

(defun nm-show-help ()
  "Show NetworkManager commands help."
  (interactive)
  (if (fboundp 'describe-keymap)
      (describe-keymap nm-prefix-map)
    (describe-variable 'nm-prefix-map)))

(defun nm-setup-which-key ()
  "Setup which-key descriptions for NetworkManager."
  (when (fboundp 'which-key-add-key-based-replacements)
    (which-key-add-key-based-replacements
      "C-c N" "NetworkManager"
      "C-c N s" "status"
      "C-c N n" "toggle networking"
      "C-c N w" "toggle wireless"
      "C-c N u" "UI dashboard"
      "C-c N W" "WiFi browser"
      "C-c N E" "Ethernet browser"
      "C-c N d" "device list"
      "C-c N c" "connections"
      "C-c N v" "activate VPN"
      "C-c N V" "deactivate all VPNs"
      "C-c N r" "reload config"
      "C-c N ?" "show help")))

(with-eval-after-load 'which-key
  (nm-setup-which-key))

;;;###autoload
(defun nm-ui-ethernet ()
  "Open NetworkManager Ethernet browser."
  (interactive)
  (unless (nm-available-p)
    (error "NetworkManager service not available"))
  (require 'nm-ui)
  (nm-ui-ethernet-internal))

;;;###autoload
(defun nm-ui-devices ()
  "Open NetworkManager device list."
  (interactive)
  (unless (nm-available-p)
    (error "NetworkManager service not available"))
  (require 'nm-ui)
  (nm-ui-devices-internal))

(defun nm-setup-global-keybindings ()
  "Setup global keybindings for NetworkManager."
  (global-set-key (kbd "C-c N") nm-prefix-map))

;;;###autoload
(defun nm-initialize-keybindings ()
  "Initialize NetworkManager global keybindings."
  (nm-setup-global-keybindings))

;;;###autoload
(add-hook 'after-init-hook #'nm-initialize-keybindings)

;;;###autoload
(defun nm-modeline-mode (&optional arg)
  "Toggle NetworkManager modeline indicator.
With ARG, enable if positive, disable otherwise."
  (interactive (list (or current-prefix-arg 'toggle)))
  (require 'nm-modeline)
  (nm-modeline-mode arg))

(provide 'nm)
;;; nm.el ends here