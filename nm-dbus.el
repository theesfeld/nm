;;; nm-dbus.el --- D-Bus communication layer for NetworkManager  -*- lexical-binding: t; -*-

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

;; Low-level D-Bus communication functions for NetworkManager.

;;; Code:

(require 'dbus)
(require 'nm)

(defconst nm-dbus-interface-manager "org.freedesktop.NetworkManager"
  "D-Bus interface for NetworkManager.")

(defconst nm-dbus-interface-settings "org.freedesktop.NetworkManager.Settings"
  "D-Bus interface for NetworkManager Settings.")

(defconst nm-dbus-interface-connection "org.freedesktop.NetworkManager.Settings.Connection"
  "D-Bus interface for NetworkManager Connection.")

(defconst nm-dbus-interface-device "org.freedesktop.NetworkManager.Device"
  "D-Bus interface for NetworkManager Device.")

(defconst nm-dbus-interface-wireless "org.freedesktop.NetworkManager.Device.Wireless"
  "D-Bus interface for NetworkManager Wireless Device.")

(defconst nm-dbus-interface-wired "org.freedesktop.NetworkManager.Device.Wired"
  "D-Bus interface for NetworkManager Wired Device.")

(defconst nm-dbus-interface-active "org.freedesktop.NetworkManager.Connection.Active"
  "D-Bus interface for NetworkManager Active Connection.")

(defconst nm-dbus-interface-vpn "org.freedesktop.NetworkManager.VPN.Connection"
  "D-Bus interface for NetworkManager VPN Connection.")

(defconst nm-dbus-interface-access-point "org.freedesktop.NetworkManager.AccessPoint"
  "D-Bus interface for NetworkManager Access Point.")

(defun nm-dbus-call-method (path interface method &rest args)
  "Call D-Bus METHOD on PATH with INTERFACE and optional ARGS."
  (when (nm-available-p)
    (apply #'dbus-call-method :system nm-service path interface method args)))

(defun nm-dbus-call-method-async (path interface method handler &rest args)
  "Call D-Bus METHOD asynchronously on PATH with INTERFACE, HANDLER and optional ARGS."
  (when (nm-available-p)
    (apply #'dbus-call-method-asynchronously :system nm-service path interface method handler args)))

(defun nm-dbus-get-property (path interface property)
  "Get D-Bus PROPERTY from PATH with INTERFACE."
  (when (nm-available-p)
    (dbus-get-property :system nm-service path interface property)))

(defun nm-dbus-set-property (path interface property value)
  "Set D-Bus PROPERTY to VALUE on PATH with INTERFACE."
  (when (nm-available-p)
    (dbus-set-property :system nm-service path interface property value)))

(defun nm-dbus-get-all-properties (path interface)
  "Get all D-Bus properties from PATH with INTERFACE."
  (when (nm-available-p)
    (dbus-get-all-properties :system nm-service path interface)))

(defun nm-dbus-introspect (path)
  "Introspect D-Bus object at PATH."
  (when (nm-available-p)
    (dbus-introspect :system nm-service path)))

(defun nm-dbus-register-signal (path interface signal handler)
  "Register HANDLER for D-Bus SIGNAL on PATH with INTERFACE."
  (when (nm-available-p)
    (dbus-register-signal :system nm-service path interface signal handler)))

(defun nm-dbus-variant (type value)
  "Create D-Bus variant with TYPE and VALUE."
  (list :variant (list type value)))

(defun nm-dbus-string-variant (value)
  "Create D-Bus string variant with VALUE."
  (nm-dbus-variant :string value))

(defun nm-dbus-uint32-variant (value)
  "Create D-Bus uint32 variant with VALUE."
  (nm-dbus-variant :uint32 value))

(defun nm-dbus-boolean-variant (value)
  "Create D-Bus boolean variant with VALUE."
  (nm-dbus-variant :boolean value))

(defun nm-dbus-array-variant (type values)
  "Create D-Bus array variant with TYPE and VALUES."
  (list :variant (cons :array (cons type values))))

(defun nm-dbus-dict-entry (key value)
  "Create D-Bus dictionary entry with KEY and VALUE."
  (list :dict-entry key value))

(defun nm-dbus-settings-dict (&rest entries)
  "Create D-Bus settings dictionary from ENTRIES."
  (list :array (apply #'append entries)))

(defun nm-dbus-connection-settings-to-alist (settings)
  "Convert D-Bus connection SETTINGS to alist."
  (let (result)
    (dolist (group settings)
      (when (and (listp group) (eq (car group) :dict-entry))
        (let ((group-name (cadr group))
              (group-settings (caddr group)))
          (when (and (listp group-settings) (eq (car group-settings) :array))
            (let (group-alist)
              (dolist (setting (cdr group-settings))
                (when (and (listp setting) (eq (car setting) :dict-entry))
                  (let ((key (cadr setting))
                        (value (caddr setting)))
                    (when (and (listp value) (eq (car value) :variant))
                      (push (cons key (cadr (cadr value))) group-alist)))))
              (push (cons group-name (nreverse group-alist)) result))))))
    (nreverse result)))

(defun nm-dbus-alist-to-connection-settings (alist)
  "Convert ALIST to D-Bus connection settings format."
  (list :array
        (mapcar (lambda (group)
                  (list :dict-entry
                        (car group)
                        (list :array
                              (mapcar (lambda (setting)
                                        (list :dict-entry
                                              (car setting)
                                              (nm-dbus-guess-variant (cdr setting))))
                                      (cdr group)))))
                alist)))

(defun nm-dbus-guess-variant (value)
  "Guess appropriate D-Bus variant type for VALUE."
  (cond
   ((stringp value) (nm-dbus-string-variant value))
   ((integerp value) (nm-dbus-uint32-variant value))
   ((booleanp value) (nm-dbus-boolean-variant value))
   ((listp value)
    (cond
     ((and (listp (car value)) (eq (caar value) :variant)) value)
     ((stringp (car value)) (nm-dbus-array-variant :string value))
     ((integerp (car value)) (nm-dbus-array-variant :uint32 value))
     (t value)))
   (t value)))

(defun nm-dbus-parse-object-path (path)
  "Parse D-Bus object PATH to extract ID."
  (when (string-match "/\\([0-9]+\\)$" path)
    (string-to-number (match-string 1 path))))

(defun nm-dbus-device-type-to-string (type)
  "Convert numeric device TYPE to string."
  (pcase type
    (0 "unknown")
    (1 "ethernet")
    (2 "wifi")
    (3 "unused1")
    (4 "unused2")
    (5 "bluetooth")
    (6 "olpc-mesh")
    (7 "wimax")
    (8 "modem")
    (9 "infiniband")
    (10 "bond")
    (11 "vlan")
    (12 "adsl")
    (13 "bridge")
    (14 "generic")
    (15 "team")
    (16 "tun")
    (17 "ip-tunnel")
    (18 "macvlan")
    (19 "vxlan")
    (20 "veth")
    (21 "macsec")
    (22 "dummy")
    (23 "ppp")
    (24 "ovs-interface")
    (25 "ovs-port")
    (26 "ovs-bridge")
    (27 "wpan")
    (28 "6lowpan")
    (29 "wireguard")
    (30 "wifi-p2p")
    (31 "vrf")
    (_ "unknown")))

(defun nm-dbus-device-state-to-string (state)
  "Convert numeric device STATE to string."
  (pcase state
    (0 "unknown")
    (10 "unmanaged")
    (20 "unavailable")
    (30 "disconnected")
    (40 "prepare")
    (50 "config")
    (60 "need-auth")
    (70 "ip-config")
    (80 "ip-check")
    (90 "secondaries")
    (100 "activated")
    (110 "deactivating")
    (120 "failed")
    (_ "unknown")))

(defun nm-dbus-active-connection-state-to-string (state)
  "Convert numeric active connection STATE to string."
  (pcase state
    (0 "unknown")
    (1 "activating")
    (2 "activated")
    (3 "deactivating")
    (4 "deactivated")
    (_ "unknown")))

(defun nm-dbus-vpn-connection-state-to-string (state)
  "Convert numeric VPN connection STATE to string."
  (pcase state
    (0 "unknown")
    (1 "prepare")
    (2 "need-auth")
    (3 "connect")
    (4 "ip-config-get")
    (5 "activated")
    (6 "failed")
    (7 "disconnected")
    (_ "unknown")))

(defun nm-dbus-wifi-mode-to-string (mode)
  "Convert numeric WiFi MODE to string."
  (pcase mode
    (0 "unknown")
    (1 "adhoc")
    (2 "infra")
    (3 "ap")
    (4 "mesh")
    (_ "unknown")))

(defun nm-dbus-security-to-string (flags wpa-flags rsn-flags)
  "Convert security FLAGS, WPA-FLAGS and RSN-FLAGS to string."
  (cond
   ((and (= flags 0) (= wpa-flags 0) (= rsn-flags 0)) "Open")
   ((> (logand flags 1) 0) "WEP")
   ((or (> wpa-flags 0) (> rsn-flags 0))
    (concat
     (when (> wpa-flags 0) "WPA")
     (when (and (> wpa-flags 0) (> rsn-flags 0)) "/")
     (when (> rsn-flags 0) "WPA2")
     (when (or (> (logand wpa-flags 512) 0)
               (> (logand rsn-flags 512) 0)) "-EAP")
     (when (or (> (logand wpa-flags 256) 0)
               (> (logand rsn-flags 256) 0)) "-PSK")))
   (t "Unknown")))

(defun nm-dbus-ssid-to-string (ssid)
  "Convert SSID byte array to string."
  (if (listp ssid)
      (apply #'string ssid)
    ssid))

(defun nm-dbus-string-to-ssid (string)
  "Convert STRING to SSID byte array."
  (append string nil))

(defun nm-dbus-mac-to-string (mac)
  "Convert MAC address byte array to string."
  (if (listp mac)
      (mapconcat (lambda (byte) (format "%02x" byte)) mac ":")
    mac))

(defun nm-dbus-string-to-mac (string)
  "Convert STRING to MAC address byte array."
  (mapcar (lambda (hex) (string-to-number hex 16))
          (split-string string ":")))

(provide 'nm-dbus)
;;; nm-dbus.el ends here