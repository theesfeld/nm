;;; nm-wifi.el --- WiFi operations for NetworkManager  -*- lexical-binding: t; -*-

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

;; WiFi-specific operations for NetworkManager.

;;; Code:

(require 'nm)
(require 'nm-dbus)
(require 'nm-device)
(require 'nm-connection)

(defvar nm-wifi-scan-callbacks nil
  "List of callbacks for WiFi scan completion.")

(defun nm-wifi-get-devices ()
  "Get list of WiFi device paths."
  (seq-filter (lambda (device)
                (eq (nm-device-get-device-type device) 2))
              (nm-get-devices)))

(defun nm-wifi-scan (device-path &optional callback)
  "Request WiFi scan on DEVICE-PATH with optional CALLBACK."
  (condition-case err
      (progn
        (when callback
          (push (cons device-path callback) nm-wifi-scan-callbacks))
        (nm-device-wireless-request-scan device-path))
    (dbus-error
     (message "Failed to scan WiFi on device: %s" (error-message-string err))
     nil)))

(defun nm-wifi-scan-all (&optional callback)
  "Request WiFi scan on all WiFi devices with optional CALLBACK."
  (let ((devices (nm-wifi-get-devices)))
    (dolist (device devices)
      (nm-wifi-scan device callback))))

(defun nm-access-point-get-flags (ap-path)
  "Get flags for AP-PATH."
  (nm-dbus-get-property ap-path nm-dbus-interface-access-point "Flags"))

(defun nm-access-point-get-wpa-flags (ap-path)
  "Get WPA flags for AP-PATH."
  (nm-dbus-get-property ap-path nm-dbus-interface-access-point "WpaFlags"))

(defun nm-access-point-get-rsn-flags (ap-path)
  "Get RSN flags for AP-PATH."
  (nm-dbus-get-property ap-path nm-dbus-interface-access-point "RsnFlags"))

(defun nm-access-point-get-ssid (ap-path)
  "Get SSID for AP-PATH."
  (nm-dbus-get-property ap-path nm-dbus-interface-access-point "Ssid"))

(defun nm-access-point-get-frequency (ap-path)
  "Get frequency in MHz for AP-PATH."
  (nm-dbus-get-property ap-path nm-dbus-interface-access-point "Frequency"))

(defun nm-access-point-get-hw-address (ap-path)
  "Get hardware address for AP-PATH."
  (nm-dbus-get-property ap-path nm-dbus-interface-access-point "HwAddress"))

(defun nm-access-point-get-mode (ap-path)
  "Get mode for AP-PATH."
  (nm-dbus-get-property ap-path nm-dbus-interface-access-point "Mode"))

(defun nm-access-point-get-max-bitrate (ap-path)
  "Get max bitrate in Kb/s for AP-PATH."
  (nm-dbus-get-property ap-path nm-dbus-interface-access-point "MaxBitrate"))

(defun nm-access-point-get-strength (ap-path)
  "Get signal strength percentage for AP-PATH."
  (nm-dbus-get-property ap-path nm-dbus-interface-access-point "Strength"))

(defun nm-access-point-get-last-seen (ap-path)
  "Get last seen timestamp for AP-PATH."
  (nm-dbus-get-property ap-path nm-dbus-interface-access-point "LastSeen"))

(defun nm-access-point-info (ap-path)
  "Get access point information for AP-PATH as alist."
  (let ((flags (nm-access-point-get-flags ap-path))
        (wpa-flags (nm-access-point-get-wpa-flags ap-path))
        (rsn-flags (nm-access-point-get-rsn-flags ap-path)))
    (list (cons 'path ap-path)
          (cons 'ssid (nm-dbus-ssid-to-string (nm-access-point-get-ssid ap-path)))
          (cons 'bssid (nm-access-point-get-hw-address ap-path))
          (cons 'frequency (nm-access-point-get-frequency ap-path))
          (cons 'channel (nm-wifi-frequency-to-channel (nm-access-point-get-frequency ap-path)))
          (cons 'mode (nm-dbus-wifi-mode-to-string (nm-access-point-get-mode ap-path)))
          (cons 'strength (nm-access-point-get-strength ap-path))
          (cons 'security (nm-dbus-security-to-string flags wpa-flags rsn-flags))
          (cons 'max-bitrate (nm-access-point-get-max-bitrate ap-path))
          (cons 'last-seen (nm-access-point-get-last-seen ap-path)))))

(defun nm-wifi-get-access-points (device-path)
  "Get all access points for DEVICE-PATH."
  (mapcar #'nm-access-point-info
          (nm-device-wireless-get-all-access-points device-path)))

(defun nm-wifi-get-all-access-points ()
  "Get all access points from all WiFi devices."
  (let (all-aps)
    (dolist (device (nm-wifi-get-devices))
      (setq all-aps (append all-aps (nm-wifi-get-access-points device))))
    all-aps))

(defun nm-wifi-frequency-to-channel (frequency)
  "Convert FREQUENCY in MHz to channel number."
  (cond
   ((and (>= frequency 2412) (<= frequency 2484))
    (if (<= frequency 2472)
        (1+ (/ (- frequency 2412) 5))
      14))
   ((and (>= frequency 5170) (<= frequency 5825))
    (/ (- frequency 5000) 5))
   (t 0)))

(defun nm-wifi-strength-bars (strength)
  "Convert signal STRENGTH to visual bars."
  (let ((bars (cond
               ((>= strength 80) 4)
               ((>= strength 60) 3)
               ((>= strength 40) 2)
               ((>= strength 20) 1)
               (t 0))))
    (concat (make-string bars ?▂)
            (make-string (- 4 bars) ?_))))

(defun nm-wifi-connect (ssid &optional password hidden bssid)
  "Connect to WiFi network with SSID, optional PASSWORD, HIDDEN and BSSID."
  (condition-case err
      (let* ((devices (nm-wifi-get-devices))
             (device (car devices))
             (settings (nm-create-wifi-connection ssid password hidden)))
        (unless device
          (error "No WiFi device available"))
        (when bssid
          (let* ((parsed (nm-dbus-connection-settings-to-alist settings))
                 (wifi-settings (assoc "802-11-wireless" parsed)))
            (setcdr wifi-settings
                    (cons (cons "bssid" (nm-dbus-string-to-mac bssid))
                          (cdr wifi-settings)))
            (setq settings (nm-dbus-alist-to-connection-settings parsed))))
        (nm-add-and-activate-connection settings device "/"))
    (dbus-error
     (message "Failed to connect to WiFi network '%s': %s" ssid (error-message-string err))
     nil)
    (error
     (message "Error connecting to WiFi network '%s': %s" ssid (error-message-string err))
     nil)))

(defun nm-wifi-find-connection-for-ssid (ssid)
  "Find existing connection path for SSID."
  (let ((connections (nm-connections-info)))
    (seq-find (lambda (conn)
                (and (equal (cdr (assoc 'type conn)) "802-11-wireless")
                     (equal (cdr (assoc 'id conn)) ssid)))
              connections)))

(defun nm-wifi-activate-connection (ssid)
  "Activate existing connection for SSID."
  (let ((conn-info (nm-wifi-find-connection-for-ssid ssid)))
    (if conn-info
        (let* ((conn-path (cdr (assoc 'path conn-info)))
               (devices (nm-wifi-get-devices))
               (device (car devices)))
          (if device
              (nm-activate-connection conn-path device "/")
            (error "No WiFi device available")))
      (error "No saved connection for SSID: %s" ssid))))

(defun nm-wifi-connect-to-ap (ap-info &optional password)
  "Connect to access point AP-INFO with optional PASSWORD."
  (let* ((ssid (cdr (assoc 'ssid ap-info)))
         (bssid (cdr (assoc 'bssid ap-info)))
         (security (cdr (assoc 'security ap-info)))
         (existing (nm-wifi-find-connection-for-ssid ssid)))
    (if (and existing (not password))
        (nm-wifi-activate-connection ssid)
      (when (and (not (string= security "Open")) (not password))
        (error "Password required for secured network"))
      (nm-wifi-connect ssid password nil bssid))))

(defun nm-wifi-get-current-ap (device-path)
  "Get current access point info for DEVICE-PATH."
  (let ((ap-path (nm-device-wireless-get-active-access-point device-path)))
    (when (and ap-path (not (string= ap-path "/")))
      (nm-access-point-info ap-path))))

(defun nm-wifi-get-current-connection ()
  "Get current WiFi connection info."
  (let ((active-conns (nm-active-connections-info)))
    (seq-find (lambda (conn)
                (equal (cdr (assoc 'type conn)) "802-11-wireless"))
              active-conns)))

(defun nm-wifi-disconnect ()
  "Disconnect current WiFi connection."
  (let ((current (nm-wifi-get-current-connection)))
    (if current
        (nm-deactivate-connection (cdr (assoc 'path current)))
      (message "No active WiFi connection"))))

(defun nm-wifi-forget-network (ssid)
  "Delete saved connection for SSID."
  (let ((conn-info (nm-wifi-find-connection-for-ssid ssid)))
    (if conn-info
        (nm-connection-delete (cdr (assoc 'path conn-info)))
      (error "No saved connection for SSID: %s" ssid))))

(defun nm-wifi-set-autoconnect (ssid autoconnect)
  "Set AUTOCONNECT for WiFi network with SSID."
  (let ((conn-info (nm-wifi-find-connection-for-ssid ssid)))
    (if conn-info
        (let* ((conn-path (cdr (assoc 'path conn-info)))
               (settings (nm-connection-get-settings conn-path))
               (parsed (nm-dbus-connection-settings-to-alist settings))
               (conn-settings (assoc "connection" parsed)))
          (setcdr (assoc "autoconnect" (cdr conn-settings)) autoconnect)
          (nm-connection-update conn-path
                                (nm-dbus-alist-to-connection-settings parsed)))
      (error "No saved connection for SSID: %s" ssid))))

(defun nm-wifi-create-hotspot (ssid password &optional band)
  "Create WiFi hotspot with SSID, PASSWORD and optional BAND."
  (let* ((devices (nm-wifi-get-devices))
         (device (car devices))
         (uuid (nm-generate-uuid))
         (settings (nm-dbus-alist-to-connection-settings
                    `(("connection" . (("id" . ,(format "Hotspot %s" ssid))
                                       ("uuid" . ,uuid)
                                       ("type" . "802-11-wireless")
                                       ("autoconnect" . nil)))
                      ("802-11-wireless" . (("ssid" . ,(nm-dbus-string-to-ssid ssid))
                                            ("mode" . "ap")
                                            ("band" . ,(or band "bg"))
                                            ("hidden" . nil)))
                      ("802-11-wireless-security" . (("key-mgmt" . "wpa-psk")
                                                     ("psk" . ,password)))
                      ("ipv4" . (("method" . "shared")))
                      ("ipv6" . (("method" . "ignore")))))))
    (if device
        (nm-add-and-activate-connection settings device "/")
      (error "No WiFi device available"))))

(defun nm-wifi-register-scan-done (device-path handler)
  "Register HANDLER for scan done signal on DEVICE-PATH."
  (nm-dbus-register-signal device-path nm-dbus-interface-wireless
                           "ScanDone" handler))

(defun nm-wifi-register-access-point-added (device-path handler)
  "Register HANDLER for access point added on DEVICE-PATH."
  (nm-dbus-register-signal device-path nm-dbus-interface-wireless
                           "AccessPointAdded" handler))

(defun nm-wifi-register-access-point-removed (device-path handler)
  "Register HANDLER for access point removed on DEVICE-PATH."
  (nm-dbus-register-signal device-path nm-dbus-interface-wireless
                           "AccessPointRemoved" handler))

(provide 'nm-wifi)
;;; nm-wifi.el ends here