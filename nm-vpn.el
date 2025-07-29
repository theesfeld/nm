;;; nm-vpn.el --- VPN management for NetworkManager  -*- lexical-binding: t; -*-

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

;; VPN management functions for NetworkManager.

;;; Code:

(require 'nm)
(require 'nm-dbus)
(require 'nm-connection)

(defconst nm-vpn-service-openvpn "org.freedesktop.NetworkManager.openvpn"
  "OpenVPN service name.")

(defconst nm-vpn-service-vpnc "org.freedesktop.NetworkManager.vpnc"
  "VPNC (Cisco) service name.")

(defconst nm-vpn-service-pptp "org.freedesktop.NetworkManager.pptp"
  "PPTP service name.")

(defconst nm-vpn-service-l2tp "org.freedesktop.NetworkManager.l2tp"
  "L2TP service name.")

(defconst nm-vpn-service-strongswan "org.freedesktop.NetworkManager.strongswan"
  "StrongSwan (IPSec) service name.")

(defconst nm-vpn-service-wireguard "org.freedesktop.NetworkManager.wireguard"
  "WireGuard service name.")

(defun nm-vpn-get-connections ()
  "Get list of VPN connection info."
  (seq-filter (lambda (conn)
                (member (cdr (assoc 'type conn))
                        '("vpn" "wireguard")))
              (nm-connections-info)))

(defun nm-vpn-get-active-connections ()
  "Get list of active VPN connections."
  (seq-filter (lambda (conn)
                (cdr (assoc 'vpn conn)))
              (nm-active-connections-info)))

(defun nm-vpn-connection-get-banner (vpn-path)
  "Get VPN banner for VPN-PATH."
  (nm-dbus-get-property vpn-path nm-dbus-interface-vpn "Banner"))

(defun nm-vpn-connection-get-vpn-state (vpn-path)
  "Get VPN state for VPN-PATH."
  (nm-dbus-get-property vpn-path nm-dbus-interface-vpn "VpnState"))

(defun nm-vpn-activate (vpn-name)
  "Activate VPN connection by VPN-NAME."
  (let ((conn-info (seq-find (lambda (conn)
                               (equal (cdr (assoc 'id conn)) vpn-name))
                             (nm-vpn-get-connections))))
    (if conn-info
        (nm-activate-connection (cdr (assoc 'path conn-info)) "/" "/")
      (error "VPN connection not found: %s" vpn-name))))

(defun nm-vpn-deactivate (vpn-name)
  "Deactivate VPN connection by VPN-NAME."
  (let ((active-conn (seq-find (lambda (conn)
                                 (equal (cdr (assoc 'id conn)) vpn-name))
                               (nm-vpn-get-active-connections))))
    (if active-conn
        (nm-deactivate-connection (cdr (assoc 'path active-conn)))
      (error "VPN connection not active: %s" vpn-name))))

(defun nm-vpn-deactivate-all ()
  "Deactivate all active VPN connections."
  (dolist (conn (nm-vpn-get-active-connections))
    (nm-deactivate-connection (cdr (assoc 'path conn)))))

(defun nm-vpn-create-openvpn (name config-file &optional username password)
  "Create OpenVPN connection NAME from CONFIG-FILE with optional USERNAME and PASSWORD."
  (let ((uuid (nm-generate-uuid))
        (data `(("ca" . ,config-file)
                ("connection-type" . "tls"))))
    (when username
      (push (cons "username" username) data))
    (when password
      (push (cons "password" password) data))
    (nm-dbus-alist-to-connection-settings
     `(("connection" . (("id" . ,name)
                        ("uuid" . ,uuid)
                        ("type" . "vpn")
                        ("autoconnect" . nil)))
       ("vpn" . (("service-type" . ,nm-vpn-service-openvpn)
                 ("data" . ,data)))
       ("ipv4" . (("method" . "auto")))
       ("ipv6" . (("method" . "auto")))))))

(defun nm-vpn-create-wireguard (name private-key endpoint public-key &optional preshared-key)
  "Create WireGuard connection NAME with PRIVATE-KEY, ENDPOINT, PUBLIC-KEY and optional PRESHARED-KEY."
  (let ((uuid (nm-generate-uuid))
        (peer-data `(("endpoint" . ,endpoint)
                     ("public-key" . ,public-key)
                     ("allowed-ips" . "0.0.0.0/0"))))
    (when preshared-key
      (push (cons "preshared-key" preshared-key) peer-data))
    (nm-dbus-alist-to-connection-settings
     `(("connection" . (("id" . ,name)
                        ("uuid" . ,uuid)
                        ("type" . "wireguard")
                        ("autoconnect" . nil)))
       ("wireguard" . (("private-key" . ,private-key)
                       ("peer-routes" . t)))
       ("wireguard-peer.0" . ,peer-data)
       ("ipv4" . (("method" . "manual")
                  ("address-data" . ,(list (list (cons "address" "10.0.0.2")
                                                 (cons "prefix" 24))))))
       ("ipv6" . (("method" . "ignore")))))))

(defun nm-vpn-create-l2tp (name gateway username password &optional psk)
  "Create L2TP connection NAME with GATEWAY, USERNAME, PASSWORD and optional PSK."
  (let ((uuid (nm-generate-uuid))
        (data `(("gateway" . ,gateway)
                ("user" . ,username)
                ("password" . ,password))))
    (when psk
      (push (cons "ipsec-psk" psk) data))
    (nm-dbus-alist-to-connection-settings
     `(("connection" . (("id" . ,name)
                        ("uuid" . ,uuid)
                        ("type" . "vpn")
                        ("autoconnect" . nil)))
       ("vpn" . (("service-type" . ,nm-vpn-service-l2tp)
                 ("data" . ,data)))
       ("ipv4" . (("method" . "auto")))
       ("ipv6" . (("method" . "auto")))))))

(defun nm-vpn-import-config (file)
  "Import VPN configuration from FILE."
  (let ((extension (file-name-extension file)))
    (cond
     ((member extension '("ovpn" "conf"))
      (nm-vpn-import-openvpn file))
     ((equal extension "conf")
      (if (string-match-p "wireguard" file)
          (nm-vpn-import-wireguard file)
        (error "Unknown VPN configuration format")))
     (t (error "Unsupported VPN configuration file: %s" file)))))

(defun nm-vpn-read-file-content (file)
  "Read content from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun nm-vpn-extract-ca-file (content)
  "Extract CA file path from OpenVPN CONTENT."
  (when (string-match "^ca\\s-+\\(.+\\)$" content)
    (match-string 1 content)))

(defun nm-vpn-file-basename (file)
  "Get basename without extension from FILE."
  (file-name-sans-extension (file-name-nondirectory file)))

(defun nm-vpn-import-openvpn (file)
  "Import OpenVPN configuration from FILE."
  (let* ((name (nm-vpn-file-basename file))
         (content (nm-vpn-read-file-content file))
         (ca-file (nm-vpn-extract-ca-file content)))
    (if ca-file
        (let ((settings (nm-vpn-create-openvpn name ca-file)))
          (nm-add-connection settings))
      (error "Could not parse OpenVPN configuration"))))

(defun nm-vpn-extract-wireguard-field (content field)
  "Extract FIELD value from WireGuard CONTENT."
  (when (string-match (format "%s\\s-*=\\s-*\\(.+\\)" field) content)
    (match-string 1 content)))

(defun nm-vpn-extract-wireguard-config (content)
  "Extract WireGuard configuration from CONTENT."
  (list (cons 'private-key (nm-vpn-extract-wireguard-field content "PrivateKey"))
        (cons 'endpoint (nm-vpn-extract-wireguard-field content "Endpoint"))
        (cons 'public-key (nm-vpn-extract-wireguard-field content "PublicKey"))
        (cons 'preshared-key (nm-vpn-extract-wireguard-field content "PresharedKey"))))

(defun nm-vpn-valid-wireguard-config-p (config)
  "Return t if CONFIG has required WireGuard fields."
  (and (cdr (assoc 'private-key config))
       (cdr (assoc 'endpoint config))
       (cdr (assoc 'public-key config))))

(defun nm-vpn-import-wireguard (file)
  "Import WireGuard configuration from FILE."
  (let* ((name (nm-vpn-file-basename file))
         (content (nm-vpn-read-file-content file))
         (config (nm-vpn-extract-wireguard-config content)))
    (if (nm-vpn-valid-wireguard-config-p config)
        (let ((settings (nm-vpn-create-wireguard 
                         name
                         (cdr (assoc 'private-key config))
                         (cdr (assoc 'endpoint config))
                         (cdr (assoc 'public-key config))
                         (cdr (assoc 'preshared-key config)))))
          (nm-add-connection settings))
      (error "Could not parse WireGuard configuration"))))

(defun nm-vpn-set-autoconnect (vpn-name autoconnect)
  "Set AUTOCONNECT for VPN connection VPN-NAME."
  (let ((conn-info (seq-find (lambda (conn)
                               (equal (cdr (assoc 'id conn)) vpn-name))
                             (nm-vpn-get-connections))))
    (if conn-info
        (let* ((conn-path (cdr (assoc 'path conn-info)))
               (settings (nm-connection-get-settings conn-path))
               (parsed (nm-dbus-connection-settings-to-alist settings))
               (conn-settings (assoc "connection" parsed)))
          (setcdr (assoc "autoconnect" (cdr conn-settings)) autoconnect)
          (nm-connection-update conn-path
                                (nm-dbus-alist-to-connection-settings parsed)))
      (error "VPN connection not found: %s" vpn-name))))

(defun nm-vpn-get-kill-switch (vpn-name)
  "Get kill switch setting for VPN-NAME."
  (let ((conn-info (seq-find (lambda (conn)
                               (equal (cdr (assoc 'id conn)) vpn-name))
                             (nm-vpn-get-connections))))
    (when conn-info
      (let* ((conn-path (cdr (assoc 'path conn-info)))
             (settings (nm-connection-get-settings conn-path))
             (parsed (nm-dbus-connection-settings-to-alist settings))
             (ipv4-settings (cdr (assoc "ipv4" parsed))))
        (equal (cdr (assoc "never-default" ipv4-settings)) nil)))))

(defun nm-vpn-set-kill-switch (vpn-name enable)
  "Set kill switch for VPN-NAME to ENABLE."
  (let ((conn-info (seq-find (lambda (conn)
                               (equal (cdr (assoc 'id conn)) vpn-name))
                             (nm-vpn-get-connections))))
    (if conn-info
        (let* ((conn-path (cdr (assoc 'path conn-info)))
               (settings (nm-connection-get-settings conn-path))
               (parsed (nm-dbus-connection-settings-to-alist settings))
               (ipv4-settings (assoc "ipv4" parsed))
               (ipv6-settings (assoc "ipv6" parsed)))
          (setcdr (assoc "never-default" (cdr ipv4-settings)) (not enable))
          (setcdr (assoc "never-default" (cdr ipv6-settings)) (not enable))
          (nm-connection-update conn-path
                                (nm-dbus-alist-to-connection-settings parsed)))
      (error "VPN connection not found: %s" vpn-name))))

(defun nm-vpn-connection-info (vpn-path)
  "Get VPN connection information for VPN-PATH."
  (let ((active-info (nm-active-connection-info vpn-path)))
    (when (cdr (assoc 'vpn active-info))
      (cons (cons 'vpn-state (nm-dbus-vpn-connection-state-to-string
                              (nm-vpn-connection-get-vpn-state vpn-path)))
            active-info))))

(defun nm-vpn-register-state-changed (vpn-path handler)
  "Register HANDLER for VPN state changes on VPN-PATH."
  (nm-dbus-register-signal vpn-path nm-dbus-interface-vpn "VpnStateChanged" handler))

(provide 'nm-vpn)
;;; nm-vpn.el ends here