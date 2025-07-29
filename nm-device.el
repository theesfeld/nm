;;; nm-device.el --- Device management for NetworkManager  -*- lexical-binding: t; -*-

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

;; Device management functions for NetworkManager.

;;; Code:

(require 'nm)
(require 'nm-dbus)

(defun nm-get-devices ()
  "Get list of all network devices."
  (nm-dbus-call-method nm-path nm-dbus-interface-manager "GetDevices"))

(defun nm-get-device-by-ip-iface (iface)
  "Get device object path by IP interface name IFACE."
  (nm-dbus-call-method nm-path nm-dbus-interface-manager "GetDeviceByIpIface" iface))

(defun nm-device-get-interface (device-path)
  "Get interface name for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Interface"))

(defun nm-device-get-ip-interface (device-path)
  "Get IP interface name for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "IpInterface"))

(defun nm-device-get-driver (device-path)
  "Get driver name for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Driver"))

(defun nm-device-get-driver-version (device-path)
  "Get driver version for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "DriverVersion"))

(defun nm-device-get-firmware-version (device-path)
  "Get firmware version for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "FirmwareVersion"))

(defun nm-device-get-capabilities (device-path)
  "Get capabilities for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Capabilities"))

(defun nm-device-get-state (device-path)
  "Get state for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "State"))

(defun nm-device-get-state-reason (device-path)
  "Get state reason for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "StateReason"))

(defun nm-device-get-active-connection (device-path)
  "Get active connection for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "ActiveConnection"))

(defun nm-device-get-ip4-config (device-path)
  "Get IPv4 configuration for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Ip4Config"))

(defun nm-device-get-dhcp4-config (device-path)
  "Get DHCP4 configuration for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Dhcp4Config"))

(defun nm-device-get-ip6-config (device-path)
  "Get IPv6 configuration for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Ip6Config"))

(defun nm-device-get-dhcp6-config (device-path)
  "Get DHCP6 configuration for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Dhcp6Config"))

(defun nm-device-managed-p (device-path)
  "Return t if DEVICE-PATH is managed."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Managed"))

(defun nm-device-set-managed (device-path managed)
  "Set DEVICE-PATH managed state to MANAGED."
  (nm-dbus-set-property device-path nm-dbus-interface-device "Managed" managed))

(defun nm-device-autoconnect-p (device-path)
  "Return t if DEVICE-PATH has autoconnect enabled."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Autoconnect"))

(defun nm-device-set-autoconnect (device-path autoconnect)
  "Set DEVICE-PATH autoconnect to AUTOCONNECT."
  (nm-dbus-set-property device-path nm-dbus-interface-device "Autoconnect" autoconnect))

(defun nm-device-firmware-missing-p (device-path)
  "Return t if DEVICE-PATH has missing firmware."
  (nm-dbus-get-property device-path nm-dbus-interface-device "FirmwareMissing"))

(defun nm-device-nm-plugin-missing-p (device-path)
  "Return t if DEVICE-PATH has missing NetworkManager plugin."
  (nm-dbus-get-property device-path nm-dbus-interface-device "NmPluginMissing"))

(defun nm-device-get-device-type (device-path)
  "Get device type for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "DeviceType"))

(defun nm-device-get-available-connections (device-path)
  "Get available connections for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "AvailableConnections"))

(defun nm-device-get-physical-port-id (device-path)
  "Get physical port ID for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "PhysicalPortId"))

(defun nm-device-get-mtu (device-path)
  "Get MTU for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Mtu"))

(defun nm-device-get-metered (device-path)
  "Get metered status for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Metered"))

(defun nm-device-real-p (device-path)
  "Return t if DEVICE-PATH is a real device."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Real"))

(defun nm-device-get-ip4-connectivity (device-path)
  "Get IPv4 connectivity for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Ip4Connectivity"))

(defun nm-device-get-ip6-connectivity (device-path)
  "Get IPv6 connectivity for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "Ip6Connectivity"))

(defun nm-device-get-interface-flags (device-path)
  "Get interface flags for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-device "InterfaceFlags"))

(defun nm-device-disconnect (device-path)
  "Disconnect DEVICE-PATH."
  (nm-dbus-call-method device-path nm-dbus-interface-device "Disconnect"))

(defun nm-device-delete (device-path)
  "Delete software DEVICE-PATH."
  (nm-dbus-call-method device-path nm-dbus-interface-device "Delete"))

(defun nm-device-get-applied-connection (device-path &optional flags)
  "Get applied connection for DEVICE-PATH with optional FLAGS."
  (nm-dbus-call-method device-path nm-dbus-interface-device "GetAppliedConnection" (or flags 0)))

(defun nm-device-reapply (device-path connection version-id &optional flags)
  "Reapply CONNECTION with VERSION-ID to DEVICE-PATH with optional FLAGS."
  (nm-dbus-call-method device-path nm-dbus-interface-device "Reapply"
                       connection version-id (or flags 0)))

(defun nm-device-get-all-properties (device-path)
  "Get all properties for DEVICE-PATH."
  (nm-dbus-get-all-properties device-path nm-dbus-interface-device))

(defun nm-device-info (device-path)
  "Get device information for DEVICE-PATH as alist."
  (let ((props (nm-device-get-all-properties device-path)))
    (list (cons 'path device-path)
          (cons 'interface (cdr (assoc "Interface" props)))
          (cons 'type (nm-dbus-device-type-to-string (cdr (assoc "DeviceType" props))))
          (cons 'state (nm-dbus-device-state-to-string (cdr (assoc "State" props))))
          (cons 'driver (cdr (assoc "Driver" props)))
          (cons 'managed (cdr (assoc "Managed" props)))
          (cons 'autoconnect (cdr (assoc "Autoconnect" props)))
          (cons 'active-connection (cdr (assoc "ActiveConnection" props))))))

(defun nm-devices-info ()
  "Get information for all devices."
  (mapcar #'nm-device-info (nm-get-devices)))

(defun nm-device-wired-get-hw-address (device-path)
  "Get hardware address for wired DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-wired "HwAddress"))

(defun nm-device-wired-get-perm-hw-address (device-path)
  "Get permanent hardware address for wired DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-wired "PermHwAddress"))

(defun nm-device-wired-get-speed (device-path)
  "Get speed in Mb/s for wired DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-wired "Speed"))

(defun nm-device-wired-carrier-p (device-path)
  "Return t if wired DEVICE-PATH has carrier."
  (nm-dbus-get-property device-path nm-dbus-interface-wired "Carrier"))

(defun nm-device-wireless-get-hw-address (device-path)
  "Get hardware address for wireless DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-wireless "HwAddress"))

(defun nm-device-wireless-get-perm-hw-address (device-path)
  "Get permanent hardware address for wireless DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-wireless "PermHwAddress"))

(defun nm-device-wireless-get-mode (device-path)
  "Get mode for wireless DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-wireless "Mode"))

(defun nm-device-wireless-get-bitrate (device-path)
  "Get bitrate in Kb/s for wireless DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-wireless "Bitrate"))

(defun nm-device-wireless-get-access-points (device-path)
  "Get access points for wireless DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-wireless "AccessPoints"))

(defun nm-device-wireless-get-active-access-point (device-path)
  "Get active access point for wireless DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-wireless "ActiveAccessPoint"))

(defun nm-device-wireless-get-wireless-capabilities (device-path)
  "Get wireless capabilities for DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-wireless "WirelessCapabilities"))

(defun nm-device-wireless-get-last-scan (device-path)
  "Get last scan timestamp for wireless DEVICE-PATH."
  (nm-dbus-get-property device-path nm-dbus-interface-wireless "LastScan"))

(defun nm-device-wireless-request-scan (device-path &optional options)
  "Request scan on wireless DEVICE-PATH with optional OPTIONS."
  (nm-dbus-call-method device-path nm-dbus-interface-wireless "RequestScan"
                       (or options '(:array :signature "{sv}"))))

(defun nm-device-wireless-get-all-access-points (device-path)
  "Get all access points for wireless DEVICE-PATH."
  (nm-dbus-call-method device-path nm-dbus-interface-wireless "GetAllAccessPoints"))

(defun nm-device-register-state-changed (device-path handler)
  "Register HANDLER for state changes on DEVICE-PATH."
  (nm-dbus-register-signal device-path nm-dbus-interface-device "StateChanged" handler))

(defun nm-devices-register-added (handler)
  "Register HANDLER for device added signals."
  (nm-dbus-register-signal nm-path nm-dbus-interface-manager "DeviceAdded" handler))

(defun nm-devices-register-removed (handler)
  "Register HANDLER for device removed signals."
  (nm-dbus-register-signal nm-path nm-dbus-interface-manager "DeviceRemoved" handler))

(provide 'nm-device)
;;; nm-device.el ends here