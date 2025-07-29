# nm - NetworkManager Interface for Emacs

A comprehensive NetworkManager interface for Emacs, providing complete control over network connections via D-Bus API. Manage WiFi, VPN, and network devices directly from Emacs with an intuitive interface.

## Features

- **Complete NetworkManager Control**: Full D-Bus API integration
- **WiFi Management**: Scan, connect, disconnect, and manage WiFi networks
- **VPN Support**: OpenVPN, WireGuard, L2TP, and more
- **Device Management**: Control network interfaces and their states
- **Connection Profiles**: Create, edit, and manage network connections
- **Interactive UI**: User-friendly interface with real-time updates
- **Auto-refresh**: Automatic status updates with configurable interval
- **Security**: Support for WPA/WPA2/WPA3 and various VPN protocols

## Requirements

- Emacs 30.1 or later
- D-Bus support compiled into Emacs
- NetworkManager service running on your system

## Installation

### Using use-package with :vc (recommended)

```elisp
(use-package nm
  :vc (:url "https://github.com/theesfeld/nm")
  :ensure t
  :config
  (setq nm-auto-refresh t
        nm-refresh-interval 5)
  :bind-keymap
  ("C-c n" . nm-prefix-map))
```

## Customization

All customizable variables with their default values:

| Variable              | Default                                      | Description                                     |
|-----------------------|----------------------------------------------|-------------------------------------------------|
| `nm-service`          | `"org.freedesktop.NetworkManager"`           | D-Bus service name for NetworkManager           |
| `nm-path`             | `"/org/freedesktop/NetworkManager"`          | D-Bus object path for NetworkManager            |
| `nm-settings-path`    | `"/org/freedesktop/NetworkManager/Settings"` | D-Bus object path for NetworkManager Settings   |
| `nm-auto-refresh`     | `t`                                          | Whether to automatically refresh network status |
| `nm-refresh-interval` | `5`                                          | Interval in seconds for automatic refresh       |

## Usage

### Basic Operations

```elisp
;; Check NetworkManager status
(nm-status)

;; Check if NetworkManager is available
(nm-available-p)

;; Get NetworkManager version
(nm-get-version)

;; Check connectivity
(nm-connectivity-to-string (nm-get-connectivity))

;; Toggle networking on/off
(nm-toggle-networking)

;; Toggle wireless on/off
(nm-toggle-wireless)
```

### WiFi Operations

```elisp
;; Scan for WiFi networks
(nm-wifi-scan-all)

;; Get all available access points
(nm-wifi-get-all-access-points)

;; Connect to open WiFi
(nm-wifi-connect "MyNetwork")

;; Connect to secured WiFi
(nm-wifi-connect "SecureNetwork" "password")

;; Disconnect current WiFi
(nm-wifi-disconnect)

;; Forget a saved network
(nm-wifi-forget-network "OldNetwork")

;; Create WiFi hotspot
(nm-wifi-create-hotspot "MyHotspot" "password123")
```

### Connection Management

```elisp
;; List all connections
(nm-connections-info)

;; Get active connections
(nm-active-connections-info)

;; Activate a connection
(nm-activate-connection connection-path device-path "/")

;; Deactivate a connection
(nm-deactivate-connection active-connection-path)

;; Delete a connection
(nm-connection-delete connection-path)

;; Create new Ethernet connection
(nm-create-ethernet-connection "Work LAN")
```

### VPN Operations

```elisp
;; List VPN connections
(nm-vpn-get-connections)

;; Activate VPN
(nm-vpn-activate "MyVPN")

;; Deactivate VPN
(nm-vpn-deactivate "MyVPN")

;; Deactivate all VPNs
(nm-vpn-deactivate-all)

;; Create OpenVPN connection
(nm-vpn-create-openvpn "Work VPN" "/path/to/config.ovpn")

;; Create WireGuard connection
(nm-vpn-create-wireguard "WG-VPN"
                         "private-key"
                         "server:51820"
                         "public-key")

;; Create L2TP connection
(nm-vpn-create-l2tp "L2TP-VPN"
                    "vpn.example.com"
                    "username"
                    "password"
                    "preshared-key")
```

### Device Management

```elisp
;; List all devices
(nm-devices-info)

;; Get device state
(nm-device-get-state device-path)

;; Set device managed state
(nm-device-set-managed device-path t)

;; Disconnect device
(nm-device-disconnect device-path)

;; Get device IP configuration
(nm-device-get-ip4-config device-path)
```

## Keybindings

### Global Prefix Map

After loading the package, all commands are available under `C-c n`:

| Key     | Command                  | Description              |
|---------|--------------------------|--------------------------|
| `C-c n s` | `nm-status`            | Show NetworkManager status |
| `C-c n n` | `nm-toggle-networking` | Toggle networking on/off |
| `C-c n w` | `nm-toggle-wireless`   | Toggle wireless on/off   |
| `C-c n u` | `nm-ui`                | Open main UI dashboard   |
| `C-c n W` | `nm-ui-wifi`           | Open WiFi browser        |
| `C-c n c` | `nm-ui-connections`    | Open connections manager |
| `C-c n v` | `nm-vpn-activate`      | Activate VPN (with completion) |
| `C-c n V` | `nm-vpn-deactivate-all`| Deactivate all VPNs     |
| `C-c n r` | `nm-reload`            | Reload NetworkManager config |
| `C-c n ?` | `nm-show-help`         | Show all keybindings     |

If you have `which-key` installed, pressing `C-c n` will show all available commands.

## User Interface

### Main Interface (`nm-ui`)

Launch the main NetworkManager interface:

```
M-x nm-ui
```

Key bindings:

| Key | Command                | Description              |
|-----|------------------------|--------------------------|
| `g` | `nm-ui-refresh`        | Refresh display          |
| `q` | `quit-window`          | Quit window              |
| `n` | `nm-toggle-networking` | Toggle networking on/off |
| `w` | `nm-toggle-wireless`   | Toggle wireless on/off   |
| `W` | `nm-ui-wifi`           | Open WiFi browser        |
| `C` | `nm-ui-connections`    | Open connections list    |

### WiFi Browser (`nm-ui-wifi`)

Browse and connect to WiFi networks:

```
M-x nm-ui-wifi
```

Key bindings:

| Key   | Command                    | Description                 |
|-------|----------------------------|-----------------------------|
| `g`   | `nm-ui-refresh`            | Refresh network list        |
| `s`   | `nm-ui-scan-wifi`          | Scan for networks           |
| `q`   | `quit-window`              | Quit window                 |
| `RET` | `nm-ui-connect-to-network` | Connect to network at point |
| `d`   | `nm-ui-disconnect`         | Disconnect current network  |
| `f`   | `nm-ui-forget-network`     | Forget network at point     |

### Connections Manager (`nm-ui-connections`)

Manage network connection profiles:

```
M-x nm-ui-connections
```

Key bindings:

| Key   | Command                       | Description              |
|-------|-------------------------------|--------------------------|
| `g`   | `nm-ui-refresh`               | Refresh connections list |
| `q`   | `quit-window`                 | Quit window              |
| `RET` | `nm-ui-activate-connection`   | Activate connection      |
| `a`   | `nm-ui-activate-connection`   | Activate connection      |
| `d`   | `nm-ui-deactivate-connection` | Deactivate connection    |
| `e`   | `nm-ui-edit-connection`       | Edit connection          |
| `D`   | `nm-ui-delete-connection`     | Delete connection        |

## Module Documentation

### nm.el - Main Module

Core NetworkManager interface providing:
- Service availability checking
- Global state management
- Connectivity monitoring
- Networking and wireless control
- Primary connection tracking

Key functions:
- `nm-available-p`: Check if NetworkManager is available
- `nm-get-state`: Get global NetworkManager state
- `nm-get-connectivity`: Get connectivity status
- `nm-check-connectivity`: Force connectivity check
- `nm-enable-networking`: Enable/disable networking

### nm-dbus.el - D-Bus Communication Layer

Low-level D-Bus interface providing:
- Method calls and property access
- Signal registration
- Data type conversion
- Settings serialization

Key functions:
- `nm-dbus-call-method`: Synchronous D-Bus method calls
- `nm-dbus-get-property`: Get D-Bus properties
- `nm-dbus-register-signal`: Register signal handlers
- `nm-dbus-connection-settings-to-alist`: Convert settings to Elisp alist
- `nm-dbus-alist-to-connection-settings`: Convert alist to D-Bus format

### nm-device.el - Device Management

Network device control providing:
- Device enumeration and properties
- State management
- Interface configuration
- Wired and wireless device control

Key functions:
- `nm-get-devices`: List all network devices
- `nm-device-info`: Get comprehensive device information
- `nm-device-disconnect`: Disconnect a device
- `nm-device-set-managed`: Set device managed state
- `nm-device-wireless-request-scan`: Request WiFi scan

### nm-connection.el - Connection Profiles

Connection management providing:
- Profile creation and modification
- Connection activation/deactivation
- Settings management
- Active connection monitoring

Key functions:
- `nm-list-connections`: List all saved connections
- `nm-add-connection`: Add new connection profile
- `nm-activate-connection`: Activate a connection
- `nm-connection-delete`: Delete a connection
- `nm-connection-get-settings`: Get connection settings

### nm-wifi.el - WiFi Operations

WiFi-specific functionality:
- Access point scanning and information
- Network connection with security
- Hotspot creation
- Signal strength visualization

Key functions:
- `nm-wifi-scan-all`: Scan all WiFi devices
- `nm-wifi-connect`: Connect to WiFi network
- `nm-wifi-get-all-access-points`: Get all visible APs
- `nm-wifi-create-hotspot`: Create WiFi hotspot
- `nm-wifi-strength-bars`: Visual signal strength

### nm-vpn.el - VPN Support

VPN management supporting:
- Multiple VPN protocols
- Connection import/export
- Profile creation
- State monitoring

Key functions:
- `nm-vpn-get-connections`: List VPN connections
- `nm-vpn-activate`: Activate VPN by name
- `nm-vpn-create-openvpn`: Create OpenVPN profile
- `nm-vpn-create-wireguard`: Create WireGuard profile
- `nm-vpn-import-config`: Import VPN configuration

### nm-ui.el - User Interface

Interactive interface providing:
- Status dashboard
- WiFi browser
- Connection manager
- Real-time updates
- Password prompts

Key functions:
- `nm-ui`: Open main interface
- `nm-ui-wifi`: Open WiFi browser
- `nm-ui-connections`: Open connection manager
- `nm-ui-refresh`: Refresh current view
- `nm-ui-password-prompt`: Secure password input

## Troubleshooting

### NetworkManager service not available

If you get "NetworkManager service not available" errors:

1. Check if NetworkManager is running:
   ```bash
   systemctl status NetworkManager
   ```

2. Verify D-Bus is running:
   ```bash
   systemctl status dbus
   ```

3. Ensure Emacs has D-Bus support:
   ```elisp
   (featurep 'dbusbind)  ; Should return t
   ```

### Connection failures

Enable D-Bus debugging:
```elisp
(setq dbus-debug t)
```

Check NetworkManager logs:
```bash
journalctl -u NetworkManager -f
```

### Permission issues

Ensure your user has permission to manage network connections:
```elisp
(nm-get-permissions)  ; Check available permissions
```

You may need to configure PolicyKit rules for your user.

### WiFi scanning issues

If WiFi scans don't show networks:

1. Check if wireless is enabled:
   ```elisp
   (nm-wireless-enabled-p)
   ```

2. Check hardware switch:
   ```elisp
   (nm-wireless-hardware-enabled-p)
   ```

3. Manually trigger scan:
   ```elisp
   (nm-wifi-scan-all)
   ```

## License

Copyright (C) 2025 William Theesfeld <william@theesfeld.net>

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
