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
- **which-key Support**: Enhanced keybinding discovery with which-key integration

## Requirements

- Emacs 30.1 or later
- D-Bus support compiled into Emacs (check with `(featurep 'dbusbind)`)
- NetworkManager service running on your system
- Optional: `which-key` package for enhanced keybinding discovery

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
  ("C-c N" . nm-prefix-map))
```

### Manual Installation

Clone the repository and add to your load path:

```bash
git clone https://github.com/theesfeld/nm ~/.emacs.d/site-lisp/nm
```

Then add to your Emacs configuration:

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/nm")
(require 'nm)
(global-set-key (kbd "C-c N") nm-prefix-map)
```

## Quick Start

After installation, you can quickly access NetworkManager features:

1. **Check status**: `C-c N s` - Shows NetworkManager status
2. **Open dashboard**: `C-c N u` - Opens the main UI
3. **Browse WiFi**: `C-c N W` - Opens WiFi browser and starts scanning
4. **Manage connections**: `C-c N c` - Opens connection manager
5. **Help**: `C-c N ?` - Shows all available keybindings

For interactive use, the UI modes provide the most convenient interface:
- `M-x nm-ui` - Main dashboard with status overview
- `M-x nm-ui-wifi` - WiFi network browser with connect/disconnect
- `M-x nm-ui-connections` - Connection profile manager
- `M-x nm-ui-devices` - Device management interface

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

;; Connect to hidden network
(nm-wifi-connect "HiddenNetwork" "password" t)

;; Connect to specific BSSID
(nm-wifi-connect "MyNetwork" "password" nil "AA:BB:CC:DD:EE:FF")

;; Disconnect current WiFi
(nm-wifi-disconnect)

;; Forget a saved network
(nm-wifi-forget-network "OldNetwork")

;; Set network autoconnect
(nm-wifi-set-autoconnect "MyNetwork" t)

;; Create WiFi hotspot
(nm-wifi-create-hotspot "MyHotspot" "password123")

;; Create hotspot with band selection
(nm-wifi-create-hotspot "MyHotspot" "password123" "bg")
```

### Connection Management

```elisp
;; List all connections
(nm-connections-info)

;; Get active connections
(nm-active-connections-info)

;; Get connection settings
(nm-connection-get-settings connection-path)

;; Activate a connection by path
(nm-activate-connection connection-path device-path "/")

;; Add and activate a new connection
(nm-add-and-activate-connection settings device-path "/")

;; Deactivate a connection
(nm-deactivate-connection active-connection-path)

;; Delete a connection
(nm-connection-delete connection-path)

;; Create new Ethernet connection
(nm-create-ethernet-connection "Work LAN")

;; Create Ethernet connection with static IP
(nm-create-ethernet-connection "Work LAN" "192.168.1.100/24")

;; Create WiFi connection (returns settings dict)
(nm-create-wifi-connection "MyNetwork" "password")

;; Create hidden WiFi connection
(nm-create-wifi-connection "HiddenNetwork" "password" t)

;; Update connection settings
(nm-connection-update settings)

;; Get connection by UUID
(nm-get-connection-by-uuid uuid)
```

### VPN Operations

```elisp
;; List VPN connections
(nm-vpn-get-connections)

;; Get active VPN connections
(nm-vpn-get-active-connections)

;; Activate VPN
(nm-vpn-activate "MyVPN")

;; Deactivate VPN
(nm-vpn-deactivate "MyVPN")

;; Deactivate all VPNs
(nm-vpn-deactivate-all)

;; Import VPN configuration (supports .ovpn, .conf, .wg)
(nm-vpn-import-config "/path/to/config.ovpn")

;; Create OpenVPN connection
(nm-vpn-create-openvpn "Work VPN" "/path/to/config.ovpn")

;; Create OpenVPN with credentials
(nm-vpn-create-openvpn "Work VPN" "/path/to/config.ovpn" "username" "password")

;; Create WireGuard connection
(nm-vpn-create-wireguard "WG-VPN"
                         "private-key"
                         "server:51820"
                         "public-key")

;; Create WireGuard with preshared key
(nm-vpn-create-wireguard "WG-VPN"
                         "private-key"
                         "server:51820"
                         "public-key"
                         "preshared-key")

;; Create L2TP connection
(nm-vpn-create-l2tp "L2TP-VPN"
                    "vpn.example.com"
                    "username"
                    "password"
                    "preshared-key")

;; Set VPN autoconnect
(nm-vpn-set-autoconnect "MyVPN" t)

;; Get VPN kill switch status
(nm-vpn-get-kill-switch "MyVPN")

;; Enable VPN kill switch
(nm-vpn-set-kill-switch "MyVPN" t)
```

### Device Management

```elisp
;; List all devices
(nm-devices-info)

;; Get device state
(nm-device-get-state device-path)

;; Get device state as string
(nm-dbus-device-state-to-string (nm-device-get-state device-path))

;; Set device managed state
(nm-device-set-managed device-path t)

;; Set device autoconnect
(nm-device-set-autoconnect device-path t)

;; Disconnect device
(nm-device-disconnect device-path)

;; Get device IP configuration
(nm-device-get-ip4-config device-path)

;; Get device IPv6 configuration
(nm-device-get-ip6-config device-path)

;; Request wireless scan on device
(nm-device-wireless-request-scan device-path)

;; Get available connections for device
(nm-device-get-available-connections device-path)

;; Get device driver
(nm-device-get-driver device-path)

;; Get device firmware version
(nm-device-get-firmware-version device-path)
```

## Keybindings

### Global Prefix Map

After loading the package, all commands are available under `C-c N`:

| Key       | Command                  | Description                    |
|-----------|--------------------------|--------------------------------|
| `C-c N s` | `nm-status`              | Show NetworkManager status     |
| `C-c N n` | `nm-toggle-networking`   | Toggle networking on/off       |
| `C-c N w` | `nm-toggle-wireless`     | Toggle wireless on/off         |
| `C-c N u` | `nm-ui`                  | Open main UI dashboard         |
| `C-c N W` | `nm-ui-wifi`             | Open WiFi browser              |
| `C-c N E` | `nm-ui-ethernet`         | Open Ethernet browser          |
| `C-c N d` | `nm-ui-devices`          | Open device list               |
| `C-c N c` | `nm-ui-connections`      | Open connections manager       |
| `C-c N v` | `nm-vpn-activate`        | Activate VPN (with completion) |
| `C-c N V` | `nm-vpn-deactivate-all`  | Deactivate all VPNs            |
| `C-c N r` | `nm-reload`              | Reload NetworkManager config   |
| `C-c N ?` | `nm-show-help`           | Show all keybindings           |

If you have `which-key` installed, pressing `C-c N` will show all available commands with descriptions.

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
| `s` | `nm-status`            | Show status              |
| `u` | `nm-ui`                | Dashboard                |
| `W` | `nm-ui-wifi`           | Open WiFi browser        |
| `E` | `nm-ui-ethernet`       | Open Ethernet browser    |
| `d` | `nm-ui-devices`        | Open device list         |
| `c` | `nm-ui-connections`    | Open connections list    |
| `C` | `nm-ui-connections`    | Open connections list    |
| `v` | `nm-vpn-activate`      | Activate VPN             |
| `V` | `nm-vpn-deactivate-all`| Deactivate all VPNs      |
| `r` | `nm-reload`            | Reload config            |
| `?` | `nm-show-help`         | Show help                |

### WiFi Browser (`nm-ui-wifi`)

Browse and connect to WiFi networks:

```
M-x nm-ui-wifi
```

Key bindings:

| Key   | Command                    | Description                 |
|-------|----------------------------|-----------------------------|
| `g`   | `nm-ui-refresh`            | Refresh network list        |
| `s`   | `nm-ui-scan-wifi`          | Scan for WiFi networks      |
| `S`   | `nm-status`                | Show status                 |
| `q`   | `quit-window`              | Quit window                 |
| `RET` | `nm-ui-connect-to-network` | Connect to network at point |
| `d`   | `nm-ui-disconnect`         | Disconnect current network  |
| `f`   | `nm-ui-forget-network`     | Forget network at point     |
| `n`   | `nm-toggle-networking`     | Toggle networking on/off    |
| `w`   | `nm-toggle-wireless`       | Toggle wireless on/off      |
| `u`   | `nm-ui`                    | Dashboard                   |
| `W`   | `nm-ui-wifi`               | WiFi browser                |
| `E`   | `nm-ui-ethernet`           | Ethernet browser            |
| `l`   | `nm-ui-devices`            | Device list                 |
| `c`   | `nm-ui-connections`        | Connections                 |
| `v`   | `nm-vpn-activate`          | Activate VPN                |
| `V`   | `nm-vpn-deactivate-all`    | Deactivate all VPNs         |
| `?`   | `nm-show-help`             | Show help                   |

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
| `s`   | `nm-status`                   | Show status              |
| `n`   | `nm-toggle-networking`        | Toggle networking on/off |
| `w`   | `nm-toggle-wireless`          | Toggle wireless on/off   |
| `u`   | `nm-ui`                       | Dashboard                |
| `W`   | `nm-ui-wifi`                  | WiFi browser             |
| `E`   | `nm-ui-ethernet`              | Ethernet browser         |
| `c`   | `nm-ui-connections`           | Connections              |
| `l`   | `nm-ui-devices`               | Device list              |
| `v`   | `nm-vpn-activate`             | Activate VPN             |
| `V`   | `nm-vpn-deactivate-all`       | Deactivate all VPNs      |
| `r`   | `nm-reload`                   | Reload config            |
| `?`   | `nm-show-help`                | Show help                |

### Ethernet Browser (`nm-ui-ethernet`)

Browse and manage Ethernet devices:

```
M-x nm-ui-ethernet
```

Key bindings:

| Key   | Command                      | Description              |
|-------|------------------------------|--------------------------|
| `g`   | `nm-ui-refresh`              | Refresh device list      |
| `q`   | `quit-window`                | Quit window              |
| `RET` | `nm-ui-activate-ethernet-device` | Activate device      |
| `a`   | `nm-ui-activate-ethernet-device` | Activate device      |
| `d`   | `nm-ui-disconnect-device`    | Disconnect device        |
| `s`   | `nm-status`                  | Show status              |
| `n`   | `nm-toggle-networking`       | Toggle networking on/off |
| `w`   | `nm-toggle-wireless`         | Toggle wireless on/off   |
| `u`   | `nm-ui`                      | Dashboard                |
| `W`   | `nm-ui-wifi`                 | WiFi browser             |
| `E`   | `nm-ui-ethernet`             | Ethernet browser         |
| `l`   | `nm-ui-devices`              | Device list              |
| `c`   | `nm-ui-connections`          | Connections              |
| `v`   | `nm-vpn-activate`            | Activate VPN             |
| `V`   | `nm-vpn-deactivate-all`      | Deactivate all VPNs      |
| `r`   | `nm-reload`                  | Reload config            |
| `?`   | `nm-show-help`               | Show help                |

### Device Manager (`nm-ui-devices`)

View and manage all network devices:

```
M-x nm-ui-devices
```

Key bindings:

| Key   | Command                       | Description                    |
|-------|-------------------------------|--------------------------------|
| `g`   | `nm-ui-refresh`               | Refresh device list            |
| `q`   | `quit-window`                 | Quit window                    |
| `RET` | `nm-ui-show-device-details`   | Show device details            |
| `m`   | `nm-ui-toggle-device-managed` | Toggle managed state           |
| `a`   | `nm-ui-toggle-device-autoconnect` | Toggle autoconnect        |
| `d`   | `nm-ui-disconnect-device`     | Disconnect device              |
| `s`   | `nm-status`                   | Show status                    |
| `n`   | `nm-toggle-networking`        | Toggle networking on/off       |
| `w`   | `nm-toggle-wireless`          | Toggle wireless on/off         |
| `u`   | `nm-ui`                       | Dashboard                      |
| `W`   | `nm-ui-wifi`                  | WiFi browser                   |
| `E`   | `nm-ui-ethernet`              | Ethernet browser               |
| `l`   | `nm-ui-devices`               | Device list                    |
| `c`   | `nm-ui-connections`           | Connections                    |
| `v`   | `nm-vpn-activate`             | Activate VPN                   |
| `V`   | `nm-vpn-deactivate-all`       | Deactivate all VPNs            |
| `r`   | `nm-reload`                   | Reload config                  |
| `?`   | `nm-show-help`                | Show help                      |

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
- `nm-wifi-connect`: Connect to WiFi network with optional password, hidden, and BSSID
- `nm-wifi-connect-to-ap`: Connect using access point info structure
- `nm-wifi-get-all-access-points`: Get all visible APs with details
- `nm-wifi-disconnect`: Disconnect current WiFi
- `nm-wifi-forget-network`: Remove saved network profile
- `nm-wifi-set-autoconnect`: Configure network autoconnect
- `nm-wifi-create-hotspot`: Create WiFi hotspot
- `nm-wifi-strength-bars`: Visual signal strength indicator
- `nm-wifi-get-current-connection`: Get current WiFi connection info

### nm-vpn.el - VPN Support

VPN management supporting:
- Multiple VPN protocols (OpenVPN, WireGuard, L2TP)
- Connection import/export
- Profile creation
- State monitoring
- Kill switch configuration

Key functions:
- `nm-vpn-get-connections`: List VPN connections
- `nm-vpn-get-active-connections`: Get active VPN connections
- `nm-vpn-activate`: Activate VPN by name
- `nm-vpn-deactivate`: Deactivate VPN by name
- `nm-vpn-deactivate-all`: Deactivate all active VPNs
- `nm-vpn-create-openvpn`: Create OpenVPN profile
- `nm-vpn-create-wireguard`: Create WireGuard profile
- `nm-vpn-create-l2tp`: Create L2TP profile
- `nm-vpn-import-config`: Import VPN configuration file
- `nm-vpn-set-autoconnect`: Configure VPN autoconnect
- `nm-vpn-get-kill-switch`: Get kill switch status
- `nm-vpn-set-kill-switch`: Configure kill switch

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
