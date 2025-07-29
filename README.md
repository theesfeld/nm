# nm - NetworkManager Interface for Emacs

A comprehensive NetworkManager interface for Emacs, providing complete control over network connections via D-Bus API. Manage WiFi, VPN, and network devices directly from Emacs with an intuitive interface.

## Features

- **Complete NetworkManager Control**: Full D-Bus API integration
- **WiFi Management**: Scan, connect, disconnect, and manage WiFi networks
- **VPN Support**: OpenVPN, WireGuard, L2TP, and more
- **Device Management**: Control network interfaces and their states
- **Connection Profiles**: Create, edit, and manage network connections
- **Interactive UI**: User-friendly interface with real-time updates
- **Tabulated List UI**: Enhanced sortable table views for WiFi, connections, and devices
- **Connection Editor**: Create and edit connections with interactive forms
- **Modeline Indicator**: Optional modeline display with customizable icons
- **Auto-refresh**: Automatic status updates with configurable interval
- **Security**: Support for WPA/WPA2/WPA3 and various VPN protocols
- **Notifications**: Desktop notifications for network state changes
- **Export/Import**: Backup and restore connection profiles
- **which-key Support**: Enhanced keybinding discovery with which-key integration

## Recent Enhancements

### Version 0.4.0 - Major UI Overhaul

This version introduces significant improvements to the user interface and experience:

#### Tabulated Interface
- Complete migration from text-based UI to `tabulated-list-mode` for all views
- Consistent, sortable columns with proper alignment
- Better keyboard navigation and standard Emacs list interactions

#### Visual Enhancements
- **Color-coded signal strength indicators**:
  - Excellent (75-100%): Green, bold
  - Good (50-74%): Lime green
  - Fair (25-49%): Yellow
  - Poor (0-24%): Orange red
- **Security indicators**: Open networks shown in red
- **Device state colors**: Connected (green), disconnected (gray), unavailable (red)  
- **Active connection highlighting**: Active connections shown in green

#### New Dashboard View
- Comprehensive overview accessible via `C-c N u`
- Shows system state, connectivity, and active connections
- Quick access to all NetworkManager functions
- Real-time status updates

#### Enhanced Modeline
- New tooltips showing connection details on hover
- Customizable display format: icon-only, text-only, or icon-and-text
- Improved formatting for all connection types

#### Interactive Elements
- Clickable buttons in connections list for New/Edit/Delete actions
- Better integration with mouse interactions
- Improved keyboard shortcuts across all modes

#### Auto-refresh Improvements
- Proper timer management for all tabulated modes
- Automatic cleanup when buffers are killed
- Configurable refresh intervals

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
  :init
  ;; Core settings (before package loads)
  (setq nm-auto-refresh t                   ; Auto-refresh network status
        nm-refresh-interval 5               ; Refresh every 5 seconds
        nm-service "org.freedesktop.NetworkManager"  ; D-Bus service name
        nm-path "/org/freedesktop/NetworkManager"    ; D-Bus object path
        nm-settings-path "/org/freedesktop/NetworkManager/Settings")  ; Settings path
  
  :config
  ;; Enable optional features
  (nm-modeline-mode 1)      ; Show connection status in modeline
  (nm-notify-mode 1)        ; Enable desktop notifications
  
  ;; Modeline customization - Display format
  (setq nm-modeline-display-format 'icon-and-text  ; 'icon-only, 'text-only, or 'icon-and-text
        nm-modeline-format " %s"           ; Format string (%s = status)
        nm-modeline-refresh-interval 5     ; Update interval in seconds
        nm-modeline-show-vpn t)            ; Show VPN status
  
  ;; Modeline icons - Connection types
  (setq nm-modeline-disconnected-icon "⚠"  ; No connection
        nm-modeline-ethernet-icon "🖧"     ; Ethernet connection
        nm-modeline-wifi-icon "📶"         ; WiFi base icon
        nm-modeline-vpn-icon "🔒")         ; VPN active
  
  ;; WiFi signal strength icons
  (setq nm-modeline-wifi-icons
        '((high . "▂▄▆█")     ; 75-100% signal
          (medium . "▂▄▆_")   ; 50-74% signal
          (low . "▂▄__")      ; 25-49% signal
          (none . "▂___")))   ; 0-24% signal
  
  ;; Alternative: Use Nerd Fonts (requires Nerd Font installed)
  ;; (setq nm-modeline-use-nerd-fonts t
  ;;       nm-modeline-nerd-icons
  ;;       '((disconnected . "")    ; nf-md-wifi_off
  ;;         (ethernet . "")        ; nf-md-ethernet
  ;;         (wifi-high . "")       ; nf-md-wifi_strength_4
  ;;         (wifi-medium . "")     ; nf-md-wifi_strength_3
  ;;         (wifi-low . "")        ; nf-md-wifi_strength_2
  ;;         (wifi-none . "")       ; nf-md-wifi_strength_1
  ;;         (vpn . "")))           ; nf-md-shield_key
  
  ;; Notification settings
  (setq nm-notify-enabled t                 ; Enable notifications
        nm-notify-use-notifications-lib t   ; Use desktop notifications
        nm-notify-connect-message "Connected to %s"
        nm-notify-disconnect-message "Disconnected from %s"
        nm-notify-state-change-message "Network state: %s"
        nm-notify-vpn-connect-message "VPN connected: %s"
        nm-notify-vpn-disconnect-message "VPN disconnected: %s")
  
  ;; Security settings
  (setq nm-secrets-use-auth-source t        ; Use Emacs auth-source
        nm-secrets-auth-source-host "NetworkManager"  ; Host for auth entries
        auth-sources '("~/.authinfo.gpg"))  ; Encrypted password storage
  
  ;; UI buffer names (for customization)
  (setq nm-ui-buffer-name "*NetworkManager*"
        nm-ui-wifi-buffer-name "*NetworkManager WiFi*"
        nm-ui-connection-buffer-name "*NetworkManager Connection*")
  
  ;; UI behavior settings (these are usually set via defvar, but can be customized)
  ;; (setq nm-ui-use-tabulated-list t      ; Use enhanced table views
  ;;       nm-ui-wifi-auto-scan t           ; Auto-scan when opening WiFi browser
  ;;       nm-ui-connections-show-auto nil) ; Hide autoconnect connections
  
  :bind-keymap
  ;; Main prefix key for all NetworkManager commands
  ("C-c N" . nm-prefix-map)
  
  :bind
  ;; Quick access bindings (optional - in addition to prefix map)
  (("C-c n s" . nm-status)              ; Quick status check
   ("C-c n w" . nm-ui-wifi)             ; Jump to WiFi browser
   ("C-c n c" . nm-ui-connections)      ; Jump to connections
   ("C-c n d" . nm-ui-dashboard)        ; Open dashboard
   ("C-c n n" . nm-toggle-networking)   ; Toggle networking
   ("C-c n W" . nm-toggle-wireless))    ; Toggle wireless
  
  :hook
  ;; Optional hooks for integration with other modes
  ((after-init . nm-modeline-mode)      ; Enable modeline on startup
   (after-init . nm-notify-mode))       ; Enable notifications on startup
  
  :custom-face
  ;; Optional face customizations for UI elements
  ;; (nm-ui-signal-excellent ((t (:foreground "green" :weight bold))))
  ;; (nm-ui-signal-good ((t (:foreground "lime green"))))
  ;; (nm-ui-signal-fair ((t (:foreground "yellow"))))
  ;; (nm-ui-signal-poor ((t (:foreground "orange red"))))
  ;; (nm-ui-security-open ((t (:foreground "red"))))
  ;; (nm-ui-active-connection ((t (:foreground "green" :weight bold))))
  )
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
2. **Open dashboard**: `C-c N u` - Opens the NetworkManager dashboard
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

### Modeline Indicator Options

| Variable                       | Default | Description                           |
|--------------------------------|---------|---------------------------------------|
| `nm-modeline-format`           | `" %s"` | Format string for modeline display    |
| `nm-modeline-refresh-interval` | `5`     | Refresh interval in seconds           |
| `nm-modeline-display-format`  | `'icon-and-text` | Display format: icon-only, text-only, or icon-and-text |
| `nm-modeline-show-vpn`         | `t`     | Whether to show VPN status            |
| `nm-modeline-disconnected-icon`| `"⚠"`   | Icon when disconnected                |
| `nm-modeline-ethernet-icon`    | `"🖧"`   | Icon for ethernet connection          |
| `nm-modeline-wifi-icon`        | `"📶"`  | Icon for WiFi connection              |
| `nm-modeline-wifi-icons`       | alist   | Icons for different WiFi strengths    |
| `nm-modeline-vpn-icon`         | `"🔒"`   | Icon for VPN connection               |
| `nm-modeline-use-nerd-fonts`   | `nil`   | Use Nerd Font icons instead of emoji  |
| `nm-modeline-nerd-icons`       | alist   | Customizable Nerd Font icons          |

You can customize WiFi strength icons separately:

```elisp
(setq nm-modeline-wifi-icons
      '((high . "📶")    ;; 75-100% signal
        (medium . "📶")  ;; 50-74% signal
        (low . "📶")     ;; 25-49% signal
        (none . "📶")))  ;; 0-24% signal
```

Or use custom Nerd Font icons:

```elisp
(setq nm-modeline-use-nerd-fonts t)
(setq nm-modeline-nerd-icons
      '((disconnected . "")
        (ethernet . "")
        (wifi-high . "")
        (wifi-medium . "")
        (wifi-low . "")
        (wifi-none . "")
        (vpn . "")))

### Notification Options

| Variable                          | Default               | Description                              |
|-----------------------------------|-----------------------|------------------------------------------|
| `nm-notify-enabled`               | `t`                   | Enable desktop notifications             |
| `nm-notify-use-notifications-lib` | `t`                   | Use notifications library if available   |
| `nm-notify-connect-message`       | `"Connected to %s"`   | Format for connection notifications      |
| `nm-notify-disconnect-message`    | `"Disconnected from %s"` | Format for disconnection notifications |

### UI Options

| Variable                      | Default | Description                           |
|-------------------------------|---------|---------------------------------------|
| `nm-ui-use-tabulated-list`    | `t`     | Use enhanced tabulated list views     |
| `nm-ui-wifi-auto-scan`        | `t`     | Auto-scan when opening WiFi browser   |
| `nm-ui-connections-show-auto` | `t`     | Show autoconnect connections          |
| `nm-ui-buffer-name`           | `"*NetworkManager*"` | Main UI buffer name      |
| `nm-ui-wifi-buffer-name`      | `"*NetworkManager WiFi*"` | WiFi browser buffer |

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
| `C-c N u` | `nm-ui-dashboard`        | Open NetworkManager dashboard  |
| `C-c N W` | `nm-ui-wifi`             | Open WiFi browser              |
| `C-c N E` | `nm-ui-ethernet`         | Open Ethernet browser          |
| `C-c N d` | `nm-ui-devices`          | Open device list               |
| `C-c N c` | `nm-ui-connections`      | Open connections manager       |
| `C-c N v` | `nm-vpn-activate`        | Activate VPN (with completion) |
| `C-c N V` | `nm-vpn-deactivate-all`  | Deactivate all VPNs            |
| `C-c N r` | `nm-reload`              | Reload NetworkManager config   |
| `C-c N ?` | `nm-show-help`           | Show all keybindings           |
| `C-c N T w` | `nm-ui-wifi-list`      | WiFi list (tabulated mode)     |
| `C-c N T c` | `nm-ui-connections-list` | Connections (tabulated mode) |
| `C-c N T d` | `nm-ui-devices-list`   | Devices (tabulated mode)       |

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
| `+`   | `nm-ui-new-connection`        | Create new connection    |
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

## Tabulated List Interfaces

The package includes enhanced tabulated-list-mode interfaces for better data presentation:

### WiFi List (`nm-ui-wifi-list`)

Displays WiFi networks in a sortable table:

```
M-x nm-ui-wifi-list
```

Features:
- Sortable columns (SSID, Channel, Security, Strength)
- Visual signal strength indicators
- Current connection highlighted
- Auto-refresh on scan

Key bindings:
- `RET` or `c` - Connect to network
- `d` - Disconnect current network
- `f` - Forget network
- `s` - Scan for networks
- `S` - Sort by column at point

### Connections List (`nm-ui-connections-list`)

Manage connections in a tabulated view:

```
M-x nm-ui-connections-list
```

Features:
- Shows all saved connections with status
- Active connections highlighted
- Sortable by name, type, or status

Key bindings:
- `RET` or `a` - Activate connection
- `d` - Deactivate connection
- `e` - Edit connection
- `D` - Delete connection
- `+` - Create new connection

### Devices List (`nm-ui-devices-list`)

View all network devices:

```
M-x nm-ui-devices-list
```

Features:
- Shows interface, type, state, driver
- Sortable columns
- Manage device settings

Key bindings:
- `RET` - Show device details
- `m` - Toggle managed state
- `a` - Toggle autoconnect
- `d` - Disconnect device

## Connection Export/Import

Export and import connection profiles for backup or sharing:

```elisp
;; Export single connection
(nm-connection-export connection-path "/path/to/file.nmconnection")

;; Export all connections
(nm-connection-export-all "/path/to/backup-directory/")

;; Import single connection
(nm-connection-import "/path/to/file.nmconnection")

;; Import directory of connections
(nm-connection-import-directory "/path/to/backup-directory/")

;; Export in NetworkManager keyfile format
(nm-connection-export-keyfile connection-path "/path/to/file.conf")
```

Interactive commands:
- `M-x nm-connection-export-all` - Export all connections to a directory
- `M-x nm-connection-import` - Import a connection file
- `M-x nm-connection-import-directory` - Import all connections from a directory

## Notifications

Enable desktop notifications for network state changes:

```elisp
;; Enable notifications
(nm-notify-mode 1)

;; Customize notification messages
(setq nm-notify-connect-message "Connected to %s 🎉")
(setq nm-notify-disconnect-message "Disconnected from %s 😢")
(setq nm-notify-vpn-connect-message "VPN secure: %s 🔒")

;; Use desktop notifications (requires notifications.el)
(setq nm-notify-use-notifications-lib t)
```

Notifications are shown for:
- Connection/disconnection events
- VPN state changes
- Network state changes
- Connectivity changes

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

### nm-modeline.el - Modeline Indicator

Optional modeline indicator showing:
- Current connection status
- Connection type (Ethernet/WiFi/VPN)
- WiFi signal strength
- VPN status indication
- Customizable icons (emoji or Nerd Fonts)

Enable the modeline indicator:
```elisp
;; Enable modeline indicator
(nm-modeline-mode 1)

;; Use Nerd Font icons instead of emoji
(setq nm-modeline-use-nerd-fonts t)

;; Customize refresh interval
(setq nm-modeline-refresh-interval 10)

;; Hide VPN status
(setq nm-modeline-show-vpn nil)

;; Show only icons without connection names
(setq nm-modeline-display-format 'icon-only)

;; Or show only text without icons
(setq nm-modeline-display-format 'text-only)
```

The modeline will display:
- `⚠ Offline` - No network connection
- `🖧 eth0` - Ethernet connection
- `📶 MyWiFi 85%` - WiFi with signal strength
- `🔒 VPN` - Active VPN connection

The display format can be customized with `nm-modeline-display-format`:
- `'icon-only` - Shows only icons
- `'text-only` - Shows only connection text
- `'icon-and-text` - Shows both icons and text (default)

Hover over the modeline indicator to see full connection details in a tooltip.

### nm-notify.el - Desktop Notifications

Desktop notification support for network state changes:
- Connection established/lost notifications
- VPN connect/disconnect alerts
- Customizable notification messages
- Integration with system notification daemon

Enable notifications:
```elisp
;; Enable desktop notifications
(nm-notify-mode 1)

;; Customize notification messages
(setq nm-notify-connect-message "Connected to %s 🎉"
      nm-notify-disconnect-message "Disconnected from %s ⚠️")

;; Use system notifications (requires notifications.el)
(setq nm-notify-use-notifications-lib t)
```

### nm-ui-tabulated.el - Enhanced Table Views

Professional tabulated list views for:
- WiFi network browser with sortable columns
- Connection manager with filtering
- Device list with detailed information

Features:
- Sortable columns (click headers or use `S`)
- Filtering and searching
- Better performance with large lists
- Consistent UI across all views

The tabulated views are automatically used when available. To force classic views:
```elisp
(setq nm-ui-use-tabulated-list nil)
```

## Security

### Password Management

NetworkManager Emacs uses secure password handling:

1. **No passwords in memory**: Passwords are cleared from memory immediately after use
2. **auth-source integration**: Passwords can be stored in your encrypted auth-source backend
3. **No passwords in connection files**: Passwords are never saved with connection profiles

Configure secure password storage:
```elisp
;; Use auth-source for password storage (default: t)
(setq nm-secrets-use-auth-source t)

;; Customize auth-source backend (e.g., use GPG-encrypted file)
(setq auth-sources '("~/.authinfo.gpg"))
```

When connecting to a secured network:
- First checks auth-source for saved passwords
- Prompts for password if not found
- Optionally saves to auth-source (with user confirmation)
- Clears password from memory after use

### NetworkManager Secret Agent

The package can act as a NetworkManager secret agent to handle authentication requests securely. This ensures passwords are only provided when NetworkManager needs them, not stored in connection profiles.

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
