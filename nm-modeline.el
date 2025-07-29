;;; nm-modeline.el --- NetworkManager modeline indicator  -*- lexical-binding: t; -*-

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

;; Modeline indicator for NetworkManager connection status.

;;; Code:

(require 'nm)
(require 'nm-dbus)
(require 'nm-device)
(require 'nm-connection)
(require 'nm-wifi)
(require 'nm-vpn)

(declare-function nm-get-primary-connection "nm" ())
(declare-function nm-active-connections-info "nm-connection" ())
(declare-function nm-device-info "nm-device" (device-path))
(declare-function nm-device-wireless-get-active-access-point "nm-device" (device-path))
(declare-function nm-access-point-get-strength "nm-wifi" (ap-path))

(defgroup nm-modeline nil
  "NetworkManager modeline indicator."
  :group 'nm)

(defcustom nm-modeline-format " %s"
  "Format string for modeline display. %s is replaced with status."
  :type 'string
  :group 'nm-modeline)

(defcustom nm-modeline-refresh-interval 5
  "Refresh interval in seconds for modeline updates."
  :type 'integer
  :group 'nm-modeline)

(defcustom nm-modeline-display-format 'icon-and-text
  "Display format for modeline status.
Possible values:
  `icon-only'        - Show only the connection icon with tooltip
  `text-only'        - Show only the connection text with tooltip
  `icon-and-text'    - Show both icon and connection text with tooltip"
  :type '(choice (const :tag "Icon only" icon-only)
                 (const :tag "Text only" text-only)
                 (const :tag "Icon and text" icon-and-text))
  :group 'nm-modeline)


(defcustom nm-modeline-show-vpn t
  "Whether to show VPN status in modeline."
  :type 'boolean
  :group 'nm-modeline)

(defcustom nm-modeline-disconnected-icon "⚠"
  "Icon to show when disconnected."
  :type 'string
  :group 'nm-modeline)

(defcustom nm-modeline-ethernet-icon "🖧"
  "Icon for ethernet connection."
  :type 'string
  :group 'nm-modeline)

(defcustom nm-modeline-wifi-icon "📶"
  "Icon for WiFi connection."
  :type 'string
  :group 'nm-modeline)

(defcustom nm-modeline-wifi-icons
  '((high . "📶")
    (medium . "📶")
    (low . "📶")
    (none . "📶"))
  "Icons for different WiFi signal strengths."
  :type '(alist :key-type symbol :value-type string)
  :group 'nm-modeline)

(defcustom nm-modeline-vpn-icon "🔒"
  "Icon for VPN connection."
  :type 'string
  :group 'nm-modeline)

(defcustom nm-modeline-use-nerd-fonts nil
  "Use Nerd Font icons instead of emoji."
  :type 'boolean
  :group 'nm-modeline)

(defvar nm-modeline-string ""
  "Current modeline string.")

(defvar nm-modeline-timer nil
  "Timer for modeline updates.")

(defcustom nm-modeline-nerd-icons
  '((disconnected . "")
    (ethernet . "")
    (wifi-high . "")
    (wifi-medium . "")
    (wifi-low . "")
    (wifi-none . "")
    (vpn . ""))
  "Nerd Font icons for modeline. Customize these to use your preferred icons."
  :type '(alist :key-type symbol :value-type string)
  :group 'nm-modeline)

(defun nm-modeline-get-icon (type &optional strength)
  "Get icon for connection TYPE with optional signal STRENGTH."
  (if nm-modeline-use-nerd-fonts
      (pcase type
        ('disconnected (alist-get 'disconnected nm-modeline-nerd-icons))
        ('ethernet (alist-get 'ethernet nm-modeline-nerd-icons))
        ('wifi (cond
                ((and strength (>= strength 75))
                 (alist-get 'wifi-high nm-modeline-nerd-icons))
                ((and strength (>= strength 50))
                 (alist-get 'wifi-medium nm-modeline-nerd-icons))
                ((and strength (>= strength 25))
                 (alist-get 'wifi-low nm-modeline-nerd-icons))
                (t (alist-get 'wifi-none nm-modeline-nerd-icons))))
        ('vpn (alist-get 'vpn nm-modeline-nerd-icons))
        (_ "?"))
    (pcase type
      ('disconnected nm-modeline-disconnected-icon)
      ('ethernet nm-modeline-ethernet-icon)
      ('wifi (if strength
                  (cond
                   ((>= strength 75) (alist-get 'high nm-modeline-wifi-icons nm-modeline-wifi-icon))
                   ((>= strength 50) (alist-get 'medium nm-modeline-wifi-icons nm-modeline-wifi-icon))
                   ((>= strength 25) (alist-get 'low nm-modeline-wifi-icons nm-modeline-wifi-icon))
                   (t (alist-get 'none nm-modeline-wifi-icons nm-modeline-wifi-icon)))
                nm-modeline-wifi-icon))
      ('vpn nm-modeline-vpn-icon)
      (_ "?"))))

(defun nm-modeline-build-tooltip (type id device-info strength)
  "Build tooltip text for connection TYPE with ID, DEVICE-INFO and signal STRENGTH."
  (pcase type
    ("802-3-ethernet"
     (format "Ethernet: %s" (or (cdr (assoc 'interface device-info)) id)))
    ("802-11-wireless"
     (if strength
         (format "WiFi: %s (%d%%)" id strength)
       (format "WiFi: %s" id)))
    ("vpn"
     (format "VPN: %s" id))
    (_
     (format "%s: %s" type id))))

(defun nm-modeline-get-primary-info ()
  "Get primary connection information."
  (let ((primary-path (nm-get-primary-connection)))
    (when (and primary-path (not (string= primary-path "/")))
      (let ((active-conns (nm-active-connections-info)))
        (when-let ((primary-conn (seq-find (lambda (conn)
                                             (string= (cdr (assoc 'path conn)) primary-path))
                                           active-conns)))
          (list :type (cdr (assoc 'type primary-conn))
                :id (cdr (assoc 'id primary-conn))
                :device-path (car (cdr (assoc 'devices primary-conn)))))))))

(defun nm-modeline-format-ethernet (id device-info)
  "Format ethernet connection with ID and DEVICE-INFO."
  (let* ((interface (or (cdr (assoc 'interface device-info)) id))
         (icon (nm-modeline-get-icon 'ethernet))
         (tooltip (nm-modeline-build-tooltip "802-3-ethernet" interface device-info nil))
         (display-text (pcase nm-modeline-display-format
                         ('icon-only icon)
                         ('text-only interface)
                         ('icon-and-text (format "%s %s" icon interface)))))
    (propertize display-text 'help-echo tooltip)))

(defun nm-modeline-format-wifi (id device-path)
  "Format WiFi connection with ID and DEVICE-PATH."
  (let* ((ap-path (when device-path
                    (nm-device-wireless-get-active-access-point device-path)))
         (strength (when ap-path
                     (nm-access-point-get-strength ap-path)))
         (icon (nm-modeline-get-icon 'wifi strength))
         (tooltip (nm-modeline-build-tooltip "802-11-wireless" id nil strength))
         (text-part (format "%s%s" id (if strength (format " %d%%" strength) "")))
         (display-text (pcase nm-modeline-display-format
                         ('icon-only icon)
                         ('text-only text-part)
                         ('icon-and-text (format "%s %s" icon text-part)))))
    (propertize display-text 'help-echo tooltip)))

(defun nm-modeline-format-vpn-status (active-conns)
  "Format VPN status from ACTIVE-CONNS."
  (when nm-modeline-show-vpn
    (let ((vpn-active (seq-find (lambda (conn) (cdr (assoc 'vpn conn))) active-conns)))
      (when vpn-active
        (let* ((vpn-id (cdr (assoc 'id vpn-active)))
               (icon (nm-modeline-get-icon 'vpn))
               (tooltip (nm-modeline-build-tooltip "vpn" vpn-id nil nil))
               (display-text (pcase nm-modeline-display-format
                               ('icon-only icon)
                               ('text-only vpn-id)
                               ('icon-and-text (format "%s %s" icon vpn-id)))))
          (propertize display-text 'help-echo tooltip))))))

(defun nm-modeline-format-status ()
  "Format current network status for modeline."
  (condition-case nil
      (let* ((primary-info (nm-modeline-get-primary-info))
             (active-conns (nm-active-connections-info))
             status-parts)
        
        (if primary-info
            (let* ((type (plist-get primary-info :type))
                   (id (plist-get primary-info :id))
                   (device-path (plist-get primary-info :device-path))
                   (device-info (when device-path (nm-device-info device-path))))
              
              (pcase type
                ("802-3-ethernet"
                 (push (nm-modeline-format-ethernet id device-info) status-parts))
                
                ("802-11-wireless"
                 (push (nm-modeline-format-wifi id device-path) status-parts))
                
                (_
                 (let* ((icon (nm-modeline-get-icon 'ethernet))
                        (tooltip (format "%s: %s" type id))
                        (display-text (pcase nm-modeline-display-format
                                        ('icon-only icon)
                                        ('text-only id)
                                        ('icon-and-text (format "%s %s" icon id)))))
                   (push (propertize display-text 'help-echo tooltip) status-parts))))
              
              (let ((vpn-status (nm-modeline-format-vpn-status active-conns)))
                (when vpn-status
                  (push vpn-status status-parts))))
          
          (let* ((icon (nm-modeline-get-icon 'disconnected))
                 (tooltip "Network: Offline")
                 (display-text (pcase nm-modeline-display-format
                                 ('icon-only icon)
                                 ('text-only "Offline")
                                 ('icon-and-text (format "%s Offline" icon)))))
            (push (propertize display-text 'help-echo tooltip) status-parts)))
        
        (mapconcat #'identity status-parts " "))
    (error "")))

(defun nm-modeline-update ()
  "Update modeline string."
  (setq nm-modeline-string
        (format nm-modeline-format (nm-modeline-format-status)))
  (force-mode-line-update t))

(defun nm-modeline-start ()
  "Start modeline updates."
  (when nm-modeline-timer
    (cancel-timer nm-modeline-timer))
  (nm-modeline-update)
  (setq nm-modeline-timer
        (run-with-timer nm-modeline-refresh-interval
                        nm-modeline-refresh-interval
                        #'nm-modeline-update)))

(defun nm-modeline-stop ()
  "Stop modeline updates."
  (when nm-modeline-timer
    (cancel-timer nm-modeline-timer)
    (setq nm-modeline-timer nil))
  (setq nm-modeline-string "")
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode nm-modeline-mode
  "Toggle NetworkManager modeline indicator."
  :global t
  :group 'nm-modeline
  (if nm-modeline-mode
      (progn
        (add-to-list 'global-mode-string '(:eval nm-modeline-string) t)
        (nm-modeline-start))
    (nm-modeline-stop)
    (setq global-mode-string (delete '(:eval nm-modeline-string) global-mode-string))))

(provide 'nm-modeline)
;;; nm-modeline.el ends here