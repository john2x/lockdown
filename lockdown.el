;;; lockdown.el --- Block network connections.

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; URL: https://github.com/john2x/lockdown
;; Version: 0.0.1-alpha

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a global minor mode that blocks network
;; connections to hosts that are not whitelisted.

;;; Code:

(require 'cl-lib)

(defgroup lockdown nil
  "Block network calls unless the host is whitelisted."
  :group 'tools)

(defcustom lockdown-hosts-whitelist nil
  "List of hostnames that Emacs will be allowed to connect to.
When set to nil, all hostnames are allowed.  Wildcards are
supported e.g. '*.example.com'.  Don't forget to call
`lockdown-hosts-whitelist-to-regexps' or toggle
`lockdown-global-mode' when updating this list."
  :group 'lockdown
  :type '(repeat string))

(defcustom lockdown-log-only nil
  "When set to non-nil, only log blocked connections."
  :group 'lockdown
  :type 'bool)

(defvar lockdown--hosts-whitelist-regexps nil
  "`lockdown-hosts-whitelist' converted to regexps.
Don't set this variable directly.  Instead, run
`lockdown-hosts-whitelist-to-regexps' to update this variable.")

(defun lockdown--host-to-regexp (host)
  "Convert HOST string to a regular expression."
  (let* ((h1 (replace-regexp-in-string "\\." "\\\\." host))
         (h2 (replace-regexp-in-string "\\*" ".*" h1)))
    (concat "^" h2 "$")))

(defun lockdown--host-allowed-p (host)
  "Check HOST against `lockdown-hosts-whitelist'."
  (if lockdown--hosts-whitelist-regexps
      (cl-some (lambda (r) (string-match r host)) lockdown--hosts-whitelist-regexps)
    t))

(defun lockdown-hosts-whitelist-to-regexps ()
  "Convert `lockdown-hosts-whitelist' to regexps and store in `lockdown--hosts-whitelist-regexps'."
  (setq lockdown--hosts-whitelist-regexps
        (mapcar 'lockdown--host-to-regexp lockdown-hosts-whitelist)))

(defun lockdown--make-network-process-advice (&rest args)
  "Advice function for `make-network-process'.
Block network calls to hosts that are not listed in `lockdown-hosts-whitelist'."
  (let ((host (plist-get args :host))
        (remote (plist-get args :remote)))
    (when (not (lockdown--host-allowed-p (or remote host)))
      (if lockdown-log-only
          (message "Lockdown: blocked! host=%s remote=%s" host remote)
        (error "Lockdown: blocked! host=%s remote=%s" host remote)))))

(define-minor-mode lockdown-global-mode
  "Toggle Lockdown mode globally.
Blocks network calls to hosts that are not listed in `lockdown-hosts-whitelist'."
  :init-value nil
  :lighter nil
  :group 'lockdown
  :global t
  (cond
   (lockdown-global-mode
    (lockdown-hosts-whitelist-to-regexps)
    (advice-add 'make-network-process :before #'lockdown--make-network-process-advice))
   (t
    (advice-remove 'make-network-process #'lockdown--make-network-process-advice))))

(provide 'lockdown)
;;; lockdown.el ends here
