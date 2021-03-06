

# Lockdown

Block network connections from Emacs to non-whitelisted hosts.

**Disclaimer/Warning**: This package is a work-in-progress/**proof-of-concept**. A malicious package could just as easily remove the `advice` set by this package, so it's not really usable.

It is implemented as an `advice` before
[`make-network-process`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Network-Processes.html#Network-Processes),
and checks the host or remote address of each network process call.

## Why

I currently have installed dozens of packages from MELPA (not stable)
which get updated regularly, and I do not have the time nor patience
to review each update if any of the packages have not been hijacked
and starts exfiltrating buffer contents to some remote server.

The only goal is to block packages from talking to unknown hosts.

## Configuration

```
(require 'lockdown)

;; Whitelisted host names. Wildcards are supported.
(setq lockdown-hosts-whitelist
  '("example.com" "*.example.com" "google.com" "*.google.com"))

;; Don't block requests, only log them to *Messages* buffer
;; (setq lockdown-log-only t)

;; Enable lockdown
(lockdown-global-mode)
```

## Future work

At the moment, only network connections done via
`make-network-process` are checked. A malicious package could still
use `shell-command` and `curl` to talk to some server. A similar
approach to advicing `make-process` and/or `call-process` to check for
`curl` usage (and `wget`, etc.) could work.

- [ ] `make-process`
- [ ] `call-process`
