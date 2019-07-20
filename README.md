# Lockdown

Block network connections to non-whitelisted hosts done by Emacs.

This package is a work-in-progress/proof-of-concept.

It is implemented as an `advice` before
[`make-network-process`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Network-Processes.html#Network-Processes),
and checks the host or remote address of each network process call.

## Why

I currently have installed dozens of packages from MELPA (not stable)
which get updated regularly, and I do not have the time nor patience
to review each update if any of the packages have not been hijacked
and starts exfiltrating buffer contents to some remote server.

The only goal is to block packages from talking to unknown hosts.

## Future work

At the moment, only network connections done via
`make-network-process` are checked. A malicious package could still
use `shell-command` and `curl` to talk to some server. A similar
approach to advicing `shell-command` to check for `curl` usage (and
`wget`, etc.) could work.
