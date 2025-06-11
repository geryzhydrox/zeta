#!/usr/bin/env -S guile
!#

(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (ice-9 match)
	     (zeta cmds)
	     (zeta term))

(match (command-line)
  ;; ((_ "init")			(zeta-init))
  ;; ((_ "init" init-location)		(zeta-init init-location))
  ((_ "add" manifest-path ...)		(zeta-add manifest-path))
  ((_ "del" manifest-path ...)		(zeta-del manifest-path))
  ((_ "install" manifest-path pkgs ...)	(zeta-install manifest-path pkgs))
  ((_  "remove" manifest-path pkgs ...)	(zeta-remove manifest-path pkgs))
  (_ (usage)))
