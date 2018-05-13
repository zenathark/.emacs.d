;;; zen-docker.el --- Docker support configuration
;;; Commentary:
;;; Code:

(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package docker-tramp
  :ensure t
  :config
  (setq docker-tramp-method "winpty docker"))

(defhydra hydra-docker (:hint nil)
  "
^Build^          ^Code Info^                            ^Actions^
^^^^^^-------------------------------------------------------
_c_: containers  _F_: pull    _P_: push   _r_: restart  _z_: leave
_d_: rmi         _k_: rm      _p_: pause  _s_: start    _q_: quit
_e_: unpause     _i_: images  _o_: stop
"
  ("c" docker-containers)
  ("d" docker-rmi)
  ("e" docker-unpause)
  ("F" docker-pull)
  ("k" docker-rm)
  ("i" docker-images)
  ("o" docker-stop)
  ("P" docker-push)
  ("p" docker-pause)
  ("r" docker-restart)
  ("s" docker-start)
  ("z" nil "leave")
  ("q" quit-window "quit" :color blue))

(cond
 ((string-equal system-type "windows-nt")
  (progn
    ;(setq tramp-default-method "plink")
					;(setq python-shell-interpreter "root@localhost#2222:/usr/bin/python")
    (custom-set-variables '(docker-tramp-docker-executable "powershell docker"))))
 ((string-equal system-type "darwin")
  (setq tramp-default-method "ssh")))

(provide 'zen-docker)
;;; zen-docker ends here
