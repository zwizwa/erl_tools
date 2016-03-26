(defun erl_tools-erl ()
  (interactive)
  (make-comint "erl_tools" "~/erl_tools/erl.sh")
  (switch-to-buffer "*erl_tools*")
  (let ((p (get-process "erl_tools")))
    (comint-send-string p "l(tools).\n")
  ))

(defun erl_tools-compile (cmd)
  (setq my-compilation-ok 'erl_tools-erl)
  (compile cmd))

(defun dev-erl_tools ()
  (interactive)
  (erl_tools-compile "make -C ~/erl_tools"))


