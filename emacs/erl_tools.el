;; Run expect test.  This depends on a Makefile to generate
;; .expect.new from .expect and the rest of the project state.

(defun erl-tools-expect-run ()
  (interactive)
  (let* ((path (buffer-file-name))
         (dir (file-name-directory path))
         (file (file-name-nondirectory path))
         (new (concat file ".notify_emacs"))
         (cmd (concat "make -C " dir " " new)))
    (save-excursion
      (save-buffer)
      ;; Makefile will call erl-tools-expect-notify
      (compile cmd))))

;; Called from Makefile
(defun erl-tools-expect-revert (file)
  (switch-to-buffer file)
  (revert-buffer t t))
  
;; Example of Makefile to place next to .expect files.
;; ---------------------------------------------------
;;
;; # Assume the test basename is the same as the module basename.
;; %.expect.new: %.expect ../src/%.erl
;; 	cd .. ; ./rebar3/rebar3 eunit --module=$* ; exit 0
;;
;; # Copy over the original and notify.
;; %.expect.notify_emacs: %.expect.new
;; 	cp -a $< $*.expect
;; 	emacsclient -e "(erl-tools-expect-revert \"$*.expect\")"






;; FIXME: still useful?

;; (defun erl_tools-erl ()
;;   (interactive)
;;   (make-comint "erl_tools" "~/erl_tools/erl.sh")
;;   (switch-to-buffer "*erl_tools*")
;;   (let ((p (get-process "erl_tools")))
;;     (comint-send-string p "l(tools).\n")
;;   ))

;; (defun erl_tools-compile (cmd)
;;   (setq my-compilation-ok 'erl_tools-erl)
;;   (compile cmd))

;; (defun dev-erl_tools ()
;;   (interactive)
;;   (erl_tools-compile "make -C ~/erl_tools"))


