;; -*- lexical-binding: t -*-

(defvar *bci/touchpad-name* "SynPS/2 Synaptics TouchPad"
  "The name of my Thinkpad X201 touchpad, found by inspecting the
  output of 'xinput list'")

(defvar *bci/touchpad-property-name* "Accel Speed"
  "The property associated with my touchpad's cursor speed.")

(defvar *bci/touchpad-speed* "1"
  "The desired touchpad speed.")

(defun bci/xinput-find-datum (commands regexp)
  "Return all match-strings associated with finding REGEXP among
the output lines of the given xinput invocation (whose input is
given as a list of COMMANDS, e.g. (list \"list-props\" \"11\").

Usually, we'll only be interested in a single match-string >= 1,
though in theory we should be allowed to use regexps with as many
regexp groups as we desire; hence we return MATCHES as a list of
all such groups, including the entire match itself (index 0)"
  (with-temp-buffer
    ;; Insert output of xinput invocation into this buffer
    (apply #'call-process "xinput" nil t nil commands)
    (let* ((output-lines (split-string (buffer-string) "\n"))
           (target-line (seq-find
                         (lambda (line) (string-match regexp line))
                         output-lines)))
      (let ((index 0)
            (matches nil)
            (current-match nil))
        (while (setq current-match (match-string index target-line))
          (push (match-string index target-line) matches)
          (cl-incf index))
        matches))))

;; Let 'er rip!  This should fix our touchpad speed every time we
;; start Emacs, which is every time we launch an X session.
(let* ((device-id
        (car (bci/xinput-find-datum (list "list") (concat *bci/touchpad-name*  "[[:space:]]+id=\\([[:digit:]]+\\)"))))
       (device-property
        (car (bci/xinput-find-datum (list "list-props" device-id) (concat *bci/touchpad-property-name* "[[:space:]](\\([[:digit:]]+\\))")))))
  (call-process "xinput" nil nil nil "set-prop" device-id device-property "1"))
