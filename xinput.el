;; -*- lexical-binding: t -*-

;; Needed for 'seq-partition' in batch mode
(require 'seq)

(defvar *touchpad-id-regexp*
  (format "%s%s"
          "SynPS/2 Synaptics TouchPad"
          "[[:space:]]+id=\\([[:digit:]]+\\)"))

(defvar *touchpad-property-regexp*
  (format "%s%s"
          "Accel Speed"
          "[[:space:]](\\([[:digit:]]+\\))"))

(defun xinput-find-match-data (regexp &rest commands)
  "Invoke 'xinput' with ARGS.

Return a list of substrings of the output that match REGEXP."
  (with-temp-buffer
    (apply #'call-process "xinput" nil t nil commands)
    (beginning-of-buffer)
    (re-search-forward regexp)
    (mapcar (lambda (pair)
              (apply #'buffer-substring-no-properties pair))
            (seq-partition
             (butlast (match-data 'integers))
             2))))

(defun xinput-set-touchpad-speed (speed)
  "Set the touchpad speed to the given SPEED parameter."
  (let* ((device-id
          (nth 1 (xinput-find-match-data *touchpad-id-regexp* "list")))
         (device-property
          (nth 1 (xinput-find-match-data *touchpad-property-regexp* "list-props" device-id))))
    (call-process "xinput" nil nil nil "set-prop" device-id device-property speed)))

;; Let 'er rip!  This should fix our touchpad speed every time we
;; start Emacs, which is every time we launch an X session.
(xinput-set-touchpad-speed "1")
