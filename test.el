(defvar ash-mark-bol
  (save-excursion
    (goto-char (or (mark) (point)))
    (forward-line 0)
    (point-marker))
  "Marker from `beginning-of-line' for for `mark'.")

(defun ash-mark-hook-fun ()
  "Run with `activate-mark-hook'."
  (let ((mark-bol-posn (save-excursion
                         (goto-char (mark))
                         (forward-line 0)
                         (point))))
    (if (markerp ash-mark-bol)
        (set-marker ash-mark-bol mark-bol-posn)
      (setq ash-mark-bol
                     (save-excursion
                     (goto-char mark-bol-posn)
                     (point-marker))))))

(add-to-list 'activate-mark-hook 'ash-mark-hook-fun)
(setq overlay-arrow-position ash-mark-bol)

;; Use this to integrate PDT with flymake and place indicators on fringe
