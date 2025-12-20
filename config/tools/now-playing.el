;;; now-playing.el --- Display currently playing media in echo area
;;; Commentary:
;; This file provides a simple utility to show the currently playing media
;; (Spotify, YouTube Music, VLC, or any MPRIS-compatible player) in the
;; Emacs echo area. The message updates automatically whenever the song
;; or media changes.

;;; Code:

(defvar my/now-playing-last-song nil
  "Stores the last song or media to avoid redundant echo area updates.")

(defun my/now-playing-current ()
  "Update the echo area only if the currently playing media has changed."
  (interactive)
  (let ((song (string-trim
               (shell-command-to-string
                "playerctl metadata --format '{{ artist }} - {{ title }}' 2>/dev/null"))))
    ;; Only update if the song is non-empty and different from last
    (unless (equal song my/now-playing-last-song)
      (setq my/now-playing-last-song song)
      (when (> (length song) 0)
        (message "Playing: %s" song)))))

;; Automatically run the updater every 3 seconds
(run-with-timer 0 3 #'my/now-playing-current)

(provide 'now-playing)
;;; now-playing.el ends here
