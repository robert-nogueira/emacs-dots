;;; aliases.el --- Personal Command Aliases
;;; Commentary:
;; This file contains a lot of command aliases :)

;;; Code:

(defalias '/spotify-search-tr 'counsel-spotify-search-track)
(defalias '/spotify-search-art 'counsel-spotify-search-artist)
(defalias '/spotify-search-alb 'counsel-spotify-search-album)
(defalias '/spotify-search-plist 'counsel-spotify-search-album)
(defalias '/spotify-n 'counsel-spotify-next)
(defalias '/spotify-p 'counsel-spotify-previous)
(defalias '/spotify-play 'counsel-spotify-play)
(defalias '/spotify-play-pause 'counsel-spotify-toggle-play-pause)
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'aliases)
;;; aliases.el ends here
