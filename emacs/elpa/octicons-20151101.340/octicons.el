;;; octicons.el --- octicons utility

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-octicons
;; Package-Version: 20151101.340
;; Package-Revision: 229286a6166d
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; octicons. provides utility function for octicons font

;;; Code:

(require 'cl-lib)
(require 'octicons-data)

(defsubst octicons--font-names ()
  (cl-loop for (name . _code) in octicons-alist
           collect name))

(defun octicons--completing-read ()
  (let ((comp-func (if ido-mode 'ido-completing-read 'completing-read)))
    (funcall comp-func "Font name: " (octicons--font-names) nil t)))

;;;###autoload
(defun octicons (font-name)
  "Return octicons code point"
  (interactive
   (list (octicons--completing-read)))
  (assoc-default font-name octicons-alist))

(provide 'octicons)

;;; octicons.el ends here
