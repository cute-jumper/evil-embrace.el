;;; evil-embrace.el --- Evil integration of embrace.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions

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

;;

;;; Code:

(defvar embrace-evil-surround-key '(?\( ?\[ ?\{ ?\) ?\] ?\} ?\" ?\' ?b ?B ?t)
  "Keys that should be processed by `evil-surround'")
(make-variable-buffer-local 'embrace-evil-surround-key)

;;; `evil-surround' integration
(defun embrace-evil-surround-delete (char &optional outer inner)
  (interactive "c")
  (cond
   ((and outer inner)
    (delete-region (overlay-start outer) (overlay-start inner))
    (delete-region (overlay-end inner) (overlay-end outer))
    (goto-char (overlay-start outer)))
   (t
    (if (member char embrace-evil-surround-key)
        (let* ((outer (evil-surround-outer-overlay char))
               (inner (evil-surround-inner-overlay char)))
          (unwind-protect
              (when (and outer inner)
                (evil-surround-delete char outer inner))
            (when outer (delete-overlay outer))
            (when inner (delete-overlay inner))))
      (embrace--delete char)))))

(defun embrace-evil-surround-change (char &optional outer inner)
  (interactive "c")
  (let (overlay)
    (cond
     ((and outer inner)
      (evil-surround-delete char outer inner)
      (let ((key (read-char)))
        (if (member key embrace-evil-surround-key)
            (evil-surround-region (overlay-start outer)
                                  (overlay-end outer)
                                  nil (if (evil-surround-valid-char-p key) key char))
          (embrace--insert key (copy-overlay outer)))))
     (t
      (if (member char embrace-evil-surround-key)
          (let* ((outer (evil-surround-outer-overlay char))
                 (inner (evil-surround-inner-overlay char)))
            (unwind-protect
                (when (and outer inner)
                  (evil-surround-change char outer inner))
              (when outer (delete-overlay outer))
              (when inner (delete-overlay inner))))
        (setq overlay (embrace--delete char t))
        (let ((key (read-char)))
          (if (member key embrace-evil-surround-key)
              (evil-surround-region (overlay-start overlay)
                                    (overlay-end overlay)
                                    nil (if (evil-surround-valid-char-p key) key char))
            (embrace--insert key overlay))
          (when overlay (delete-overlay overlay))))))))

;;;###autoload
(defun embrace-enable-evil-surround-integration ()
  (interactive)
  (when (require 'evil-surround nil t)
    (advice-add 'evil-surround-change :override 'embrace-evil-surround-change)
    (advice-add 'evil-surround-delete :override 'embrace-evil-surround-delete)))

;;;###autoload
(defun embrace-disable-evil-surround-integration ()
  (interactive)
  (when (require 'evil-surround nil t)
    (advice-remove 'evil-surround-change 'embrace-evil-surround-change)
    (advice-remove 'evil-surround-delete 'embrace-evil-surround-delete)))

(provide 'evil-embrace)
;;; evil-embrace.el ends here
