;;; evil-embrace.el --- Evil integration of embrace.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Package-Requires: ((embrace "0.1.0") (evil-surround "0") (evil "1.2.12"))
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

;;                              ______________

;;                               EVIL-EMBRACE

;;                               Junpeng Qiu
;;                              ______________


;; Table of Contents
;; _________________

;; 1 Overview
;; 2 Why
;; 3 Usage


;; Evil integration of [embrace.el].


;; [embrace.el] https://github.com/cute-jumper/embrace.el


;; 1 Overview
;; ==========

;;   This package provides evil integration of [embrace.el]. Since
;;   `evil-surround' provides a similar set of features as `embrace.el',
;;   this package aims at adding the goodies of `embrace.el' to
;;   `evil-surround' and making `evil-surround' even better.


;; [embrace.el] https://github.com/cute-jumper/embrace.el


;; 2 Why
;; =====

;;   `evil-surround' is good when there is a text object defined. But
;;   unfortunately, if you want to add custom surrouding pairs,
;;   `evil-surround' will not be able to delete/change the pairs if there
;;   are no evil text objects defined for these pairs. For example, if you
;;   want to make `\textbf{' and `}' as a surround pair in `LaTeX-mode',
;;   you can't either change or delete the surround pair since there is no
;;   text object for `\textbf{' and `}'. However, using `embrace', you can
;;   define whatever surrounding pairs you like, and adding, changing, and
;;   deleting will *always* work.

;;   The idea of this package is that let `evil-surround' handle the keys
;;   that corresponds to existing text objects (i.e., `(', `[', etc.),
;;   which is what `evil-surround' is good at, and make `embrace' handles
;;   all the other keys of custom surrounding pairs so that you can also
;;   benifit from the extensibility that `embrace' offers.

;;   In a word, you can use the default `evil-surround'. You don't have to
;;   change it. But whenever you want to add a custom surrounding pair, use
;;   `evil-embrace' instead.


;; 3 Usage
;; =======

;;   To enable the `evil-surround' integration:
;;   ,----
;;   | (evil-embrace-enable-evil-surround-integration)
;;   `----

;;   And use `evil-embrace-disable-evil-surround-integration' to disable
;;   whenever you don't like it.

;;   The keys that are processed by `evil-surround' are saved in the
;;   variable `evil-embrace-evil-surround-key'. The default value is:
;;   ,----
;;   | (?\( ?\[ ?\{ ?\) ?\] ?\} ?\" ?\' ?b ?B ?t)
;;   `----

;;   Note that this variable is also buffer-local. You should change it in
;;   the hook:
;;   ,----
;;   | (add-hook 'LaTeX-mode-hook
;;   |     (lambda ()
;;   |        (add-to-list 'evil-embrace-evil-surround-key ?o)))
;;   `----

;;   Only these keys saved in the variable are processed by
;;   `evil-surround', and all the other keys will be processed by
;;   `embrace'.

;;; Code:

(require 'embrace)
(require 'evil-surround)

(defvar evil-embrace-evil-surround-key '(?\( ?\[ ?\{ ?\) ?\] ?\} ?\" ?\' ?b ?B ?t)
  "Keys that should be processed by `evil-surround'")
(make-variable-buffer-local 'evil-embrace-evil-surround-key)

;;; `evil-surround' integration
(defun evil-embrace-evil-surround-delete (char &optional outer inner)
  (interactive "c")
  (cond
   ((and outer inner)
    (delete-region (overlay-start outer) (overlay-start inner))
    (delete-region (overlay-end inner) (overlay-end outer))
    (goto-char (overlay-start outer)))
   (t
    (if (member char evil-embrace-evil-surround-key)
        (let* ((outer (evil-surround-outer-overlay char))
               (inner (evil-surround-inner-overlay char)))
          (unwind-protect
              (when (and outer inner)
                (evil-surround-delete char outer inner))
            (when outer (delete-overlay outer))
            (when inner (delete-overlay inner))))
      (embrace--delete char)))))

(defun evil-embrace-evil-surround-change (char &optional outer inner)
  (interactive "c")
  (let (overlay)
    (cond
     ((and outer inner)
      (evil-surround-delete char outer inner)
      (let ((key (read-char)))
        (if (member key evil-embrace-evil-surround-key)
            (evil-surround-region (overlay-start outer)
                                  (overlay-end outer)
                                  nil (if (evil-surround-valid-char-p key) key char))
          (embrace--insert key (copy-overlay outer)))))
     (t
      (if (member char evil-embrace-evil-surround-key)
          (let* ((outer (evil-surround-outer-overlay char))
                 (inner (evil-surround-inner-overlay char)))
            (unwind-protect
                (when (and outer inner)
                  (evil-surround-change char outer inner))
              (when outer (delete-overlay outer))
              (when inner (delete-overlay inner))))
        (setq overlay (embrace--delete char t))
        (let ((key (read-char)))
          (if (member key evil-embrace-evil-surround-key)
              (evil-surround-region (overlay-start overlay)
                                    (overlay-end overlay)
                                    nil (if (evil-surround-valid-char-p key) key char))
            (embrace--insert key overlay))
          (when overlay (delete-overlay overlay))))))))

;;;###autoload
(defun evil-embrace-enable-evil-surround-integration ()
  (interactive)
  (advice-add 'evil-surround-change :override 'evil-embrace-evil-surround-change)
  (advice-add 'evil-surround-delete :override 'evil-embrace-evil-surround-delete))

;;;###autoload
(defun evil-embrace-disable-evil-surround-integration ()
  (interactive)
  (advice-remove 'evil-surround-change 'evil-embrace-evil-surround-change)
  (advice-remove 'evil-surround-delete 'evil-embrace-evil-surround-delete))

(provide 'evil-embrace)
;;; evil-embrace.el ends here
