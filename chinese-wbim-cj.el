;;; chinese-wbim-cj --- Enable Wubi(五笔) Input Method in Emacs.

;; Copyright (C) 2015-2016, Guanghui Qu

;; Author: Guanghui Qu<guanghui8827@gmail.com>
;; URL: https://github.com/andyque/chinese-wbim
;; Version: 0.1
;; Keywords: Wubi Input Method.
;;
;; This file is not part of GNU Emacs.

;;; Credits:

;; - Original Author: wenbinye@163.com

;;; License:

;; This file is part of chinese-wbim
;;
;; chinese-wbim is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; chinese-wbim is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'chinese-wbim-table)

(defvar eim-cj-package nil)
(defvar eim-cj-punctuation-list nil)
(defvar eim-cj-initialized nil)
(defvar eim-cj-load-hook nil)
(defvar eim-cj-char-table (make-vector 1511 0))

(unless eim-cj-initialized
  (setq eim-cj-package eim-current-package)
  (setq eim-cj-punctuation-list
        (eim-read-punctuation eim-cj-package))
  (run-hooks 'eim-cj-load-hook)
  (let ((path (file-name-directory load-file-name)))
    (load (concat path "eim-cj-chars")))
  (eim-set-option 'char-table eim-cj-char-table)
  (eim-set-option 'punctuation-list 'eim-cj-punctuation-list)
  (eim-set-option 'max-length 5)
  (eim-set-option 'translate-chars '(?x ?z))
  (eim-set-option 'all-completion-limit 3)
  (eim-set-active-function 'eim-table-active-function)
  (setq eim-cj-initialized t))

(provide 'chinese-wbim-cj)
;;; chinese-wbim-cj.el ends here
