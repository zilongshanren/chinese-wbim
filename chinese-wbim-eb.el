;;; chinese-wbim-eb --- Enable Wubi(五笔) Input Method in Emacs.

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

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'eim-eb)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'chinese-wbim-table)

(defvar eim-eb-user-file nil)
(defvar eim-eb-history-file nil)
(defvar eim-eb-package nil)
(defvar eim-eb-punctuation-list nil)
(defvar eim-eb-load-hook nil)
(defvar eim-eb-initialized nil)
(defvar eim-eb-char-table (make-vector 1511 0))

(defun eim-eb-create-word (word)
  "Insert word to database and write into user file."
  (let ((len (length word))
        code)
    (setq code
     (cond
      ((= len 2)
       (concat (substring (eim-table-get-char-code (aref word 0)) 0 2)
               (substring (eim-table-get-char-code (aref word 1)) 0 2)))
      ((= len 3)
       (concat (substring (eim-table-get-char-code (aref word 0)) 0 2)
               (substring (eim-table-get-char-code (aref word 1)) 0 1)
               (substring (eim-table-get-char-code (aref word 2)) 0 1)))
      (t
       (concat (substring (eim-table-get-char-code (aref word 0)) 0 1)
               (substring (eim-table-get-char-code (aref word 1)) 0 1)
               (substring (eim-table-get-char-code (aref word 2)) 0 1)
               (substring (eim-table-get-char-code (aref word (1- (length word)))) 0 1)))))))

(unless eim-eb-initialized
  (setq eim-eb-package eim-current-package)
  (setq eim-eb-punctuation-list
        (eim-read-punctuation eim-eb-package))
  (run-hooks 'eim-eb-load-hook)
  (let ((path (file-name-directory load-file-name)))
    (load (concat path "eim-eb-map")))
  (let ((map (eim-mode-map)))
    (define-key map "\t" 'eim-table-show-completion))
  
  (eim-table-add-user-file eim-eb-user-file)
  (eim-table-load-history eim-eb-history-file)
  (eim-set-option 'table-create-word-function 'eim-eb-create-word)
  (eim-set-option 'char-table eim-eb-char-table)
  (eim-set-option 'punctuation-list 'eim-eb-punctuation-list)
  (eim-set-option 'max-length 4)
  (eim-set-option 'translate-chars '(?\[))
  (eim-set-option 'all-completion-limit 3)
  (eim-set-active-function 'eim-table-active-function)
  (setq eim-eb-initialized t))

(provide 'chinese-wbim-eb)
;;; chinese-wbim-eb.el ends here
