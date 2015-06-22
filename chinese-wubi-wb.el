;;; chinese-wubi-wb --- Enable Wubi(五笔) Input Method in Emacs.

;; Copyright (C) 2015-2016, Guanghui Qu

;; Author: Guanghui Qu<guanghui8827@gmail.com>
;; URL: https://github.com/andyque/chinese-wubi
;; Version: 0.1
;; Keywords: Wubi Input Method.
;;
;; This file is not part of GNU Emacs.

;;; Credits:

;; - Original Author: wenbinye@163.com

;;; License:

;; This file is part of chinese-wubi
;;
;; chinese-wubi is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; chinese-wubi is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Features:
;; 1. 能导入输入历史
;; 2. 提供造词的命令
;; 3. 提供候选的单字
;; 4. 拼音输入，提示五笔字根
;; 5. 处理标点
;; 6. 使用 ; ' 快速选择

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'chinese-wubi-table)

(defgroup eim-wb nil
  "eim wubi input method"
  :group 'eim)
  
(defcustom eim-wb-history-file "~/.emacs.d/wbx-history"
  "保存选择的历史记录."
  :type 'file
  :group 'eim-wb)

(defcustom eim-wb-user-file "mywb.txt"
  "保存用户自造词."
  :type 'file
  :group 'eim-wb)

(defcustom eim-wb-save-always nil
  "是否每次加入新词都要保存.
当然设置为 nil，也会在退出 Emacs 里保存一下的."
  :type 'boolean
  :group 'eim-wb)

(defcustom eim-wb-add-all-completion-limit 3
  "在超过输入字符串超过这个长度时会添加所有补全."
  :type 'integer
  :group 'eim-wb)

(defvar eim-wb-load-hook nil)
(defvar eim-wb-package nil)
(defvar eim-wb-char-table (make-vector 1511 0))
(defvar eim-wb-punctuation-list nil)
(defvar eim-wb-initialized nil)

(defun eim-wb-create-word (word)
  "Insert WORD to database and write into user file."
  (let ((len (length word))
        code)
    (setq code
     (cond
      ((= len 2)
       (concat (substring (eim-table-get-char-code (aref word 0)) 0 2)
               (substring (eim-table-get-char-code (aref word 1)) 0 2)))
      ((= len 3)
       (concat (substring (eim-table-get-char-code (aref word 0)) 0 1)
               (substring (eim-table-get-char-code (aref word 1)) 0 1)
               (substring (eim-table-get-char-code (aref word 2)) 0 2)))
      (t
       (concat (substring (eim-table-get-char-code (aref word 0)) 0 1)
               (substring (eim-table-get-char-code (aref word 1)) 0 1)
               (substring (eim-table-get-char-code (aref word 2)) 0 1)
               (substring (eim-table-get-char-code (aref word (1- (length word)))) 0 1)))))))

;;;_. load it
(unless eim-wb-initialized
  (setq eim-wb-package eim-current-package)
  (setq eim-wb-punctuation-list
        (eim-read-punctuation eim-wb-package))
  (let ((map (eim-mode-map)))
    (define-key map "\t" 'eim-table-show-completion)
    (define-key map ";" 'eim-quick-select-1)
    (define-key map "'" 'eim-quick-select-2))
  (defvar eim-wb-use-gbk nil)
  (let ((path (file-name-directory load-file-name)))
    (load (concat path
                  (if (and (boundp 'eim-wb-use-gbk)
                           eim-wb-use-gbk)
                      "eim-wb-gbk" "eim-wb-gb2312"))))

  (eim-table-add-user-file eim-wb-user-file)
  (eim-table-load-history eim-wb-history-file)
  (run-hooks 'eim-wb-load-hook)
  (eim-set-option 'table-create-word-function 'eim-wb-create-word)
  (eim-set-option 'punctuation-list 'eim-wb-punctuation-list)
  (eim-set-option 'max-length 4)
  (eim-set-option 'translate-chars '(?z))
  (eim-set-option 'all-completion-limit eim-wb-add-all-completion-limit)
  (eim-set-option 'char-table eim-wb-char-table)
  (eim-set-active-function 'eim-table-active-function)
  (setq eim-wb-initialized t))

(provide 'chinese-wubi-wb)
;;; chinese-wubi-wb.el ends here
