;;; org-chinese-utils.el --- Some org-mode utils for Chinese users

;; * Header
;; Copyright (c) 2016, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/org-chinese-utils.git
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; * 介绍                                                             :README:
;; org-chinese-utils 包含了以下工具，可以方便 org-mode 中文用户：
;; 1. 将 org 导出为 HTML 时删除不必要的空格。
;; 2. 按 'C-c C-c', 根据当前内容智能折行。
;; 3. 如果 org-babel 结果中包含表格时，对表格进行对齐处理。

;; ** 安装
;; org-chinese-utils is now available from the famous emacs package repo
;; [[http://melpa.milkbox.net/][melpa]], so the recommended way is to install it
;; through emacs package management system.

;; ** 使用
;; #+BEGIN_EXAMPLE
;; (require 'org)
;; (require 'ox)
;; (require 'org-chinese-utils)
;; (org-chinese-utils-enable)
;; #+END_EXAMPLE

;;; Code:
;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(defgroup org-chinese-utils nil
  "Some org-mode utils for Chinese users."
  :group 'org)

(defconst org-chinese-utils-list
  '((clean-paragraph-space
     :document "删除中文段落中，中文之间多余的空格。"
     :function org-chinese-utils:clean-useless-space
     :hook org-export-filter-paragraph-functions)
    (clean-headline-space
     :document "删除中文标题中，中文之间多余的空格。"
     :function org-chinese-utils:clean-useless-space
     :hook org-export-filter-headline-functions)
    (align-babel-table
     :document "当 org-babel 输出一个 org 表格时，使这个表格对齐。"
     :function org-chinese-utils:align-babel-table
     :hook org-babel-after-execute-hook)
    (smart-truncate-lines
     :document "让 'C-c C-c' 可以根据光标处的内容，智能的折行。"
     :function org-chinese-utils:smart-truncate-lines
     :hook org-mode-hook))
  "A list of utils that can be enabled.

A utils is a plist, which form is like:

  (NAME :document DOC :function FN :hook HOOK)

NAME is a symbol, which can be passed to `org-chinese-utils-activate'.
FN is a function which will be added to HOOK.")

(defun org-chinese-utils:clean-useless-space (text backend info)
  "导出 org file 时，删除中文之间不必要的空格。"
  (when (or (org-export-derived-backend-p backend 'html)
            (memq backend '(odt)))
    (let ((regexp "[[:multibyte:]]")
          (string text))
      ;; org-mode 默认将一个换行符转换为空格，但中文不需要这个空格，删除。
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
             "\\1\\2" string))
      ;; 删除粗体之前的空格
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\)[ \n]+\\(<\\)" regexp)
             "\\1\\2" string))
      ;; 删除粗体之后的空格
      (setq string
            (replace-regexp-in-string
             (format "\\(>\\)[ \n]+\\(%s\\)" regexp)
             "\\1\\2" string))
      string)))

(defun org-chinese-utils:smart-truncate-lines (&optional arg)
  (interactive)
  (org-defkey org-mode-map "\C-c\C-c" 'org-chinese-utils:ctrl-c-ctrl-c))

(defun org-chinese-utils:ctrl-c-ctrl-c (&optional arg)
  "根据光标处内容，智能折行，比如，在表格中禁止折行。"
  (interactive "P")
  (cond ((or (and (boundp 'org-clock-overlays)
                  org-clock-overlays)
             org-occur-highlights)
         (and (boundp 'org-clock-overlays)
              (org-clock-remove-overlays))
         (org-remove-occur-highlights)
         (org-remove-latex-fragment-image-overlays)
         (message "Temporary highlights/overlays removed from current buffer"))
        (t (let* ((context (org-element-context))
                  (type (org-element-type context)))
             (case type
               ((table table-cell table-row item plain-list)
                (toggle-truncate-lines 1))
               (t (toggle-truncate-lines -1))))))
  (org-ctrl-c-ctrl-c arg))

(defun org-chinese-utils:align-babel-table (&optional info)
  "Align all tables in the result of the current babel source."
  (interactive)
  (when (not org-export-current-backend)
    (let ((location (org-babel-where-is-src-block-result nil info)))
      (when location
        (save-excursion
          (goto-char location)
          (when (looking-at (concat org-babel-result-regexp ".*$"))
            (while (< (point) (progn (forward-line 1) (org-babel-result-end)))
              (when (org-at-table-p)
                (toggle-truncate-lines 1)
                (org-table-align)
                (goto-char (org-table-end)))
              (forward-line))))))))

(defun org-chinese-utils-activate (utils-list)
  "Activate certain utils of org-chinese-utils.

UTILS-LIST should be a list of utils (defined in `org-chinese-utils-list') which
should be activated."
  (dolist (utils utils-list)
    (let* ((plist (cdr (assq utils org-chinese-utils-list)))
           (fn (plist-get plist :function))
           (hook (plist-get plist :hook)))
      (when (and fn hook)
        (add-hook hook fn)))))

(defun org-chinese-utils-deactivate (utils-list)
  "Deactivate certain utils of org-chinese-utils.

This function is the opposite of `org-chinese-utils-deactive'.  UTILS-LIST
should be a list of utils (defined in `org-chinese-utils-list') which should
be activated."
  (dolist (utils utils-list)
    (let* ((plist (cdr (assq utils org-chinese-utils-list)))
           (fn (plist-get plist :function))
           (hook (plist-get plist :hook)))
      (when (and fn hook)
        (remove-hook hook fn)))))

(defun org-chinese-utils-enable (&optional disable)
  "Enable all org-chinese-utils, when DISABLE is t, disable all utils."
  (interactive)
  (let ((utils-list (mapcar 'car org-chinese-utils-list)))
    (if (and (featurep 'org)
             (featurep 'ox))
        (if disable
            (org-chinese-utils-deactivate utils-list)
          (org-chinese-utils-activate utils-list))
      (message "Package 'org' or 'ox' is unavailable."))))
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'org-chinese-utils)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; org-chinese-utils.el ends here
;; #+END_SRC
