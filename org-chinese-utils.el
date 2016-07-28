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
;; (require 'org-chinese-utils)
;; (org-chinese-utils-enable)
;; #+END_EXAMPLE

;; ** 定制 org-chinese-utils
;; 运行下面的命令，一个 utils 选择器就会弹出，用鼠标或者回车选择需要激活的 utils 就可以了。

;; #+BEGIN_EXAMPLE
;; M-x org-chinese-utils
;; #+END_EXAMPLE

;; [[./snapshots/org-chinese-utils.png]]

;;; Code:
;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(require 'cl-lib)
(require 'cus-edit)

(defgroup org-chinese-utils nil
  "Some org-mode utils for Chinese users."
  :group 'org)

(defconst org-chinese-utils-list
  '((clean-paragraph-space
     :document "Org 文件导出为 HTML 或 odt 文件时，删除中文段落中多余的空格。"
     :function org-chinese-utils:clean-useless-space
     :hook org-export-filter-paragraph-functions)
    (clean-headline-space
     :document "Org 文件导出为 HTML 或 odt 文件时，删除中文标题中多余的空格。"
     :function org-chinese-utils:clean-useless-space
     :hook org-export-filter-headline-functions)
    (align-babel-table
     :document "让 org-babel 运行结果中包含的 org 表格对齐。"
     :function org-chinese-utils:align-babel-table
     :hook org-babel-after-execute-hook)
    (smart-truncate-lines
     :document "按 'C-c C-c' 快捷键时，根据光标处的内容智能折行。"
     :function org-chinese-utils:smart-truncate-lines
     :hook org-mode-hook)
    (show-babel-image
     :document "让 org-babel 运行结果中包含的图片链接自动显示。"
     :function org-chinese-utils:show-babel-image
     :hook org-babel-after-execute-hook)
    (visual-line-mode
     :document "打开 org 文件时，激活 visual-line-mode."
     :function org-chinese-utils:visual-line-mode
     :hook org-mode-hook)
    (org-cdlatex
     :document "打开 org 文件时，激活 cdlatex 功能."
     :function org-chinese-utils:org-cdlatex
     :hook org-mode-hook))
  "A list of utils that can be enabled.

A utils is a plist, which form is like:

  (NAME :document DOC :function FN :hook HOOK)

NAME is a symbol, which can be passed to `org-chinese-utils-activate'.
FN is a function which will be added to HOOK.")

(defvar org-chinese-utils-enabled
  '(clean-paragraph-space
    clean-headline-space
    align-babel-table
    smart-truncate-lines
    show-babel-image
    visual-line-mode)
  "The utils of org-chinese-utils which will be activated.")

(defvar org-chinese-utils-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map (make-composed-keymap widget-keymap
                                                 special-mode-map))
    (suppress-keymap map)
    (define-key map "\C-x\C-s" 'org-chinese-utils-save-setting)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    map)
  "Keymap for `org-chinese-utils-mode'.")

(define-derived-mode org-chinese-utils-mode special-mode "org-chinese-utils"
  "Major mode for selecting org-chinese-utils.
Do not call this mode function yourself.  It is meant for internal use."
  (use-local-map org-chinese-utils-mode-map)
  (custom--initialize-widget-variables)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (_ignore-auto noconfirm)
         (when (or noconfirm (y-or-n-p "Discard current choices? "))
           (org-chinese-utils (current-buffer))))))
(put 'org-chinese-utils-mode 'mode-class 'special)

;;;###autoload
(defun org-chinese-utils (&optional buffer)
  (interactive)
  (switch-to-buffer (get-buffer-create (or buffer "*Org-chinese-utils chooser*")))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (org-chinese-utils-mode)
  (setq truncate-lines t)
  (widget-insert "Type RET or click to enable/disable utils of org-chinese-utils.\n\n")
  (widget-create 'push-button
                 :tag " Save settings! "
                 :help-echo "Save the selected utils for future sessions."
                 :action 'org-chinese-utils-save-setting)
  (widget-insert "\n\nAvailable utils of org-chinese-utils:\n\n")
  (let ((help-echo "mouse-2: Enable this utils for this session")
        widget)
    (dolist (utils (mapcar 'car org-chinese-utils-list))
      (setq widget (widget-create 'checkbox
                                  :value (memq utils org-chinese-utils-enabled)
                                  :utils-name utils
                                  :help-echo help-echo
                                  :action 'org-chinese-utils-checkbox-toggle))
      (widget-create-child-and-convert widget 'push-button
                                       :button-face-get 'ignore
                                       :mouse-face-get 'ignore
                                       :value (format " %s " utils)
                                       :action 'widget-parent-action
                                       :help-echo help-echo)
      (widget-insert " -- " (plist-get (cdr (assq utils org-chinese-utils-list))
                                       :document)
                     ?\n)))
  (goto-char (point-min))
  (widget-setup))

(defun org-chinese-utils-save-setting (&rest _ignore)
  (interactive)
  (customize-save-variable
   'org-chinese-utils-enabled org-chinese-utils-enabled)
  (message "Setting of org-chinese-utils is saved."))

(defun org-chinese-utils-checkbox-toggle (widget &optional event)
  (let ((utils (widget-get widget :utils-name)))
    (widget-toggle-action widget event)
    (if (widget-value widget)
        (progn (push utils org-chinese-utils-enabled)
               (org-chinese-utils-activate (list utils)))
      (setq org-chinese-utils-enabled
            (remove utils org-chinese-utils-enabled))
      (org-chinese-utils-deactivate (list utils)))))

;;;###autoload
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

;;;###autoload
(defun org-chinese-utils-deactivate (&optional utils-list)
  "Deactivate certain utils of org-chinese-utils.

This function is the opposite of `org-chinese-utils-deactive'.  UTILS-LIST
should be a list of utils (defined in `org-chinese-utils-list') which should
be activated."
  (let ((utils-list (or utils-list (mapcar 'car org-chinese-utils-list))))
    (dolist (utils utils-list)
      (let* ((plist (cdr (assq utils org-chinese-utils-list)))
             (fn (plist-get plist :function))
             (hook (plist-get plist :hook)))
        (when (and fn hook)
          (remove-hook hook fn))))))

;;;###autoload
(defun org-chinese-utils-enable ()
  "Enable all org-chinese-utils, when DISABLE is t, disable all utils."
  (interactive)
  (if (and (featurep 'org)
           (featurep 'ox))
      (add-hook 'org-mode-hook
                (lambda ()
                  (org-chinese-utils-deactivate)
                  (org-chinese-utils-activate org-chinese-utils-enabled)))
    (message "Package 'org' or 'ox' is unavailable.")))

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

(defun org-chinese-utils:visual-line-mode ()
  (setq visual-line-fringe-indicators '(nil nil))
  (visual-line-mode)
  (if visual-line-mode
      (setq word-wrap nil)))

(defun org-chinese-utils:show-babel-image ()
  (when (not org-export-current-backend)
    (org-display-inline-images)))

(defun org-chinese-utils:org-cdlatex ()
  (if (featurep 'cdlatex)
      (turn-on-org-cdlatex)
    (message "Fail to active org-cdlatex, you should load cdlatex first.")))

;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'org-chinese-utils)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; org-chinese-utils.el ends here
;; #+END_SRC
