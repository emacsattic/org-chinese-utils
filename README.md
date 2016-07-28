- [介绍](#介绍)
  - [安装](#安装)
  - [使用](#使用)
  - [定制 org-chinese-utils](#定制-org-chinese-utils)

# 介绍<a id="orgheadline4"></a>

org-chinese-utils 包含了以下工具，可以方便 org-mode 中文用户：

1.  将 org 导出为 HTML 时删除不必要的空格。
2.  按 'C-c C-c', 根据当前内容智能折行。
3.  如果 org-babel 结果中包含表格时，对表格进行对齐处理。

## 安装<a id="orgheadline1"></a>

org-chinese-utils is now available from the famous emacs package repo
[melpa](http://melpa.milkbox.net/), so the recommended way is to install it
through emacs package management system.

## 使用<a id="orgheadline2"></a>

    (require 'org)
    (require 'org-chinese-utils)
    (org-chinese-utils-enable)

## 定制 org-chinese-utils<a id="orgheadline3"></a>

运行下面的命令，一个 utils 选择器就会弹出，用鼠标或者回车选择需要激活的 utils 就可以了。

    M-x org-chinese-utils

![img](./snapshots/org-chinese-utils.png)
