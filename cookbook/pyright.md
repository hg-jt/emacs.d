# Pyright and lsp-mode

## Problem

Using Pyright as an Language Server Protocol (LSP) backend for Python
development in Emacs.

## Solution

In order to use Pyright with Emacs you will need to install the Pyright
application as well as LSP support in Emacs (i.e. via the *lsp-mode* and
*lsp-pyright* packages). You will also need to update your Emacs configuration
to enable these packages and update your project's local configuration so that
Emacs knows what =python= executable to use for interactive features.

1. Install Pyright

   Pyright is installed/packaged as an NPM module. The [Pyright installation
   instructions] recommend installing it globally (e.g. `npm install -g
   pyright`). This is sufficient regardless of how you installed node/npm.

   You will need to know where the `pyright` executable gets installed. You can
   find this out by running `which pyright`. For example on OS X, using [nvm], the
   pyright executable is in `~/.nvm/versions/node/<version>/bin`.

2. Install LSP/Pyright support in Emacs

   At the bare minimum, you will need to install the *lsp-mode* and
   *lsp-pyright* packages. It is not required, but having *company-mode*
   installed is well worth the effort, since lsp-mode integrates with it and
   provides a usable out-of-the-box completion setup. All three of these
   packages are available on MELPA.

   ```
   M-x package-install lsp-mode
   M-x package-install lsp-pyright
   M-x package-install company
   ```

3. Configure Emacs to find the Pyright executables

   The GUI version of Emacs is not usually run with the same environment
   variables as the CLI version. This means that the path that the GUI app is
   configured with does not include directories outside of the standard OS paths
   (i.e the paths that Homebrew and NPM use will need to be added to your local
   configuration).

   ```lisp
   (when window-system
     ;; configure path for external processes
     (setenv "PATH"
             (format "/usr/local/bin:%s:%s"
                     (getenv "PATH")
                     (expand-file-name "~/.nvm/versions/node/v14.17.1/bin")))
     
     ;; configure path for subprocesses
     (setq exec-path
           (append exec-path
                   `("/usr/local/bin"
                     ,(expand-file-name "~/.nvm/versions/node/v14.17.1/bin")))) )
   ```

4. Configure lsp-mode and lsp-pyright

   If your project uses or generates directories that should be ignored by
   lsp-mode's file watchers, you may need to customize
   `lsp-file-watch-ignored-directories` to ignore these directories. The default
   configuration ignores some common directories (e.g. `.git`, `node_modules`,
   etc.) but does not include local python virtual environents (e.g. `venv` or
   `.venv`).

   ```lisp
   (with-eval-after-load 'lsp-mode
     (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'"))
   ```

   This is covered in the [File Watchers] section of the lsp-mode docs.

5. Add a `.dir-locals` file to your project

   In order for python-mode's interactive features to work, you will need to
   configure the location of the Python interpreter. If you are using a
   virtual environment, you should configure `python-shell-interpreter` to point to
   the `python` executable in the virtual environment so that any packages you
   installed will be found when you run the interpreter.

   ```lisp
   ;;; .dir-locals.el --- Local project configuration.  -*- lexical-binding: t; -*-
   ((python-mode
     (eval . (let* ((project-root (locate-dominating-file default-directory ".dir-locals.el"))
                    (python-path (concat project-root ".venv/bin/python")))
               (setq-local python-shell-interpreter python-path) ))
     (python-shell-interpreter-args . "-i") ))
   ```

[Pyright installation instructions]: https://github.com/microsoft/pyright#emacs
[nvm]: https://github.com/nvm-sh/nvm
[File Watchers]: https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
