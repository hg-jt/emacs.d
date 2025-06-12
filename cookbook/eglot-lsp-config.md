# Configuring eglot and LSP Servers

## Problem

You want to use [eglot] with LSP servers for Python (and other languages) in
Emacs 29+.


## Solution

Configuring eglot to use LSP servers typically involves:

1. Installing the LSP server
2. Adding the LSP server to the `exec-path` in Emacs
3. Configuring eglot


### Python LSP with Ruff

To configure Eglot in Emacs 29 for Python development with Ruff:

1. Install Ruff in a virtual environment

   ```
   python -m venv --prompt emacs-lsp .venv
   source .venv/bin/activate
   pip install --upgrade pip
   pip install ruff
   ```

2. Configure Eglot to use Ruff

   ```lisp
   (add-to-list 'eglot-server-programs
                '(python-ts-mode . ("path/to/.venv/bin/ruff" "server" "--preview")))
   ```
