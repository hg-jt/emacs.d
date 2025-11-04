# Configuring eglot and LSP Servers

## Problem

You want to use [eglot] with LSP servers for Python (and other languages) in
Emacs 29+.


## Solution

Configuring eglot to use LSP servers typically involves:

1. Installing the LSP server
2. Adding the LSP server to the `exec-path` in Emacs
3. Configuring eglot

> ***TIP*** Consider using a local directory for installing LSP servers. For example:
>
> ```
> # establish local LSP server directory
> mkdir -p ~/.emacs.d/lsp
> cd ~/.emacs.d/lsp
>
> # set python version for LSP servers
> echo "3.13.5" > .python-version
>
> # set node version for LSP servers
> echo "lts/*" > .nvmrc
> ```

### Python LSP with Ruff

To configure Eglot in Emacs 29+ for Python development with Ruff:

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
                '(python-ts-mode . ("path/to/.venv/bin/ruff" "server")))
   ```

## Dockerfile LSP with docker-langserver

To configure Eglot in Emacs 29+ for editing Dockerfiles:

1. Install [dockerfile-language-server-nodejs]

    ```
    npm init --init-module emacs-lsp-config -y --init-license MIT
    npm install --save dockerfile-language-server-nodejs
    ```

    This can be installed globally, but you may want to consider installing it
    in a local project and setup a script to call `docker-langserver`.

    ```
    #!/bin/sh
    # ~/.local/bin/docker-langserver --- Wrapper for using a local docker-langserver.
    LSP_DIR=~/.emacs.d/lsp

    cd $LSP_DIR
    npx docker-langserver $@
    ```


2. Configure Eglot to use docker-langserver

   ```lisp
   (add-to-list 'eglot-server-programs
                '(dockerfile-ts-mode . ("docker-langserver" "--stdio")))
   ```

[dockerfile-language-server-nodejs]: https://www.npmjs.com/package/dockerfile-language-server-nodejs
[eglot]: https://www.gnu.org/software/emacs/manual/html_mono/eglot.html
