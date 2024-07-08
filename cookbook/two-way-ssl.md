# Two-way SSL w/custom client certs with *Emacs for OSX*

## Problem

Using Emacs rest-client for two-way SSL authentication on OS X.


## Solution

> Since certs are likely unique to your user/machine, the following
> configuration should be added to `~/.emacs.d/site-lisp/default.el`, rather
> than the main `~/.emacs.d/init.el` file.

### 1. Ensure you have the latet CA cert bundle

Install /openssl/ with Homebrew:

```sh
brew install openssl
```

This will install a the CA bundle in `/usr/local/etc/openssl/cert.pem`.


### 2. Add the latest CA bundle to the list of common trustfiles

```lisp
(eval-after-load "gnutls"
  (lambda ()
    (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")))
```


### 3. Configure Emacs to ignore the built-in gnutls in favor of the command line app

```lisp
(eval-after-load "tls"
  (lambda ()
  ;; force the use of external gnutls-cli for custom certs
  (when (fboundp 'gnutls-available-p)
  (defun gnutls-available-p () nil)) ))
```


### 4. Configure tls-program to use your custom certs

In the same block as the previous step, add a custom string for invoking
*gnutls-cli* with your custom certs.

```lisp
(eval-after-load "tls"
  (lambda ()
    ;; force the use of external gnutls-cli for custom certs
    (when (fboundp 'gnutls-available-p)
      (defun gnutls-available-p () nil))

    ;; add custom gnutls-cli command
    (add-to-list 'tls-program
                 "gnutls-cli -p %p --x509cafile /path/to/ca/ca.pem --x509keyfile /path/to/key/key.pem --x509certfile /path/to/cert/public.pem %h"
                 t)))
```
