# Archiving org-mode Entries

## Problem

Org-mode raises an error when archiving entries:

```
org-copy-subtree: Invalid function: org-preserve-local-variables
```

## Solution

In some configurations, org-mode cannot find a function that it needs. The
workaround is to delete the compiled elc files. While non-ideal, this does
resolve the issue.

```sh
cd ~/.emacs.d/elpa
find org-plus*/*.elc -print0 | xargs -0 rm
```
