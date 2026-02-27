# Emacs Config

This is an Emacs configuration. The main file is `init.el`, with personal
elisp modules in `elisp/`. Installed packages live in `elpa/` (do not edit).

## Validating changes

After editing any `.el` file in this repo, validate by running both checks:

### Byte-compile (catches syntax errors, undefined functions, wrong arg counts)

```sh
emacs --batch --eval '(progn (package-initialize) (add-to-list (quote load-path) "~/.emacs.d/elisp"))' -f batch-byte-compile init.el && rm -f init.elc
```

For files in `elisp/`:

```sh
emacs --batch --eval '(progn (package-initialize) (add-to-list (quote load-path) "~/.emacs.d/elisp"))' -f batch-byte-compile elisp/FILENAME.el && rm -f elisp/FILENAME.elc
```

- Exit code 1 = real errors (must fix)
- Exit code 0 with warnings = OK (many warnings are pre-existing and benign)
- Always clean up `.elc` files after validation (they are gitignored)

### Load test (catches runtime errors during config loading)

```sh
emacs --batch -l init.el --eval '(message "Config loaded successfully")' 2>&1
```

A non-zero exit code or an error traceback means the config is broken.

## Notes

- This config uses `use-package` with `ensure t` (auto-installs packages)
- `elisp/` files are personal modules loaded via `load-path`
- `custom.el` is for Emacs customize — avoid hand-editing
- Many byte-compile warnings are pre-existing (obsolete APIs, docstring width, etc.) — focus on errors, not warnings
