# Prelude Company

!!! Note

    This module is enabled by default.

[company](https://company-mode.github.io/) is a completion library.

This module simply provides some reasonable defaults for it and enables `company-mode`:

```
(setq company-idle-delay 0.5)
(setq company-show-numbers t)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)

(global-company-mode 1)
```

You can adjust the configuration further in your personal config.

`company-mode` has [many extensions](https://github.com/company-mode/company-mode/wiki/Third-Party-Packages)
for various programming languages.
Some of Prelude's modules will install and enable the relevant extensions when necessary.
