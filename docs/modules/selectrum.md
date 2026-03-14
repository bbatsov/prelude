# Prelude Selectrum

!!! Warning

    Selectrum is no longer actively maintained. The author recommends switching
    to [Vertico](vertico.md) instead.

## Overview

[Selectrum](https://github.com/radian-software/selectrum) is a completion
framework for Emacs, similar to Ivy and Ido but with a simpler design.

!!! Note

    This module is an alternative to the [Helm](helm.md), [Ido](ido.md),
    [Ivy](ivy.md), and [Vertico](vertico.md) modules - you should
    only enable one completion framework.

## Packages

- [selectrum](https://github.com/radian-software/selectrum) -
  the completion framework
- [selectrum-prescient](https://github.com/radian-software/prescient.el) -
  intelligent sorting and filtering

## Features

- **Prescient sorting**: completion candidates are sorted by
  frequency and recency
- **Persistent history**: command history is saved to disk,
  so sorting improves over time
