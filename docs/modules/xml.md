# Prelude XML

## Overview

This module provides configuration for editing XML files via the built-in
`nxml-mode`.

## Configuration

Prelude sets the following defaults:

- **Child indent**: 4 spaces
- **Attribute indent**: 4 spaces
- **Slash auto-complete**: enabled (typing `/` in a closing tag auto-completes it)
- **Meta-Tab completion**: enabled
- **No auto XML declaration**: the XML declaration is not auto-inserted

## File Associations

- Files starting with `<?xml` are automatically opened in `nxml-mode`
- `.pom` files (Maven) are recognized as XML
