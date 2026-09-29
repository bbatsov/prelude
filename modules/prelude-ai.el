;;; prelude-ai.el --- Emacs Prelude: AI assistant configuration.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2011-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration for using LLM-backed AI assistants in Emacs via
;; gptel.  gptel is a multi-backend chat client supporting Anthropic
;; (Claude), OpenAI (GPT), Google (Gemini), local Ollama, and other
;; providers.
;;
;; This module installs gptel and binds `gptel-menu' (the transient
;; entry point that exposes all gptel commands) to `C-c q'.  You still
;; need to configure a backend and supply an API key in your personal
;; config -- see the module's documentation for examples.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; gptel: a multi-backend LLM client.  See https://github.com/karthink/gptel
;; for the full list of supported backends and configuration options.
(use-package gptel
  :ensure t
  :bind (("C-c q" . gptel-menu)))

(provide 'prelude-ai)
;;; prelude-ai.el ends here
