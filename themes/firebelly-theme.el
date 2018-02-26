;;; firebelly-theme.el --- a subdued, dark theme with pastelish colors.

;; Author: startling <tdixon51793@gmail.com>
;; URL: https://github.com/startling/firebelly
;; Version: 1.0
;; Package-Requires: ((cl-lib "0.5"))

;; "I came up to a clearing, where the crosswinds cooled my face.  I
;;  would've sat down there in the middle, I would've rested just a
;;  little, but for the fire-bellied toads. See, the ground was wet and
;;  they were everywhere. Fire-bellied toad number five from what may
;;  or may not have been unlimited series opened up his little mouth
;;  as though to speak, and then he spoke to me, and he said:
;; 'You can't holler down a rain barrel. You can't climb around a tree.
;;  I don't want to play in your yard if you won't be good to me.'
;;  Honey, it was downright creepy."

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(require 'cl-lib)

(deftheme firebelly)
(provide-theme 'firebelly)
(let
  ;; This beautiful palette is shamelessly stolen from chris
  ;; kempson's base16: https://github.com/chriskempson/base16
  ((palette
    '("#ac4142" "#d28445" "#f4bf75" "#90a959"
      "#75b5aa" "#6a9fb5" "#aa759f" "#8f5536"))
   ;; Simple grayscale palette.
   (greys
    '("#222222" "#292929" "#444444" "#555555"
      "#666666" "#777777" "#888888" "#999999")))
  ;; Tiny functions to make getting from the palettes easier.
  (cl-flet
      ((color (n) (nth n palette))
       (grey (n) (nth n greys)))
    ;; Define some faces for our theme.
    (custom-theme-set-faces
     'firebelly
    ;;;; Styling emacs.
     `(default
        ((t (:background ,(grey 0) :foreground ,(grey 7)))))
     `(cursor
       ((t (:background ,(grey 3)))))
     `(highlight
       ((t (:background ,(color 5) :foreground ,(grey 1)))))
     `(shadow
       ((t (:foreground ,(grey 6)))))
     `(isearch
       ((t (:background ,(grey 2) :foreground ,(color 6)))))
     `(query-replace
       ((t (:background ,(grey 2) :foreground ,(color 6)))))
     `(lazy-highlight
       ((t (:background ,(grey 1) :foreground ,(grey 3)))))
     `(minibuffer-prompt
       ((t (:foreground ,(color 1)))))
     `(trailing-whitespace
       ((t (:background ,(grey 1)))))
     `(nobreak-space
       ((t (:background ,(grey 1)))))
     `(escape-glyph
       ((t (:foreground ,(color 2)))))
     ;; Fringes are ugly.
     `(fringe
       ((t (:background ,(grey 0)))))
     ;; Highlight the border.
     `(vertical-border
       ((t (:foreground ,(grey 1)))))
     ;; Mode lines look the same but for the text.
     `(mode-line
       ((t (:background ,(grey 1) :foreground ,(grey 6) :box nil))))
     `(mode-line-inactive
       ((t (:background ,(grey 1) :foreground ,(grey 3) :box nil))))
     `(header-line
       ((t (:background ,(grey 1) :foreground ,(grey 3) :box nil))))
     `(mode-line-buffer-id
       ((t (:bold t))))
     `(mode-line-highlight
       ((t (:foreground ,(grey 7)))))
     ;; Regions are slightly lighter.
     `(region
       ((t (:background ,(grey 1)))))
     `(secondary-selection
       ((t (:background ,(grey 2)))))
    ;;;; Styling code.
     ;; Comments are lighter than their delimiters.
     `(font-lock-comment-face
       ((t (:foreground ,(grey 3)))))
     `(font-lock-comment-delimiter-face
       ((t (:foreground ,(grey 1)))))
     ;; Docstrings are slightly lighter.
     `(font-lock-doc-face
       ((t (:foreground ,(grey 4) :background ,(grey 0)))))
     `(font-lock-function-name-face
       ((t (:foreground ,(color 3)))))
     `(font-lock-variable-name-face
       ((t (:foreground ,(grey 2)))))
     `(font-lock-builtin-face
       ((t (:foreground ,(color 6)))))
     `(font-lock-constant-face
       ((t (:foreground ,(color 1)))))
     `(font-lock-type-face
       ((t (:foreground ,(color 4)))))
     `(font-lock-string-face
       ((t (:foreground ,(color 5) :background ,(grey 1)))))
     `(font-lock-keyword-face
       ((t (:foreground ,(color 6)))))
    ;;;; Styling extensions.
     ;; Erc faces.
     `(erc-notice-face
       ((t (:foreground ,(grey 2)))))
     `(erc-current-nick-face
       ((t (:foreground ,(color 3) :bold t))))
     `(erc-prompt-face
       ((t (:foreground ,(color 1) :background ,(grey 0)))))
     `(erc-input-face
       ((t (:foreground ,(grey 6)))))
     `(erc-my-nick-face
       ((t (:foreground ,(color 1)))))
     `(erc-error-face
       ((t (:foreground ,(color 0)))))
     `(erc-timestamp-face
       ((t (:foreground ,(color 3)))))
     ;; Rainbow delimiters are mostly in order.
     `(rainbow-delimiters-depth-1-face
       ((t (:foreground ,(color 7)))))
     `(rainbow-delimiters-depth-2-face
       ((t (:foreground ,(color 6)))))
     `(rainbow-delimiters-depth-3-face
       ((t (:foreground ,(color 5)))))
     `(rainbow-delimiters-depth-4-face
       ((t (:foreground ,(grey 3)))))
     `(rainbow-delimiters-depth-5-face
       ((t (:foreground ,(color 4)))))
     `(rainbow-delimiters-depth-6-face
       ((t (:foreground ,(color 3)))))
     `(rainbow-delimiters-depth-7-face
       ((t (:foreground ,(color 2)))))
     `(rainbow-delimiters-depth-8-face
       ((t (:foreground ,(color 1)))))
     `(rainbow-delimiters-depth-9-face
       ((t (:foreground ,(color 0)))))
     ;; shm faces
     `(shm-current-face
       ((t (:background ,(grey 1)))))
     `(shm-quarantine-face
       ((t (:foreground ,(color 0) :background ,(grey 1)))))
     ;; linum-relative faces
     `(linum-relative-current-face
       ((t (:background ,(grey 0) :foreground ,(color 1))))))))

;;; firebelly-theme.el ends here
