;;This module gives Doom its signature look: powered by the doom-one theme (inspired by Atom’s One Dark theme) and solaire-mode.
;;
;;    A colorscheme inspired by Atom’s One Dark theme.
;;    A custom folded-region indicator for hideshow.
;;    “Thin bar” fringe bitmaps for git-gutter-fringe.
;;    File-visiting buffers are slightly brighter (thanks to solaire-mode).

;; solaire-mode is an aesthetic plugin that makes non-file-visiting buffers darker than the rest of the Emacs’ frame
;; (to visually differentiate temporary windows or sidebars from editing windows).
;; This looks great in GUI Emacs, but can look questionable in the terminal.
(solaire-global-mode +1)

;; Although this module uses the doom-one theme by default, doom-themes offers a number of alternatives:
;;
;;    doom-one: doom-themes’ flagship theme, inspired by Atom’s One Dark themes.
;;    doom-vibrant: a more vibrant version of doom-one.
;;    doom-molokai: based on Textmate’s monokai.
;;    doom-nova: adapted from Nova.
;;    doom-one-light: light version of doom-one.
;;    doom-peacock: based on Peacock from daylerees’ themes.
;;    doom-tomorrow-night: by Chris Kempson.
;;
;; This can be changed by changing the doom-theme variable, e.g. )
(setq doom-theme 'doom-one)

;; core/core-ui.el has four relevant variables:
;;
;; doom-font
;;    the default font to use in Doom Emacs.
;; doom-big-font
;;    the font to use when doom-big-font-mode is enabled.
;; doom-variable-font
;;    the font to use when variable-pitch-mode is active (or where the variable-pitch face is used).
;; doom-unicode-font
;;    the font used to display unicode symbols. This is ignored if the :ui unicode module is enabled.
(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 14)
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "JetBrains Mono" :size 18))
