# emacs-config

My personal Emacs configuration files.

## Packages

For package managers, I only use MELPA. I currently have installed:

- solarized-theme
- rainbow-delimiters
- smex
- popup
- auto-complete

## Disabled Features

I have disable certain annoyances that are default in Emacs:

- blinking cursor
- audible bell
- tool bar (in GUI mode)

## Enabled Features

I have also enabled a number of features that I find useful:

- display column number on mode bar
- display line number on left
- highlight the current line
- turned off splash screen and replaced with empty buffer
- set C-t to GOTO-line shortcut
- (for OS X) switched Meta to the CMD key (from ALT)

## Personal Features

Created a popup buffer switcher that keeps them in order of most recently accessed. Located in lib/buffer-cycle/.