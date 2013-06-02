# emacs-config

My personal Emacs configuration files. "init.el" contains auto-generated code created by themes and loads the file "user-init.el" just to keep things clean.

## Packages

For package managers, I only use MELPA. I currently have installed:

- solarized-theme
- rainbow-delimiters
- smex
- popup
- auto-complete

## Disabled Features

I have disabled certain annoyances that are default in Emacs:

- blinking cursor
- audible bell
- tool bar (in GUI mode)
- C-x f (changed set line width to find file)

## Enabled Features

I have also enabled a number of features that I find useful:

- display column number on mode bar
- display line number on left
- highlight the current line
- turned off splash screen and replaced with empty buffer
- set C-t to GOTO-line shortcut
- (for OS X) switched Meta to the CMD key (from ALT)

## Personal Features

Created a popup buffer switcher that keeps them in order of most recently accessed. Located in "lib/buffer-cycle/".