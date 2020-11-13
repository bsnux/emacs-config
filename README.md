# Emacs configuration

Set of configuration files for [Emacs](https://www.emacs.org):

* `osx-init.el`: Specific for [XEmacs](https://www.xemacs.org/) on macOS.
* `better-defaults.el`: No packages and ready to use in terminal and GUI.
* `init.el`: Generic file with packages and ready to use in terminal and GUI.
* `doom`: Directory for configuration files for [Doom Emacs](https://github.com/hlissner/doom-emacs)
* `devops-init.el`: Configuration for console emacs + GUI Emacs on macOS. Specific for DevOps

## Installation

```sh
git clone https://github.com/bsnux/emacs-config.git ~/.emacs.d
```

## Usage

Pick your favorite and **symlinking** that to your `~/.emacs.d/init.el`

Example:

```
$ ln -s better-defaults.ini ~/.emacs.d/init.el
```

## Doom Emacs

On *macOs* you should use [Emacs Plus](https://github.com/d12frosted/homebrew-emacs-plus):

```
$ brew tap d12frosted/emacs-plus
$ brew install emacs-plus
```
