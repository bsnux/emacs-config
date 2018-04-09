# Simple command reference for Emacs

## Basic

**C-x C-c**: Exit

## Movement

**C-p** : Up

**C-n** : Down

**C-f** : Right

**C-b** : Left

## Copy & paste

**M-w** : Copy

**C-y** : Paste

**C-space** : Mark a region

## Files

**C-x C-f** : Open a file

**C-x C-s** : Save buffer to file

**C-x i** : Insert another file in this buffer

**M-<** : Go to beginning of file

**M->** : Go to end of file

## Buffers

**C-x k** : Kill the buffer

**C-x C-w** : Save buffer as

**C-x b** : Switch between buffers

## Windows & frames

**C-x o** : Switch between windows

**C-x 2** : Open a new window (vertically)

**C-x 3** : Open a new window (side by side)

**C-x 0**: Close current window

**C-x 5 2** : Create a new frame without a new process

**C-x 5 0** : Close this frame

## Finding

**M-x find-name-dired** : Find a file using *dired* mode

## Search & replace

**C-s** : Search forward (incremental)

**C-r** : Search backward

**C-s C-s** : Search with previously searched string

**M-x query-replace** : Search and replace, asking for confirmation

**M-x replace-string**

**C-x h** : Select all

**M-x rgrep** : Searching recursively for some strings (regexps) in a file tree

## Undo & redo

**C-/** : Undo

**C-x u** : Undo

**C_** : Redo

## Deleting

**C-k** : Kill to end of file

**C-d** : Delete current character

## Column mode

For editing columns we can use *CUA* mode:

1. **M-x** cua-mode
2. **C-Enter**
3. Mark text
4. Insert text!

## Tags

We can use *etags* command for generating our *TAGS* file, which is
used by *Emacs*

**M-.** : Find tag

**M-x find-tag** : Find tag

**M-,** : Go to next match

## Lisp programming

**C-x C-e**: Evaluate current buffer

## Input methods

* **M-x describe-input-method**: Info. about current input method

## Tramp

**/sudo::** => Open a file as *root*

**/user@server:** => Open a remote shell for editing files

## Coding

**C-x RET f undecided-unix**: Change line terminator from Dos/Mac to UNIX

## Case conversion

**M-l**: Convert following word to lower case

**M-u**: Convert following word to upper case

## Macros

**C-(**: Start recording macro

**C-)**: Stop recording macro

**C-e**: Execute macro

**C-x n title-case-macro**: Give *title-case-macro* name and save macro

**M-x title-case-macro**: Execute *title-case-macro*

**C-u 3 M-x title-case-macro**: Execte 3 times *title-case-macro*

## Spell checking

**M-x ispell**: Spell check using `Ispell` command

## Version control/git

**C-x v g**: Display who changed what (`blame`)

## Org mode

**Shif-left/right**: cycle workflow

**C-c C-v**: show todos in current document

**C-c a** : call the agenda

**C-c C-q** : set tag

**C-c ,** : set priority

**M-enter** : new item at current level

**C-c C-d** : Insert a DEADLINE

**C-c .** : Prompt for date

**C-c C-s** : Insert SCHEDULED timestamp

**C-c .** (one for each date) : Selecting interval dates

**C-c C-x a** : Archive a DONE task

## Misc

**C-x z** : Repeat last command. To repeat second time type **z**

**M-x shell-command** : Execute command

**M-x tmm-menubar** : Shows menu options

**M-x comment-region** : Set comments on marked region

**C-c C-c** : Execute script (Python mode and others)

**C-x C-f RET /user@host:/home/arturo/.emacs** : Open remote file using TRAMP

**C-s C-w** : Search word under cursor (equivalent to '*' in Vim)

**M-x align-regexp**: Align code based on regexp. 1) Mark region. 2) Execute it

**M-x occur**: Display a new buffer with founded occurences in file

**M-x sort-lines**: Sorting all lines in a file

**M-x tabify**: Converts spaces to tabs

**M-x untabify**: Converts tabs to spaces
