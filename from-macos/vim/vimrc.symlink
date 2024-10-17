" .vimrc for Paul Lambert
" OS X specific
" lambertington @ gmail . com

" basics
syntax on
set nu                " line numbers on
set nocompatible      " don't need VI compatibility anymore
set scrolloff=4    	  " 4 lines above/below cursor while scrolling
let mapleader=','     " leader for plugins
set history=1000      " remember more commands
set showcmd           " shows what you are typing as a command
set foldmethod=marker " folding stuff
set modelines=0       " security
set encoding=utf-8    " utf-8

" word wrap without line breaks
set wrap              " turn on text wrapping
set linebreak         " only wrap at a character in breakat option
set nolist            " list disables linebreak
set textwidth=0       " ???
set wrapmargin=0      " ???

" searching
set ignorecase		  " case insensitive searching
set smartcase		  " but searching becomes proper case if typed that way
set incsearch         " vim searches forward in real time as typing continues
set nohlsearch        " permanately disables hlsearch

" tabs
set smarttab          " uses SHIFTWIDTH rather than TABSTOP for beginning of lines
set autoindent        " automatically indents next line
set smartindent       " automatically indents next line at beginning of loops, etc
set expandtab         " inserts spaces rather than <TAB>s

" turn off space-tabs for makefiles; change python tabs to 4 spaces
" default to 2-space tabs
let _curfile = expand("%:t")
if _curfile =~ "Makefile" || _curfile =~ "makefile" || _curfile =~ ".*\.mk"
    set noexpandtab
elseif _curfile =~ ".*\.py"
    set expandtab
    set tabstop=4       " width of <TAB> character
    set shiftwidth=4    " handles auto indentation; >>, <<, and ==

else
    set expandtab
    set tabstop=4       " width of <TAB> character
    set shiftwidth=4    " handles auto indendentation; >>, <<, and ==
endif

" etc
set noshowmatch       " disables stupid coloring for matching {, [, (, etc.
set backspace=2       " fix for weird backspace bug on windows machines
set ruler             " show current row and column
set ai                " turn on auto indenting
set showmatch         " jump to matches when entering regex

" annoying bells
set noeb vb t_vb=     " same as  - set noerrorbells visualbell t_vb=
if has('autocmd')
    au GUIEnter * set vb t_vb=
endif

" use English for spell checking
if version >= 700
    set spl=en spell
    set nospell
endif

" SEPARATE OPTIONS FOR GVIM/MVIM VS TERMINAL VIM
if has('gui_running')
  set guioptions-=T   " no toolbar
endif

set background=dark   " because light hurts my eyes
" colorscheme solarized " gvim/mvim color scheme
colorscheme zenburn
" colorscheme molokai2
" colorscheme rdark
" colorscheme ir_black

" automatically cd into file's directory
autocmd BufEnter * execute "chdir ".escape(expand("%:p:h"), ' ')

" remove trailing whitespace in file
autocmd BufRead,BufWrite * if ! &bin | silent! %s/\s\+$//ge | endif

" file-specific options
let php_htmlInStrings=1
let php_parent_error_close=1
let php_noShortTags=1
let php_baselib=1

let apache_version="2.2"
