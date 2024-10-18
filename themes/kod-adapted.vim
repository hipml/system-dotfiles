" kod theme for Python

" Set the normal background and foreground colors
hi Normal         ctermfg=white   guifg=#eee guibg=#222
hi Cursor         ctermfg=white   guifg=#eee guibg=#ff3319
hi CursorLine     ctermfg=NONE    guifg=NONE guibg=#333
hi LineNr         ctermfg=darkgrey guifg=#666 guibg=#222
hi Selection      ctermfg=NONE    guifg=NONE guibg=#2F3F52

" Syntax highlighting for Python
hi Keyword        ctermfg=cyan     guifg=#66c8ef    " Keywords
hi Type           ctermfg=lightred guifg=lightsalmon " Built-in types
hi UserType       ctermfg=yellow   guifg=#CAC059    " User-defined types
hi ClassName      ctermfg=red      guifg=#eb7962    " Class names
hi String         ctermfg=green    guifg=#9aca7e guibg=#212A24 " Strings
hi SpecialChar    ctermfg=lightgreen guifg=#C0D164 guibg=#2B2F26 " Special characters
hi Regexp         ctermfg=yellow   guifg=#FFB14B guibg=#342C22 " Regular expressions
hi Comment        ctermfg=darkgrey  guifg=#666 gui=italic " Comments
hi Number         ctermfg=magenta   guifg=#C969B6    " Numbers
hi PreProc        ctermfg=darkgrey  guifg=#9C8B7C    " Preprocessor directives
hi Symbol         ctermfg=lightgrey guifg=#A19DBF    " Symbols
hi Function       ctermfg=cyan      guifg=#85FFDF guibg=#1F2B31  " Function definitions
hi FunctionCall   ctermfg=cyan      guifg=#85FFDF    " Function calls (colored the same as functions)
hi Variable       ctermfg=brown     guifg=#cda869    " Variables
hi Argument       ctermfg=lightblue guifg=#77B5FF gui=bold  " Named arguments
hi Parameter      ctermfg=lightblue guifg=#77B5FF gui=bold  " Function parameters
hi CurlyBracket   ctermfg=lightgrey guifg=#ddd        " Curly brackets
hi Todo           ctermfg=lightyellow guifg=#946B57 gui=bold " TODO comments

" Internet related
hi Url            ctermfg=lightblue guifg=#77B5FF gui=underline " URLs

" Other elements for ChangeLog and Log files
hi Date           ctermfg=red      guifg=#F09C9F gui=bold   " Dates
hi Time           ctermfg=lightgrey guifg=#A78AB0 gui=bold   " Times
hi File           ctermfg=lightgrey guifg=#A78AB0 gui=bold   " File names
hi Ip             ctermfg=green    guifg=#8CB194    " IP addresses
hi Name           ctermfg=green    guifg=#8CB194    " Names

" For LaTeX, markdown, etc
hi Italics        ctermfg=NONE     guifg=NONE gui=underline   " Italics
hi Bold           ctermfg=white    guifg=#fff gui=bold         " Bold text

" For diffs
hi OldFile        ctermfg=red      guifg=#f7bfb6 guibg=#42201C  " Old files
hi NewFile        ctermfg=green    guifg=#cff7bf guibg=#13340C  " New files
hi DiffLines      ctermfg=white    guifg=white guibg=#3D96DE   " Diff lines

" Additional Python specific syntax
hi Decorator      ctermfg=cyan     guifg=#C69C6D    " Decorators
hi AsyncFunction   ctermfg=cyan     guifg=#A7FF5D    " Async function declarations
