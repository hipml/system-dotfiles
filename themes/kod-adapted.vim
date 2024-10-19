" Ensure Vim clears old highlights and uses truecolor
set termguicolors
set background=dark
highlight clear
if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "kod-adapted"

" UI Elements
highlight Normal          guibg=#222222 guifg=#eeeeee ctermbg=235 ctermfg=255
highlight CursorLine      guibg=#333333 ctermbg=236
highlight LineNr          guifg=#444444 ctermfg=239
highlight CursorLineNr    guifg=#666666 ctermfg=243

" Keywords, Imports, and Types
highlight Keyword         guifg=#66c8ef ctermfg=81
highlight link Import     Keyword  " Link 'import' statements to Keyword
highlight Type            guifg=#FFA07A ctermfg=216

" Functions and Method Calls
highlight Function        guifg=#85FFDF guibg=#1F2B31 ctermfg=122 ctermbg=234
highlight link Method     Function  " Link method calls to Function

" Named Arguments
highlight Identifier      guifg=#CAC059 ctermfg=222
highlight Parameter       guifg=#A19DBF ctermfg=146
highlight link NamedArgument Parameter  " Link named arguments to Parameter

" Strings and Comments
highlight String          guifg=#9aca7e guibg=#212A24 ctermfg=108 ctermbg=235
highlight Comment         guifg=#666666 gui=italic ctermfg=243

" Operators and Special Characters
highlight Operator        guifg=#A19DBF ctermfg=146
highlight SpecialChar     guifg=#C0D164 guibg=#2B2F26 ctermfg=186 ctermbg=236
