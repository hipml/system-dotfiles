" Vim colorscheme: gptkod
" Maintainer: You
" Description: A custom dark theme with vibrant colors

hi clear
if exists("syntax_on")
  syntax reset
endif
set background=dark
let g:colors_name = "gptkod"

" Core colors
hi Normal guifg=#eeeeee guibg=#222222
hi Cursor guifg=#ff3319
hi Visual guibg=#2F3F52
hi LineNr guifg=#666666 guibg=#333333
hi CursorLineNr guifg=#eeeeee guibg=#333333 gui=bold
hi Comment guifg=#666666 gui=italic
hi Constant guifg=#C0D164
hi String guifg=#9aca7e guibg=#212A24
hi Number guifg=#C969B6
hi Keyword guifg=#66c8ef
hi Function guifg=#85FFDF guibg=#1F2B31
hi Type guifg=#FFA07A
hi PreProc guifg=#9C8B7C
hi Special guifg=#A19DBF
hi Todo guifg=#946B57 gui=bold
hi MatchParen guifg=#66c8ef gui=bold
hi Error guifg=#ff3319 gui=bold
hi StatusLine guifg=#eeeeee guibg=#444444
hi StatusLineNC guifg=#666666 guibg=#333333
hi VertSplit guifg=#444444 guibg=#444444
hi Pmenu guifg=#eeeeee guibg=#333333
hi PmenuSel guibg=#2F3F52
hi DiffAdd guibg=#212A24
hi DiffDelete guibg=#342C22
hi DiffChange guibg=#2B2F26
hi DiffText guibg=#1F2B31
