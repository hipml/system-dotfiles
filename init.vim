" Enable true colors
set termguicolors

" Set the background to dark
set background=dark

" Load your color scheme (replace with the path to kod-adapted if needed)
colorscheme kod-adapted

" Ensure syntax highlighting is enabled
syntax on

" Highlight groups for better Python experience
highlight link Import Keyword
highlight link Method Function
highlight link NamedArgument Identifier

" Set cursor line for better visibility
set cursorline
highlight CursorLine guibg=#333333

" Enable line numbers and cursorline numbers
set number
highlight LineNr guifg=#444444
highlight CursorLineNr guifg=#666666
