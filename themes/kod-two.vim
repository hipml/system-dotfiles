" kod theme

" Normal colors
hi Normal       ctermbg=235  ctermfg=255  guibg=#222  guifg=#eee
hi Cursor       ctermbg=160  ctermfg=255  guibg=#ff3319  guifg=#eee
hi CursorLine   ctermbg=236  ctermfg=NONE guibg=#333  guifg=NONE
hi LineNr       ctermbg=236  ctermfg=240  guibg=#333  guifg=#666

" Syntax highlighting
hi Keyword      ctermfg=68   guifg=#66c8ef
hi Type         ctermfg=208  guifg=lightsalmon
hi Typedef      ctermfg=142  guifg=#CAC059
hi Class        ctermfg=168  guifg=#eb7962
hi String       ctermfg=108  guifg=#9aca7e  guibg=#212A24
hi SpecialChar  ctermfg=144  guifg=#C0D164  guibg=#2B2F26
hi Regexp       ctermfg=184  guifg=#FFB14B  guibg=#342C22
hi Comment      ctermfg=240  guifg=#666  gui=italic
hi Number       ctermfg=170  guifg=#C969B6
hi PreProc      ctermfg=178  guifg=#9C8B7C
hi Symbol       ctermfg=146  guifg=#A19DBF
hi Function     ctermfg=190  guifg=#85FFDF  guibg=#1F2B31
hi FuncName     ctermfg=190  guifg=#85FFDF
hi Bracket      ctermfg=253  guifg=#ddd
hi Todo         ctermfg=226  guifg=#946B57  gui=bold

" Internet related
hi Url          ctermfg=117  guifg=#77B5FF  gui=underline

" other elements for ChangeLog and Log files
hi Date         ctermfg=216  guifg=#F09C9F  gui=bold
hi Time         ctermfg=142  guifg=#A78AB0  gui=bold
hi File         ctermfg=142  guifg=#A78AB0  gui=bold
hi Ip           ctermfg=108  guifg=#8CB194
hi Name         ctermfg=108  guifg=#8CB194

" for Prolog, Perl, lang
hi Variable     ctermfg=173  guifg=#cda869
hi Italic       gui=italic
hi Bold         ctermfg=208  guifg=lightsalmon  gui=bold

" for LaTeX, markdown, etc
hi Underline    ctermfg=64   guifg=#3C7E5B  gui=underline
hi Fixed        ctermfg=194  guifg=#bbeecc  guibg=#2c2c2c
hi Argument     ctermfg=146  guifg=#9194BB
hi Math         ctermfg=208  guifg=orange
hi Bibtex       ctermfg=105  guifg=#8D86EE
hi Heading      ctermfg=208  guifg=lightsalmon
hi List         ctermfg=226  guifg=yellow

" for diffs
hi Oldfile      ctermfg=216  guifg=#f7bfb6  guibg=#42201C
hi Newfile      ctermfg=194  guifg=#cff7bf  guibg=#13340C
hi Difflines    ctermfg=255  guifg=#3D96DE  guibg=#3D96DE

" for css
hi Selector     ctermfg=173  guifg=#cda869
hi Property     ctermfg=143  guifg=#c4af75
hi Value        ctermfg=228  guifg=#f9ed97

" for Oz, Erlang, etc
hi Atom         ctermfg=208  guifg=orange
hi Meta         gui=italic

" CSS v2 tests
hi CssValue     ctermfg=196  guifg=red
hi JsonValue    ctermfg=226  guifg=yellow
hi JsonString   ctermfg=196  guifg=#ffaa22