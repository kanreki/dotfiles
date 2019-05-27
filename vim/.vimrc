:set cpoptions+=J  "Sentence motion requires two spaces after punctuation
:set backup        "Make back-up files, with ~ appended to the file name.
:set helpheight=40
:set ignorecase smartcase
:set nowrapscan
:set encoding=utf-8

:let g:ledger_main = 'inc'

" http://learnvimscriptthehardway.stevelosh.com/chapters/10.html
" (31-May-2018)
:inoremap jk <Esc>
:inoremap <Esc> <Nop>

:nnoremap <F2> <CR>^i* <Esc><C-w><C-w>
