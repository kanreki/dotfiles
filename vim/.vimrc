let mapleader = ','
let maplocalleader = ','

set cpoptions+=J  "Sentence motion requires two spaces after punctuation
set backup        "Make back-up files, with ~ appended to the file name.
set helpheight=40
set ignorecase smartcase
set nowrapscan
set encoding=utf-8
set hidden        "Allow unsaved buffers to be buried from view

let g:ledger_main = 'inc'

" http://learnvimscriptthehardway.stevelosh.com/chapters/10.html
" (31-May-2018)
inoremap jk <Esc>

let g:ledger_qf_register_format = '%(date) %-4(code) %-30(payee) %15(amount)\n'
"
" TODO: consider overriding the default "Register" output format only while
" doing a reconcile, maybe by wrapping the :Reconcile command and saving the 
" global value currently in effect.
"
let g:ledger_qf_vertical = v:true
let g:ledger_qf_size = 80

" This may have been needed as a fix for DEL in insert mode, in the 
" days when I had mapped <Esc> to <Nop> (re: the "jk" trick, above).
" set t_kD=[3~

" The following two lines seem quite the hack, but have the effect 
" of disabling Netrw (see ":help netrw").
let g:loaded_netrw       = 1
let g:loaded_netrwPlugin = 1
