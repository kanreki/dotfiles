normal Gzz

" TODO: I thought about implementing custom completion, but on second
" thought maybe it's not worth the trouble, just to abbreviate typing
" "bank.check" or "visa" once a month.
command! -nargs=1 Myrecon let @t = '' | call my#reconcilefunc(<q-args>)
