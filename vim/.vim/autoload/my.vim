" Convenience for Ledger reconciling:

" TODO: Store the log of reconciled and missing txns somewhere more sensible
" than register "t".
"
function! my#clearthislist() abort
	let @t .= getqflist()[line('.')-1].text . "\n"
	execute "normal \<CR>i* \<Esc>\<C-w>wj"
endfunction

function! my#stashunknowntxn()
	let new = input("Enter missing txn info: ") . "\n"
	let @t .= new
	execute "normal \<C-w>wG"
	$put =new
	execute "normal o\<Esc>\<C-w>w"
endfunction

function! my#reconcilefunc(acct) abort
	call ledger#register(g:ledger_main, '-UR ' . a:acct)
	nnoremap <buffer> <localleader>r :call my#clearthislist()<CR>
	nnoremap <buffer> <localleader>u :call my#stashunknowntxn()<CR>
endfunction
