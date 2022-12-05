" vim:ts=2:sw=2
" Convenience for Ledger reconciling:

" TODO: Store the log of reconciled and missing txns somewhere more sensible
" than register "t".
"
function! my#clearthislist() abort
	let @t .= getqflist()[line('.')-1].text . "\n"
	execute "normal \<CR>i* \<Esc>\<C-w>wj"
endfunction

function! my#stashunknowntxn()
	let note = "; " . input("Enter missing txn info: ") . "\n"
	let @t .= note
	wincmd w
	$put =note . \"\n\"
	wincmd w
endfunction

function! my#reconcilefunc(acct) abort
	call ledger#register(g:ledger_main, '-UR ' . a:acct)
	nnoremap <buffer> <localleader>r :call my#clearthislist()<CR>
	nnoremap <buffer> <localleader>u :call my#stashunknowntxn()<CR>
endfunction

function my#recorrection(l, amt)
	let entry = ledger#transaction_from_lnum(a:l)
	if empty(entry)
		throw 'cant isolate Ledger entry line ' . a:l
	endif
	let [h,t] = [entry.head, entry.tail]
	let goal = 0
	for i in range(h+1, t)
		if i == a:l
			continue
		endif
		if match(getline(i), '^\s*\(;.*\)\=$') >= 0
			continue
		endif
		if goal > 0
			throw 'line ' . a:l . ' is part of a multi-leg txn'
		endif
		let goal = i
	endfor

	let found = 0
	for i in [a:l, goal]
		let s = getline(i)
		let ml = matchlist(getline(i), '\(^[^*$]*\$-\=\)\([,0-9]\+\.\d\{2\}\)\(.*$\)')
		if len(ml) == 0
			continue
		endif
		let found = 1
		call setline(i, join([ml[1], a:amt, ml[3]], ''))
	endfor
	if !found
		throw 'no place near line ' . a:l . ' to put correction $' . a:amt
	endif
	execute a:l . " normal ^i* \<Esc>"
endfunction
