HMatcher
========

A simple command line fuzzy matcher, written in Haskell

Building
--------

Just a `make` away

Usage with CtrlP & vim
----------------------

Adapted from [Burke's matcher](https://github.com/burke/matcher), replace
`g:path_to_hmatcher` with the path to the hmatcher executable:

```{.vimL}
let g:path_to_hmatcher = "/path/to/hmatcher"

if !empty(glob(g:path_to_hmatcher))
  let g:ctrlp_match_func = { 'match': 'HMatch' }
endif

function! HMatch(items, str, limit, mmode, ispath, crfile, regex)

  let cachefile = ctrlp#utils#cachedir().'/matcher.cache'
  if !( filereadable(cachefile) && a:items == readfile(cachefile) )
    call writefile(a:items, cachefile)
  endif
  if !filereadable(cachefile)
    return []
  endif

  let cmd = g:path_to_hmatcher . " " . shellescape(a:str) . " " . a:limit . " < " . cachefile

  return split(system(cmd), "\n")

endfunction
```
