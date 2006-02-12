" Vim syntax file
" Language:				M4
" Maintainer:			AIDA Shinra <shinra@j10n.org>
" Former Maintainer:	Claudio Fleiner
" Last Change:			2006 Feb 12

" This file will highlight user function calls if they use only
" capital letters and have at least one argument (i.e. the '('
" must be there). Let me know if this is a problem.

" If you are using the changequote() and changecom(), you can specify
" the quote and comment delimiters just like vim modelines in the
" following way:
"
" <BLANK>vim-m4-syntax: quote=[,] comment=<!--,-->:
"
" You can specify your default delimiters at your vimrc:
"
" :let g:m4_default_quote="`,'"
" :let g:m4_default_comment='#'
"
" After the file read, you can change them in the following way:
"
" :let b:m4_quote='<<,>>'
" :let b:m4_comment=''
" :call M4UpdateSyntax()

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  " we define it here so that included files can test for it
  let main_syntax='m4'
endif

function! s:TransformString(in)
  let out = substitute(a:in, '@', '@a', 'g')
  let out = substitute(out, '\\\\', '@b', 'g')
  let out = substitute(out, '\\:', '@c', 'g')
  let out = substitute(out, '\\ ', '@s', 'g')
  let out = substitute(out, '\\', '', 'g')
  return out
endfunction

function! s:RetransformString(in)
  let out = substitute(a:in, '@s', ' ', 'g')
  let out = substitute(out, '@c', ':', 'g')
  let out = substitute(out, '@b', '\\', 'g')
  let out = substitute(out, '@a', '@', 'g')
  return out
endfunction

function! s:EscapeRegexp(s)
  return escape(a:s, '"\[].*^$')
endfunction

function! s:CheckModeline(line)
  let match = matchstr(a:line, '\svim-m4-syntax:.*')
  if match == ""
    return
  endif
  let allopts = substitute(match, '^\svim-m4-syntax:\s*', '', '')
  let allopts = matchstr(s:TransformString(allopts), '^[^:]*')

  while allopts != ""
    let opt = matchstr(allopts, '^\S*')
    let allopts = substitute(allopts, '^\S*\s*', '', '')
    let optname = s:RetransformString(matchstr(opt, '^[^=]*'))
    let optarg = s:RetransformString(substitute(opt, '^[^=]*=\?', '', ''))

    if optname == 'quote'
      let b:m4_quote = optarg
    elseif optname == 'comment'
      let b:m4_comment = optarg
    endif
  endwhile
endfunction

function! M4UpdateSyntax()
  if !exists('b:m4_quote')
    echohl Error
    echo 'Not a m4 file'
    echohl None
    return -1
  endif
  let qopen = matchstr(b:m4_quote, '[^,]*')
  let qclose = substitute(b:m4_quote, '[^,]*,\?', '', '')
  if qopen == '' || qclose == ''
    echohl Error
    echo 'm4_quote must be specified in OPEN,CLOSE form'
    echohl None
    return -1
  endif
  let qopen = s:EscapeRegexp(qopen)
  let qclose = s:EscapeRegexp(qclose)

  let copen = matchstr(b:m4_comment, '[^,]*')
  let cclose = substitute(b:m4_comment, '[^,]*,\?', '', '')
  if copen == '' && b:m4_comment != ''
    echohl Error
    echo 'm4_comment must be specified in [OPEN[,CLOSE]] form'
    echohl None
    return -1
  endif
  let copen = s:EscapeRegexp(copen)
  let cclose = s:EscapeRegexp(cclose)

  if exists('b:current_syntax')
    syn clear m4Quote m4NotExpanded m4String m4OuterComment m4InnerComment m4StringInComment m4QuoteError m4QuoteSplit
    if s:multiline_comment
      syn clear m4StringInMLComment m4QuoteInMLComment m4CommentError
    endif
  endif
  let s:multiline_comment=0
  execute 'syn match m4QuoteError "'.qclose.'"'
  " Allow constructs like `FOO($'`1, bar)'
  execute 'syn match m4QuoteSplit "'.qclose.qopen.'" contained'
  " Leave outermost quoted string black
  execute 'syn region m4NotExpanded matchgroup=m4Quote start="'.qopen.'" end="'.qclose.'" contains=@m4StringContents,m4NotExpanded,SpellErrors'
  execute 'syn region m4String matchgroup=m4Quote start="'.qopen.'" end="'.qclose.'" contained contains=@m4Top,@m4StringContents,SpellErrors'
  syn match m4OuterComment "\<\(m4_\)\=dnl\>.*" contains=SpellErrors
  " Allow FOO(`xyz dnl bar')
  execute 'syn region m4InnerComment start="\<\(m4_\)\=dnl\>" end="$\|'.qclose.'"me=s-1 oneline contained contains=m4StringInComment,SpellErrors'
  " Allow FOO(`xyz dnl `bar' buz')
  " XXX: How to detect quoting errors like this?
  "      FOO(`xyz dnl ``bar'<NEWLINE>')
  execute 'syn region m4StringInComment start="'.qopen.'"ms=e end="'.qclose.'" oneline transparent contained contains=m4StringInComment,SpellErrors'
  if copen != ''
    if cclose == ''
      execute 'syn region m4OuterComment start="'.copen.'" end="$" contains=SpellErrors'
      execute 'syn region m4StringInComment start="'.qopen.'"ms=e end="'.qclose.'" oneline transparent contained contains=m4StringInComment,SpellErrors'
      " Allow FOO(`xyz # bar')
      execute 'syn region m4InnerComment start="'.copen.'" end="$\|\ze'.qclose.'" oneline contained contains=m4StringInComment,SpellErrors'
    else
      execute 'syn match m4CommentError "'.cclose.'"'
      execute 'syn region m4OuterComment start="'.copen.'" end="'.cclose.'" contains=SpellErrors'
      " Allow FOO(`/*') (but FOO(`*/') is still an m4CommentError)
      execute 'syn region m4InnerComment start="'.copen.'" end="'.cclose.'\|\ze'.qclose.'" contained contains=m4StringInMLComment,SpellErrors'
      " XXX: How to detect quoting errors like this?
      "      FOO(`xyz <!-- ``bar' -->')
      execute 'syn region m4StringInMLComment matchgroup=m4QuoteInMLComment start="'.qopen.'"ms=e end="\ze'.cclose.'\|'.qclose.'" transparent contained contains=m4StringInMLComment,SpellErrors'
      let s:multiline_comment=1
    endif
  endif
  return 0
endfunction

function! s:Initialize()
  if !exists('b:m4_quote')
    let b:m4_quote = exists('g:m4_default_quote') ? g:m4_default_quote : "`,'"
  endif
  if !exists('b:m4_comment')
    let b:m4_comment = exists('g:m4_default_comment') ? g:m4_default_comment : '#'
  endif

  let lnum = 1
  while lnum <= &modelines && lnum <= line('$')
    call s:CheckModeline(getline(lnum))
    let lnum = lnum + 1
  endwhile
  if lnum + &modelines < line('$')
    let lnum = line('$') - &modelines
  endif
  while lnum <= line('$')
    call s:CheckModeline(getline(lnum))
    let lnum = lnum + 1
  endwhile

  if M4UpdateSyntax() != 0
    let b:m4_quote = "`,'"
    let b:m4_comment = '#'
    call M4UpdateSyntax()
  endif
endfunction

call s:Initialize()

" define the m4 syntax
syn match  m4Variable contained "\$\d\+"
syn match  m4Special  contained "$[@*#]"
syn match  m4Constants "\<\(m4_\)\=__file__\>"
syn match  m4Constants "\<\(\<m4_\)\=__line__\>"
syn match  m4ParenError ")"
syn keyword m4Constants divnum sysval m4_divnum m4_sysval
syn region m4Paren    matchgroup=m4Delimiter start="(" end=")" transparent contains=@m4InParen,
syn region m4Command  matchgroup=m4Function  start="\<\(m4_\)\=\(define\|defn\|pushdef\)(" end=")" contains=@m4InParen
syn region m4Command  matchgroup=m4Preproc   start="\<\(m4_\)\=\(include\|sinclude\)("he=e-1 end=")" contains=@m4InParen
syn region m4Command  matchgroup=m4Statement start="\<\(m4_\)\=\(syscmd\|esyscmd\|ifdef\|ifelse\|indir\|builtin\|shift\|errprint\|m4exit\|changecom\|changequote\|changeword\|m4wrap\|debugfile\|divert\|undivert\)("he=e-1 end=")" contains=@m4InParen
syn region m4Command  matchgroup=m4Builtin start="\<\(m4_\)\=\(len\|index\|regexp\|substr\|translit\|patsubst\|format\|incr\|decr\|eval\|maketemp\)("he=e-1 end=")" contains=@m4InParen
syn match m4Statement  '\<\(m4_\)\=\(divert\|undivert\)\>(\@!'
syn region m4Command  matchgroup=m4Type      start="\<\(m4_\)\=\(undefine\|popdef\)("he=e-1 end=")" contains=@m4InParen
syn region m4Function matchgroup=m4Type      start="\<[_A-Z][_A-Z0-9]*("he=e-1 end=")" contains=@m4InParen
syn cluster m4InParen contains=@m4Top,m4QuoteError,m4QuoteSplit
syn cluster m4Top     contains=m4InnerComment,m4Constants,m4Special,m4Variable,m4String,m4BracketString,m4Paren,m4Command,m4Statement,m4Function,m4ParenError,m4CommentError

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_m4_syn_inits")
  if version < 508
    let did_m4_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink m4Delimiter Delimiter
  HiLink m4OuterComment Comment
  HiLink m4InnerComment Comment
  HiLink m4QuoteInMLComment Comment
  HiLink m4Function  Function
  HiLink m4Keyword   Keyword
  HiLink m4Special   Special
  HiLink m4String    String
  HiLink m4Quote     String
  HiLink m4QuoteSplit String
  HiLink m4Statement Statement
  HiLink m4Preproc   PreProc
  HiLink m4Type      Type
  HiLink m4Special   Special
  HiLink m4Variable  Special
  HiLink m4Constants Constant
  HiLink m4Builtin   Statement
  HiLink m4QuoteError Error
  HiLink m4ParenError Error
  HiLink m4CommentError Error
  delcommand HiLink
endif

let b:current_syntax = "m4"
setlocal cpoptions-=%
setlocal cpoptions+=M

if main_syntax == 'm4'
  unlet main_syntax
endif

" vim: ts=4 sw=2:
