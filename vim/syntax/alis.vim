" syntax/alis.vim
if exists("b:current_syntax")
  finish
endif

syntax clear
let b:current_syntax = "alis"

syntax keyword alisKeywords
	\ template
	\ mixin
	\ import
	\ as
	\ this
	\ alias
	\ fn
	\ var
	\ enum
	\ struct
	\ union
	\ utest
	\ pub
	\ ipub
	\ return
	\ auto
	\ const
	\ static
	\ int
	\ uint
	\ float
	\ char
	\ string
	\ bool
	\ if
	\ else
	\ while
	\ do
	\ for
	\ switch
	\ case
	\ break
	\ continue

syntax keyword alisBool
	\ true
	\ false

highlight default link alisKeywords Keyword
highlight default link alisBool Boolean

syntax match alisNumber "\v<\d+(.\d+(e\d+)?)?>"
syntax match alisComment "//.*$"
syntax region alisMultilineComment start="/\*" end="\*/"
syntax region alisString start="\"" end="\"" skip="\\\\|\\\""
syntax match alisIdentifier "\v<[a-zA-Z_]+[A-Za-z0-9_]+>"

highlight default link alisNumber Number
highlight default link alisComment Comment
highlight default link alisMultilineComment Comment
highlight default link alisString String
highlight default link alisIdentifier Identifier

let b:current_syntax = "alis"
