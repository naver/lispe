;Date: 21/07/2022
;Author: Claude Roux
;Description: Règles

; This program modifies the tokenization rules to better tokenize a grammar

; We create a tokenizer object
(setq tok (tokenizer_rules))

(setq rg `#32+=#
#9+=#
#10+=#
"{[\-"] ~%r}*"=34
%%?=0
${%a %d %h}+=0
%d+=57
%o=63
%p=32
%h{%h %d}*=65
%a{%a %d}*=65
`
)

(setq rg (split rg "\n"))
(setq rg (maplist 'trim rg))

; rg is a list of rules
(set_tokenizer_rules tok rg)

