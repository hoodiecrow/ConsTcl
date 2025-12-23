
.PHONY: all constcl.tcl append apply error eval if list read variable

all: append apply error eval if list read variable | constcl.tcl
	grep -wsnP append $| >temp-append
	grep -wsnP apply $| >temp-apply
	grep -wsnP error $| >temp-error
	grep -wsnP eval $| >temp-eval
	grep -wsnP if $| >temp-if
	grep -wsnP list $| >temp-list
	grep -wsnP read $| >temp-read
	grep -wsnP variable $| >temp-variable
	#$(foreach item,$^,grep -wsnP $(item) $| >temp-$(item))
	#$(foreach item,$^,grep -P '$(item)' $|)
	#$(foreach item,$^,$(shell grep -P '(?<!(::|.-))\b$(item)\b' $|))
	#grep $^ $|
