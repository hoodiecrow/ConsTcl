
unset -nocomplain M
# memory cell number
set M 0

unset -nocomplain S
# string store number
set S 0

unset -nocomplain StrSto
set StrSto [list]

interp alias {} #NIL {} [NIL create Mem0]

interp alias {} #t {} [::constcl::MkBoolean #t]

interp alias {} #f {} [::constcl::MkBoolean #f]

interp alias {} #-1 {} [::constcl::MkNumber -1]

interp alias {} #0 {} [::constcl::MkNumber 0]

interp alias {} #1 {} [::constcl::MkNumber 1]

set ::constcl::_quote [::constcl::MkSymbol quote]
interp alias {} #Q {} $::constcl::_quote

interp alias {} #+ {} [::constcl::MkSymbol +]

interp alias {} #- {} [::constcl::MkSymbol -]

interp alias {} #EOF {} [EndOfFile create Mem[incr ::M]]

CB
dict set ::standard_env pi [::constcl::MkNumber 3.1415926535897931]
CB


reg atom? ::constcl::atom?

proc ::constcl::atom? {obj} {
    if {[symbol? $obj] eq "#t" || [number? $obj] eq "#t" || [string? $obj] eq "#t" || [char? $obj] eq "#t" || [boolean? $obj] eq "#t" || [vector? $obj] eq "#t"} {
        return #t
    } else {
        return #f
    }
}

