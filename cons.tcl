
unset -nocomplain M
# memory cell number
set M 0

unset -nocomplain S
# string store number
set S 0

unset -nocomplain StrSto
set StrSto [list]

interp alias {} #NIL {} [NIL create Mem0]

interp alias {} #t {} [Boolean create Mem[incr ::M] #t]

interp alias {} #f {} [Boolean create Mem[incr ::M] #f]

interp alias {} #-1 {} [Number create Mem[incr ::M] -1]

interp alias {} #0 {} [Number create Mem[incr ::M] 0]

interp alias {} #1 {} [Number create Mem[incr ::M] 1]

interp alias {} #Q {} [Symbol create Mem[incr ::M] quote]

interp alias {} #+ {} [Symbol create Mem[incr ::M] +]

interp alias {} #- {} [Symbol create Mem[incr ::M] -]

interp alias {} #EOF {} [EndOfFile create Mem[incr ::M]]


proc ::constcl::atom? {obj} {
    if {[symbol? $obj] eq "#t" || [number? $obj] eq "#t" || [string? $obj] eq "#t" || [char? $obj] eq "#t" || [boolean? $obj] eq "#t" || [vector? $obj] eq "#t"} {
        return #t
    } else {
        return #f
    }
}

