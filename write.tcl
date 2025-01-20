
CB
proc ::constcl::write-pair {obj} {
    # take an object and print the car and the cdr of the stored value
    set a [car $obj]
    set d [cdr $obj]
    # print car
    write $a
    if {[pair? $d] eq "#t"} {
        # cdr is a cons pair
        puts -nonewline " "
        write-pair $d;
    } elseif {$d eq "#NIL"} {
        # cdr is nil
        return
    } else {
        # it is an atom
        puts -nonewline " . "
        write $d
    }
}
CB

CB
proc ::constcl::write {obj args} {
    # take an object and print the value
    if {[number? $obj] eq "#t"} {
        puts -nonewline [$obj value]
    } elseif {[boolean? [interp alias {} $obj]] eq "#t"} {
        if {$obj eq "#t"} {
            puts -nonewline "#t"
        } else {
            puts -nonewline "#f"
        }
    } elseif {[symbol? $obj] eq "#t"} {
        puts -nonewline [$obj name]
    } elseif {[pair? $obj] eq "#t"} {
        # it is a cons pair
        puts -nonewline "("
        write-pair $obj
        puts -nonewline ")"
    }
}
CB

TT(

::tcltest::test write-1.0 {read and write a number} -body {
    set ::inputstr "99.99"
    set obj [::constcl::read]
    ::constcl::write $obj
} -output "99.99"

::tcltest::test write-1.1 {read and write a boolean} -body {
    set ::inputstr "#t"
    set obj [::constcl::read]
    ::constcl::write $obj
} -output "#t"

TT)

TT(

::tcltest::test write-2.0 {read and write a list} -body {
    set ::inputstr "(a b c)"
    set obj [::constcl::read]
    ::constcl::write $obj
} -output "(a b c)"

TT)

