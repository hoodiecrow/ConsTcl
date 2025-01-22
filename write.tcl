
CB
reg write ::constcl::write

proc ::constcl::write {obj args} {
    ::constcl::write-value $obj
    puts {}
}
CB

CB
proc ::constcl::write-value {obj} {
    # take an object and print the value
    $obj write
}
CB

CB
proc ::constcl::write-pair {obj} {
    # take an object and print the car and the cdr of the stored value
    set a [car $obj]
    set d [cdr $obj]
    # print car
    write-value $a
    if {[pair? $d] eq "#t"} {
        # cdr is a cons pair
        puts -nonewline " "
        write-pair $d
    } elseif {$d eq "#NIL"} {
        # cdr is nil
        return
    } else {
        # it is an atom
        puts -nonewline " . "
        write-value $d
    }
}
CB

TT(

::tcltest::test write-1.0 {read, eval, and write a number} -body {
    pep "99.99"
} -output "99.99\n"

::tcltest::test write-1.1 {read, eval, and write a boolean} -body {
    pep "#t"
} -output "#t\n"

::tcltest::test write-1.2 {read, eval, and write a list} -body {
    pep "'(a b c)"
} -output "(a b c)\n"

TT)

