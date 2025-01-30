
MD(
## write

The third member in the great triad is `write`. As long as the object
given to it isn't `#NONE`, it passes it to `write-value` and prints
a newline.
MD)

CB
reg write ::constcl::write

proc ::constcl::write {val args} {
    if {$val ne "#NONE"} {
        ::constcl::write-value $val
        puts {}
    }
    return #NONE
}
CB

MD(
`write-value` simply calls an object's `write` method, letting the object
write itself.
MD)

CB
proc ::constcl::write-value {val} {
    $val write
}
CB

MD(
The `display` procedure is like `write` but doesn't print a newline.
MD)

CB
reg display ::constcl::display

proc ::constcl::display {val args} {
    ::constcl::write-value $val
    flush stdout
    return #NONE
}
CB

MD(
The `write-pair` procedure prints a Pair object.
MD)

CB
proc ::constcl::write-pair {val} {
    # take an object and print the car and the cdr of the stored value
    set a [car $val]
    set d [cdr $val]
    # print car
    write-value $a
    if {[pair? $d] eq "#t"} {
        # cdr is a cons pair
        puts -nonewline " "
        write-pair $d
    } elseif {[null? $d] eq "#t"} {
        # cdr is nil
        return
    } else {
        # it is an atom
        puts -nonewline " . "
        write-value $d
    }
    return #NONE
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

