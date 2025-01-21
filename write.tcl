
CB
proc ::constcl::write {obj args} {
    ::constcl::write-value $obj
    puts {}
}
CB

CB
proc ::constcl::write-value {obj} {
    # take an object and print the value
catch {    $obj write}
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

::tcltest::test write-1.0 {read and write a number} -body {
    namespace eval ::constcl {
        set ::inputstr "99.99"
        write [eval [read]]
    }
} -output "99.99\n"

::tcltest::test write-1.1 {read and write a boolean} -body {
    namespace eval ::constcl {
        set ::inputstr "#t"
        write [eval [read]]
    }
} -output "#t\n"

TT)

TT(

::tcltest::test write-2.0 {read and write a list} -body {
    set ::inputstr "(a b c)"
    set obj [::constcl::read]
    ::constcl::write $obj
} -output "(a b c)\n"

::tcltest::test write-2.1 {read and write a list} -body {
    namespace eval ::constcl {
        set ::inputstr "'(a b c)"
        write [eval [read]]
    }
} -output "(a b c)\n"

TT)

