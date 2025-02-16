
MD(
## Output

__write__

The third member in the great triad is `write`. As long as the object
given to it isn't the empty string, it passes it to `write-value` and prints
a newline.
MD)

PR(
write (public);val val ?port? port -> none
PR)

CB
reg write ::constcl::write

proc ::constcl::write {val args} {
  if {$val ne ""} {
    if {[llength $args]} {
      lassign $args port
    } else {
      set port [MkOutputPort stdout]
    }
    set ::constcl::Output_port $port
    write-value [$::constcl::Output_port handle] $val
    puts [$::constcl::Output_port handle] {}
    set ::constcl::Output_port [MkOutputPort stdout]
  }
  return
}
CB

MD(
__write-value__

`write-value` simply calls an object's `write` method, letting the object
write itself.
MD)

PR(
write-value (internal);handle handle val val -> none
PR)

CB
proc ::constcl::write-value {handle val} {
  $val write $handle
  return
}
CB

MD(
__display__

The `display` procedure is like `write` but doesn't print a newline.
MD)

PR(
display (public);val val ?port? port -> none
PR)

CB
reg display ::constcl::display

proc ::constcl::display {val args} {
  if {$val ne ""} {
    if {[llength $args]} {
      lassign $args port
    } else {
      set port [MkOutputPort stdout]
    }
    set ::constcl::Output_port $port
    $val display [$::constcl::Output_port handle]
    flush [$::constcl::Output_port handle]
    set ::constcl::Output_port [MkOutputPort stdout]
  }
  return
}
CB

MD(
__write-pair__

The `write-pair` procedure prints a Pair object.
MD)

PR(
write-pair (internal);handle handle pair pair -> none
PR)

CB
proc ::constcl::write-pair {handle pair} {
  # take an object and print the car
  # and the cdr of the stored value
  set a [car $pair]
  set d [cdr $pair]
  # print car
  write-value $handle $a
  if {[pair? $d] ne "#f"} {
    # cdr is a cons pair
    puts -nonewline $handle " "
    write-pair $handle $d
  } elseif {[null? $d] ne "#f"} {
    # cdr is nil
    return
  } else {
    # it is an atom
    puts -nonewline $handle " . "
    write-value $handle $d
  }
  return
}
CB

TT(

::tcltest::test write-1.0 {read, eval, and write a number} -body {
    pew "99.99"
} -output "99.99\n"

::tcltest::test write-1.1 {read, eval, and write a boolean} -body {
    pew "#t"
} -output "#t\n"

::tcltest::test write-1.2 {read, eval, and write a list} -body {
    pew "'(a b c)"
} -output "(a b c)\n"

TT)

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
