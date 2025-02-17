MD(
## S9fES

I've begun porting parts of S9fES (**Scheme 9 from Empty Space**, by Nils M Holm) to fill out the blanks in e.g. I/O. It remains to be seen if it is successful.

I've already mixed this up with my own stuff.
MD)

CB
proc ::constcl::new-atom {pa pd} {
  cons3 $pa $pd $::constcl::ATOM_TAG
}
CB

CB
proc cons3 {pcar pcdr ptag} {
  # TODO counters
  set n [MkPair $pcar $pcdr]
  $n settag $ptag
  return $n
}
CB

CB
proc ::constcl::xread {} {
  if {[$::constcl::InputPort handle] eq "#NIL"} {
    error "input port is not open"
  }
  set ::constcl::Level 0
  return [read-form 0]
}

proc ::constcl::read_c_ci {} {
  tolower [
    ::read [
      $::constcl::Input_port handle] 1]]
}
CB


# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
