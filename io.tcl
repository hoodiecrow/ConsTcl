
MD(
### Input and output

MD)

CB
catch { ::constcl::Port destroy }

oo::class create ::constcl::Port {
  variable handle
  constructor {args} {
    if {[llength $args]} {
      lassign $args handle
    } else {
      set handle #NIL
    }
  }
  method handle {} {
    set handle
  }
  method close {} {
    close $handle
    set handle #NIL
  }
  method write {handle} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $handle "#<port-$num>"
  }
  method display {handle} {
    my write $handle
  }
}

oo::class create ::constcl::InputPort {
  superclass ::constcl::Port
  variable handle
  method open {name} {
    try {
      set handle [open [$name value] "r"]
    } on error {} {
      set handle #NIL
      return -1
    }
    return $handle
  }
  method write {handle} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $handle "#<input-port-$num>"
  }
  method display {handle} {
    my write $handle
  }
}

oo::class create ::constcl::OutputPort {
  superclass ::constcl::Port
  variable handle
  method open {name} {
    try {
      set handle [open [$name value] "w"]
    } on error {} {
      set handle #NIL
      return -1
    }
    return $handle
  }
  method write {handle} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $handle "#<output-port-$num>"
  }
  method display {handle} {
    my write $handle
  }
}

interp alias {} ::constcl::MkInputPort {} ::constcl::InputPort new
interp alias {} ::constcl::MkOutputPort {} ::constcl::OutputPort new

set ::constcl::Input_port [::constcl::MkInputPort stdin]
set ::constcl::Output_port [::constcl::MkOutputPort stdout]

proc ::constcl::port? {val} {
  if {[info object isa typeof $val ::constcl::Port]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Port]} {
    return #t
  } else {
    return #f
  }
}
CB

CB
proc ::constcl::call-with-input-file {string proc} {
    # TODO
}
CB

CB
proc ::constcl::call-with-output-file {string proc} {
    # TODO
}
CB

CB
proc ::constcl::input-port? {obj} {
  if {[info object isa typeof $val ::constcl::InputPort]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::InputPort]} {
    return #t
  } else {
    return #f
  }
}
CB

CB
proc ::constcl::output-port? {obj} {
  if {[info object isa typeof $val ::constcl::OutputPort]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::OutputPort]} {
    return #t
  } else {
    return #f
  }
}
CB

CB
proc ::constcl::current-input-port {} {
  return $::constcl::Input_port
}
CB

CB
proc ::constcl::current-output-port {} {
  return $::constcl::Output_port
}
CB

CB
reg with-input-from-file

proc ::constcl::with-input-from-file {string thunk} {
  set newport [open-input-file $string]
  if {[$newport handle] ne "#NIL"} {
    set oldport $::constcl::Input_port
    set ::constcl::Input_port $newport
    eval $thunk
    set ::constcl::Input_port $oldport
    close-input-port $newport
  }
}
CB

TT(
::tcltest::test io-1.0 {try with-input-from-file} -setup {
  ::tcltest::makeFile {42} foo.txt
} -body {
  pep "(with-input-from-file \"foo.txt\" '(write (read)))"
} -cleanup {
  ::tcltest::removeFile foo.txt
} -output "42\n()\n"
TT)

CB
reg with-output-to-file

proc ::constcl::with-output-to-file {string thunk} {
  set newport [open-output-file $string]
  if {[$newport handle] ne "#NIL"} {
    set oldport $::constcl::Output_port
    set ::constcl::Output_port $newport
    eval $thunk
    set ::constcl::Output_port $oldport
    close-input-port $newport
  }
}
CB

CB
reg open-input-file

proc ::constcl::open-input-file {filename} {
  set p [MkInputPort]
  $p open $filename
  if {[$p handle] eq "#NIL"} {
    error "open-input-file: could not open file $filename"
  }
  return $p
}
CB

CB
reg open-output-file

proc ::constcl::open-output-file {filename} {
  if {[file exists $filename]} {
    error "open-output-file: file already exists $filename"
  }
  set p [MkOutputPort]
  $p open $filename
  if {[$p handle] eq "#NIL"} {
    error "open-output-file: could not open file $filename"
  }
  return $p
}
CB

CB
reg close-input-port

proc ::constcl::close-input-port {port} {
  if {[$port handle] eq "stdin"} {
    error "don't close the standard input port"
  }
  $port close
}
CB

CB
reg close-output-port

proc ::constcl::close-output-port {port} {
  if {[$port handle] eq "stdout"} {
    error "don't close the standard output port"
  }
  $port close
}
CB

MD(
`read` is implemented in the read[#](https://github.com/hoodiecrow/ConsTcl#read) section.
MD)

CB
proc ::constcl::--read {args} {
  if {[llength $args]} {
    set new_port [lindex $args 0]
  } else {
    set new_port $::constcl::Input_port
  }
  set old_port $::constcl::Input_port
  set ::constcl::Input_port $new_port
  set n [xread]
  set ::constcl::Input_port $old_port
  return $n
}
CB

CB
proc ::constcl::read-char {args} {
    # TODO
}
CB

CB
proc ::constcl::peek-char {args} {
    # TODO
}
CB

CB
proc ::constcl::char-ready? {args} {
    # TODO
}
CB

MD(
`write` is implemented in the write[#](https://github.com/hoodiecrow/ConsTcl#write) section.
MD)

MD(
`display` is implemented in the write section.
MD)

CB
reg newline

proc ::constcl::newline {args} {
  if {[llength $args]} {
    lassign $args port
  } else {
    set port [current-output-port]
  }
  pe "(display #\\newline $port)"
#  display [p "#\\A"] $port
}
CB

TT(
::tcltest::test io-2.0 {try newline} -body {
  pe "(display \"foo\")"
  pe "(newline)"
  pe "(display \"bar\")"
} -output "foo\nbar"
TT)

CB
proc ::constcl::write-char {args} {
    # TODO
}
CB

MD(
`--load` is a raw port of the S9fES implementation. `----load` is my original
straight-Tcl version. `load` is my ConsTcl mix of Scheme calls and Tcl syntax.
MD)

CB
proc ::constcl::--load {filename} {
  set new_port [MkInputPort]
  $new_port open $filename
  if {[$new_port handle] eq "#NIL"} {
    return -1
  }
  set ::constcl::File_list [cons [MkString $filename] $::constcl::File_list]
  set save_env $env
  set env ::constcl::global_env
  set outer_loading [$::constcl::S_loading cdr]
  set-cdr! ::constcl::S_loading #t
  set old_port $::constcl::Input_port
  set outer_lno $::constcl::Line_no
  set ::constcl::Line_no 1
  while true {
    set ::constcl::Input_port $new_port
    set n [xread]
    set ::constcl::Input_port $old_port
    if {$n == $::constcl::END_OF_FILE} {
      break
    }
    set n [eval $n $env]
  }
  $new_port close
  set $::constcl::Line_no $outer_lno
  set-cdr! ::constcl::S_loading $outer_loading
  set ::constcl::File_list [cdr $::constcl::File_list]
  set env $save_env
  return 0
}

proc ::constcl::----load {filename} {
  set f [open $filename]
  set src [::read $f]
  close $f
  set ib [::constcl::IB new $src]
  while {[$ib first] ne {}} {
    eval [parse $ib]
  }
}

reg load

proc ::constcl::load {filename} {
  set p [open-input-file $filename]
  set n [read $p]
  while {$n ne "#EOF"} {
    eval $n ::constcl::global_env
    set n [read $p]
  }
  close-input-port $p
}
CB

CB
proc ::constcl::transcript-on {filename} {
    # TODO
}
CB

CB
proc ::constcl::transcript-off {} {
    # TODO
}
CB

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
