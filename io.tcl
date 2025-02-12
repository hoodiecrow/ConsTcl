
MD(
### Input and output

MD)

CB
catch { Port destroy }

oo::class create Port {
  variable handle
  constructor {args} {
    if {[llength $args]} {
      lassign $args handle
    } else {
      set handle #NIL
    }
  }
  method handle {} {set handle}
  method close {} {
    close $handle
    set handle #NIL
  }
}

oo::class create InputPort {
  superclass Port
  variable handle
  method open {name} {
    try {
      set handle [open $name "r"]
    } on error {} {
      set handle #NIL
      return -1
    }
  }
}

oo::class create OutputPort {
  superclass Port
  variable handle
  method open {name} {
    try {
      set handle [open $name "w"]
    } on error {} {
      set handle #NIL
      return -1
    }
  }
}

interp alias {} ::constcl::MkInputPort {} InputPort new
interp alias {} ::constcl::MkOutputPort {} OutputPort new

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
proc ::constcl::with-input-from-file {string thunk} {
    # TODO
}
CB


CB
proc ::constcl::with-output-to-file {string thunk} {
    # TODO
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
proc ::constcl::close-input-port {port} {
  if {[$port handle] eq "stdin"} {
    error "don't close the standard input port"
  }
  $port close
}
CB

CB
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
  write #\\newline $port
}
CB

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
