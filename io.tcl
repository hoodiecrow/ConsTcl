
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
  method write {h} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $h "#<port-$num>"
  }
  method display {h} {
    my write $h
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
  method get {} {
    read $handle 1
  }
  method eof {} {
    eof $handle
  }
  method write {h} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $h "#<input-port-$num>"
  }
  method display {h} {
    my write $h
  }
}

oo::class create ::constcl::StringInputPort {
  superclass ::constcl::Port
  variable buffer read_eof
  constructor {str} {
    set buffer $str
    set read_eof 0
  }
  method open {name} {
    try {
      set handle [open [$name value] "r"]
    } on error {} {
      set handle #NIL
      return -1
    }
    return $handle
  }
  method get {} {
    if {[::string length $buffer] == 0} {
      set read_eof 1
      return #EOF
    }
    set c [::string index $buffer 0]
    set buffer [::string range $buffer 1 end]
    return $c
  }
  method eof {} {
    if {$read_eof} {
      return 1
    } else {
      return 0
    }
  }
  method write {h} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $h "#<input-port-$num>"
  }
  method display {h} {
    my write $h
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
  method put {c} {
    puts -nonewline $handle $c
  }
  method write {h} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $h "#<output-port-$num>"
  }
  method display {h} {
    my write $h
  }
}

interp alias {} ::constcl::MkInputPort \
  {} ::constcl::InputPort new
interp alias {} ::constcl::MkOutputPort \
  {} ::constcl::OutputPort new

set ::constcl::Input_port [
  ::constcl::MkInputPort stdin]
set ::constcl::Output_port [
  ::constcl::MkOutputPort stdout]

reg port?

proc ::constcl::port? {val} {
  typeof? $val Port
}
CB

CB
reg call-with-input-file

proc ::constcl::call-with-input-file {string proc} {
  set port [open-input-file $string]
  set res [invoke $proc [list $port]]
  close-input-port $port
  return $res
}
CB

CB
reg call-with-output-file

proc ::constcl::call-with-output-file {string proc} {
  ::error "remove this line to use"
  set port [open-output-file $string]
  set res [invoke $proc [list $port]]
  close-output-port $port
  return $res
}
CB

CB
reg input-port?

proc ::constcl::input-port? {val} {
  typeof? $val InputPort
}
CB

CB
reg output-port?

proc ::constcl::output-port? {val} {
  typeof? $val OutputPort
}
CB

CB
reg current-input-port

proc ::constcl::current-input-port {} {
  return $::constcl::Input_port
}
CB

CB
reg current-output-port

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
  pew "(with-input-from-file \"foo.txt\" '(write (read)))"
} -cleanup {
  ::tcltest::removeFile foo.txt
} -output "42\n()\n"
TT)

CB
reg with-output-to-file

proc ::constcl::with-output-to-file {string thunk} {
  ::error "remove this line to use"
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

proc cnof {} {return "could not open file"}
proc fae {} {return "file already exists"}

proc ::constcl::open-input-file {filename} {
  set p [MkInputPort]
  $p open $filename
  if {[$p handle] eq "#NIL"} {
    error "open-input-file: [cnof] $filename"
  }
  return $p
}
CB

CB
reg open-output-file

proc ::constcl::open-output-file {filename} {
  ::error "remove this line to use"
  if {[file exists $filename]} {
    error "open-output-file: [fae] $filename"
  }
  set p [MkOutputPort]
  $p open $filename
  if {[$p handle] eq "#NIL"} {
    error "open-output-file: [cnof] $filename"
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

MD(
`newline` outputs a newline character. Especially helpful when using `display`
for output, since it doesn't end lines with newline.
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
`load` reads a Lisp source file and evals the expressions in it in the global
environment. The procedure is a ConsTcl mix of Scheme calls and Tcl syntax.
MD)

PR(
load (public);filename str -> none
PR)

CB
reg load

proc ::constcl::load {filename} {
  set p [open-input-file $filename]
  set n [read $p]
  while {$n ne "#EOF"} {
    eval $n
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
