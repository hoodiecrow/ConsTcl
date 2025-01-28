
MD(
### Input and output

I may never get around to implementing these.
MD)

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
    # TODO
}
CB

CB
proc ::constcl::output-port? {obj} {
    # TODO
}
CB

CB
proc ::constcl::current-input-port {} {
    # TODO
}
CB

CB
proc ::constcl::current-output-port {} {
    # TODO
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
proc ::constcl::open-input-file {filename} {
    # TODO
}
CB

CB
proc ::constcl::open-output-file {filename} {
    # TODO
}
CB

CB
proc ::constcl::close-input-port {port} {
    # TODO
}
CB

CB
proc ::constcl::close-output-port {port} {
    # TODO
}
CB

MD(
`read` implemented in [read](https://github.com/hoodiecrow/ConsTcl#read) section.
MD)

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

CB
if no {
proc ::constcl::write {obj args} {
    # TODO write [$obj write]
}
}
CB

MD(
`display` implemented in [write](https://github.com/hoodiecrow/ConsTcl#write) section.
MD)

CB
reg newline ::constcl::newline

proc ::constcl::newline {args} {
    # TODO write newline
}
CB

CB
proc ::constcl::write-char {args} {
    # TODO
}
CB

CB
proc ::constcl::load {filename} {
    # TODO
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

