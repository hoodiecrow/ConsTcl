
MD(
On startup, two `Environment` objects called `null_env` (the null environment,
not the same as `null-environment` in Scheme) and `global_env` (the global
environment) are created. 

Make `null_env` empty and unresponsive: this is where searches for unbound
symbols end up.
MD)

CB
::constcl::Environment create ::constcl::null_env #NIL {}

oo::objdefine ::constcl::null_env {
  method find {sym} {self}
  method get {sym} {::error "Unbound variable: [$sym name]"}
  method set {sym val} {::error "Unbound variable: [$sym name]"}
}
CB

MD(
Meanwhile, `global_env` is populated with all the definitions from the
definitions register, `defreg`. This is where top level evaluation happens.
MD)

CB
namespace eval ::constcl {
  set keys [list {*}[lmap k [dict keys $defreg] {MkSymbol $k}]]
  set vals [dict values $defreg]
  Environment create global_env $keys $vals ::constcl::null_env
}
CB

MD(
Load the Scheme base to add more definitions to the global environment.
MD)

CB
pe {(load "schemebase.lsp")}
CB

MD(
Thereafter, each time a user-defined procedure is called, a new `Environment`
object is created to hold the bindings introduced by the call, and also a link
to the outer environment (the one closed over when the procedure was defined).
MD)

MD(
#### Lexical scoping


Example:

```
ConsTcl> (define (circle-area r) (* pi (* r r)))
ConsTcl> (circle-area 10)
314.1592653589793
```

During a call to the procedure `circle-area`, the symbol `r` is bound to the
value 10. But we don't want the binding to go into the global environment,
possibly clobbering an earlier definition of `r`. The solution is to use
separate (but linked) environments, making `r`'s binding a
*local variable[#](https://en.wikipedia.org/wiki/Local_variable)*
in its own environment, which the procedure will be evaluated in. The symbols
`*` and `pi` will still be available through the local environment's link
to the outer global environment. This is all part of
**lexical scoping[#](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scope)**.

In the first image, we see the global environment before we call `circle-area`
(and also the empty null environment which the global environment links to):

![A global environment](/images/env1.png)

During the call. Note how the global `r` is shadowed by the local one, and how
the local environment links to the global one to find `*` and `pi`. 

![A local environment shadows the global](/images/env2.png)

After the call, we are back to the first state again.

![A global environment](/images/env1.png)

MD)

TT(
::tcltest::test global_env-1.0 {check for a symbol} -body {
    pep "pi"
} -output 3.1415926535897931\n
TT)

TT(
::tcltest::test global_env-2.0 {dereference an unbound symbol} -body {
    pep "foo"
} -returnCodes error -result "Unbound variable: foo"

::tcltest::test global_env-2.1 {dereference an unbound symbol: procedure} -body {
    pep "(foo)"
} -returnCodes error -result "Unbound variable: foo"
TT)

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
