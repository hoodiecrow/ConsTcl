
MD(
### Resolving local defines

This section is ported from 'Scheme 9 from Empty Space'. `resolve-local-defines`
is the topmost procedure in rewriting local defines as essentially a `letrec`
form. It takes a list of expressions and extracts variables and values from the
defines in the beginning of the list. It builds a double lambda expression with
the variables and values, and the rest of the expressions from the original list
as body.

__resolve-local-defines__
MD)

PR(
resolve-local-defines;exps lexprs -> expr
PR)

CB
proc ::constcl::resolve-local-defines {exps} {
  set rest [lassign [
    extract-from-defines $exps VALS] a error]
  if {$error ne "#f"} {
    return #NIL
  }
  set rest [lassign [
    extract-from-defines $exps VARS] v error]
  if {$error ne "#f"} {
    return #NIL
  }
  if {$rest eq "#NIL"} {
    set rest [cons #UNSP #NIL]
  }
  return [make-lambdas $v $a $rest]
}
CB

MD(
__extract-from-defines__

`extract-from-defines` visits every define in the given list of expressions and
extracts either a variable name or a value, depending on the state of the _part_
flag, from each one of them. A Tcl list of 1) the resulting list of names or
values, 2) error state, and 3) the rest of the expressions in the original list
is returned.
MD)

PR(
extract-from-defines (internal);exps lexprs part varsvals -> tvals
PR)

CB
proc ::constcl::extract-from-defines {exps part} {
  set a #NIL
  while {$exps ne "#NIL"} {
    if {[atom? $exps] ne "#f" ||
        [atom? [car $exps]] ne "#f" ||
        [eq? [caar $exps] [S define]] eq "#f"} {
      break
    }
    set n [car $exps]
    set k [length $n]
    if {[list? $n] eq "#f" ||
        [$k numval] < 3 ||
        [$k numval] > 3 ||
        ([argument-list? [cadr $n]] ne "#f" ||
        [symbol? [cadr $n]] eq "#f")
      eq "#f"} {
        return [::list #NIL "#t" #NIL]
      }
      if {[pair? [cadr $n]] ne "#f"} {
        if {$part eq "VARS"} {
          set a [cons [caadr $n] $a]
        } else {
          set a [cons #NIL $a]
          set new [cons [cdadr $n] [cddr $n]]
          set new [cons [S lambda] $new]
          set-car! $a $new
        }
      } else {
        if {$part eq "VARS"} {
          set a [cons [cadr $n] $a]
        } else {
          set a [cons [caddr $n] $a]
        }
      }
      set exps [cdr $exps]
    }
    return [::list $a #f $exps]
}
CB

MD(
__argument-list?__

`argument-list?` accepts a Scheme formals list and rejects other values.
MD)

PR(
argument-list? (internal);val val -> bool
PR)

CB
proc ::constcl::argument-list? {val} {
  if {$val eq "#NIL"} {
    return #t
  } elseif {[symbol? $val] ne "#f"} {
    return #t
  } elseif {[atom? $val] ne "#f"} {
    return #f
  }
  while {[pair? $val] ne "#f"} {
    if {[symbol? [car $val]] eq "#f"} {
      return #f
    }
    set val [cdr $val]
  }
  if {$val eq "#NIL"} {
    return #t
  } elseif {[symbol? $val] ne "#f"} {
    return #t
  }
}
CB

MD(
__make-lambdas__

`make-lambdas` builds the `letrec` structure.
MD)

PR(
make-lambdas (internal);vars lsyms args lexprs body lexprs -> expr
PR)

CB
proc ::constcl::make-lambdas {vars args body} {
  set tmps [make-temporaries $vars]
  set body [append-b [
    make-assignments $vars $tmps] $body]
  set body [cons $body #NIL]
  set n [cons $tmps $body]
  set n [cons [S lambda] $n]
  set n [cons $n $args]
  set n [cons $n #NIL]
  set n [cons $vars $n]
  set n [cons [S lambda] $n]
  set n [cons $n [make-undefineds $vars]]
  return $n
}
CB

MD(
__make-temporaries__

`make-temporaries` creates the symbols that will act as middlemen in
transferring the values to the variables.
MD)

PR(
make-temporaries (internal);vals lvals -> lvals
PR)

CB
proc ::constcl::make-temporaries {vals} {
  set res #NIL
  while {$vals ne "#NIL"} {
    set res [cons [gensym "g"] $res]
    set vals [cdr $vals]
  }
  return $res
}
CB

MD(
__gensym__

`gensym` generates an unique symbol.
MD)

PR(
gensym (internal);prefix str -> sym
PR)

CB
proc ::constcl::gensym {prefix} {
  set symbolnames [
    dict keys $::constcl::symbolTable]
  set s $prefix<[incr ::constcl::gensymnum]>
  while {$s in $symbolnames} {
    set s $prefix[incr ::constcl::gensymnum]
  }
  return [MkSymbol $s]
}
CB

MD(
__append-b__

`append-b` joins two lists together.
MD)

PR(
append-b (internal);a lvals b lvals -> lvals
PR)

CB
proc ::constcl::append-b {a b} {
  if {$a eq "#NIL"} {
    return $b
  }
  set p $a
  while {$p ne "#NIL"} {
    if {[atom? $p] ne "#f"} {
      ::error "append: improper list"
    }
    set last $p
    set p [cdr $p]
  }
  set-cdr! $last $b
  return $a
}
CB

MD(
__make-assignments__

`make-assignments` creates the structure that holds the assignment statements.
Later on, it will be joined to the body of the finished expression.
MD)

PR(
make-assignments (internal);vars lsyms tmps lsyms -> expr
PR)

CB
proc ::constcl::make-assignments {vars tmps} {
  set res #NIL
  while {$vars ne "#NIL"} {
    set asg [cons [car $tmps] #NIL]
    set asg [cons [car $vars] $asg]
    set asg [cons [S set!] $asg]
    set res [cons $asg $res]
    set vars [cdr $vars]
    set tmps [cdr $tmps]
  }
  return [cons [S begin] $res]
}
CB

MD(
__make-undefineds__

Due to a mysterious bug, `make-undefineds` actually creates a list of NIL
values instead of undefined values.
MD)

PR(
make-undefineds (internal);vals lvals -> lnils
PR)

CB
proc ::constcl::make-undefineds {vals} {
  # TODO find bug, substitute #UNDF
  set res #NIL
  while {$vals ne "#NIL"} {
    set res [cons #NIL $res]
    set vals [cdr $vals]
  }
  return $res
}
CB

TT(

::tcltest::test rld-1.0 {expand local defines} -body {
    set x [p "((define n 0) (define a 3) (define b 4) (set! n (+ a b)) (* n n))"]
    w [::constcl::resolve-local-defines $x]
} -output "((lambda (b a n) ((lambda (g<3> g<2> g<1>) (begin (set! n g<1>) (set! a g<2>) (set! b g<3>) (set! n (+ a b)) (* n n))) 4 3 0)) () () ())\n"

::tcltest::test rld-1.1 {run local defines} -body {
    set x [p "((define n 0) (define a 3) (define b 4) (set! n (+ a b)) (* n n))"]
    w [e [::constcl::resolve-local-defines $x]]
} -output "49\n"

::tcltest::test rld-1.2 {expand local defines w/o defines} -body {
    set x [p "((set! n (+ a b)) (* n n))"]
    w [::constcl::resolve-local-defines $x]
} -output "((lambda () ((lambda () (begin (set! n (+ a b)) (* n n))))))\n"

::tcltest::test rld-1.3 {expand local defines with begin} -body {
    set x [p "(begin (set! n (+ a b)) (* n n))"]
    w [::constcl::resolve-local-defines $x]
} -output "((lambda () ((lambda () (begin begin (set! n (+ a b)) (* n n))))))\n"

::tcltest::test rld-1.4 {expand local defines with proc definition} -body {
    set x [p "((define (foo x) (* x x)) (set! n (+ a b)) (* n n))"]
    w [::constcl::resolve-local-defines $x]
} -output "((lambda (foo) ((lambda (g<7>) (begin (set! foo g<7>) (set! n (+ a b)) (* n n))) (lambda (x) (* x x)))) ())\n"

TT)

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
