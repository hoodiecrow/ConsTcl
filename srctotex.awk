# see https://www.mgmarlow.com/words/2024-03-23-markdown-awk/

{ gsub(/\r/, ""); }

BEGIN {
}

/^H1 / {
    sub(/\n$/, "", $0)
    printf "\\part{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    next 
}
/^H2 / {
    sub(/\n$/, "", $0)
    printf "\\chapter{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    next
}

/^H3 / {
    sub(/\n$/, "", $0)
    printf "\\section{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    next
}

/^H4 / {
    sub(/\n$/, "", $0)
    printf "\\subsection{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    next
}

/^H5 / {
    sub(/\n$/, "", $0)
    printf "\\subsubsection{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    next
}

/^H6 / {
    sub(/\n$/, "", $0)
    printf "\\paragraph{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    next
}

$1 == "CB(" { in_code_block = 1 ; print "\\begin{lstlisting}" ; next }
$1 == "CB)" { in_code_block = 0 ; print "\\end{lstlisting}" ; next }
in_code_block && /./  { print ; next }
in_code_block && /^$/ { print " " ; next }


$1 == "TblSynForms" {
    print "\\begin{tabular}{|l l|}"
    print "\\hline"
    print "Syntactic form & Syntax \\\\"
    print "\\hline"
    print "Variable reference & variable \\\\"
    print "Constant literal & number or boolean, etc \\\\"
    print "Quotation & quote datum \\\\"
    print "Sequence & begin expression... \\\\"
    print "Conditional & if test conseq alt \\\\"
    print "Definition & define identifier expression \\\\"
    print "Assignment & set! variable expression \\\\"
    print "Procedure definition & lambda formals body \\\\"
    print "Procedure call & operator operand... \\\\"
    print "\\hline"
    print "\\end{tabular}"
}

/^IX/ { printf("\\index{%s}\n", $2) ; next }
/^IG/ { printf("\\includegraphics{%s}\n", substr($2, 2)) ; next }

$1 == "MD(" { next }
$1 == "MD)" { next }

$1 == "PR(" { in_pr_block = 1 ; next }
$1 == "PR)" { in_pr_block = 0 ; next }
in_pr_block {
    # render prototype table
}

$1 == "TT(" { in_tt_block = 1 ; next }
$1 == "TT)" { in_tt_block = 0 ; next }
in_tt_block { next }

/./  { for (i=1; i<=NF; i++) collect($i) }
/^$/ { flushp() }

END {  }

# Concatenate our multi-line string
function collect(v) {
  line = line sep v
  sep = " "
}

# Flush the string, rendering any inline LaTeX elements
function flushp() {
  if (line) {
    print "\n" render(line) "\n"
    line = sep = ""
  }
}

function render(line) {
    if (match(line, /\\/)) { gsub(/\\/, "\\textbackslash\\ ", line) }

    while (match(line, /B{([^}]+)}/)) {
        sub(/B{([^}]+)}/, sprintf("\\textbf{%s}", substr(line, RSTART+2, RLENGTH-3)), line)
    }
    
    while (match(line, /E{([^}]+)}/)) {
        sub(/E{([^}]+)}/, sprintf("\\emph{%s}", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /K{([^}]+)}/)) {
        sub(/K{([^}]+)}/, sprintf("\\texttt{%s}", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /I{([^}]+)}/)) {
        sub(/I{([^}]+)}/, sprintf("\\index{%s}", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /R{([^}]+)}/)) {
        sub(/R{([^}]+)}/, sprintf("\\footnote{See page \\pageref{%s}}", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    if (match(line, /\$/)) { gsub(/\$/, "\\$", line) }

    if (match(line, /#/)) { gsub(/#/, "\\#", line) }

    if (match(line, /&/)) { gsub(/&/, "\\&", line) }

    if (match(line, /_/)) { gsub(/_/, "\\_", line) }

    if (match(line, /%/)) { gsub(/%/, "\\%", line) }

    if (match(line, /⇒/)) { gsub(/⇒/, "=>", line) }

    return line
}

function makelabel (str) {
    gsub(/ /, "-", str)
    str = tolower(str)
    while (str c in labels) {
	c++
	str = str c
    }
    labels[str] = 1
    c = ""
    return str
}

# Vim: tabstop=4 shiftwidth=4 softtabstop=4 expandtab:

