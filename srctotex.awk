# see https://www.mgmarlow.com/words/2024-03-23-markdown-awk/

{ gsub(/\r/, ""); }

BEGIN { modeline = "[#;] v" "im:" }

# load the key/values pairs from dict.txt
# NOTE: NR is equal to FNR only while processing the first file
NR == FNR {
	if (match($0, /[[:space:]]*->[[:space:]]*/))
		dict[substr($0, 1, RSTART-1)] = substr($0, RSTART+RLENGTH);
	next;
}

# expand the PR blocks as HTML tables in the remainder file(s)
$1 == "PR(" { inside_pr_block = 1; next; }
$1 == "PR)" { inside_pr_block = 0; next; }
inside_pr_block {
	if (match($0, /;/)) {
		print "\\begin{tabular}{ |l l| }\n\\hline";
		printf "\\multicolumn{2}{|l|}{%s} \\\\\n", substr($0, 1, RSTART-1);
		$0 = substr($0, RSTART+RLENGTH);
		for (i = 1; i <= NF; i += 2) {
			td1 = ($i == "->" ? "\\textit{Returns:}" : $i);
			td2 = dict[$(i+1)];
			printf "%s & %s \\\\\n", td1, td2;
		}
		print "\\hline\n\\end{tabular}";
	}
	next;
}

# skip any modeline
$0 ~ modeline { next; }

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
    printf "\\index{%s}\n", tolower(substr($0, 4))
    next
}

/^H4 / {
    sub(/\n$/, "", $0)
    printf "\\subsection{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    printf "\\index{%s}\n", tolower(substr($0, 4))
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
in_code_block && /^$/ { print " " ; next }


/^IX/ { printf("\\index{%s}\n", $2) ; next }
/^IG/ { printf("\\includegraphics{%s}\n", substr($2, 2)) ; next }

$1 == "MD(" { in_md_block = 1 ; print "" ; next }
$1 == "MD)" { in_md_block = 0 ; flushp() ; next }

in_md_block && /./  { for (i=1; i<=NF; i++) collect($i) }
in_md_block && /^$/ { flushp() }

$1 == "VB(" { in_vb_block = 1 ; print "\\begin{verbatim}" ; next }
$1 == "VB)" { in_vb_block = 0 ; print "\\end{verbatim}"   ; next }
in_vb_block { print }

$1 == "TT(" { in_tt_block = 1 ; next }
$1 == "TT)" { in_tt_block = 0 ; next }
in_tt_block { next }

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

    while (match(line, /B{([^{}]+)}/)) {
        sub(/B{([^{}]+)}/, sprintf("\\textbf{%s}", substr(line, RSTART+2, RLENGTH-3)), line)
    }
    
    while (match(line, /E{([^{}]+)}/)) {
        sub(/E{([^{}]+)}/, sprintf("\\emph{%s}", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /K{([^{}]+)}/)) {
        sub(/K{([^{}]+)}/, sprintf("\\texttt{%s}", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /I{([^{}]+)}/)) {
        sub(/I{([^{}]+)}/, sprintf("\\index{%s}", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /R{([^{}]+)}/)) {
        sub(/R{([^{}]+)}/, sprintf("\\footnote{See page \\pageref{%s}}", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /L{([^{}]+)}/)) {
        sub(/L{([^{}]+)}/, sprintf("\\footnote{See \\texttt{%s}}", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    if (match(line, /\$/)) { gsub(/\$/, "\\$", line) }

    while (match(line, /P{([^{}]+)}{([^{}]+)}/)) {
	patsplit(substr(line, RSTART+2, RLENGTH-3), pow, /[^{}]+/)
        sub(/P{([^{}]+)}{([^{}]+)}/, sprintf("${%s}^{%s}$", pow[1], pow[2]), line)
    }

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

