# see https://www.mgmarlow.com/words/2024-03-23-markdown-awk/

BEGIN { hyperref = 0}

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
		print "\\noindent\\begin{tabular}{ |p{1.9cm} p{8cm}| }\n\\hline";
		printf "\\rowcolor[HTML]{CCCCCC} \\multicolumn{2}{|l|}{\\bf %s} \\\\\n", substr($0, 1, RSTART-1);
		$0 = substr($0, RSTART+RLENGTH);
		for (i = 1; i <= NF; i += 2) {
			td1 = ($i == "->" ? "\\textit{Returns:}" : $i);
			td2 = dict[$(i+1)];
			printf "%s & %s \\\\\n", latexify(td1), latexify(td2);
		}
		print "\\hline\n\\end{tabular}";
	}
	next;
}

function latexify (s) {
    if (match(s, /\$/)) { gsub(/\$/, "\\$", s) }
    if (match(s, /#/)) { gsub(/#/, "\\#", s) }
    if (match(s, /&/)) { gsub(/&/, "\\&", s) }
    if (match(s, /_/)) { gsub(/_/, "\\_", s) }
    if (match(s, /%/)) { gsub(/%/, "\\%", s) }
    return s
}

# skip any modeline
$0 ~ modeline { next; }

/^[Hh]1 / {
    sub(/\n$/, "", $0)
    printf "\\part{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    next 
}

/^[Hh]2 / {
    sub(/\n$/, "", $0)
    printf "\\chapter{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    next
}

/^H3 / {
    sub(/\n$/, "", $0)
    printf "\\section{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    printf "\\index{%s}\n", substr($0, 4)
    next
}

/^h3 / {
    sub(/\n$/, "", $0)
    printf "\\section{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    printf "\\index{%s}\n", tolower(substr($0, 4))
    next
}

/^H4 / {
    sub(/\n$/, "", $0)
    heading = substr($0, 4)
    gsub(/"!/, "!", heading)
    gsub(/"@/, "@", heading)
    gsub(/"\|/, "|", heading)
    printf "\\subsection{%s}\n", heading
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    printf "\\index{%s}\n", substr($0, 4)
    next
}

/^h4 / {
    sub(/\n$/, "", $0)
    heading = substr($0, 4)
    gsub(/"!/, "!", heading)
    gsub(/"@/, "@", heading)
    gsub(/"\|/, "|", heading)
    printf "\\subsection{%s}\n", heading
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    printf "\\index{%s}\n", tolower(substr($0, 4))
    next
}

/^H5 / {
    sub(/\n$/, "", $0)
    printf "\\subsubsection{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    printf "\\index{%s}\n", substr($0, 4)
    next
}

/^h5 / {
    sub(/\n$/, "", $0)
    printf "\\subsubsection{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    printf "\\index{%s}\n", tolower(substr($0, 4))
    next
}

/^[Hh]6 / {
    sub(/\n$/, "", $0)
    printf "\\paragraph{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    next
}

$1 == "H9" { print "\\section{Unfinished code in file " FILENAME " line " (FNR+2) "}" ; next }

$1 == "CB("      { in_code_block = 1 ; print "\\begin{lstlisting}" ; next }
$1 == "CB)"      { in_code_block = 0 ; print "\\end{lstlisting}" ; next }
in_code_block   { print ; next }


$1 == "IX" { printf("\\index{%s}\n", substr($0, 4)) ; next }
$1 == "IG" { printf("\\includegraphics{%s}\n", substr($2, 2)) ; next }
$1 == "IF" {
    for (i=3; i<=NF; i++) { caption = caption " " $i }
    printf("\\begin{figure}[h!]\\includegraphics{%s}\\captionsetup{labelformat=empty}\\caption{%s}\\label{fig:%s}\\end{figure}\n", substr($2, 2), caption, makelabel(caption))
    caption = ""
    next
}
$1 == "EM" {
    for (i=2; i<=NF; i++) collect($i)
    line = render(line)
    line = sprintf("\\emph{%s}", line)
    print "\n" line "\n"
    line = sep = ""
    next
}
$1 == "KB" {
    for (i=2; i<=NF; i++) collect($i)
    line = render(line)
    line = sprintf("\\texttt{%s}", line)
    print "\n" line "\n"
    line = sep = ""
    next
}
$1 == "NI" {
    for (i=2; i<=NF; i++) collect($i)
    line = render(line)
    line = sprintf("\\noindent %s", line)
    print "\n" line "\n"
    line = sep = ""
    next
}
$1 == "BS" { print "\\bigskip" }

$1 == "IT" { if (!init) print "\\begin{itemize}";init = 1; print "\\item " render(substr($0, 3));next }
init && $1 != "IT" { print "\\end{itemize}"; init = 0; next }

$1 == "EN" { if (!inen) print "\\begin{enumerate}";inen = 1; print "\\item " render(substr($0, 3));next }
inen && $1 != "EN" { print "\\end{enumerate}"; inen = 0; next }

$1 == "MD(" { in_md_block = 1 ; print "" ; next }
$1 == "MD)" { in_md_block = 0 ; flushp() ; next }

in_md_block && /./  { for (i=1; i<=NF; i++) collect($i) }
in_md_block && /^$/ { flushp() }

$1 == "PT(" { print "\n\\begin{pulledtext}" ; next }
$1 == "PT)" { print "\\end{pulledtext}\n" ; next }

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
    if (match(line, /\\{/)) { gsub(/\\{/, "LBRACE", line) }
    if (match(line, /\\}/)) { gsub(/\\}/, "RBRACE", line) }
    if (match(line, /\\/)) { gsub(/\\/, "\\textbackslash ", line) }
    if (match(line, /LBRACE/)) { gsub(/LBRACE/, "\\{", line) }
    if (match(line, /RBRACE/)) { gsub(/RBRACE/, "\\}", line) }

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

    while (match(line, /F{([^{}]+)}/)) {
        sub(/F{([^{}]+)}/, sprintf("\\footnote{%s}", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /M{([^{}]+)}/)) {
        sub(/M{([^{}]+)}/, sprintf("\\marginpar[%s]{%s\\raggedright}", substr(line, RSTART+2, RLENGTH-3), substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /D{([^{}]*)}/)) {
        sub(/D{([^{}]*)}/, sprintf("\\ldots "), line)
    }

    if (match(line, /\.\.\./)) {
        gsub(/\.\.\./, sprintf("\\ldots "), line)
    }

    while (match(line, /\(La\)TeX/)) {
        sub(/\(La\)TeX/, sprintf("\\LaTeX{}"), line)
    }

    while (match(line, /R{([^{}]+)}{([^{}]+)}/)) {
	patsplit(substr(line, RSTART+2, RLENGTH-3), ref, /[^{}]+/)
        sub(/R{([^{}]+)}{([^{}]+)}/, sprintf("%s (see page \\pageref{%s})", ref[1], ref[2]), line)
    }

    while (match(line, /S{([^{}]+)}{([^{}]+)}/)) {
	patsplit(substr(line, RSTART+2, RLENGTH-3), ref, /[^{}]+/)
        sub(/S{([^{}]+)}{([^{}]+)}/, sprintf("\\texttt{%s} (see page \\pageref{%s})", ref[1], ref[2]), line)
    }

    while (match(line, /L{([^{}]+)}{([^{}]+)}/)) {
	patsplit(substr(line, RSTART+2, RLENGTH-3), link, /[^{}]+/)
        sub(/L{([^{}]+)}{([^{}]+)}/, sprintf("%s\\footnote{See \\%s{%s}}", link[1], hyperref?"url":"texttt", link[2]), line)
    }

    while (match(line, /W{([^{}]+)}{([^{}]+)}/)) {
	patsplit(substr(line, RSTART+2, RLENGTH-3), wiki, /[^{}]+/)
        sub(/W{([^{}]+)}{([^{}]+)}/, sprintf("%s\\footnote{See \\%s{https://en.wikipedia.org/wiki/%s}}", wiki[1], hyperref?"url":"texttt", wiki[2]), line)
    }

    if (match(line, /\$/)) { gsub(/\$/, "\\$", line) }

    while (match(line, /P{([^{}]+)}{([^{}]+)}/)) {
	patsplit(substr(line, RSTART+2, RLENGTH-3), pow, /[^{}]+/)
        sub(/P{([^{}]+)}{([^{}]+)}/, sprintf("${%s}^{%s}$", pow[1], pow[2]), line)
    }

    if (match(line, /===>/)) { gsub(/===>/, "$\\Longrightarrow$", line) }

    if (match(line, /==>/)) { gsub(/==>/, "$\\Rightarrow$", line) }

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
    gsub(/[:.]/, "", str)
    while (str c in labels) {
	c++
	str = str c
    }
    labels[str] = 1
    c = ""
    return str
}

# Vim: tabstop=4 shiftwidth=4 softtabstop=4 expandtab:

