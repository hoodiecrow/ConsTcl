# see https://www.mgmarlow.com/words/2024-03-23-markdown-awk/

BEGIN {
	print "\\documentclass{report}"
	print "\\usepackage{graphicx}"
	print "\\usepackage{listings}"
	print "\\title{ConsTcl}"
	print "\\author{Peter Lewerin}"
	print "\\date{\\today}"
	print "\\begin{document}"
	print "\\maketitle"
	print "\\tableofcontents"
	in_verbatim = 0

}



/^# /    { next }
/^## /   { printf "\\section{%s}\n\n", substr($0, 4); next }
/^### /  { printf "\\subsection{%s}\n\n", substr($0, 5); next }
/^#### / { printf "\\subsubsection{%s}\n\n", substr($0, 6); next }

/^```$/ {
       if (in_verbatim == 0) {
	       print "\\begin{lstlisting}";
	       in_verbatim = 1;
       } else {
	       print "\\end{lstlisting}";
	       in_verbatim = 0;
       };
       next
}

/./  { if (in_verbatim == 0) {for (i=1; i<=NF; i++) collect($i) }}
/^$/ { if (in_verbatim == 0) {flushp()} else { print " " }}
in_verbatim {
       	print
}

END {
	print "\\end{document}"
}

# Concatenate our multi-line string
function collect(v) {
  line = line sep v
  sep = " "
}

# Flush the string, rendering any inline HTML elements
function flushp() {
  if (line) {
    print "\n" render(line) "\n"
    line = sep = ""
  }
}

function render(line) {
    while (match(line, /__([^_]+)__/)) {
        sub(/__([^_]+)__/, sprintf("\\textbf{%s}", substr(line, RSTART+2, RLENGTH-4)), line)
    }
    
    while (match(line, /_([^_]+)_/)) {
        sub(/_([^_]+)_/, sprintf("\\emph{%s}", substr(line, RSTART+1, RLENGTH-2)), line)
    }

    while (match(line, /`([^`]+)`/)) {
        sub(/`([^`]+)`/, sprintf("\\texttt{%s}", substr(line, RSTART+1, RLENGTH-2)), line)
    }

    if (match(line, /\$/)) {        gsub(/\$/, "\\$", line)    }

    if (match(line, /#/)) {        gsub(/#/, "\\#", line)    }

    if (match(line, /\[.+\]\(.+\)/)) {
        inner = substr(line, RSTART+1, RLENGTH-3)
        split(inner, spl, /\]\(/)
        gsub(/\[.+\]\(.+\)/, sprintf("%s\\footnote{%s}", spl[1], spl[2]), line)
    }

    return line
}
