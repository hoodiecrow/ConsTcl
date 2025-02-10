# see https://www.mgmarlow.com/words/2024-03-23-markdown-awk/

BEGIN {
	print "\\documentclass{report}"
	print "\\usepackage{graphicx}"
	print "\\usepackage{listings}"
	print "\\lstset{"
	print "  showstringspaces=false,"
	print "  language=tcl,"
	print "}"
	print "\\renewcommand{\\thesection}{\\arabic{section}}"
	print "\\title{ConsTcl}"
	print "\\author{Peter Lewerin}"
	print "\\date{\\today}"
	print "\\begin{document}"
	print "\\maketitle"
	print "\\tableofcontents"
	print " "
}



/^# /    { next }
/^## /   {
    label = substr($0, 4)
    printf "\\section{%s}\n", label
    gsub(/ /, "-", label)
    printf "\\label{%s}\n", tolower(label)
    next
}

/^### /  {
    label = substr($0, 5)
    printf "\\subsection{%s}\n", label
    gsub(/ /, "-", label)
    printf "\\label{%s}\n", tolower(label)
    next
}

/^#### / {
    label = substr($0, 6)
    printf "\\subsubsection{%s}\n", label
    gsub(/ /, "-", label)
    printf "\\label{%s}\n", tolower(label)
    next
}

/^```/ {
       if (!in_listing) {
	       print "\\noindent\\makebox[\\linewidth]{\\rule{\\linewidth}{0.4pt}}"
	       print "\\begin{lstlisting}";
	       in_listing = 1;
       } else {
	       print "\\end{lstlisting}";
	       print "\\noindent\\makebox[\\linewidth]{\\rule{\\linewidth}{0.4pt}}"
	       in_listing = 0;
       };
       next
}
/^[-*] /            { if (!inul) print "\\begin{itemize}"; inul = 1; print "\\item " render(substr($0, 3)); next }
inul && !/^[-*] /   { print "\\end{itemize}"; inul = 0; next }

/<table/ { next }

/./  { if (!in_listing) {for (i=1; i<=NF; i++) collect($i) } else { print }}
/^$/ { if (!in_listing) {flushp()} else { print " " }}

END { print "\n\\end{document}" }

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
    if (match(line, /__([^_]+)__/)) {
        gsub(/__([^_]+)__/, sprintf("\\textbf{%s}", substr(line, RSTART+2, RLENGTH-4)), line)
    }
    
    if (match(line, /\*\*([^*]+)\*\*/)) {
        gsub(/\*\*([^*]+)\*\*/, sprintf("\\textbf{%s}", substr(line, RSTART+2, RLENGTH-4)), line)
    }
    
    if (match(line, /\*([^*]+)\*/)) {
        gsub(/\*([^*]+)\*/, sprintf("\\emph{%s}", substr(line, RSTART+1, RLENGTH-2)), line)
    }

    if (match(line, /`([^`]+)`/)) {
        gsub(/`([^`]+)`/, sprintf("\\texttt{%s}", substr(line, RSTART+1, RLENGTH-2)), line)
    }

    if (match(line, /\[#\]\(.+\)/)) {
        inner = substr(line, RSTART+4, RLENGTH-5)
        gsub(/\[#\]\(.+\)/, sprintf("\\footnote{See \\texttt{%s}}", inner), line)
    }

    if (match(line, /\$/)) { gsub(/\$/, "\\$", line) }

    if (match(line, /#/)) { gsub(/#/, "\\#", line) }

    if (match(line, /&/)) { gsub(/&/, "\\&", line) }

    if (match(line, /_/)) { gsub(/_/, "\\_", line) }

	if (match($0, /\{/)) { gsub(/\{/, "\\{", $0) }
	if (match($0, /\}/)) { gsub(/\}/, "\\}", $0) }

#    if (match(line, /\[.+\]\(.+\)/)) { inner = substr(line, RSTART+1, RLENGTH-2);split(inner, spl, /\]\(/);gsub(/\[.+\]\(.+\)/, sprintf("%s\\footnote{%s}", spl[1], spl[2]), line) }

    return line
}
