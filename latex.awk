# see https://www.mgmarlow.com/words/2024-03-23-markdown-awk/

BEGIN {
	print "\\documentclass[twoside,9pt]{report}"
	print "\\usepackage[a5paper]{geometry}"
	print "\\usepackage{newpxmath}"
	print "\\usepackage{graphicx}"
	print "\\usepackage{listings}"
	print "\\lstset{"
	print "  showstringspaces=false,"
	print "  language=tcl,"
	print "}"
	print "%\\renewcommand{\\thesection}{\\arabic{section}}"
	print "\\title{ConsTcl}"
	print "\\author{Peter Lewerin}"
	print "\\date{\\today}"
	print "\\makeatletter"
        print "\\def\\@makechapterhead#1{%"
        print "  \\vspace*{50\\p@}%"
        print "    {\\parindent \\z@ \\raggedright \\normalfont"
        print "      \\ifnum \\c@secnumdepth >\\m@ne"
        print "        %\\if@mainmatter"
        print "          %\\huge\\bfseries \\@chapapp\\space \\thechapter"
        print "          \\Huge\\bfseries \\thechapter.\\space%"
        print "          %\\par\\nobreak"
        print "          %\\vskip 20\\p@"
        print "        %\\fi"
        print "      \\fi"
        print "      \\interlinepenalty\\@M"
        print "      \\Huge \\bfseries #1\\par\\nobreak"
        print "      \\vskip 40\\p@"
        print "   }}"
        print "\\makeatother"
	print "\\begin{document}"
	print "\\pagestyle{headings}"
	print "\\maketitle"
	print "\\tableofcontents"
	print " "
}



/^# /    { next }
/^## /   {
    printf "\\chapter{%s}\n", substr($0, 4)
    printf "\\label{%s}\n", makelabel(substr($0, 4))
    next
}

/^### /   {
    printf "\\section{%s}\n", substr($0, 5)
    printf "\\label{%s}\n", makelabel(substr($0, 5))
    next
}

/^#### /  {
    printf "\\subsection{%s}\n", substr($0, 6)
    printf "\\label{%s}\n", makelabel(substr($0, 6))
    next
}

/^##### / {
    printf "\\subsubsection{%s}\n", substr($0, 7)
    printf "\\label{%s}\n", makelabel(substr($0, 7))
    next
}

/^###### / {
    printf "\\paragraph{%s}\n", substr($0, 8)
    printf "\\label{%s}\n", makelabel(substr($0, 8))
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
/^[-*] / { if (!init) print "\\begin{itemize}";init = 1; print "\\item " render(substr($0, 3));next }
init && !/^[-*] /   { print "\\end{itemize}"; init = 0; next }

/^[0-9]+\. / { if (!inen) print "\\begin{enumerate}";inen = 1; print "\\item " render(substr($0, 3));next }
inen && !/^[0-9]+\. /   { print "\\end{enumerate}"; inen = 0; next }

/<table id="syntaxforms"/ {
    if (!intsf) {
	print "\\begin{tabular}{|l l|}\n\\hline"
	print "Syntactic form & Syntax \\\\\n\\hline"
    }
    intsf = 1
    next
}

intsf && $1 == "<tr>" {
    patsplit($0, sftablefs, />[^<]+<\//)
    printf "%s & %s \\\\\n", clean(sftablefs[1]), clean(sftablefs[2])
}

intsf && /^</ { next }

intsf && !/^</ { print "\\hline\n\\end{tabular}\n" ; intsf = 0 ; next }

/<table border/ {
    patsplit($0, tablefs, />[^<]+<\//)
    printf "\\begin{tabular}{ |l l| }\n"
    printf "\\hline\n"
    printf "\\multicolumn{2}{|l|}{%s} \\\\\n", clean(tablefs[1])
    printf "\\hline\n"
    for (i=2 ; i in tablefs ; i += 2) {
        printf "%s & %s \\\\\n", clean(tablefs[i]), clean(tablefs[i+1])
    }
    printf "\\hline\n"
    printf "\\end{tabular}\n\n"
    next
}

/./  { if (!in_listing) {for (i=1; i<=NF; i++) collect($i) } else { print }}
/^$/ { if (!in_listing) {flushp()} else { print " " }}

END { print "\n\\end{document}" }

function clean (str) {
    gsub(/<\//, "", str)
    gsub(/>/, "", str)
    gsub(/&lt;/, "<", str)
    gsub(/&gt;/, ">", str)
    gsub(/&amp;/, "&", str)
    if (match(str, /_/)) { gsub(/_/, "\\_", str) }
    if (match(str, /#/)) { gsub(/#/, "\\#", str) }
    if (str == "Returns:") str = sprintf("\\textit{%s}", str)
    return str
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
    if (match(line, /\\/)) { gsub(/\\/, "\\textbackslash\\ ", line) }

    while (match(line, /__([^_]+)__/)) {
        sub(/__([^_]+)__/, sprintf("\\textbf{%s}", substr(line, RSTART+2, RLENGTH-4)), line)
    }
    
    while (match(line, /\*\*([^*]+)\*\*/)) {
        sub(/\*\*([^*]+)\*\*/, sprintf("\\emph{%s}", substr(line, RSTART+2, RLENGTH-4)), line)
    }

    while (match(line, /`([^`]+)`/)) {
        sub(/`([^`]+)`/, sprintf("\\texttt{%s}", substr(line, RSTART+1, RLENGTH-2)), line)
    }

    if (match(line, /!\[[^]]+\]\(.+\)/)) {
        inner = substr(line, RSTART, RLENGTH)
	match(inner, /\(.+\)/)
	line = sprintf("\\includegraphics{%s}", substr(inner, RSTART+2, RLENGTH-3))
    }

    if (match(line, /\[#\]\(.+\)/)) {
        inner = substr(line, RSTART+4, RLENGTH-5)
	if (match(inner, /ConsTcl#.*/)) {
	    label = substr(inner, RSTART+8)
	    gsub(/\[#\]\(.+\)/, sprintf(" (see page \\pageref{%s})", label), line)
        } else {
	    gsub(/\[#\]\(.+\)/, sprintf("\\footnote{See \\texttt{%s}}", inner), line)
    	}
    }

    if (match(line, /\$/)) { gsub(/\$/, "\\$", line) }

    if (match(line, /#/)) { gsub(/#/, "\\#", line) }

    if (match(line, /&grave;/)) { gsub(/&grave;/, "`", line) }

    if (match(line, /&/)) { gsub(/&/, "\\&", line) }

    if (match(line, /_/)) { gsub(/_/, "\\_", line) }

    if (match(line, /%/)) { gsub(/%/, "\\%", line) }

    if (match(line, /⇒/)) { gsub(/⇒/, "=>", line) }

	if (match($0, /\{/)) { gsub(/\{/, "\\{", $0) }
	if (match($0, /\}/)) { gsub(/\}/, "\\}", $0) }

#    if (match(line, /\[.+\]\(.+\)/)) { inner = substr(line, RSTART+1, RLENGTH-2);split(inner, spl, /\]\(/);gsub(/\[.+\]\(.+\)/, sprintf("%s\\footnote{%s}", spl[1], spl[2]), line) }

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
