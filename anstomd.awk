# see https://www.mgmarlow.com/words/2024-03-23-markdown-awk/

{ gsub(/\r/, ""); }

BEGIN { modeline = "[#;] v" "im:" }

END { print "\n" }

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
		printf "<table border=1>";
		th = substr($0, 1, RSTART-1);
		printf "<thead><tr><th colspan=2 align=\"left\">%s</th></tr></thead>", html_textify(th);
		$0 = substr($0, RSTART+RLENGTH);
		for (i = 1; i <= NF; i += 2) {
			td1 = ($i == "->" ? "Returns:" : $i);
			td2 = dict[$(i+1)];
			if (td1 == "Returns:")
				printf "<tr><td><i>%s</i></td><td>%s</td></tr>", html_textify(td1), html_textify(td2);
			else
				printf "<tr><td>%s</td><td>%s</td></tr>", html_textify(td1), html_textify(td2);
		}
		print "</table>\n";
	}
	next;
}

# minimalist function that encodes a string as HTML text
function html_textify(str) {
	gsub(/&/, "\\&amp;", str);
	gsub(/</, "\\&lt;", str);
	gsub(/>/, "\\&gt;", str);
	return str;
}

# skip any modeline
$0 ~ modeline { next; }

/^H1 / { printf "# %s\n", substr($0, 4) ; next }
/^H2 / { printf "## %s\n", substr($0, 4) ; next }
/^H3 / { printf "### %s\n", substr($0, 4) ; next }
/^H4 / { printf "#### %s\n", substr($0, 4) ; next }
/^H5 / { printf "##### %s\n", substr($0, 4) ; next }
/^H6 / { printf "###### %s\n", substr($0, 4) ; next }

$1 == "CB(" { in_code_block = 1 ; print "```" ; next }
$1 == "CB)" { in_code_block = 0 ; print "```" ; next }
in_code_block { print ; next }

$1 == "IX" { next }
$1 == "IG" { printf("![#](%s)\n", substr($2, 2)) ; next }
$1 == "EM" { for (i=2; i<=NF; i++) collect($i) ; line = sprintf("_%s_", line) ; flushp() ; next }
$1 == "IT" { init = 1; print "* " render(substr($0, 3));next }
init && $1 != "IT" { init = 0; next }
$1 == "EN" { inen = 1; print "1. " render(substr($0, 3));next }
inen && $1 != "EN" { inen = 0; next }

$1 == "MD(" { in_md_block = 1 ; print "" ; next }
$1 == "MD)" { in_md_block = 0 ; flushp() ; next }

in_md_block && /./  { for (i=1; i<=NF; i++) collect($i) }
in_md_block && /^$/ { flushp() }

$1 == "VB(" { in_vb_block = 1 ; print "```" ; next }
$1 == "VB)" { in_vb_block = 0 ; print "```"   ; next }
in_vb_block { print ; next }

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
    #if (match(line, /`/)) { gsub(/`/, "`` ` ``", line) }

    while (match(line, /B{([^{}]+)}/)) {
        sub(/B{([^{}]+)}/, sprintf("__%s__", substr(line, RSTART+2, RLENGTH-3)), line)
    }
    
    while (match(line, /E{([^{}]+)}/)) {
        sub(/E{([^{}]+)}/, sprintf("_%s_", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /K{([^{}]+)}/)) {
        sub(/K{([^{}]+)}/, sprintf("`` %s ``", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /I{([^{}]+)}/)) {
        sub(/I{([^{}]+)}/, "", line)
    }

    while (match(line, /F{([^{}]+)}/)) {
        sub(/F{([^{}]+)}/, sprintf(" (%s)", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /M{([^{}]+)}/)) {
        sub(/M{([^{}]+)}/, sprintf("__%s__", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /D{([^{}]*)}/)) {
        sub(/D{([^{}]*)}/, sprintf("..."), line)
    }

    while (match(line, /R{([^{}]+)}{([^{}]+)}/)) {
	patsplit(substr(line, RSTART+2, RLENGTH-3), ref, /[^{}]+/)
        sub(/R{([^{}]+)}{([^{}]+)}/, sprintf("[%s](https://github.com/hoodiecrow/ConsTcl#%s)", ref[1], ref[2]), line)
    }

    while (match(line, /L{([^{}]+)}{([^{}]+)}/)) {
	patsplit(substr(line, RSTART+2, RLENGTH-3), link, /[^{}]+/)
        sub(/L{([^{}]+)}{([^{}]+)}/, sprintf("[%s](%s)", link[1], link[2]), line)
    }

    while (match(line, /W{([^{}]+)}{([^{}]+)}/)) {
	patsplit(substr(line, RSTART+2, RLENGTH-3), wiki, /[^{}]+/)
        sub(/W{([^{}]+)}{([^{}]+)}/, sprintf("[%s](https://en.wikipedia.org/wiki/%s)", wiki[1], wiki[2]), line)
    }

    while (match(line, /P{([^{}]+)}{([^{}]+)}/)) {
	patsplit(substr(line, RSTART+2, RLENGTH-3), pow, /[^{}]+/)
        sub(/P{([^{}]+)}{([^{}]+)}/, sprintf("%s<sup>%s</sup>", pow[1], pow[2]), line)
    }

    if (match(line, /⇒/)) { gsub(/⇒/, "=>", line) }

    return line
}

# Vim: tabstop=4 shiftwidth=4 softtabstop=4 expandtab:

