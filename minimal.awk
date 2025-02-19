
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
    if (match(line, /`/)) { gsub(/`/, "\\&grave;", line) }

    while (match(line, /B{([^{}]+)}/)) {
        sub(/B{([^{}]+)}/, sprintf("__%s__", substr(line, RSTART+2, RLENGTH-3)), line)
    }
    
    while (match(line, /E{([^{}]+)}/)) {
        sub(/E{([^{}]+)}/, sprintf("_%s_", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    if (match(line, /K{([^{}]+)}/)) {
        gsub(/K{([^{}]+)}/, sprintf("`%s`", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /I{([^{}]+)}/)) {
        sub(/I{([^{}]+)}/, "", line)
    }

    while (match(line, /R{([^{}]+)}/)) {
        sub(/R{([^{}]+)}/, sprintf("[#](https://github.com/hoodiecrow/ConsTcl#%s)", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /L{([^{}]+)}/)) {
        sub(/L{([^{}]+)}/, sprintf("[#](https://github.com/hoodiecrow/ConsTcl#%s)", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /W{([^{}]+)}/)) {
        sub(/W{([^{}]+)}/, sprintf("[wiki](https://en.wikipedia.org/wiki/%s)", substr(line, RSTART+2, RLENGTH-3)), line)
    }

    while (match(line, /P{([^{}]+)}{([^{}]+)}/)) {
	patsplit(substr(line, RSTART+2, RLENGTH-3), pow, /[^{}]+/)
        sub(/P{([^{}]+)}{([^{}]+)}/, sprintf("%s<sup>%s</sup>", pow[1], pow[2]), line)
    }

    if (match(line, /⇒/)) { gsub(/⇒/, "=>", line) }

    return line
}




