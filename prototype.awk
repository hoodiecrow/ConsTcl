# due to Fravadona at stackoverflow.com
# 
# remove the potential CR characters in the input line
{ gsub(/\r/, ""); }

BEGIN { modeline = " [#;] v" "im:" }

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
		print "</table>";
	}
	next;
}

$1 == "TblSynForms" {
    print "<table id=\"syntaxforms\"><thead>"
    print "<tr><th>Syntactic form</th> <th>Syntax</th> </tr>"
    print "</thead>"
    print "<tbody>"
    print "<tr> <td>Variable reference</td><td>variable</td></tr>"
    print "<tr> <td>Constant literal</td><td>number or boolean, etc</td></tr>"
    print "<tr> <td>Quotation</td><td>quote datum</td></tr>"
    print "<tr> <td>Sequence</td><td>begin expression...</td></tr>"
    print "<tr> <td>Conditional</td><td>if test conseq alt</td></tr>"
    print "<tr> <td>Definition</td><td>define identifier expression</td></tr>"
    print "<tr> <td>Assignment</td><td>set! variable expression</td><td></tr>"
    print "<tr> <td>Procedure definition</td><td>lambda formals body</td></tr>"
    print "<tr> <td>Procedure call</td><td>operator operand...</td></tr>"
    print "</tbody></table>"
    next
}

# skip any modeline
$0 ~ modeline { next; }

# output non PR lines
{ print; }

# minimalist function that encodes a string as HTML text
function html_textify(str) {
	gsub(/&/, "\\&amp;", str);
	gsub(/</, "\\&lt;", str);
	gsub(/>/, "\\&gt;", str);
	return str;
}

# Vim: tabstop=4 shiftwidth=4 softtabstop=4 expandtab:
