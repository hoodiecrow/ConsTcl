    # remove the potential CR characters in the input line
    { gsub(/\r/, ""); }

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
            printf "<table>";
            th = substr($0, 1, RSTART-1);
            printf "<thead><tr colspan=2><th>%s</th></tr></thead>", \
                html_textify(th);
            $0 = substr($0, RSTART+RLENGTH);
            for (i = 1; i <= NF; i += 2) {
                td1 = ($i == "->" ? "Returns:" : $i);
                td2 = dict[$(i+1)];
                printf "<tr><td>%s</td><td>%s</td></tr>", \
                    html_textify(td1), html_textify(td2);
            }
            print "</table>";
        }
        next;
    }

    # output non PR lines
    { print; }

    # minimalist function that encodes a string as HTML text
    function html_textify(str) {
        gsub(/&/, "\\&amp;", str);
        gsub(/</, "\\&lt;", str);
        gsub(/>/, "\\&gt;", str);
        return str;
    }

    # <table border="1"><thead><tr><th colspan="2" align="left">eval (internal)</th></tr></thead><tbody><tr><td>e</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></tbody></table>
