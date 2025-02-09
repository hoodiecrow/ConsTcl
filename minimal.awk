BEGIN { modeline = " [#;] v" + "im:"}
BEGINFILE {
	if (FILENAME=="schemebase.lsp") {
		print "## A Scheme base\n"
	}
	print "```"
}
ENDFILE {
	print "```"
}

# skip any modeline
$0 ~ modeline { next }

{ print }

