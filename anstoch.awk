
$1 == "PN" {
	if (projh == "") {
		projname = $2
		projh = projname ".h"
		print "#ifndef " toupper(projname) "_H\n#define " toupper(projname) "_H\n" >projh
		print "#include \"" projh "\"\n" >itemc
		print "#include \"" itemh "\"\n" >projh
	}
	next
}

BEGINFILE {
	split(FILENAME, base, /\.ans/)
	itemname = base[1]
	itemh = itemname ".h"
	itemc = itemname ".c"
	print "#ifndef " toupper(itemname) "_H\n#define " toupper(itemname) "_H\n" >itemh
	if (projh != "") {
		print "#include \"" projh "\"\n" >itemc
		print "#include \"" itemh "\"\n" >projh
	}
	itemt = ""
	next
}

ENDFILE {
    print "\n#endif" >itemh
    print itemt >itemc
}

END {
    print "\n#endif" >projh
}

$1 == "IN" {
	include_file = $2
	if (!(include_file in includes)) {
		if ((getline < $2) < 0) {
			print "#include <" include_file ">" > itemc
		} else {
			print "#include \"" include_file "\"" > itemc
		}
	} else {
		includes[include_file] = 1
	}
	next
}

$1 == "PT" {
	for (i=2; i<=NF; i++) { text = text tsep $i ; tsep = " " } 
	itemt = itemt text "\n"
	print text ";" > itemh
	text = tsep = ""
	next
}

{ itemt = itemt $0 "\n" }

