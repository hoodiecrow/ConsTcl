#include <string.h>
#include <stdio.h>

// eval (internal);e expr env env -> val
// eval (internal);e val env env -> val

char *dictionary[] = {
    "env",    "an environment",
    "dc",     "-don't care-",
    "nil",    "the empty list",
    "invoke", "what pr returns",
    "formals","a Scheme formals list",
    "lisp",   "Lisp source text",
    "none",   "nothing",
    "pterm",  "the terminating paren or bracket",
    "pstr",   "a structure of pair values",
    "sharp",  "a vector, boolean, or character value",
    "quote",  "a value wrapped in the quote symbol",
    "unquote","a value wrapped in the unquote/-splicing symbol",
    "qquote", "a value wrapped in the quasiquote symbol",
    "pm",     "either the symbols + or -, or a number",
    "val",    "a Lisp value",
    "vals",   "some Lisp values",
    "lvals",  "a Lisp list of Lisp values",
    "tvals",  "a Tcl list of Lisp values",
    "num",    "a number",
    "nums",   "some numbers",
    "lnums",  "a Lisp list of numbers",
    "tnums",  "a Tcl list of numbers",
    "bool",   "a boolean",
    "bools",  "some booleans",
    "lbools", "a Lisp list of booleans",
    "tbools", "a Tcl list of booleans",
    "char",   "a character",
    "chars",  "some characters",
    "lchars", "a Lisp list of characters",
    "tchars", "a Tcl list of characters",
    "proc",   "a procedure",
    "procs",  "some procedures",
    "lprocs", "a Lisp list of procedures",
    "tprocs", "a Tcl list of procedures",
    "pair",   "a pair",
    "pairs",  "some pairs",
    "lpairs", "a Lisp list of pairs",
    "tpairs", "a Tcl list of pairs",
    "list",   "a list",
    "lists",  "some lists",
    "llists", "a Lisp list of lists",
    "tlists", "a Tcl list of lists",
    "str",    "a string",
    "strs",   "some strings",
    "lstrs",  "a Lisp list of strings",
    "tstrs",  "a Tcl list of strings",
    "sym",    "a symbol",
    "syms",   "some symbols",
    "lsyms",  "a Lisp list of symbols",
    "tsyms",  "a Tcl list of symbols",
    "vec",    "a vector",
    "vecs",   "some vectors",
    "lvecs",  "a Lisp list of vectors",
    "tvecs",  "a Tcl list of vectors",
    "expr",   "an expression",
    "exprs",  "some expressions",
    "lexprs", "a Lisp list of expressions",
    "texprs", "a Tcl list of expressions",
    "arg",    "an argument",
    "args",   "some arguments",
    "largs",  "a Lisp list of arguments",
    "targs",  "a Tcl list of arguments",
    "int",    "an integer",
    "ints",   "some integers",
    "lints",  "a Lisp list of integers",
    "tints",  "a Tcl list of integers",
    "bsym",   "a bound symbol",
    "bsyms",  "some bound symbols",
    "lbsyms", "a Lisp list of bound symbols",
    "tbsyms", "a Tcl list of bound symbols",
    NULL,    NULL,
};

char *lookup (const char *key) {
    char **p;
    for (p = dictionary ; *p != NULL ; p += 2) {
        if (strcmp(key, *p) == 0) {
            return *(p + 1);
        }
    }
    return "not found";
}

int main (void) {
    char buf[256];

    fgets(buf, sizeof buf, stdin);

    // get the header
    char *header = strdup(buf);
    char *endp = strchr(header, ';');
    *endp = 0;
    printf("<table border=\"1\"><thead><tr><th colspan=\"2\" align=\"left\">%s</th></tr></thead>", header);
    char *tokens = endp + 1;
    int loop = 1;
    char *name = strtok(tokens, " \t\n");
    char *type = strtok(NULL, " \t\n");
    if (strcmp(name, "->") == 0) {
        loop = 0;
        printf("<tr><td><i>%s</i></td><td>%s</td></tr></table>", "Returns:", lookup(type));
    } else {
        printf("<tr><td>%s</td><td>%s</td></tr>", name, lookup(type));
    }
    // loop: get one item (two tokens)
    while (loop) {
        name = strtok(NULL, " \t\n");
        type = strtok(NULL, " \t\n");
        if (strcmp(name, "->") == 0) {
            loop = 0;
            printf("<tr><td><i>%s</i></td><td>%s</td></tr></table>", "Returns:", lookup(type));
        } else {
            printf("<tr><td>%s</td><td>%s</td></tr>", name, lookup(type));
        }
    // if it begins with ->, it's the return form: start a tr, add a <td><i>Returns:</i></td> and add the rest of the item, expanded, in a td, ending the row
    // otherwise, begin a tr and add the item in a td
    // then read one more item, expand it and add it in a td, ending the tr
    // goto loop
    }
    return 0;
}
