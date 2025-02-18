# see https://www.mgmarlow.com/words/2024-03-23-markdown-awk/

{ gsub(/\r/, ""); }

$1 == "TT(" { in_test_block = 1 ; print "" ; next }
$1 == "TT)" { in_test_block = 0 ; next }
in_test_block && /./  { print }
in_test_block && /^$/ { print }

{ next }

# Vim: tabstop=4 shiftwidth=4 softtabstop=4 expandtab:

