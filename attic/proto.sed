echo 'lorem ipsum foobar' | sed '
    s/\(lorem ipsum \)\(foobar\)/printf "%s%s" "\1" "$(echo "\2" | tr o a)"/e
  '

cat constcl.md | sed 's/\(eval (.*);.*\)/printf "%s" "$(echo "\1" | tr e i)"/e'

cat constcl.md | sed 's/\(eval (.*);.*\)/printf "%s" "$(echo "\1" | proto)"/e'

cat constcl.md | sed 's/\(eval .*\)/printf "%s" "$(echo "\1" | proto)"/e' |less

cat constcl.md | sed 's/\(eval .*\)/printf "%s" "$(echo "\1" | tr e i)"/e'
