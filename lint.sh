

for name in $@
    do
        echo $name
        cat $name | node ../clolint-js/index.js
        echo
    done
