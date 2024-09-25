#!/usr/bin/env sh

test_cases=("fact 6" "mod_test 3" "if_test 11" "many_ops 36" "smol_test 4")
failed_tests=0

for test_case in "${test_cases[@]}"; do
    read name expected <<< "$test_case"
    ./$name.bin
    result=$(echo $?)

    if [ $result != $expected ]; then
        echo "$name test failed; got: $result, expected: $expected"
        failed_tests=$(($failed_tests + 1))
    fi
done

if [ $failed_tests == 0 ]; then
    echo "All tests passed!"
else
    echo "$failed_tests out of ${#test_cases[@]} tests failed"
fi
