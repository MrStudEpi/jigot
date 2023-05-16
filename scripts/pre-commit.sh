#!/bin/sh

RED='\033[0;31m'
GREEN='\033[0;32m'
WHITE='\033[1;37m'

echo "${WHITE}Running pre-commit verification script"
echo ""

echo -n "${WHITE}Step 1: Launch all tests"
stack test --coverage --no-terminal 2> output_file

ERROR=$(cat ./output_file | grep "Failures: 1")

if [ ! -z "$ERROR" ]; then
    echo "${RED}Abort - Tests didn't not pass"
    exit 1
fi

echo "${GREEN} OK${WHITE}"
echo -n "Step 2: Verify the coverage report"
OUTPUT=$(cat ./output_file | grep -Eo "([0-9])+%" | head -1 | sed 's/.$//')

if [ $((OUTPUT)) -lt 80 ]; then
    echo "${RED}Abort - Test coverage is less than 80%"
    exit 1
fi
echo "${GREEN} OK${WHITE}"

echo ""
echo -n "${WHITE}All Test Passed + Coverage greater than 80% -> "
echo "${GREEN}$OUTPUT%"