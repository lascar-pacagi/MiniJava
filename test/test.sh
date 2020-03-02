#!/bin/bash

set -o nounset
set -o pipefail
#set -o xtrace

SCRIPT_DIR="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
MINIJAVA=${SCRIPT_DIR}/../mini-java
BUILD_DIR=
BAD_PARSING_DIR=${SCRIPT_DIR}/bad_syntax
BAD_TYPECHECKING_DIR=${SCRIPT_DIR}/bad_typechecking
GOOD_DIR=${SCRIPT_DIR}/good
COLOR_BLACK=0
COLOR_RED=1
COLOR_GREEN=2
COLOR_YELLOW=3
COLOR_BLUE=4
COLOR_MAGENTA=5
COLOR_CYAN=6
COLOR_WHITE=7
OK=0
TOTAL=0

function show_help() {
	cat > /dev/stdout << END
${0} -p [-b <bad lexing and parsing dir>] [-d <bad typechecking dir>] [-g <good dir>] [-h]
${0} -t [-b <bad lexing and parsing dir>] [-d <bad typechecking dir>] [-g <good dir>] [-h]
${0} -r [-b <bad lexing and parsing dir>] [-d <bad typechecking dir>] [-g <good dir>] [-h]
${0} -a [-b <bad lexing and parsing dir>] [-d <bad typechecking dir>] [-g <good dir>] [-h]

ARGUMENTS:
-p - test lexing and parsing only
-t - test typechecking only
-r - test runtime only
-a - test all
-b - path to directory containing the programs that contain lexical and syntax errors
     by default they are searched in ${SCRIPT_DIR}/bad_parsing
-d - path to directory containing the programs that contain semantic errors
     by default they are searched in ${SCRIPT_DIR}/bad_typechecking
-g - path to directory containing the good programs (compile and run correctly)
     by default they are searched in ${SCRIPT_DIR}/good
-h - show help
END
}

function create_tmp_dir() {
    until [ -n "${BUILD_DIR}" -a ! -d "${BUILD_DIR}" ]
    do
        BUILD_DIR="/tmp/minijava_tests.${RANDOM}${RANDOM}${RANDOM}"
    done
    mkdir -p -m 0700 ${BUILD_DIR}  || { echo "FATAL: Failed to create temp dir '${BUILD_DIR}': $?"; exit 1; }
    cp -r ${GOOD_DIR} ${BUILD_DIR} || { echo "FATAL: Failed to create temp dir 'good': $?"; exit 1; }
    cp -r ${BAD_PARSING_DIR} ${BUILD_DIR}  || { echo "FATAL: Failed to create temp dir 'bad_syntax': $?"; exit 1; }
    cp -r ${BAD_TYPECHECKING_DIR} ${BUILD_DIR}  || { echo "FATAL: Failed to create temp dir 'bad_typechecking': $?"; exit 1; }
    GOOD_DIR=${BUILD_DIR}/good
    BAD_PARSING_DIR=${BUILD_DIR}/bad_syntax
    BAD_TYPECHECKING_DIR=${BUILD_DIR}/bad_typechecking
}

function cleanup() {
    exec &> /dev/tty
    tput cnorm # cursor normal
    tput sgr0  # turn off all attributes
    rm -rf ${BUILD_DIR}
}

function println() {
    tput setaf ${1} # set color
    echo -e "${2}"
}

function print() {
    tput setaf ${1}
    echo -ne "${2}"
}

function space() {
    head -c ${1} < /dev/zero | tr '\0' ' '
}

function draw_bar() {
    tput bold
    for i in $(seq ${1})
    do
        print ${COLOR_BLUE} "\u2015"
    done
    tput sgr0
}

function title() {
    draw_bar 6
    tput bold
    print ${COLOR_BLUE} "${1}"
    tput sgr0
    draw_bar 6
    echo
}

function test_common() {
    if [ -d "${1}" ]
    then
        println ${COLOR_WHITE} "$(space 1)Entering directory ${1}"
    fi
    local total=0
    local ok=0
    pushd "${1}" > /dev/null
    for file in *.java
    do
        ${2} "${file}" &> /dev/null
        if [ $? -eq ${3} ]
        then
            print ${COLOR_WHITE} "$(space 2)\u2022"
            println ${COLOR_GREEN} "${file} \u2714"
            ((ok++))
            ((OK++))
        else
            print ${COLOR_WHITE} "$(space 2)\u2022"
            println ${COLOR_RED} "${file} \u26a0"
        fi
        ((total++))
        ((TOTAL++))
    done
    popd > /dev/null
    tput bold
    println ${COLOR_WHITE} "$(space 1)Summary: ${ok}/${total} PASSED"
    tput sgr0
}

function test_parsing() {
    title "TEST PARSING"
    test_common ${BAD_PARSING_DIR} "${MINIJAVA} --stop-at-parsing" 1
    test_common ${GOOD_DIR} "${MINIJAVA} --stop-at-parsing" 0
}

function test_typechecking() {
    title "TEST TYPECHECKING"
    test_common ${BAD_TYPECHECKING_DIR} "${MINIJAVA} --stop-at-typechecking" 1
    test_common ${GOOD_DIR} "${MINIJAVA} --stop-at-typechecking" 0
}

function diff_runtime() {
    diff <(${MINIJAVA} --tgc-path=${SCRIPT_DIR}/../tgc ${1} && ./${1/.java/}) <(javac ${1} && java ${1/.java/})
}

function test_runtime() {
    title "TEST RUNTIME"
    test_common ${GOOD_DIR} "diff_runtime" 0
}

function total() {
    echo
    title "TOTAL"
    tput bold
    println ${COLOR_WHITE} "$(space 1)${OK}/${TOTAL} PASSED"
    tput sgr0
}

trap "cleanup; exit 1" TERM INT QUIT

clear
tput civis # make cursor invisible

while getopts "hb:d:g:ptra" opts
do
     case ${opts} in
         p)
             create_tmp_dir
             test_parsing
             total
             break
             ;;
         t)
             create_tmp_dir
             test_typechecking
             total
             break
             ;;
         r)
             create_tmp_dir
             test_runtime
             break
             ;;
         a)
             create_tmp_dir
             test_parsing
             echo
             test_typechecking
             echo
             test_runtime
             total
             break
             ;;
         b) BAD_PARSING_DIR=${OPTARG};;
         d) BAD_TYPECHECKING_DIR=${OPTARG};;
         g) GOOD_DIR=${OPTARG};;
         h) show_help;;
         ?)
            show_help
            cleanup
            exit 1;;
     esac
done
if [ ${OPTIND} = 1 ]
then
    show_help
    cleanup
    exit 1
fi
cleanup
exit 0