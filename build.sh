#!/bin/bash
SCRIPTNAME=$(basename "$0")

function show_help() {
    echo "Usage: ./$SCRIPTNAME [--in | --test | --clean]"
}

function check_params() {
    if [ "$1" == "--in-docker" ]
    then
        IN_DOCKER=1
        shift
    fi

    if [ "$1" == "-h" -o "$1" == "--help" ]
    then
        SHOW_HELP=1
    elif [ "$1" == "--test" ]
    then
        TEST=1
    elif [ "$1" == "--clean" ]
    then
        CLEAN=1
    elif [ "$1" == "--in" ]
    then
        DROPIN=1
    elif [ ! -z "$1" ]
    then
        echo "Did not understand $@"
        show_help
        exit 1
    fi
}

check_params "$@"

function elm_make() {
    elm make --yes --warn "$1".elm --output "Elm_$1".js || exit 1
}

function run_build_cmds() {
    echo "Building elm code..."

    elm_make Ping

}

function launch_elm_docker() {
    docker run -it --rm \
               -v "$(pwd):/code" \
               -w "/code" \
               -e "HOME=/tmp" \
               -u $UID:$GID \
               -p 8000:8000 \
               tagtime-elm "./$SCRIPTNAME" --in-docker "$@" || exit 1
}

function in_docker_elm() {
    docker run -it --rm \
               -v "$(pwd):/code" \
               -w "/code" \
               -e "HOME=/tmp" \
               -u $UID:$GID \
               -p 8000:8000 \
               tagtime-elm bash
}

function lint() {
    TO=TO
    DO=DO
    grep $TO$DO *.sh
    elm analyse || true
}

#       understand/
# We can both static and dynamic tests to give us confidence in our
# code. 'lint' is a static test system.
function runtests() {
    lint "$@"
}



#       situation/
# Once the elm container has started we need a script to compile the elm
# code (and run analyze etc).
#
#       outcome/
# As we are loading this folder we already have access to this script -
# we will detect if we are running inside or outside the docker
# container and run appropriately.
if [[ $IN_DOCKER == 1 ]]
then
    run_build_cmds "$@"
    if [[ $TEST == 1 ]]
    then
        runtests "$@"
    fi
elif [[ $DROPIN == 1 ]]
then
    in_docker_elm
elif [[ $SHOW_HELP == 1 ]]
then
    show_help
elif [[ $CLEAN == 1 ]]
then
    rm -f *.js
    rm -rf elm-stuff/
else
    launch_elm_docker "$@"
fi

