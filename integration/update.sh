#!/bin/bash
echo update

# build docker image
if [ -n "$IS_DOCKER" ]; then
    export VERSION=0.1
    export DOCKER_IMAGE_TAG=ddopsndd-$VERSION-docker
    if [ -n "$TRAVIS_TAG" ]; then
	export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$TRAVIS_TAG
    else
	export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$TRAVIS_BRANCH-${TRAVIS_COMMIT:0:7}
    fi
    export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-$(uname)-$OS_DISTRIBUTOR-$OS_CORENAME-GHC_$GHC_VER-$(uname -m)
    if [ -n "$LLVM" ]; then
	export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-llvm-$LLVM
    fi
    if [ -n "$THREADED" ]; then
	export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-threaded
    fi
    if [ -n "$DEBUG" ]; then
	export DOCKER_IMAGE_TAG=$DOCKER_IMAGE_TAG-debug
	export DOCKERFILE_EXT=.debug
    fi
    echo copy files
    cd $TRAVIS_BUILD_DIR
    mkdir -p docker.tmp/bin
    sudo cp $HOME/.local/bin/ddopsndd docker.tmp/bin
    sudo cp $TRAVIS_BUILD_DIR/integration/Dockerfile$DOCKERFILE_EXT docker.tmp/Dockerfile
    echo build docker
    cd docker.tmp
    docker build -t qinka/ddopsndd:$DOCKER_IMAGE_TAG .
    docker push  qinka/ddopsndd
fi
