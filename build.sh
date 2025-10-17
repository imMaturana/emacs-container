#!/usr/bin/env bash

podman build \
    --build-arg=WAYLAND_DISPLAY=$WAYLAND_DISPLAY \
    -t emacs .

