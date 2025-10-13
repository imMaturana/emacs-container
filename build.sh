#!/usr/bin/env bash

podman build \
    --build-arg=WAYLAND_DISPLAY=$WAYLAND_DISPLAY \
    --build-arg=FONT_NERDFONT="Hack" \
    -t emacs .

