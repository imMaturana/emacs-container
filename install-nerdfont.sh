#!/usr/bin/env bash

font="Hack"
version="3.4.0"

font_tarxz="${font}.tar.xz"
url="https://github.com/ryanoasis/nerd-fonts/releases/download/v${version}/${font_tarxz}"
tmp_dir="$(mktemp -d)"
tarxz_file="${tmp_dir}/${font_tarxz}"
out_dir="/usr/share/fonts/${font} Nerd Font"

if wget -q -P "$tmp_dir" "$url"; then
	mkdir -p "$out_dir"
	tar xvf "$tarxz_file" -C "$out_dir"
fi

fc-cache -vf

