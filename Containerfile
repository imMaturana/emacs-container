FROM registry.fedoraproject.org/fedora:latest

ARG WAYLAND_DISPLAY

ENV XDG_RUNTIME_DIR=/tmp
ENV WAYLAND_DISPLAY=$WAYLAND_DISPLAY

RUN dnf install -y \
	coreutils \
	wget \
	emacs

# latex tools
RUN dnf install -y \
	texlive-scheme-basic \
	texlive-collection-basic \
	texlive-collection-fontsrecommended \
	texlive-ulem \
	texlive-wrapfig \
	texlive-capt-of

# tree-sitter
RUN dnf install -y \
	tree-sitter \
	libtree-sitter-go \
	libtree-sitter-python \
	libtree-sitter-elixir

# extra
RUN dnf install -y \
	dvisvgm \
	git

# fonts
RUN dnf install -y \
	google-noto-serif-fonts \
	google-noto-mono-fonts

# install mise
RUN curl https://mise.run | MISE_INSTALL_PATH=/usr/bin/mise sh

COPY ./install-nerdfont.sh /install-nerdfont.sh
RUN /install-nerdfont.sh

COPY ./emacs-config /root/.config/emacs
COPY ./git-config /root/.config/git

WORKDIR /workdir
VOLUME /workdir

VOLUME /root/.config/emacs
VOLUME /root/.ssh
VOLUME /root/Org
VOLUME /root/Roam

ENTRYPOINT ["emacs"]

