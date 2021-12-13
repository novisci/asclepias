FROM nixos/nix:2.3.12

COPY nix/nix.conf /etc/nix/nix.conf
WORKDIR /project
COPY .. .

RUN nix-build