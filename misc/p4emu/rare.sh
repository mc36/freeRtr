#!/bin/sh
apt-get install sudo mc curl wget git make default-jdk-headless default-jre-headless bc jq
bash <(curl -L https://nixos.org/nix/install) --daemon </dev/null
echo onie_machine=model > /etc/machine.conf
cat >> /etc/freertr/ports.json << EOF
{
    "PortToIf": [
        {
            "device_port":4,
            "if":"ens5"
        },
        {
            "device_port":8,
            "if":"ens6"
        },
        {
            "device_port":12,
            "if":"ens7"
        },
        {
            "device_port":16,
            "if":"ens8"
        }
    ],
    "PortToVeth": [
        {
            "device_port": 64,
            "veth1": 250,
            "veth2": 251
        }
    ]
}
EOF
cat >> /etc/nix/nix.conf << EOF
extra-substituters = https://cache.nixos.org http://p4.cache.nix.net.switch.ch
trusted-substituters = https://cache.nixos.org http://p4.cache.nix.net.switch.ch
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= p4.cache.nix.net.switch.ch:cR3VMGz/gdZIdBIaUuh42clnVi5OS1McaiJwFTn5X5g=
EOF
sudo systemctl restart nix-daemon
git clone ssh://git@bitbucket.software.geant.org:7999/rare/rare-nix.git
make install
nix-store --add-fixed sha256 bf-sde-9.7.2.tgz
sudo /nix/var/nix/profiles/RARE/bin/release-manager --activate
