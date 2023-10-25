#!/bin/sh

ARCH=-`uname -m`
IMG=`cd ../../binImg/;pwd`

qemu-img create $IMG/rtr$ARCH.dsk 2G
mkdir ../../binMnt
sudo ./i.sh $ARCH
rm -rf ../../binMnt
qemu-img convert -O qcow2 -c $IMG/rtr$ARCH.dsk $IMG/rtr$ARCH.qcow2
qemu-img convert -O vmdk -o subformat=streamOptimized $IMG/rtr$ARCH.dsk $IMG/rtr$ARCH.vmdk

SIZ=`stat --printf="%s" $IMG/rtr$ARCH.qcow2`
VER=`java -jar ../../src/rtr.jar show version number | dos2unix | head -n 1`
SUM=`md5sum $IMG/rtr$ARCH.qcow2 | awk '{ print $1 }'`
cat > $IMG/rtr$ARCH.gns3a << EOF
{
    "name": "freeRouter",
    "category": "router",
    "description": "networking swiss army knife - it speaks routing protocols, and (re)encapsulates packets on interfaces",
    "vendor_name": "freeRouter",
    "vendor_url": "http://www.freertr.org/",
    "documentation_url": "http://www.freertr.org/",
    "product_name": "freeRouter",
    "product_url": "http://www.freertr.org/",
    "registry_version": 3,
    "status": "stable",
    "maintainer": "GNS3 Team",
    "maintainer_email": "developers@gns3.net",
    "usage": "there is no default password. a default configuration is present.",
    "port_name_format": "ethernet{port1}",
    "qemu": {
        "adapter_type": "virtio-net-pci",
        "adapters": 8,
        "ram": 1024,
        "hda_disk_interface": "ide",
        "arch": "x86_64",
        "console_type": "telnet",
        "kvm": "require"
    },
    "images": [
        {
            "filename": "rtr$ARCH.qcow2",
            "version": "$VER",
            "md5sum": "$SUM",
            "filesize": $SIZ,
            "download_url": "http://www.freertr.org/",
            "direct_download_url": "http://dl.nop.hu/rtr$ARCH.qcow2"
        }
    ],
    "versions": [
        {
            "name": "$VER",
            "images": {
                "hda_disk_image": "rtr$ARCH.qcow2"
            }
        }
    ]
}
EOF

hashFile()
{
sum=`sha1sum $1 | awk '{ print $1 }'`
echo SHA1\($2\)= $sum >> $IMG/rtr$ARCH.mf
}

echo -n "" > $IMG/rtr$ARCH.mf
hashFile $IMG/rtr$ARCH.qcow2 rtr$ARCH.qcow2
hashFile $IMG/rtr$ARCH.vmdk rtr$ARCH.vmdk
hashFile package.ovf package.ovf
hashFile package.ver package.ver
hashFile package.vsh package.vsh
hashFile package.yaml package.yaml
tar cf $IMG/rtr$ARCH.ova package.ovf package.ver package.vsh package.yaml
echo `cd $IMG/;tar rf rtr$ARCH.ova rtr$ARCH.qcow2`
echo `cd $IMG/;tar rf rtr$ARCH.ova rtr$ARCH.vmdk`
echo `cd $IMG/;tar rf rtr$ARCH.ova rtr$ARCH.mf`
ls -l $IMG/
