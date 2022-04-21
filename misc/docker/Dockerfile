FROM debian
MAINTAINER Frederic LOUI <frederic.loui@@renater.fr>

RUN apt-get update
RUN apt-get -f -y dist-upgrade
RUN apt-get -f -y install wget unzip net-tools libpcap-dev dpdk openvswitch-switch ethtool default-jre-headless
RUN apt-get clean

RUN mkdir -p /opt/freertr
RUN mkdir -p /opt/freertr/bin
RUN mkdir -p /opt/freertr/src
RUN mkdir -p /opt/freertr/run

WORKDIR /opt/freertr/

RUN wget http://www.freertr.org/rtr.zip
RUN wget http://www.freertr.org/rtr.jar
RUN wget http://www.freertr.org/rtr.tar
RUN mv ./rtr.jar ./bin
RUN unzip ./rtr.zip -d /opt/freertr/src
WORKDIR /opt/freertr/bin
RUN tar xvf ../rtr.tar

COPY . /opt/freertr/

WORKDIR /opt/freertr/

VOLUME ./run:/opt/freertr/run

ENV FREERTR_HOSTNAME=freertr  \
    FREERTR_INTF_LIST="eth2/20010/20011"

CMD ./scripts/freertr.sh -i "$FREERTR_INTF_LIST" -r $FREERTR_HOSTNAME
