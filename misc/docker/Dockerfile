FROM alpine
MAINTAINER Frederic LOUI <frederic.loui@@renater.fr>

RUN mkdir -p /opt/freertr
RUN mkdir -p /opt/freertr/bin
RUN mkdir -p /opt/freertr/src
RUN mkdir -p /opt/freertr/run

WORKDIR /opt/freertr/

RUN wget http://freerouter.nop.hu/rtr.zip
RUN wget http://freerouter.nop.hu/rtr.jar
RUN wget http://freerouter.nop.hu/rtr.tar
RUN mv ./rtr.jar ./bin
RUN unzip ./rtr.zip -d /opt/freertr/src
WORKDIR /opt/freertr/bin
RUN tar xvf ../rtr.tar

COPY . /opt/freertr/

RUN apk update && apk upgrade && apk add --no-cache libpcap-dev ethtool openjdk11-jre-headless

VOLUME ./run:/opt/freertr/run

ENV FREERTR_HOSTNAME=freertr  \
    FREERTR_INTF_LIST="eth2/20010/20011"

CMD ./scripts/freertr.sh -i "$FREERTR_INTF_LIST" -r $FREERTR_HOSTNAME
