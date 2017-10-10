FROM debian:stretch

LABEL maintainer="Valentino Lauciani <valentino.lauciani@ingv.it>"

ENV DEBIAN_FRONTEND=noninteractive
ENV INITRD No
ENV FAKE_CHROOT 1
#ENV NLL_BASEURL=http://alomax.free.fr/nlloc/soft7.00/tar/
#ENV NLL_FILENAME=NLL7.00_src.tgz

# To compile nll
#ENV MYBIN=/opt/nll/bin

# install packages
RUN apt-get update \
    && apt-get dist-upgrade -y --no-install-recommends \
    && apt-get install -y \
        vim \
	git \
	telnet \
        dnsutils \
        cmake \
        wget \
	curl \
        gcc \
        make \
        build-essential \
        ftp \
        fort77

# Set 'root' pwd
WORKDIR /opt
RUN echo root:toor | chpasswd

# Set .bashrc
WORKDIR /opt
RUN echo "" >> /root/.bashrc \
     && echo "##################################" >> /root/.bashrc \
     && echo "alias ll='ls -l --color'" >> /root/.bashrc \
     && echo "" >> /root/.bashrc \
     && echo "export LC_ALL=\"C\"" >> /root/.bashrc \
     && echo "" >> /root/.bashrc \

# Get hypoellipse
WORKDIR /opt
RUN wget https://pubs.usgs.gov/of/1999/ofr-99-0023/HYPOELLIPSE_UNIX_Downloads.tar.gz \
     && tar xvzf HYPOELLIPSE_UNIX_Downloads.tar.gz

# For Debian version?!?!?!
WORKDIR /opt
RUN wget http://jclahr.com/science/software/hypoellipse/hypoel/unix_version/source/linux/hypoe.c \
     && wget http://jclahr.com/science/software/hypoellipse/hypoel/unix_version/source/linux/listen_serv.c \
     && wget http://jclahr.com/science/software/hypoellipse/hypoel/unix_version/source/linux/makefile \
     && wget http://jclahr.com/science/software/hypoellipse/hypoel/unix_version/source/linux/setup_server.c \
     && wget http://jclahr.com/science/software/hypoellipse/hypoel/unix_version/source/linux/squish_uacal.c
