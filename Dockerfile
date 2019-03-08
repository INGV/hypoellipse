FROM ubuntu:18.10

LABEL maintainer="Valentino Lauciani <valentino.lauciani@ingv.it>"

ENV DEBIAN_FRONTEND=noninteractive
ENV INITRD No
ENV FAKE_CHROOT 1

# install packages
RUN apt-get update \
    && apt-get dist-upgrade -y --no-install-recommends \
    && apt-get install -y \
        vim \
	git \
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
     && echo "" >> /root/.bashrc 

# Copy source
WORKDIR /opt/ 
RUN mkdir /opt/hypoellipse 
COPY tot-pasq.f /opt/hypoellipse 
COPY params.inc /opt/hypoellipse 

# COpy entrypoint
COPY entrypoint.sh /opt/
RUN chmod 755 /opt/entrypoint.sh

# Compile
WORKDIR /opt/hypoellipse
RUN fort77 -v -c tot-pasq.f \
     && gcc -o hypoellipse tot-pasq.o -static -lf2c -lm

# Run Hypoc
ENTRYPOINT ["/opt/entrypoint.sh"]
