FROM ocaml/opam2
MAINTAINER Kang HJ <hjkang.2018@phdis.smu.edu.sg>

RUN sudo apt-get update
RUN sudo apt-get install -y git make autoconf

RUN git clone https://github.com/kanghj/coccinelle
WORKDIR coccinelle
RUN opam update
RUN git checkout java

RUN ./autogen
RUN opam config exec ./configure
RUN  sudo make install