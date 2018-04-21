FROM ubuntu:18.04
ARG DEBIAN_FRONTEND=noninteractive
RUN apt update
RUN apt install -y --force-yes texlive-xetex pandoc
RUN apt install -y --force-yes curl wget
RUN curl -L https://github.com/hbin/top-programming-fonts/raw/master/install.sh | bash
ADD idris_for_haskell_developers /src/idris_for_haskell_developers
ADD idris_talk_slides.md /src/
WORKDIR /src
RUN pandoc --latex-engine=xelatex --variable monofont="Menlo" -t beamer -s ./idris_talk_slides.md -o ./idris_talk_slides.pdf
