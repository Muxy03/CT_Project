FROM ocaml/opam:ubuntu-24.04-ocaml-5.4

USER root
RUN apt-get update && apt-get install -y --no-install-recommends llvm clang make

USER opam
WORKDIR /home/opam/app

COPY --chown=opam:opam . .

RUN opam install dune

RUN cd MiniFun && opam exec -- dune build
RUN cd MiniImp && opam exec -- dune build

CMD ["bash"]
