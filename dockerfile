FROM ocaml/opam:debian-ocaml
WORKDIR /app
COPY . /app
# Install needed build tools (e.g., menhir, dune):
RUN opam install dune menhir
# Optional: build commands go here
CMD ["bash"]