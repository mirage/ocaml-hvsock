FROM alpine
RUN apk update && apk add opam alpine-sdk bash
RUN opam init --comp=4.06.1 --switch=4.06.1 -y

# We don't want to rebuild this whole image when the source code changes
# so we only copy in the package metadata.
COPY hvsock.opam /src/hvsock.opam
RUN opam pin add hvsock /src/ -n
RUN opam config exec -- opam depext hvsock -y
RUN opam config exec -- opam install hvsock --deps-only

WORKDIR /src
