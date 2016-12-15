FROM haskell:8.0.1
WORKDIR /opt/server
RUN cabal update
COPY ./greater-prompt.cabal /opt/server/greater-prompt.cabal
RUN cabal install --only-dependencies -j4
COPY . /opt/server
RUN cabal build
RUN groupadd -r greater-prompt-server && useradd -r -g greater-prompt-server greater-prompt-server
USER greater-prompt-server
CMD ["./dist/build/greater-prompt-server/greater-prompt-server"]
