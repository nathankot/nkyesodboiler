FROM haskell:7.8.4

RUN ["apt-get", "update"]
RUN cabal update

# Put cabal bin's in PATH
ENV PATH /root/.cabal/bin:$PATH

# Get yesod
RUN cabal install yesod-bin -j

# Get Postgresql
RUN apt-get install -y libpq-dev python-dev

# Use the frozen dependencies in cabal.config as a marker
ADD ./cabal.config /opt/server/cabal.config
RUN cd /opt/server && cabal sandbox init
RUN cabal update

# Cache large dependencies that have been frozen
RUN cd /opt/server && cabal install -j base --disable-documentation
RUN cd /opt/server && cabal install -j yesod --disable-documentation
RUN cd /opt/server && cabal install -j classy-prelude --disable-documentation
RUN cd /opt/server && cabal install -j classy-prelude-conduit --disable-documentation
RUN cd /opt/server && cabal install -j classy-prelude-yesod --disable-documentation
RUN cd /opt/server && cabal install -j esqueleto --disable-documentation
RUN cd /opt/server && cabal install -j persistent-postgresql --disable-documentation
RUN cd /opt/server && cabal install -j aws --disable-documentation

# Get third-party packages
ADD ./server.cabal /opt/server/server.cabal
RUN cabal update # Do another update when .cabal file has new deps
RUN cd /opt/server && cabal install --only-dependencies -j --disable-documentation

# Custom entry point
RUN apt-get install -y netcat
COPY entrypoint.sh /
ENTRYPOINT ["/entrypoint.sh"]

WORKDIR /opt/server

EXPOSE 80 443

# Install application
ADD . .
RUN cabal install
RUN cabal clean
RUN cabal configure && cabal build

# Run it
CMD dist/build/server/server
