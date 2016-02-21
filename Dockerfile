# This file was generated from the README.md in the GitHub repository.
FROM erlang:18

RUN cd /usr/local/lib/erlang/lib \
    && git clone https://github.com/rvirding/lfe.git \
    && cd /usr/local/lib/erlang/lib/lfe \
    && git checkout v0.10.1 \
    && make compile install
RUN apt-get update \
    && apt-get install -y inotify-tools \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY files/lfe-watch.sh /usr/local/bin/lfe-watch.sh
RUN chmod +x /usr/local/bin/lfe-watch.sh

WORKDIR /workdir
CMD ["lfe-watch.sh"]
COPY files/dot_erlang /root/.erlang
COPY files/shell /usr/local/bin/shell
RUN chmod +x /usr/local/bin/shell