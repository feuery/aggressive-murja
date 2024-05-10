FROM ubuntu:jammy
COPY . /src
RUN cd /src; \
    ls -la . ; \
    ./build_murja.sh \
    cd ..

CMD ["/murja_server"]
