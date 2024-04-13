FROM ubuntu:latest
COPY . /src
RUN cd /src; \
    ls -la . ; \
    ./build_murja.sh \
    cd ..

CMD ["/murja_server"]
