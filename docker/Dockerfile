# Builder stage: compile LispE from source
FROM ubuntu:22.04 AS builder

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    build-essential \
    g++ \
    make \
    python3 \
    libboost-regex-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libfltk1.3-dev \
    libfontconfig1-dev \
    libgmp-dev \
    libldap2-dev \
    libmpg123-dev \
    libpython3.10-dev \
    libsndfile1-dev \
    libsqlite3-dev \
    libx11-dev \
    libxcursor-dev \
    libxext-dev \
    libxfixes-dev \
    libxft-dev \
    libxinerama-dev \
    libxml2-dev \
    libxrender-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

COPY . .

RUN python3 configure.py && make all libs

# Final stage: runtime image
FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

# Update packages and install system dependencies
RUN apt-get update && apt-get install -y \
    libboost-regex-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libfltk1.3-dev \
    libfontconfig1-dev \
    libgmp-dev \
    libldap2-dev \
    libmpg123-dev \
    libpython3.10 \
    libsndfile1-dev \
    libsqlite3-dev \
    libx11-dev \
    libxcursor-dev \
    libxext-dev \
    libxfixes-dev \
    libxft-dev \
    libxinerama-dev \
    libxml2-dev \
    libxrender-dev \
    x11-apps \
    xauth \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Create a directory for the interpreter and its libraries
WORKDIR /app

# Define the LISPELIBS environment variable
ENV LISPEPATH=/app/

# Copy the compiled interpreter and libraries from the builder stage
COPY --from=builder /build/bin/jag /app/
COPY --from=builder /build/bin/libgui.so /app/
COPY --from=builder /build/bin/liblispe.a /app/
COPY --from=builder /build/bin/liblispe_blas.so /app/
COPY --from=builder /build/bin/liblispe_curl.so /app/
COPY --from=builder /build/bin/liblispe_sqlite.so /app/
COPY --from=builder /build/bin/liblispe_transducer.so /app/
COPY --from=builder /build/bin/liblispe_xml.so /app/
COPY --from=builder /build/bin/pylispe.so /app/
COPY --from=builder /build/bin/lispe /usr/local/bin/lispe

ENV LD_LIBRARY_PATH=/app:/usr/local/lib:$LD_LIBRARY_PATH

# Define the default command to run the LISPE interpreter
ENTRYPOINT ["/usr/local/bin/lispe"]

