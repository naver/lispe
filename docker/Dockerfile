# Use Ubuntu 22.04 as the base image
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

# Copy the interpreter and its libraries from the bin directory to the container
COPY bin/jag /app/
COPY bin/libgui.so /app/
COPY bin/liblispe.a /app/
COPY bin/liblispe_blas.so /app/
COPY bin/liblispe_curl.so /app/
COPY bin/liblispe_sqlite.so /app/
COPY bin/liblispe_transducer.so /app/
COPY bin/liblispe_xml.so /app/
COPY bin/pylispe.so /app/
COPY bin/lispe /usr/local/bin/lispe

ENV LD_LIBRARY_PATH=/app:/usr/local/lib:$LD_LIBRARY_PATH

# Define the default command to run the LISPE interpreter
ENTRYPOINT ["/usr/local/bin/lispe"]
