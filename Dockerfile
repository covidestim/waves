# Official Julia imagej
FROM julia

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/covidestim/waves" \
      org.label-schema.vendor="Covidestim" \
      maintainer="Marcus Russi <marcus.russi@yale.edu>"

# Install make, to run the makefile
RUN apt-get update && \
  apt-get install -y --no-install-recommends make && \
  rm -rf /var/lib/apt/lists/*

# Copy repo files to /opt/waves. Large files are ignored - see .dockerignore
COPY . /opt/waves

# Make the scripts runnable and install the Julia package dependencies
RUN chmod a+rx /opt/waves/**/*.jl && \
  cd /opt/waves && \
  julia scripts/deps.jl

