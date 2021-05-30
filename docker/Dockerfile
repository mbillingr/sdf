# Docker image that contains MIT/GNU Scheme and SDF's accompanying
# software. Enter the managed environment by running the following
# command inside the container:
#   > scheme --load /sdf/manager/load.scm

FROM schemers/mit-scheme:10

RUN apt-get update && apt-get install -y \
    rlwrap \
    vim \
    && rm -rf /var/lib/apt/lists/*

ADD http://groups.csail.mit.edu/mac/users/gjs/sdf.tgz /tmp

RUN tar xzvf /tmp/sdf.tgz

CMD ["bash"]

