FROM opensuse/leap:15.1

WORKDIR /app

# Due to Docker's caching model, only append RUNs to the end of this
# during "production".
# Minmizie RUNs before a new "release".

RUN zypper --non-interactive install nasm git-core libcrypto43 libssl45 binutils make

RUN ln -s /usr/lib64/libssl.so.1.1 /usr/lib64/libssl.so
RUN ln -s /usr/lib64/libcrypto.so.1.1 /usr/lib64/libcrypto.so

RUN git clone --single-branch --depth 1 --branch v7.5 https://github.com/racket/racket

# build deps
RUN zypper --non-interactive install gcc libglib-2_0-0 libgmodule-2_0-0

WORKDIR /app/racket
RUN make PKGS="base" PREFIX="/usr/" unix-style
RUN raco pkg install --auto compiler-lib

# graph-lib deps; graph lib sucks and we need to remove it.
RUN zypper --non-interactive install libgobject-2_0-0 fontconfig libcairo2 libpango-1_0-0 libjpeg62
RUN raco pkg install --auto graph-lib
RUN raco pkg install --auto plai-lib

# For course staff who need to build the website.
# RUN raco pkg install --auto graph-doc

WORKDIR /app/workspace