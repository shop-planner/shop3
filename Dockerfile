FROM clfoundation/sbcl:latest

COPY . /shop

ENV CL_SOURCE_REGISTRY=/shop/shop3/:/shop/jenkins/ext//:

RUN sbcl --script /shop/jenkins/compile-shop3.lisp

RUN sbcl --eval '(require :asdf)' --eval '(asdf:load-system "shop3")' --eval '(save-lisp-and-die "/shop/sbcl-shop3.core")'

RUN echo "#!/bin/bash\nsbcl --core /shop/sbcl-shop3.core" > /usr/bin/shop3

RUN chmod +x /usr/bin/shop3
