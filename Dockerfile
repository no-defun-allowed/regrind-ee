FROM debian:bookworm
RUN apt update
RUN apt install -y build-essential curl git sbcl
# Build SWCL
RUN git clone https://github.com/no-defun-allowed/swcl /root/swcl
WORKDIR /root/swcl
RUN ./make.sh
WORKDIR /root/
# Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))" --quit
# Install OMRN and Regrind EE
RUN git clone -b hc-table https://github.com/telekons/one-more-re-nightmare /root/quicklisp/local-projects/one-more-re-nightmare
RUN git clone https://github.com/no-defun-allowed/regrind-ee /root/quicklisp/local-projects/regrind-ee
ENTRYPOINT ["/root/swcl/run-sbcl.sh", "--eval", "(ql:quickload :regrind-ee)", "--eval", "(regrind-ee:start-regrind 4)", "--eval", "(regrind-ee:start-ui)", "--eval", "(loop (sleep 5))"]
EXPOSE 8080
