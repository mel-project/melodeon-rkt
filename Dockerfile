FROM racket/racket:8.1-full
RUN mkdir cur
WORKDIR cur
RUN yes | raco pkg install memo
ENTRYPOINT ["racket", "meloc.rkt"]
