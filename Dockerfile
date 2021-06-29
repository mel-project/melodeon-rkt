FROM racket/racket:8.1-full
RUN mkdir cur
#COPY . ./cur
#ENTRYPOINT "bin/bash"
#CMD racket meloc.rkt 
WORKDIR cur
RUN yes | raco pkg install memo
ENTRYPOINT ["racket", "meloc.rkt"]
