# placid/project:latest
FROM ubuntu:18.04

# Clean up files to avoid problems with files deleted in the project,
# but remaining in the image, that would confuse 'stack build'.
RUN rm -rf /haskell-starter-kit
COPY ./ /haskell-starter-kit/

WORKDIR /haskell-starter-kit/
RUN stack build

ENV DB_HOST=${DB_HOST}  \
  DB_PORT=${DB_PORT}  \
  DB_NAME=${DB_NAME}  \
  DB_USER=${DB_USER} \
  DB_PASS=${DB_PASS} 

EXPOSE 80
EXPOSE 8080
CMD [ "stack", "exec", "haskell-starter-kit-exe", "server" ]
