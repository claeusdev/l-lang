FROM haskell:9.8.4

WORKDIR /app

# Copy stack files first to leverage Docker layer cache
COPY stack.yaml package.yaml /app/

# Install compiler + deps
RUN stack setup
RUN stack build --only-dependencies

# Copy the rest of the code
COPY . /app/

# Build the executable
RUN stack build

EXPOSE 3000

CMD stack exec l-lang-exe -- -w

