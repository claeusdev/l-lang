# Use a Haskell base image
FROM haskell:9.2.8

# Install needed system tools
RUN apt-get update && apt-get install -y libpq-dev

# Create app directory
WORKDIR /app

# Copy stack config and resolver
COPY . /app

# Download dependencies (for better caching)
RUN stack setup
RUN stack build --install-ghc


# Expose Heroku-assigned port (use env var in CMD)
EXPOSE 3030

# Set default command
CMD stack exec l-lang-exe -- -w
