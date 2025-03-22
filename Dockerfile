# Use a lightweight Node.js image as base
FROM node:18-alpine

# Install necessary tools
RUN apk add --no-cache curl bash

# Set working directory
WORKDIR /app

# Copy project files
COPY . .

# Install Elm 0.19.1
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz && \
    gunzip elm.gz && \
    chmod +x elm && \
    mv elm /usr/local/bin/

# Build the Elm application
RUN elm make --optimize src/Main.elm

# Install a simple HTTP server
RUN npm install -g http-server

# Expose port 8000
EXPOSE 8000

# Start the HTTP server
CMD ["http-server", "-p", "8000"] 