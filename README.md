# Conway Game of life

This is an implementation of a visual sandbox to experiment with Conway's game of life.
This is been my playground to learn Elm 0.18, then 0.19.

Any advice on how I designed the piece is welcome.

## Usage

```
docker build . \
    --build-arg HOME=/conway \
    --build-arg UID \
    --build-arg USER=conway \
    --build-arg ANVIL=/conway/anvil \
    -t conway

# Run the app
docker run --rm -it conway

# Run unit tests
docker run --rm -it conway npm test

# Access coverage report
docker run --rm -it conway npm run coverage
```
