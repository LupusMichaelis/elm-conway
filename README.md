# Conway Game of life

This is an implementation of a visual sandbox to experiment with Conway's game of life.
This is been my playground to learn Elm 0.18, then 0.19.

Any advice on how I designed the piece is welcome.

## Usage

```
{
	echo LP_DEV_UID=$(id -u)
	echo LP_DEV_GID=$(id -g)
	echo LP_DEV_USER=conway
	echo ANVIL=/conway/anvil
} > .env

docker build . \
    -t conway

# Run the app
docker run --env-file .env --rm -it conway

# Run unit tests
docker run --env-file .env --rm -it conway npm test

# Access coverage report
docker run --env-file .env --rm -it conway npm run coverage

# For development sessions:
docker run --env-file .env --rm -it \
	-v $PWD/src:/conway/anvil/src:rw \
	-v $PWD/tests:/conway/anvil/tests \
	conway

docker run --env-file .env --rm -it \
	-v $PWD/src:/conway/anvil/src:rw \
	-v $PWD/tests:/conway/anvil/tests \
	conway npm test
```
