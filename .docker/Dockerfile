################################################################################
FROM lupusmichaelis/alpine-elm:1.2.1 AS dev

USER root
COPY ../LICENSE $ANVIL/

RUN lupusmichaelis-packaging-add uglify-js

VOLUME $ANVIL

################################################################################
FROM dev AS builder

LABEL description="Conway's Game of Life"

# NPM subsystem
COPY ../package.json $ANVIL/
RUN npm install

# Source
COPY ../public/ $ANVIL/public/
COPY ../src/ $ANVIL/src/
COPY ../assets/ $ANVIL/assets/
COPY ../tests/ $ANVIL/tests/

# Elm stuff
COPY ../elm.json ../elm-package.json $ANVIL/
RUN elm make src/Conway.elm --optimize --output=$ANVIL/public/conway.js
RUN uglifyjs $ANVIL/public/conway.js \
	--compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" \
	| uglifyjs --mangle --output $ANVIL/public/conway.min.js

################################################################################
FROM scratch AS image

COPY --from=builder /home/anvil/public/index.html /
COPY --from=builder /home/anvil/assets/ /assets/
COPY --from=builder /home/anvil/public/conway.min.js /

#COPY --from=builder /home/anvil/combined.html /index.html

################################################################################
FROM lupusmichaelis/alpine-lighttpd-secure:1.2.3 AS secure-static

COPY ../.docker/security-header.conf $LIGHTTPD_CONF_DIR/security-header.conf

COPY --from=image /index.html $LIGHTTPD_WWW_ROOT/
COPY --from=image /conway.min.js $LIGHTTPD_WWW_ROOT/
COPY --from=image /assets $LIGHTTPD_WWW_ROOT/assets
#COPY --from=image /conway.min.js $LIGHTTPD_WWW_ROOT/
