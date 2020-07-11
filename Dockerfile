FROM lupusmichaelis/alpine-elm:1.0.1

LABEL description="Conway's Game of Life"

USER root

COPY elm.json elm-package.json package.json LICENSE ${ANVIL}/
COPY src/ ${ANVIL}/src/
COPY assets/ ${ANVIL}/assets/
COPY tests/ ${ANVIL}/tests/
