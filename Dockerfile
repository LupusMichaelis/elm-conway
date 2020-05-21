FROM lupusmichaelis/alpine-elm-0.19

LABEL description="Conway's Game of Life"

ARG USER
ARG UID
ARG GID

USER root

COPY elm.json elm-package.json package.json LICENSE ${ANVIL}/
COPY src/ ${ANVIL}/src/
COPY assets/ ${ANVIL}/assets/
COPY tests/ ${ANVIL}/tests/
RUN chown -R ${UID}:${GID} ${ANVIL}

USER ${USER}

RUN npm i

CMD ["npm", "start"]
