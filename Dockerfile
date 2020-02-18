FROM debian:buster

LABEL description="Debian Buster GNU/Linux with Elm"

ARG HOME
ARG USER
ARG UID
ARG ANVIL

RUN sed -i 's/main/main non-free contrib/g' /etc/apt/sources.list
# libtinfo5 : elm dependency
RUN apt update \
    && apt install -y \
		bash \
		libtinfo5 \
		npm \
	&& apt-get clean
RUN adduser --uid ${UID} --disabled-password --home ${HOME} ${USER}

ENV PATH "${HOME}/bin:$PATH"
ENV PATH "${ANVIL}/node_modules/.bin:$PATH"

RUN echo \
	'export PS1="\w $ "'\
	> ${HOME}/.bashrc

RUN mkdir ${HOME}/bin
RUN mkdir ${ANVIL}
WORKDIR ${ANVIL}

COPY elm.json elm-package.json package.json LICENSE ${ANVIL}/
COPY src/ ${ANVIL}/src/
COPY assets/ ${ANVIL}/assets/
COPY tests/ ${ANVIL}/tests/
RUN chown -R ${USER}:${USER} ${ANVIL}

USER ${USER}

RUN npm i

CMD ["npm", "start"]
