project = deptree
setting_file = ./config/settings.json

build:
	docker run -v ${PWD}:/workspace -t heroku-haskell /bin/bash -xc 'cd /workspace/ \
	&& cabal update \
	&& cabal --sandbox-config-file=./heroku.cabal.sandbox.config sandbox --sandbox=/workspace/.heroku-cabal-sandbox init \
	&& cabal --sandbox-config-file=./heroku.cabal.sandbox.config install --force-reinstall --dependencies-only \
	&& cabal --sandbox-config-file=./heroku.cabal.sandbox.config configure --builddir=./dist-heroku \
	&& cabal --sandbox-config-file=./heroku.cabal.sandbox.config build ${project} --builddir=./dist-heroku \
	&& strip --strip-unneeded ./dist-heroku/build/${project}/${project}'
	mkdir -p ./dist/build/${project}
	cp ./dist-heroku/build/${project}/${project} ./dist/build/${project}/${project}

run:
	sed -i -e s/ENV_PORT/${PORT}/ ${setting_file}
	cat ${setting_file}
	./dist/build/${project}/${project}
