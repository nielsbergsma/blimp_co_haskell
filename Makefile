.PHONY= update build optim

CABAL_ARGS += --allow-newer=base,template-haskell --with-compiler=wasm32-wasi-ghc --with-hc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs --with-haddock=wasm32-wasi-haddock
RELEASE_CHANNEL := https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/ghcup-wasm-0.0.9.yaml
WASM_BOOTSTRAP := https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh

all: update build optim

js: update-js build-js

update:
	wasm32-wasi-cabal update

repl: update
	wasm32-wasi-cabal repl app -finteractive --repl-options='-fghci-browser -fghci-browser-port=8080'

watch:
	ghciwatch --after-startup-ghci :main --after-reload-ghci :main --watch app/*.hs --debounce 50ms --command 'wasm32-wasi-cabal repl app -finteractive --repl-options="-fghci-browser -fghci-browser-port=8080"'

build:
	wasm32-wasi-cabal build 
	rm -rf dist
	mkdir dist
	cp -r img dist
	cp -r js dist
	cp index.html dist
	npx --yes @tailwindcss/cli --minify -i css/app.css -o dist/css/app.css

	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin app | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output dist/js/ghc_wasm_jsffi.js
	cp -v $(my_wasm) dist/
	
	sed -i -e 's/https:\/\/cdn.jsdelivr.net\/npm\/@bjorn3\/browser_wasi_shim@0.3.0\/dist\/index.js/@bjorn3\/browser_wasi_shim/g' dist/js/index.js
	cd dist && npm install @bjorn3/browser_wasi_shim

	cd dist && npx --yes esbuild --minify --format=esm --bundle js/index.js --outfile=js/index.js --allow-overwrite
	rm dist/js/components.js dist/js/ghc_wasm_jsffi.js dist/js/session.js

optim:
	wasm-opt -all -O2 dist/app.wasm -o dist/app.wasm
	wasm-tools strip -o dist/app.wasm dist/app.wasm

serve:
	http-server dist

clean:
	rm -rf dist-newstyle dist

update-js:
	cabal update --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

build-js:
	cabal build --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg
	cp -v ./dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/*/x/app/build/app/app.jsexe/all.js .
	rm -rf dist
	cp -rv static dist
	bunx --bun swc ./all.js -o dist/index.js

ghcup-update:
	cabal update $(CABAL_ARGS)

ghcup-build: | install-wasm-via-ghcup ghcup-update
	. ~/.ghc-wasm/env && \
		cabal build $(CABAL_ARGS)

install-wasm-via-ghcup:
	curl $(WASM_BOOTSTRAP) | SKIP_GHC=1 sh
	. ~/.ghc-wasm/env && \
		ghcup config add-release-channel $(RELEASE_CHANNEL) && \
		ghcup install ghc --set wasm32-wasi-9.15 -- $$CONFIGURE_ARGS

deploy: 
	npx --yes wrangler pages deploy dist/ --project-name backoffice-hs