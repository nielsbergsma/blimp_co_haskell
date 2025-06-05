clean:
	rm -rf dist/

compile: clean
	mkdir dist
	cp -R js dist/
	cp -R img dist/
	cp index.html dist/

	wasm32-wasi-cabal build backoffice

build@debug: compile
	cp $(shell find dist-newstyle/build -name "*.wasm") dist/app.wasm
	${shell wasm32-wasi-ghc --print-libdir}/post-link.mjs --input dist/app.wasm --output dist/js/ghc_wasm_jsffi.js

	npx --yes tailwindcss -i css/app.css --minify -o dist/css/app.css    
	
build@release: compile
	cp $(shell find dist-newstyle/build -name "*.wasm") dist/app.wasm.0
	${shell wasm32-wasi-ghc --print-libdir}/post-link.mjs --input dist/app.wasm.0 --output dist/js/ghc_wasm_jsffi.js

	env -i GHCRTS=-H64m "$(shell type -P wizer)" --allow-wasi --wasm-bulk-memory true --inherit-env true --init-func _initialize -o dist/app.wasm.1 dist/app.wasm.0
	wasm-opt --low-memory-unused --strip-dwarf --converge -O4 -Oz  dist/app.wasm.1 -o dist/app.wasm.2
	wasm-tools strip -o dist/app.wasm dist/app.wasm.2
	rm dist/app.wasm.*

	npx --yes tailwindcss -i css/app.css --minify -o dist/css/app.css    

	sed -i -e 's/return (await import("node:timers")).setImmediate;//g' dist/js/ghc_wasm_jsffi.js
	sed -i -e 's/https:\/\/cdn.jsdelivr.net\/npm\/@bjorn3\/browser_wasi_shim@0.3.0\/dist\/index.js/@bjorn3\/browser_wasi_shim/g' dist/js/index.js
	
	cd dist && npm install @bjorn3/browser_wasi_shim
	cd dist && npx --yes esbuild --minify --format=esm --bundle js/index.js --outfile=js/index.js --allow-overwrite
	rm dist/js/components.js dist/js/ghc_wasm_jsffi.js dist/js/routing.js dist/js/session.js

deploy: 
	sed -i -e 's/backendUrl = "http:\/\/localhost:5002"/backendUrl = "TODO"/g' dist/index.html
	npx --yes wrangler pages deploy dist/ --project-name backoffice-hs

serve:
	node local/server.js