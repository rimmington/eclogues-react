#!/bin/bash

set -e

stack build

mkdir -p js-build
echo "(function(global, React, ReactDOM) {" > js-build/all.js
cat $(stack path --local-install-root)/bin/eclogues-react.jsexe/all.js >> js-build/all.js
echo "})(window, window['React'], window['ReactDOM']);" >> js-build/all.js
sed -i 's/goog.provide.*//' js-build/all.js
sed -i 's/goog.require.*//' js-build/all.js
ccjs js-build/all.js --compilation_level=ADVANCED_OPTIMIZATIONS > js-build/all.min.js
