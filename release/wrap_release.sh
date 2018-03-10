#!/bin/sh

set -e

(cat release/wrapper.prefix; cat resources/public/js/compiled/mdparser.min.js; cat release/wrapper.suffix) > release/md-parser.min.js

echo "Packed release/md-parser.min.js"
