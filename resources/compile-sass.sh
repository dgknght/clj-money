#!/bin/bash

echo "Downloading sass..."
SASS_URL=https://github.com/sass/dart-sass/releases/download/1.42.1/dart-sass-1.42.1-linux-x64.tar.gz
curl -L $SASS_URL > target/dart-sass.tar.gz

echo "Unpacking sass..."
tar -xvf target/dart-sass.tar.gz -C target/

echo "Compiling style sheet..."
target/dart-sass/sass -s compressed src/scss/site.scss resources/public/css/site.css

echo "Release script done"
ls resources/public/css
