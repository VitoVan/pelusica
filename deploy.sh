#!/bin/bash

set -x

if [[ ! -v PELUSICA_VERSION ]]; then
    echo "PELUSICA_VERSION is not set"
    exit 42
fi

rm -rf ./*/${PELUSICA_VERSION}

curl -LO https://github.com/VitoVan/pelusica/releases/download/${PELUSICA_VERSION}/Pelusica-web.zip
unzip Pelusica-web.zip
mv web "${PELUSICA_VERSION}"

rm *.zip

git add .
git commit -m "Deploy to gh-pages"
git push --set-upstream origin gh-pages
