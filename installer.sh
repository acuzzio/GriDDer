rm -r dist
cabal install
cp dist/build/GriDDer/GriDDer ~/bin/
# cp dist/build/GriDDer/GriDDer ${PATH%%:*}
echo 'Installed !!!!'
cd src
