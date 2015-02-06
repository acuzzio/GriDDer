cd ..
rm -r dist
cabal install
cp dist/build/GriDDer/GriDDer ~/bin/
echo 'Installed !!!!'
cd src
