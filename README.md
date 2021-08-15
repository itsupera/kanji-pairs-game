Kanji Pairs Game
=================

A game where you match kanjis together to form jukugos (2 kanji words).

[Try it here](http://itsupera.co/kanjipairs.html)

Setup
------

[Install Elm 0.19](https://guide.elm-lang.org/install/elm.html)

On Linux you can do this:
```bash
curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
gunzip elm.gz
chmod +x elm
sudo mv elm /usr/local/bin/
```

For development:
```bash
elm reactor
x-www-browser http://localhost:8000/src/Main.elm
```

Build for production:
```bash
elm make --optimize src/Main.elm
x-www-browser index.html
```