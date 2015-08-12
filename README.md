# CloudFlare Paper Club Presentation on Monads

For the Paper Club at CloudFlare I gave a talk on [Monads for functional
programming](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)
by Philip Wadler from the University of Glasgow. This repository contains the
examples used in the presentation and the source for the slides.

To build the presentation:
- `git submodule update --init --recursive`
- `cabal install pandoc`
- `pandoc -t revealjs -V theme=moon -V transition=linear --slide-level=2 -s monads.md -o monads.html`
- `rsync -avzP monads.html reveal.js example.com:/var/www`
