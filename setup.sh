#!/bin/bash

brew install haskell-stack
stack setup
stack install hlint stylish-haskell intero ghc-mod
stack test
brew cask install intellij-idea-ce
