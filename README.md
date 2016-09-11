yesod-fast-devel
================
- - -
Fast live-reloading for yesod applications.

_Forked from [`fast-yesod-devel`](https://github.com/Codas/fast-yesod-devel)_

## Install
```
$ git clone git@github.com:haskellbr/yesod-fast-devel
$ stack install
```

## Usage
- Have [browser-sync](https://www.browsersync.io/) installed (`npm install -g browser-sync`)
- Have `foreign-store` on the cabal file (maybe [`cabal-add
  foreign-store`](https://github.com/yamadapc/cabal-add))

```
$ yesod-fast-devel
```

## Example using Spock
The [git-issues](https://github.com/yamadapc/git-issues) project shows how you can use this with Spock applications.

## License
This code is published under the BSD-3 license.
