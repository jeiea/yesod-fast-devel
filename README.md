yesod-fast-devel
================
Fast live-reloading for yesod applications.

_Forked from [`fast-yesod-devel`](https://github.com/Codas/fast-yesod-devel)_

# Install
```
$ git clone git@github.com:haskellbr/yesod-fast-devel
$ stack install
```

# Usage
```
$ yesod-fast-devel
```

# Why not just use `yesod devel` or `DevelMain.hs`?
Because yesod devel takes longer to reload applications and DevelMain has no
autoreload feature.
I could just modify DevelMain.hs, but then I had to modify every single
scaffold, an external binary is simply more convenient.

- - -

# Current state
This is still more or less experimental. That said, this script should not eat
your pet or start a nuclear war. If it does, feel free to send a pull request.

It does has a fairly high CPU utilization (20% of one core on my MacBook
Pro). If anyone knows why that might be, please help?
