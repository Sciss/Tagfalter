[![Build Status](https://github.com/Sciss/Tagfalter/workflows/Scala%20CI/badge.svg?branch=main)](https://github.com/Sciss/Tagfalter/actions?query=workflow%3A%22Scala+CI%22)

# Tagfalter

This repository contains code for an ongoing art project.
See [Research Catalogue](https://www.researchcatalogue.net/view/1506240/1506241).

(C)opyright 2022 by Hanns Holger Rutz. All rights reserved. This project is released under the
[GNU Affero General Public License](https://github.com/Sciss/Tagfalter/blob/main/LICENSE) v3+ and
comes with absolutely no warranties.
To contact the author, send an e-mail to `contact at sciss.de`.

## building

Builds with sbt against Scala 2.13.

## settings

Assuming Jack runs at 1024 block size and 3 blocks, 48 kHz.

Biphase tests:

    --biphase-f1a 5000 --biphase-f2a 6000 --biphase-f1b 7000 --biphase-f2b 8000 --dec-amp2 0.8

Crypsis (outside):

    --cryp-mod-freq 0.747 --cmp-thresh-in -10 --cryp-mic-amp 100 --cryp-speaker-amp 10

## running via ssh

```
ssh pi@klangpi01.local
```

To avoid killing the process when ssh ends / times out, use
[screen](https://raspi.tv/2012/using-screen-with-raspberry-pi-to-avoid-leaving-ssh-sessions-open):

```
screen bash
cd ~/src/klangnetze/src/
./hanns/tag-falter.sh
```

Followed by `Ctrl-A`, `D` to close the screen terminal. To return to the screen terminal: `screen -r`.

In screen, to scroll back the console output, enter 'Copy mode' via `Ctrl-A`, `Esc`. Then page up/down should
work. To return press `Esc` again.
