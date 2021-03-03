SMS Pro Action Replay
=====================

Disassembly of the Master System Pro Action Replay.

See https://www.smspower.org/forums/13877-SMSPARDisassembly and https://www.smspower.org/forums/1166-ActionReplayGameGenie for discussion.

How it works
------------

The PAR (Pro Action Replay) has a PAL (programmable array logic) chip which is responsible for swapping between the ROM/RAM on the PAR and the ROM on the game cartridge. This acts to interpret various control signals from the cartridge slot, the position of the switch (see below) and writes to the ROM area made by the software. These control two things:

1. The enablement of the cartridge ROM vs. the PAR ROM/RAM
2. Which of the PAR ROM's two 16KB banks are used

There is a switch on the side of the PAR device. This has three positions which trigger three types of behaviour by the device:

1. In the upper position, game cheats are enabled. The PAL enables the PAR RAM/ROM whenever an INT signal is triggered. This causes interception of the frame (and line) interrupts, and the PAR ROM is executed at address $0038 to handle the interrupt. This jumps to some generated code in PAR RAM which applies any active cheats and then re-enables the game ROM, such that execution flows into its interrupt handler. Thus it may cause timing problems in some games.
2. In the middle position, this interception is disabled; the PAR is inactive.
3. In the lower position, the device will jump to the game menu when Pause is pressed. The PAL enables the PAR RAM/ROM whenever both an NMI and an INT signal are triggered. This also triggers a selection of the first 16KB bank of PAR ROM. The INT handler at $0038 in this ROM bank will simply jump to the start of its ROM when a frame/line interrupt happens.

The software is able to force the controls by writing to special addresses. Writes to $6000..$7fff will select the cartridge ROM, and write to $2000..$3fff will select the upper 16KB PAR ROM bank. Code is run from system RAM to enable switching back to the game.

Oddities
--------

- When searching for cheats, it writes a value to unused ROM address $0068. This seems to do nothing, but perhaps was a useful debugging aid on a RAM-based development board.
- There are many left-over debugging tools in the ROM, inaccessible from the menus. For example, there is a memory viewer tool.
- There are some chunks of the Master System BIOS in the ROM, including its font, but they are unused.
- The RAM could be used better to allow slightly improved cheat-finding. There is not enough PAR RAM to capture all 8KB of system RAM when searching for cheats; so instead you must go through more iterations of the cheat finder until it can clear space to cover it all (for the more tricky search algorithms). It has storage for 1792 candidate RAM locations at once, but it could fit a few hundred more.
