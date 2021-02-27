SMS Pro Action Replay
=====================

Disassembly of the Master System Pro Action Replay.

See https://www.smspower.org/forums/13877-SMSPARDisassembly and https://www.smspower.org/forums/1166-ActionReplayGameGenie for discussion.

How it works
------------

The PAR has a PAL (programmable logic array) which is responsible for swapping between the ROM/RAM on the PAR and the ROM on the game cartridge. This has two registers:

- To select between the first and second 16KB chunks of ROM. The default is the first, the second is chosen by writing any value to address $2000
- To select between the PAR RAM/ROM and the game cartridge in at least the first 24KB of address space (upper regions are not accessed). The default is the PAR RAM/ROM, the cartridge memory is chosen (or swapped?) by writing any value to address $6000

There is a switch on the side of the PAR device. This has three positions which trigger three types of behaviour by the device:

- In the upper position, game cheats are enabled. The PAL seems to then enable the PAR RAM/ROM whenever address $0038 is read/executed (perhaps by treating it the same as $2000). This causes interception of the frame (and line) interrupts, and the device ROM is executed at this address. This jumps to some generated code in PAR RAM which applies any active cheats and then re-enables the game ROM, such that execution flows into its interrupt handler. Thus it may cause timing problems in some games.
- In the middle position, this interception is disabled.
- In the lower position, the device will jump to the game menu when Pause is pressed. Presumably this means that read/execute of $0066 triggers a selection of the PAR ROM/RAM again (similar to intercepting $0038 above). My best guess is that this also triggers a selection of the first 16KB bank of PAR ROM, as well as interception of $0038 as in the upper position; this lower 16KB will simply jump to the start of its ROM when a frame/line interrupt happens.

To do
-----

- Why does it write to $0068?