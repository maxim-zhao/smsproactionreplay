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
- In the lower position, the interception is enabled but the ROM bank selection is set to the first 16KB, which causes the next interrupt to instead flow to the PAR menu the same as at startup.

To do
-----

- Why does it write to $0068?