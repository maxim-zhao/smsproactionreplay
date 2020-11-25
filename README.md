SMS Pro Action Replay
=====================

Disassembly of the Master System Pro Action Replay.

See https://www.smspower.org/forums/13877-SMSPARDisassembly for discussion.

To do
-----

- It writes seemingly uninitialised data to $2000 and $6000 to switch from PAR ROM to cart ROM? 
- Why does it write to $0068? 
- How does it execute the generated code? 
- Why does the generated code jump to $0035? 
