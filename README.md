SMS Pro Action Replay
=====================

Disassembly of the Master System Pro Action Replay.

See http://www.smspower.org/htdocs/forums/viewtopic.php?t=13877 for discussion.

To do
-----

- It writes seemingly uninitialised data to $2000 and $6000 to switch from PAR ROM to cart ROM? 
- Why does it write to $0068? 
- How does it execute the generated code? 
- Why does the generated code jump to $0035? 
