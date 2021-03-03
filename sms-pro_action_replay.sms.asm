;==============================================================
; WLA-DX banking setup
;==============================================================
.memorymap
defaultslot 0
slotsize $4000 ; ROM
slot 0 $0000
slotsize $4000 ; RAM
slot 1 $4000
slotsize $4000 ; ROM
slot 2 $8000
slotsize $2000 ; RAM
slot 3 $c000
.endme

.rombankmap
bankstotal 2
banksize $4000
banks 2
.endro


; PAR RAM
.define RAM_START $4000
.define TRAINER_CODES_SIZE $1bff ; just under 8KB = <2K codes
.enum RAM_START export
  Unused1                          dsb 2
  BUTTONS_PRESSED                  db      ; --21RLDU
  Unused2                          dsb 1
  MEMORY_DUMP_CURRENT_ADDRESS      dw      ; pointer to RAM being dumped
  MEMORY_DUMP_ADDRESS_DELTA        dw      ; delta applied during memory dump scrolling
  DRAW_LARGE_SCRATCH               db      ; used for holding delta between tile indices when drawing
  HEX_ENTRY_XY                     dw      ; x/y
  CURRENT_CODE_VALUE_POINTER       dw      ; pointer to current code's value
  HEX_ENTRY_VALUE                  db      ; the value being entered
  CODE_ENTRY_CURRENT_INDEX         db      ; index of current code
  CODE_ENTRY_CURRENT_CHAR          db      ; index of currently edited char in current code
  MENU_WAIT_FRAMES                 db      ; # of frames to wait before the menu reacts
  CODE_1                           dsb 4
  CODE_2                           dsb 4
  CODE_3                           dsb 4
  CODE_4                           dsb 4
  CLEAR_CODE_DEBOUNCE              db      ; flag set at start of button-holding wait
  Unused3                          dsb 46
  TRAINER_CHANGE_TYPE              db      ; see "Values for TRAINER_CHANGE_TYPE" below
  LIST_CODES_CURRENT_CODES_POINTER dw      ; pointer into TRAINER_CODES while drawing current codes
  TIMER_DELTA_AMOUNT               db      ; delta absolute amount (BCD)
  TIMER_DELTA_SIGN                 db      ; delta direction: zero = more, nonzer = less
  TIMER_LAST_DELTA_HEX             db      ; used during code checking
  TIMER_DELTA_HEX                  db      ; delta signed amount (hex)
  TIMER_LAST_DELTA_BCD             db      ; used during code checking
  TIMER_DELTA_BCD                  db      ; delta signed amount (BCD)
  LOST_TRAINER_POINTER_1           dw
  LOST_TRAINER_POINTER_2           dw
  Unused4                          dsb 28
  GENERATED_CODE                   dsb 25
  Unused5                          dsb 5
  DRAW_XY                          dw      ; X, Y position for drawing
  CURSOR_TILE                      db
  CURSOR_X                         db      ; in tile space
  CURSOR_Y                         db      ; in tile space
  INITIALISED_MARKER               dw      ; checked for INITIALISED_MARKER_VALUE
  TRAINER_MODE                     db      ; 0 = inactive, else which trainer is active - see "Values for TRAINER_MODE" below
  TRAINER_CURRENT_CODES_POINTER    dw      ; last trainer code written
  TRAINER_CURRENT_SRAM_POINTER     dw      ; last place we looked
  TRAINER_VALUES_TO_LOOK_FOR       dw      ; in Lives mode only?
  STACK_SPACE                      dsb 858 ; loads of room
  STACK_TOP                        db      ; unsed byte?
  TRAINER_CODES                    dsb TRAINER_CODES_SIZE ; Holds 4-byte PAR codes...
  ; The first byte is used for housekeeping - the high bit indicates whether the slot is used,
  ; the remaining seven bits are used for status info:
  ; - Known value (lives): index of which representation is used (BCD/literal, +1/0/-1)
  ; - Start/change: 0 for "same as start", 1 for "changed". This is updated, along with the final byte, after each "train".
  ; - Timer: the BCD delta from the original value, plus $15. This is updated, along with the final byte, after each "train".
  ; - Energy bar: always 0
  ; The next two bytes are the address (little-endian); the last byte is the value to write.
  TRAINER_CODES_END                .db
  RAM_END                          .db
.ende


.define SRAM_CODE_BOOTGAME $d400
.define SRAM_DATA_NOTUSED $c700

.define STRING_TERMINATOR $1a
.define INITIALISED_MARKER_VALUE $5742
.define CREDITS_CODE $00c82606

; SMS hardware
.define VDP_DATA $be
.define VDP_STATUS $bf
.define VDP_REGISTER $bf
.define VDP_ADDRESS $bf
.define PSG $7f
.define IO_PORT_A $dc
.define IO_PORT_B $dd
.define MEMORY_CONTROL $3e

.define BUTTON_U      %00000001
.define BUTTON_D      %00000010
.define BUTTON_L      %00000100
.define BUTTON_R      %00001000
.define BUTTON_1      %00010000
.define BUTTON_2      %00100000
.define BUTTON_RESET  %01000000 ; Only for P2 inputs after shifting

.define VRAM_WRITE $4000
.define NAME_TABLE $3800
.define SPRITE_TABLE $3f00

.define SRAM_START $c000
.define SRAM_END SRAM_START + $2000

; String handling
.define NEWLINE $0a
.define STARTOFLINE $0d

.define PAR_REGISTER_SEARCHFORVALUE $0068 ; ?
.define PAR_REGISTER_ENABLE_GAME_ROM $6000 ; Writes here force the device to enable the game ROM, not its ROM/RAM
.define PAR_REGISTER_ENABLE_UPPER_ROM_BANK $2000 ; Writes here force the upper 16KB of PAR ROM to be used

; Values for TRAINER_MODE
.enum $81
  MODE_LIVES        db ; $81
  MODE_TIMER        db ; $82
  MODE_ENERGYBAR    db ; $83
  MODE_STARTCHANGE  db ; $84
.ende

; Values for TRAINER_CHANGE_TYPE
.enum $80
  START_CHANGE_VALUE_IS_START db ; $80
  START_CHANGE_VALUE_CHANGED  db ; $81
.ende

.enum $80
  ENERGY_BAR_VALUE_IS_START   db ; $80
  ENERGY_BAR_75_PERCENT       db ; $81
  ENERGY_BAR_50_PERCENT       db ; $82
  ENERGY_BAR_25_PERCENT       db ; $83
.ende

.define TIMER_MAX_DELTA $15 ; BCD

; Macros for some common, repetitive code

.macro ldhlxy args x,y
  ld hl, x << 8 | y
.endm

.macro setxyfromhl
  ld (DRAW_XY), hl
  call SetVRAMWriteAddressXY
.endm

.macro setxy args x,y
  ldhlxy x,y
  setxyfromhl
.endm

.macro drawtext args x,y,textpointer
  setxy x,y
  ld hl, \3 ; WLA doesn't like the named arg here
  call DrawText
.endm

.macro domenu args x,y,textpointer,menupointer
  drawtext x,y,\3 ; WLA doesn't like the named arg here
  ld hl, \4       ; or here
  jp MenuHandler
.endm

.org 0

.emptyfill $ff

.section "Boot" force
  di
  im 1
  ld sp, STACK_TOP
  jr Start
  .dsb 8 $00 ; blank
.ends

.org $10
.section "rst $10" force
WriteAToVDP:
  ; Write a to VDP (zero-extended)
  out (VDP_DATA), a
  push af
    xor a
    out (VDP_DATA), a
  pop af
  ret
  .dsb 8 $00 ; blank
.ends

.org $20
.section "rst $20" force
SetVRAMAddressToDE:
  ; Set VDP address to de, also write VDP register d value e
  ; Trashes a
  ld a, e
  out (VDP_ADDRESS), a
  ld a, d
  out (VDP_ADDRESS), a
  ret
  .db $00 ; blank
.ends
.macro SetVDPRegister args reg, value
  ld de, (reg << 8 | value) | $8000
  rst SetVRAMAddressToDE
.endm

.org $28
.section "rst $28" force
FillVRAM:
  ; Write l to VRAM bc times from address de
  rst SetVRAMAddressToDE
  ld a, c ; get count
  or a    ; if zero, increment b (because else the count will be wrong)
  jr nz, +
  inc b
+:ld a, l
-:out (VDP_DATA), a ; output bc bytes to VDP
  dec c
  jr nz, -
  djnz -
  ret
  .db $00 ; blank
.ends

.org $38
.section "INT hander" force
  ; This is used to jump to the menu. The lower 16KB is only enabled when the switch is in the lower position, 
  ; and it is enabled instead of the cart ROM when the Pause button is pressed and when a regular interrupt happens.
  ; If the NMI handler jumped to 0 itself, it'
  jp 0
.ends

.org $66
.section "NMI hander" force
  reti
.ends

.section "Start, credits" force
Start:
  ld sp, STACK_TOP

  ; Turn off PSG
  ld hl, PSGSilence
  ld c, PSG
  ld b, PSGSilenceEnd-PSGSilence
  otir

  ; Set VDP registers
  SetVDPRegister  0, %00110110 ; blank left column, line interrupts enabled, SMS mode, extra height modes enabled
  SetVDPRegister  1, %11100000 ; screen on, vblanks, 28 row mode
  SetVDPRegister  2, %11110001 | (NAME_TABLE>>10) ; name table address
  SetVDPRegister  3, $ff ; unused
  SetVDPRegister  4, $ff ; unused
  SetVDPRegister  5, %10000001 | (SPRITE_TABLE>>7) ; sprite table address
  SetVDPRegister  6, %11111011 | (0 << 2) ; sprites use low tile set
  SetVDPRegister  8, 0 ; hscroll 0
  SetVDPRegister  9, 0 ; vscroll 0
  SetVDPRegister 10, $ff ; line interrupts disabled
  SetVDPRegister  7, $10 | 0 ; border colour 0

  ld hl, (INITIALISED_MARKER)
  ld a, l
  cp INITIALISED_MARKER_VALUE & $ff
  jr nz, _NotInitialised
  ld a, h
  cp INITIALISED_MARKER_VALUE >> 8
  jr z, _Initialised
_NotInitialised:
  call BlankCodes
  xor a
  ld (TRAINER_MODE), a ; Not active

_Initialised:
  call ClearNameTable
  call LoadTiles
  call LoadPalette
  call WaitForVBlank
  call GetInputs
  or a
  cp BUTTON_2 ; Button 2
  jp nz, MainMenu

ShowCredits:
  call ClearNameTable
  drawtext 0, 0, Text_Credits
  call WaitForButtonPress

  ; Load "secret" code 00C82606
  ld hl, CREDITS_CODE >> 16
  ld (CODE_1 + 2), hl
  ld hl, CREDITS_CODE & $ffff
  ld (CODE_1 + 0), hl
  jp MainMenu

Text_Credits:
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "     Program   ... W.H.Beckett", NEWLINE
.db "     Design    ... W.H.Beckett", NEWLINE
.db "     Hardware  ... C.R.Harding", NEWLINE
.db "     Rev. Eng. ... W.H.Beckett", NEWLINE
.db "        &      ... C.R.Harding", NEWLINE
.db "     C.O.      ... M.J.Connors", NEWLINE
.db "     Ind. Esp. ... M.  Wallace", NEWLINE
.db "     2 I.C.    ... I.  Ryles", NEWLINE
.db "     Testing   ... Bazz &", NEWLINE
.db "               ... Trev", NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "      Software 04/3/93 v1.02", NEWLINE
.db "   (C) Datel Electronics Ltd."
.db STRING_TERMINATOR
.ends

.section "Main menu" force
MainMenu: ; 0258
  call ClearNameTable
  drawtext 11, 2, Text_MainMenu

  ; Patched out?
  nop
  nop
  nop
  nop

  ld a, (TRAINER_MODE)
  or a
  jr nz, +
  ld hl, MenuData_NotTraining
  jp MenuHandler

+:ld hl, MenuData_Training
  jp MenuHandler

; Unused jump to flashing border mode
; 0280
  jp FlashBorder

MenuData_NotTraining:
.db 3, 7 ; Initial cursor x, y
.db 3    ; # options
.dw StartTrainer
.dw ParametersScreen
.dw ReturnToGame
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "     1) Start Trainer", NEWLINE
.db "     2) Parameters screen", NEWLINE
.db "     3) Return to Game"
.db STRING_TERMINATOR

MenuData_Training:
.db 3, 7 ; Initial cursor x, y
.db 5    ; # options
.dw ContinueTrainer
.dw ParametersScreen
.dw ReturnToGame
.dw ClearTrainer
.dw ListPossibilities
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "     1) Continue Trainer", NEWLINE
.db "     2) Parameters screen", NEWLINE
.db "     3) Return to Game", NEWLINE
.db "     4) Clear trainer", NEWLINE
.db "     5) List possibilities"
.db STRING_TERMINATOR

Text_MainMenu:
.db "Main Menu"
.db STRING_TERMINATOR
.ends

.section "Unused 2" force
; 0375 unused - infinite loop with no interrupts
LockUp:
-:di
  jp -
.ends

.section "Continue trainer" force
ContinueTrainer: ; 0379
  ld a, (TRAINER_MODE)
  cp MODE_LIVES
  jp z, ContinueLivesTechnique
  cp MODE_TIMER
  jp z, ContinueTimerTechnique
  cp MODE_ENERGYBAR
  jp z, ContinueEnergyBarTechnique
  cp MODE_STARTCHANGE
  jp z, ContinueStartChangeTechnique
  jp MainMenu
.ends

.section "Wait for button press" force
WaitForButtonPress: ; 0393
-:call WaitForVBlank
  call GetInputs
  or a
  jr nz, - ; wait for no button

-:call WaitForVBlank
  call GetInputs
  or a
  jr z, - ; wait for a button

-:call WaitForVBlank
  call GetInputs
  or a
  jr nz, -
  ret
.ends

.section "Lost trainer" force
; Similar to other trainers, seems confusing though
LostTrainer: ;03af
  ld hl,0
  ld (HEX_ENTRY_XY),hl ; ?
  ld hl,TRAINER_CODES - 4
  ld (LOST_TRAINER_POINTER_1),hl        ; ?

--:
  ld hl,(HEX_ENTRY_XY) ; Increment counter
  inc hl
  ld (HEX_ENTRY_XY),hl

  ld hl,(LOST_TRAINER_POINTER_1) ; Add four to pointer
  ld bc,4
  add hl,bc
  ld (LOST_TRAINER_POINTER_1),hl

  ld bc,TRAINER_CODES_END - 1 ; End point
  or a          ; clear carry
  sbc hl,bc     ; subtract
  jp nc,_end    ; done if we got that far

  ld hl,(LOST_TRAINER_POINTER_1) ; Get pointer back
  bit 7,(hl)    ; High bit zero -> loop
  jr z,--

  ld hl,TRAINER_CODES ; Write this
  ld (LOST_TRAINER_POINTER_2),hl ; To here

-:ld hl,(LOST_TRAINER_POINTER_2) ; Read it back
  inc hl        ; Look up 1 byte further
  bit 7,(hl)    ; Check high bit
  jr nz,+       ; set -> skip?

  ld hl,(LOST_TRAINER_POINTER_1) ; bc = *p1 + 1
  inc hl
  ld c,(hl)
  inc hl
  ld b,(hl)
  ld hl,(LOST_TRAINER_POINTER_2) ; de = *p2 + 1
  inc hl
  ld e,(hl)
  inc hl
  ld d,(hl)

  ex de,hl      ; subtract one from the other
  or a
  sbc hl,bc
  jr nc,+       ; hl bigger -> skip?

  ld d,(hl)     ; Swap bytes pointed by each
  ld a,(bc)
  ld (hl),a
  ld a,d
  ld (bc),a

  inc hl        ; Increment
  inc bc

  ld d,(hl)
  ld a,(bc)
  ld (hl),a
  ld a,d
  ld (bc),a     ; Swap bytes

  inc hl        ; Increment
  inc bc

  ld d,(hl)     ; #3
  ld a,(bc)
  ld (hl),a
  ld a,d
  ld (bc),a

  inc hl
  inc bc

  ld d,(hl)     ; #4
  ld a,(bc)
  ld (hl),a
  ld a,d
  ld (bc),a

  inc hl
  inc bc

  jp --         ; and loop

+:ld hl,4       ; add 4 to ???
  ld bc,(LOST_TRAINER_POINTER_2)
  add hl,bc
  ld (LOST_TRAINER_POINTER_2),hl

  ld bc,TRAINER_CODES + 40   ; subtract from hl
  or a
  sbc hl,bc
  jr nc,-       ; if hl was bigger, go there
  jp --         ; else go there

_end:
  ret
.ends

.section "List possibilities" force
ListPossibilities: ; 0434
  ld hl,TRAINER_CODES
  ld (LIST_CODES_CURRENT_CODES_POINTER),hl

  call ClearNameTable
  setxy 12, 1

  xor a
  ld c,a  ; Zero means "nothing drawn"
  ld b,10 ; maximum number to list

_Loop:
  ld hl,(LIST_CODES_CURRENT_CODES_POINTER) ; Get pointer to code
  bit 7,(hl)    ; Check pointed value high bit
  jr z,_NextCode

  ld c,$ff      ; Signal that we got here, i.e. there were any codes drawn

  xor a
  call DrawHexByte ; First byte is always 0
  inc hl           ; Display remaining three bytes - but the address is shown in big-endian order
  inc hl
  ld a,(hl)        ; First the address
  call DrawHexByte
  dec hl
  ld a,(hl)
  call DrawHexByte
  inc hl
  inc hl
  ld a,(hl)
  call DrawHexByte ; Then the value
  ld a, NEWLINE
  call DrawTextChar

  dec b            ; Decrement counter

_NextCode:
  ld hl,(LIST_CODES_CURRENT_CODES_POINTER) ; Move on
  ld de,4
  add hl,de
  ld (LIST_CODES_CURRENT_CODES_POINTER),hl

  xor a            ; Check for end
  ld de,TRAINER_CODES_END - 1
  sbc hl,de
  jr nc,_EndOfCodes

  ld a,b           ; Check if we've drawn enough yet
  or a
  jr nz,_Loop

_EndOfCodes:
  ld a,c           ; Check if we drew any codes
  or a
  jr nz,_HaveSomeCodes

  ld hl,(TRAINER_CURRENT_SRAM_POINTER) ; We ran out of codes - did we also get to the end of SRAM?
  xor a
  ld de,SRAM_END - 4 + 1
  sbc hl,de
  jr nc,_GotToEndOfSRAM

  drawtext 2, 2, Text_PleaseContinue ; Didn't get to the end of SRAM yet
  jr +

_GotToEndOfSRAM:
  drawtext 3, 2, Text_NoPossibilities
  ; fall through

_HaveSomeCodes:
  drawtext 3, 18, Text_PressButton
  ; fall through

+:call WaitForButtonPress
  jp MainMenu

Text_PressButton: ; 4c8
.db "Press A button for main menu."
.db STRING_TERMINATOR

Text_NoPossibilities: ; 4e6
.db "There are no possibilities!"
.db STRING_TERMINATOR

Text_PleaseContinue: ; 502
.db "There are no possibilities", NEWLINE
.db "At the moment, please continue", NEWLINE
.db "further with the trainer."
.db STRING_TERMINATOR
.ends

.section "Clear trainer" force
ClearTrainer: ; 0556
  xor a
  ld (TRAINER_MODE),a
  jp MainMenu
.ends

.section "More of the lost trainer?" force
LostTrainerPart2: ; $055d
  ld hl,TRAINER_CODES + 40 ; Code slot 10
  ld (LIST_CODES_CURRENT_CODES_POINTER),hl
  bit 7,(hl)
  jr z,+        ; Do-nothing jump
+:ld hl,(LIST_CODES_CURRENT_CODES_POINTER) ; Move to next
  ld de,4
  add hl,de
  ld (LIST_CODES_CURRENT_CODES_POINTER),hl ; Check for end
  ld de,TRAINER_CODES_END - 1
  xor a
  sbc hl,de
  ret                                      ; Premature end?
.ends

.section "Blank PAR RAM - unused" force
BlankDeviceRAM: ; 0578
  ld hl,RAM_START
  ld de,RAM_START + 1
  ld bc,RAM_END - RAM_START - 1 ; Misses the last byte
  ld (hl),0
  ldir
  ret
.ends

.section "Start trainer menu" force
StartTrainer: ; 0586
  call ClearNameTable
  domenu 10, 2, Text_TrainerMenu, MenuData_Trainer

Text_TrainerMenu: ; 059e
.db "Trainer Menu"
.db STRING_TERMINATOR

MenuData_Trainer: ; 05ab
.db 3, 7
.db 5
.dw LivesTechniqueConfirm
.dw TimerTechniqueConfirm
.dw EnergyBarTechniqueConfirm
.dw StartChangeTechniqueConfirm
.dw MainMenu
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "     1)  Lives technique.", NEWLINE
.db "     2)  Timer technique.", NEWLINE
.db "     3)  Energy bar technique.", NEWLINE
.db "     4)  Start/Change technique.", NEWLINE
.db "     5)  Back to Main Menu."
.db STRING_TERMINATOR
.ends

.section "Lives technique start confirmation" force
LivesTechniqueConfirm: ; 0650
  call ClearNameTable
  domenu 0, 0, Text_LivesTechniqueConfirm, MenuData_LivesTechniqueConfirm

Text_LivesTechniqueConfirm: ; 0668
.db "  Use this technique for any", NEWLINE
.db "  value which is countable", NEWLINE
.db "  e.g. lives, ammunition etc."
.db STRING_TERMINATOR

MenuData_LivesTechniqueConfirm: ; 06be
.db 3, 7
.db 2
.dw LivesTechnique
.dw MainMenu
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "     1)  Use this technique.", NEWLINE
.db "     2)  Back to Main Menu"
.db STRING_TERMINATOR
.ends

.section "Start/change technique start confirmation" force
StartChangeTechniqueConfirm: ; 0705
  call ClearNameTable
  domenu 0, 0, Text_StartChangeTechniqueConfirm, MenuData_StartChangeTechniqueConfirm

Text_StartChangeTechniqueConfirm: ; $71d
.db "  Use this technique for any", NEWLINE
.db "  case where other techniques", NEWLINE
.db "  fail."
.db STRING_TERMINATOR

MenuData_StartChangeTechniqueConfirm: ; 0760
.db 3, 7
.db 2
.dw StartChangeTechniqueConfirmStartValue
.dw MainMenu
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "     1)  Use this technique.", NEWLINE
.db "     2)  Back to Main Menu"
.db STRING_TERMINATOR
.ends

.section "Timer technique start confirmation" force
TimerTechniqueConfirm: ; 07a7
  call ClearNameTable
  domenu 0, 0, Text_TimerTechniqueConfirm, MenuData_TimerTechniqueConfirm

Text_TimerTechniqueConfirm: ; 07bf
.db "  Use this technique for any", NEWLINE
.db "  value which is countable", NEWLINE
.db "  but technique 1 does not work", NEWLINE
.db "  e.g. Timers and where values", NEWLINE
.db "  are greater than 99"
.db STRING_TERMINATOR

MenuData_TimerTechniqueConfirm: ; 084c
.db 3, 7
.db 2
.dw TimerTechniqueConfirmStartValue ; 0c0e
.dw MainMenu
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "     1)  Use this technique.", NEWLINE
.db "     2)  Back to Main Menu"
.db STRING_TERMINATOR
.ends

.section "Energy bar technique start confirmation" force
EnergyBarTechniqueConfirm: ; 0893
  call ClearNameTable
  domenu 0, 0, Text_EnergyBarTechniqueConfirm, MenuData_EnergyBarTechniqueConfirm

Text_EnergyBarTechniqueConfirm: ; 08ab
.db "  Use this technique for any", NEWLINE
.db "  energy bars where a value", NEWLINE
.db "  is not known but a comparison", NEWLINE
.db "  between values can be made."
.db STRING_TERMINATOR

MenuData_EnergyBarTechniqueConfirm: ; 0922
.db 3, 7
.db 2
.dw EnergyBarTechniqueConfirmStartValue ; 0add
.dw MainMenu
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "     1)  Use this technique.", NEWLINE
.db "     2)  Back to Main Menu"
.db STRING_TERMINATOR
.ends

.section "Start/change technique confirm start value" force
StartChangeTechniqueConfirmStartValue: ; 0969
  call ClearNameTable
  call ClearCodes ; Unnecessary?
  domenu 0, 0, Text_StartChangeTechniqueConfirmStartValue, MenuData_StartChangeTechniqueConfirmStartValue

; 0984
  jp MainMenu ; Unused

Text_StartChangeTechniqueConfirmStartValue: ; 0987
.db "  You should start this trainer", NEWLINE
.db "  with what you want to posses", NEWLINE
.db "  e.g. if you want to find an", NEWLINE
.db "  extra weapon then start the", NEWLINE
.db "  trainer with that weapon."
.db STRING_TERMINATOR

MenuData_StartChangeTechniqueConfirmStartValue: ; 0a1e
.db 3, 7
.db 2
.dw StartChangeTechnique ; 0a65
.dw MainMenu
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "    1) Accept this start value", NEWLINE
.db "    2) Back to Main Menu"
.db STRING_TERMINATOR
.ends

.section "Start/change technique" force
StartChangeTechnique: ; 0a65
  drawtext 2, 16, Text_Scanning

  ld hl,SRAM_START - 1
  ld (TRAINER_CURRENT_SRAM_POINTER),hl
  ld a,START_CHANGE_VALUE_IS_START
  ld (TRAINER_CHANGE_TYPE),a
  call SearchForMatches_StartChange

  ld a,MODE_STARTCHANGE
  ld (TRAINER_MODE),a
  jp MainMenu
.ends

.section "Start/change technique: search for matches" force
SearchForMatches_StartChange: ; $0a8a
  ld hl,TRAINER_CODES - 4
  ld (TRAINER_CURRENT_CODES_POINTER),hl

_Loop:
  ; Search for a vacant slot
  ld hl,(TRAINER_CURRENT_CODES_POINTER)  ; Get pointer
  ld a,4         ; Add 4
  add a,l
  jr nc,+
  inc h
+:ld l,a
  ld (TRAINER_CURRENT_CODES_POINTER),hl

  ld bc,TRAINER_CODES_END - 5    ; Check for reaching the end
  xor a
  sbc hl,bc
  jr nc,_StopSearching

  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  bit 7,(hl)     ; Check if slot is used
  jr nz,_Loop

  ld de,(TRAINER_CURRENT_SRAM_POINTER) ; Increment RAM pointer
  inc de
  ld (TRAINER_CURRENT_SRAM_POINTER),de
  ld a,d         ; Check if we've reached $dfff (end of RAM)
  cp ((SRAM_END - 1) >> 8)
  jr c,_CheckSRAMByte
  jr nz,_StopSearching
  ld a,e
  cp ((SRAM_END - 1) & $ff)
  jr c,_CheckSRAMByte
  jr _StopSearching

_CheckSRAMByte:
  ; Do no check - just put it in RAM as a possible match
  ld a,(TRAINER_CHANGE_TYPE)
  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  ld (hl),a
  inc hl
  ld (hl),e
  inc hl
  ld (hl),d
  inc hl
  ld a,(de)
  ld (hl),a
  jr _Loop

_StopSearching:
  ld de,(TRAINER_CURRENT_SRAM_POINTER)
  dec de
  ld (TRAINER_CURRENT_SRAM_POINTER),de
  ret
.ends

.section "Energy bar technique: confirm start value" force
EnergyBarTechniqueConfirmStartValue: ; 0add
  call ClearNameTable
  call ClearCodes
  domenu 0, 0, Text_EnergyBarTechniqueConfirmStartValue, MenuData_EnergyBarTechniqueConfirmStartValue

; 0af8
  jp MainMenu ; unused

Text_EnergyBarTechniqueConfirmStartValue: ; 0afb
.db "  You should start this trainer", NEWLINE
.db "  with the energy bar you are", NEWLINE
.db "  searching for at full value.", NEWLINE
.db "  All future comparisons are", NEWLINE
.db "  made with respect to this", NEWLINE
.db "  start value."
.db STRING_TERMINATOR

MenuData_EnergyBarTechniqueConfirmStartValue: ; 0ba0
.db 3, 7
.db 2
.dw EnergyBarTechnique ; 0be9
.dw MainMenu
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "     1) Accept this start value", NEWLINE ; note: one extra space compared to other places...
.db "     2) Back to Main Menu"
.db STRING_TERMINATOR
.ends

.section "Energy bar technique begin" force
EnergyBarTechnique: ; 0be9
  drawtext 2, 16, Text_Scanning

  ld hl,SRAM_START - 1
  ld (TRAINER_CURRENT_SRAM_POINTER),hl

  ld a, ENERGY_BAR_VALUE_IS_START
  ld (TRAINER_CHANGE_TYPE),a

  call SearchForMatches_EnergyBarTechnique

  ld a, MODE_ENERGYBAR
  ld (TRAINER_MODE),a
  jp MainMenu
.ends

.section "Timer technique confirm start value" force
TimerTechniqueConfirmStartValue: ; 0c0e
  call ClearNameTable
  call ClearCodes
  domenu 0, 0, Text_TimerTechniqueConfirmStartValue, MenuData_TimerTechniqueConfirmStartValue

  jp MainMenu ; unused

Text_TimerTechniqueConfirmStartValue: ; 0c2c
.db "  Remember all future values", NEWLINE
.db "  will be compared to your", NEWLINE
.db "  start value, select accept", NEWLINE
.db "  start if you know this value."
.db STRING_TERMINATOR

MenuData_TimerTechniqueConfirmStartValue: ; 0ca1
.db 3, 7
.db 2
.dw TimerTechnique ; 0ce8
.dw MainMenu
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "    1) Accept this start value", NEWLINE
.db "    2) Back to Main Menu"
.db STRING_TERMINATOR
.ends

.section "Timer technique start" force
TimerTechnique: ; 0ce8
  drawtext 2, 16, Text_Scanning
  ld a, %10000000 | TIMER_MAX_DELTA ; High bit + TIMER_MAX_DELTA to signify a delta of 0
  ld (TRAINER_CHANGE_TYPE),a
  ld hl,SRAM_START - 1
  ld (TRAINER_CURRENT_SRAM_POINTER),hl
  call SearchForMatches_TimerTechnique
  ld a,MODE_TIMER
  ld (TRAINER_MODE),a
  jp MainMenu
.ends

.section "Timer technique: search for matches" force
SearchForMatches_TimerTechnique: ; 0d0d
  ; Just marks every SRAM location as a candidate.

  ld hl,TRAINER_CODES - 4 ; Start at beginning
  ld (TRAINER_CURRENT_CODES_POINTER),hl

_Loop:
  ld hl,(TRAINER_CURRENT_CODES_POINTER) ; p += 4
  ld a,4
  add a,l
  jr nc,+
  inc h
+:ld l,a
  ld (TRAINER_CURRENT_CODES_POINTER),hl

  ld bc,TRAINER_CODES_END - 5 ; End point
  xor a
  sbc hl,bc
  jr nc,_StopSearching  ; If so, stop looking

  ld hl,(TRAINER_CURRENT_CODES_POINTER) ; Get value
  bit 7,(hl)            ; Check high bit
  jr nz,_Loop           ; If set, move on again

  ld de,(TRAINER_CURRENT_SRAM_POINTER) ; p++
  inc de
  ld (TRAINER_CURRENT_SRAM_POINTER),de

  ld a,d                ; Check for end of RAM
  cp ((SRAM_END - 1) >> 8)
  jr c,_AddSRAMByte
  jr nz,_StopSearching
  ld a,e
  cp ((SRAM_END - 1) & $ff)
  jr c,_AddSRAMByte
  jr _StopSearching

_AddSRAMByte:
  ld a,(TRAINER_CHANGE_TYPE)
  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  ld (hl),a              ; Status
  inc hl
  ld (hl),e              ; Address
  inc hl
  ld (hl),d
  inc hl
  ld a,(de)              ; Value
  ld (hl),a
  jr _Loop

_StopSearching:
  ld de,(TRAINER_CURRENT_SRAM_POINTER)
  dec de
  ld (TRAINER_CURRENT_SRAM_POINTER),de
  ret
.ends


.section "Energy bar technique: search for matches" force
SearchForMatches_EnergyBarTechnique: ; 0d60 ? Accept all matches?
  ld a,(TRAINER_CHANGE_TYPE)
  cp ENERGY_BAR_VALUE_IS_START
  jr nz,_Exit

  ld hl,TRAINER_CODES - 4            ; Start at beginning
  ld (TRAINER_CURRENT_CODES_POINTER),hl

_Loop:
  ; Search for a vacant slot
  ld hl,(TRAINER_CURRENT_CODES_POINTER)  ; Get pointer
  ld a,4         ; Add 4
  add a,l
  jr nc,+
  inc h
+:ld l,a
  ld (TRAINER_CURRENT_CODES_POINTER),hl

  ld bc,TRAINER_CODES_END - 5 ; Check for reaching the end
  xor a
  sbc hl,bc
  jr nc,_StopSearching

  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  bit 7,(hl)     ; Check if slot is used
  jr nz,_Loop

  ld de,(TRAINER_CURRENT_SRAM_POINTER) ; Increment RAM pointer
  inc de
  ld (TRAINER_CURRENT_SRAM_POINTER),de
  ld a,d         ; Check if we've reached $dfff (end of RAM)
  cp ((SRAM_END - 1) >> 8)
  jr c,_CheckSRAMByte
  jr nz,_StopSearching
  ld a,e
  cp ((SRAM_END - 1) & $ff)
  jr c,_CheckSRAMByte
  jr _StopSearching

_CheckSRAMByte:
  ; We accept all bytes.
  ld a,(TRAINER_CHANGE_TYPE)
  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  ld (hl),a
  inc hl
  ld (hl),e
  inc hl
  ld (hl),d
  inc hl
  ld a,(de)
  ld (hl),a
  jr _Loop

_StopSearching:
  ld de,(TRAINER_CURRENT_SRAM_POINTER)          ; 0db0: ed 5b a1 40
  dec de                 ; 0db4: 1b
  ld (TRAINER_CURRENT_SRAM_POINTER),de          ; 0db5: ed 53 a1 40
_Exit:
  ret                    ; 0db9: c9
.ends

.section "Lives technique: confirm initial value" force
LivesTechnique: ; 0dba
  call ClearNameTable
  call ClearCodes

  drawtext 0, 0, Text_EnterCurrentValue

  ldhlxy 15, 11
  call InputHexNumber ; Get the value (returns in a)

  push af
    drawtext 2, 16, Text_Scanning
  pop af

  ld (PAR_REGISTER_SEARCHFORVALUE),a
  call SetValuesToSearchFor_LivesTechnique

  ld hl,SRAM_START - 1
  ld (TRAINER_CURRENT_SRAM_POINTER),hl

  call SearchForMatches_LivesTechnique

  ld a, MODE_LIVES
  ld (TRAINER_MODE),a
  jp MainMenu

Text_EnterCurrentValue: ; 0dfd
.db "  Enter the current value for", NEWLINE
.db "  lives, ammunition etc. Use", NEWLINE
.db "  the 'D' pad to enter and the", NEWLINE
.db "  button 1 or 2 to accept."
.db STRING_TERMINATOR
.ends

.section "Search for matches to known values" force
SearchForMatches_LivesTechnique: ; 0e72
  ld hl,TRAINER_CODES - 4 ; Start at beginning
  ld (TRAINER_CURRENT_CODES_POINTER),hl

_Loop:
  ld hl,(TRAINER_CURRENT_CODES_POINTER) ; p += 4
  ld a,4
  add a,l
  jr nc,+
  inc h
+:ld l,a
  ld (TRAINER_CURRENT_CODES_POINTER),hl

  ld bc,TRAINER_CODES_END - 5 ; End point
  xor a
  sbc hl,bc
  jr nc,_StopSearching  ; If so, stop looking

  ld hl,(TRAINER_CURRENT_CODES_POINTER) ; Get value
  bit 7,(hl)            ; Check high bit
  jr nz,_Loop           ; If set, move on again

_NextSRAM:
  ld de,(TRAINER_CURRENT_SRAM_POINTER) ; p++
  inc de
  ld (TRAINER_CURRENT_SRAM_POINTER),de

  ld a,d                ; Check for end of RAM
  cp ((SRAM_END - 1) >> 8)
  jr c,_CheckSRAMByte
  jr nz,_StopSearching
  ld a,e
  cp ((SRAM_END - 1) & $ff)
  jr c,_CheckSRAMByte
  jr _StopSearching

_CheckSRAMByte:
  ld a,(de)             ; Get value
  ld hl,TRAINER_VALUES_TO_LOOK_FOR
  ld c, 0               ; Index of value in TRAINER_VALUES_TO_LOOK_FOR
  ld b, 6               ; Number of values to check
-:cp (hl)               ; Compare
  jr z,_Match
  inc hl
  inc c
  djnz -
  jr _NextSRAM

_Match:
  push af
    ld a,c              ; Index of value in TRAINER_VALUES_TO_LOOK_FOR
    or %10000000        ; Set high bit
    ld hl,(TRAINER_CURRENT_CODES_POINTER)
    ld (hl),a           ; Save: byte 0 = match count + used flag
    inc hl
    ld (hl),e           ; Bytes 1-2  = address
    inc hl
    ld (hl),d
    inc hl
  pop af
  ld (hl),a             ; Byte 3 = value
  jr _Loop              ; Keep going

_StopSearching:
  ld de,(TRAINER_CURRENT_SRAM_POINTER) ; Save the current position so we can consider the remaining SRAM once some code slots are available
  dec de
  ld (TRAINER_CURRENT_SRAM_POINTER),de
  ret
.ends

.section "Continue lives technique" force
ContinueLivesTechnique: ; 0ed7
  call ClearNameTable
  drawtext 0, 0, Text_ContinueLivesTechnique

  setxy 15, 11
  call InputHexNumber
  ld (PAR_REGISTER_SEARCHFORVALUE),a
  call SetValuesToSearchFor_LivesTechnique

  drawtext 2, 16, Text_Scanning
  call CheckForFailedMatches_LivesTechnique
  call SearchForMatches_LivesTechnique
  jp MainMenu

Text_ContinueLivesTechnique: ; $0f13
.db "  Enter the current value of the", NEWLINE
.db "  parameter you are searching for"
.db STRING_TERMINATOR

Text_Scanning: ; $0f56
.db "Scanning Memory... Please wait"
.db STRING_TERMINATOR
.ends

.section "Continue start/change technique" force
ContinueStartChangeTechnique: ; 0f75
  call ClearNameTable
  domenu 0, 0, Text_ContinueStartChangeTechnique, MenuData_ContinueStartChangeTechnique

Text_ContinueStartChangeTechnique: ; 0f8d
.db "  Select the relation between", NEWLINE
.db "  the current value and the", NEWLINE
.db "  start value."
.db STRING_TERMINATOR

MenuData_ContinueStartChangeTechnique: ; 0fd6
.db 3, 7
.db 3
.dw SameAsStartValue
.dw DifferentFromStartValue
.dw MainMenu
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "     1) Same as start value", NEWLINE
.db "     2) Different to start value", NEWLINE
.db "     3) Back to Main menu"
.db STRING_TERMINATOR

SameAsStartValue: ; $103e
  ld a,START_CHANGE_VALUE_IS_START
  jr +

DifferentFromStartValue: ; $1042
  ld a,START_CHANGE_VALUE_CHANGED
+:ld (TRAINER_CHANGE_TYPE),a

  drawtext 2, 16, Text_Scanning

  call CheckForFailedMatches_StartChange
  call SearchForMatches_StartChange
  jp MainMenu
.ends

.section "Start/change technique: check for failed matches" force
CheckForFailedMatches_StartChange: ; 105f
  ; Initialise pointer
  ld hl,TRAINER_CODES - 4
  ld (TRAINER_CURRENT_CODES_POINTER),hl

_Loop:
  ; Next code slot
  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  ld a,4
  add a,l
  jr nc,+
  inc h
+:ld l,a
  ld (TRAINER_CURRENT_CODES_POINTER),hl

  ; Check for reaching the end
  ld de,TRAINER_CODES_END - 1
  xor a
  sbc hl,de
  jp nc,_StopSearching

  ; Check if it's used
  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  bit 7,(hl)
  jr z,_Loop

  ; Yes - get the code
  ld b,(hl)
  inc hl
  ld e,(hl)
  inc hl
  ld d,(hl)
  inc hl

  ld a,(TRAINER_CHANGE_TYPE)
  cp START_CHANGE_VALUE_IS_START
  jr nz,_Changed

  ; Current value should match the start value
  ld a,b                 ; Is the saved value the wanted value?
  cp START_CHANGE_VALUE_IS_START
  jr nz,+

  ; The code value should match SRAM
  ld a,(de)
  cp (hl)
  jr nz,_RejectCode
  jp _Loop

+:; The code value should *not* match SRAM
  ld a,(de)
  cp (hl)
  jr z,_RejectCode

  ld (hl),a              ; Save the current SRAM value (even if it's wrong)

  ld hl,(TRAINER_CURRENT_CODES_POINTER) ; Save whether we've stored the "right" or "wrong" value
  ld a,(TRAINER_CHANGE_TYPE)
  ld (hl),a

  jp _Loop

_Changed:
  ld a,b                 ; Is the code value the initial one?
  cp START_CHANGE_VALUE_IS_START
  jp nz,_Loop            ; If not, we can't compare it

  ld a,(de)              ; It is: we expect it not to match
  cp (hl)
  jr z,_RejectCode
  jp _Loop

_RejectCode:
  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  xor a
  ld (hl),a
  jp _Loop
_StopSearching:
  ret
.ends

.section "Energy bar technique: continue menu" force
ContinueEnergyBarTechnique: ; 10c0
  call ClearNameTable
  domenu 0, 0, Text_ContinueEnergyBarTechnique, MenuData_ContinueEnergyBarTechnique

Text_ContinueEnergyBarTechnique: ; $10d8
.db "  Select the relation between", NEWLINE
.db "  the current energy bar and", NEWLINE
.db "  it's start value."
.db STRING_TERMINATOR

MenuData_ContinueEnergyBarTechnique: ; $1127
.db 3, 7
.db 5
.dw ExactlySame
.dw About75Percent
.dw About50Percent
.dw About25Percent
.dw MainMenu
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "    1)  Exactly same as start", NEWLINE
.db "    2)  About 75% of start", NEWLINE
.db "    3)  About 50% of start", NEWLINE
.db "    4)  About 25% of start", NEWLINE
.db "    5)  Back to Main menu"
.db STRING_TERMINATOR
.ends

.section "Energy bar technique: continue" force
ExactlySame: ; 11c5
  ld a,ENERGY_BAR_VALUE_IS_START
  jr +
About75Percent: ; 11c9
  ld a,ENERGY_BAR_75_PERCENT
  jr +
About50Percent: ; 11cd
  ld a,ENERGY_BAR_50_PERCENT
  jr +
About25Percent: ; 11d1
  ld a,ENERGY_BAR_25_PERCENT
+:ld (TRAINER_CHANGE_TYPE),a

  drawtext 2, 16, Text_Scanning
  call CheckForFailedMatches_EnergyBarTechnique
  call SearchForMatches_EnergyBarTechnique
  jp MainMenu
.ends

.section "Energy bar technique: check for failed matches" force
CheckForFailedMatches_EnergyBarTechnique: ; 11ee
  ; Initialise pointer
  ld hl,TRAINER_CODES - 4
  ld (TRAINER_CURRENT_CODES_POINTER),hl

_Loop:
  ; Next code slot
  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  ld a,4
  add a,l
  jr nc,+
  inc h
+:ld l,a
  ld (TRAINER_CURRENT_CODES_POINTER),hl

  ; Check for reaching the end
  ld de,TRAINER_CODES_END - 1
  xor a
  sbc hl,de
  jp nc,_StopSearching

  ; Check if it's used
  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  bit 7,(hl)
  jr z,_Loop

  ; Yes - get the code
  ld b,(hl)
  inc hl
  ld e,(hl)
  inc hl
  ld d,(hl)
  inc hl

  ld a,b                     ; Get flag byte
  and %01111111              ; Reset high bit
  jr nz,_Loop                ; Must signal "same as start"

  ld a,(TRAINER_CHANGE_TYPE)
  cp ENERGY_BAR_VALUE_IS_START
  jr nz,+

_SameAsStart:
  ; Checks for current == original
  ld a,(de)                  ; Get SRAM
  cp (hl)                    ; Compare to original
  jp nz,_Failed              ; Should be exactly equal
  jp _Loop

+:cp ENERGY_BAR_75_PERCENT
  jr nz,+

_75Percent:
  ; Checks for original / 2 < current < original
  ld (HEX_ENTRY_XY),hl       ; Backup pointer (unused)

  ld c,(hl)                  ; Get original value in bc
  ld a,(de)                  ; Get current value in hl
  ld l,a
  xor a
  ld b,a
  ld h,a
  ld (CURRENT_CODE_VALUE_POINTER),hl ; Backup current value

  sbc hl,bc                  ; Should be less than the original
  jp nc,_Failed

  ld hl,(CURRENT_CODE_VALUE_POINTER) ; Restore current value
  sla l                      ; Multiply by 2
  rl h                       ; Now it should be bigger than the original (else it was 50% or smaller)
  xor a
  sbc hl,bc
  jp z,_Failed
  jp c,_Failed
  jp _Loop

+:cp ENERGY_BAR_50_PERCENT
  jr nz,+

_50Percent:
  ; checks for original / 4 < current < original * 3/4
  ld a,(hl)                  ; Get original in hl
  ld l,a
  xor a
  ld h,a
  ld (HEX_ENTRY_XY),hl       ; Back it up
  push hl
    sla l                    ; bc = original * 2
    rl h
  pop bc
  add hl,bc                  ; hl = original * 3
  ld a,(de)                  ; Get current in bc
  ld c,a
  xor a
  ld b,a
  sla c                      ; Multiply by 4
  rl b
  sla c
  rl b
  xor a
  sbc hl,bc                  ; 3 * original - 4 * current - should be positive for current < original*3/4
  jr c,_Failed
  jr z,_Failed

  ld hl,(HEX_ENTRY_XY)       ; Restore value
  xor a
  sbc hl,bc                  ; original - 4 * current - should be negative for current > original/4
  jr nc,_Failed
  jp _Loop

+:cp ENERGY_BAR_25_PERCENT
  jr nz, +

_25Percent:
  ; Checks for 0 < current < original / 2
  ld a,(de)                  ; Get current
  or a                       ; Cannot be zero
  jr z,_Failed
  ld c,a                     ; bc = current * 2
  xor a
  ld b,a
  sla c
  rl b
  ld a,(hl)                  ; hl = original
  ld l,a
  xor a
  ld h,a
  sbc hl,bc                  ; original - current * 2 should be positive for current < original / 2
  jr c,_Failed
  jr z,_Failed
  jp _Loop

+:; shouldn't happen
  jp _Loop

_Failed:
  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  xor a
  ld (hl),a
  jp _Loop

_StopSearching:
  ret
.ends

.section "Continue timer technique" force
ContinueTimerTechnique: ; 12ab
  call ClearNameTable
  domenu 0, 0, Text_ContinueTimerTechnique, MenuData_ContinueTimerTechnique

Text_ContinueTimerTechnique: ; 12c3
.db "  Is the current value more or", NEWLINE
.db "  less than the start value?"
.db STRING_TERMINATOR

MenuData_ContinueTimerTechnique: ; 12ff
.db 3, 7
.db 3
.dw TimerTechnique_MoreThanStart
.dw TimerTechnique_LessThanStart
.dw MainMenu
.db STARTOFLINE, NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "    1)  More than start", NEWLINE
.db "    2)  Less than start", NEWLINE
.db "    3)  Back to Main menu"
.db STRING_TERMINATOR
.ends

.section "Timer technique" force
TimerTechnique_MoreThanStart: ; 135a
  ld a,-1
  jr +
TimerTechnique_LessThanStart: ; 135e
  ld a, 0
+:ld (TIMER_DELTA_SIGN),a     ; Remember more or less-ness
  xor a
  ld (TIMER_DELTA_AMOUNT),a   ; Delta initial value
  ld (MENU_WAIT_FRAMES),a

  call ClearNameTable
  call SetVRAMWriteAddressXY
  ld hl,Text_ByHowMuch1
  call DrawText
  ld a,TIMER_MAX_DELTA
  call DrawHexByte
  ld hl,Text_ByHowMuch2
  call DrawText

_InputLoop:
  call WaitForVBlank
  call GetInputs              ; A bit unnecessary to do inside the loop
  ld a,(MENU_WAIT_FRAMES)     ; Input repeat delay
  or a
  jr z,+
  sub 1
  ld (MENU_WAIT_FRAMES),a
  jr _InputLoop

+:setxy 15, 11
  ld a,(TIMER_DELTA_AMOUNT)
  call DrawHexByte

  ld a,(BUTTONS_PRESSED)      ; Handle inputs
  ld b,a
  bit 0,b                     ; Up
  jr z,_NotUp
  ld a,(TIMER_DELTA_AMOUNT)   ; Check for max
  cp TIMER_MAX_DELTA
  jr z,_InputEnd
  add a, 1                    ; Else increment
  daa
  ld (TIMER_DELTA_AMOUNT),a
  jr _InputHandled

_NotUp:
  bit 1,b                     ; Down
  jr z,_NotDown
  ld a,(TIMER_DELTA_AMOUNT)   ; Check for min
  or a
  jr z,_InputEnd
  dec a                       ; Else decrement
  daa
  ld (TIMER_DELTA_AMOUNT),a
  jr _InputHandled

_NotDown:
  and BUTTON_1 | BUTTON_2     ; Check for button
  jr nz,_InputDone

_InputHandled:
  ld a, 8
  ld (MENU_WAIT_FRAMES),a
  ; fall through
_InputEnd:
  jr _InputLoop

_InputDone:
  ; Make signed BCD and hex deltas
  ld a,(TIMER_DELTA_SIGN)
  or a
  jr nz,_IsPositive
  ld a,(TIMER_DELTA_AMOUNT)
  ld b,a
  xor a
  sub b
  daa
  ld (TIMER_DELTA_BCD),a
  ld a,(TIMER_DELTA_AMOUNT)
  call BCDtoHex
  neg
  ld (TIMER_DELTA_HEX),a
  jr _SignAdjustmentEnd

_IsPositive:
  ld a,(TIMER_DELTA_AMOUNT)
  ld (TIMER_DELTA_BCD),a
  call BCDtoHex
  ld (TIMER_DELTA_HEX),a

_SignAdjustmentEnd:
  ld a,(TIMER_DELTA_BCD)               ; Save the BCD delta + 15 (so it's positive) to the status byte
  add a,TIMER_MAX_DELTA                ; No daa here..?
  or %10000000
  ld (TRAINER_CHANGE_TYPE),a

  drawtext 2, 16, Text_Scanning
  call CheckForFailedMatches_TimerTechnique
  call SearchForMatches_TimerTechnique
  jp MainMenu

Text_ByHowMuch1: ; 1421
.db NEWLINE
.db "     By how much? Max - ",
.db STRING_TERMINATOR

Text_ByHowMuch2: ; 143b
.db ".", STARTOFLINE, NEWLINE
.db "  Use up and down to change ", NEWLINE
.db "  value and button 1 to accept."
.db STRING_TERMINATOR
.ends


.section "Timer technique: check for failed matches" force
CheckForFailedMatches_TimerTechnique: ; 147b
  ; Initialise pointer
  ld hl,TRAINER_CODES - 4
  ld (TRAINER_CURRENT_CODES_POINTER),hl

_Loop:
  ; Next code slot
  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  ld a,4
  add a,l
  jr nc,+
  inc h
+:ld l,a
  ld (TRAINER_CURRENT_CODES_POINTER),hl

  ; Check for reaching the end
  ld de,TRAINER_CODES_END - 1
  xor a
  sbc hl,de
  jp nc,_StopSearching

  ; Check if it's used
  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  bit 7,(hl)
  jp z,_Loop

  ; Yes - get the code
  ld b,(hl)
  inc hl
  ld e,(hl)
  inc hl
  ld d,(hl)
  inc hl

  ld (CURRENT_CODE_VALUE_POINTER),hl ; Save the pointer to the value
  ld a,b                ; Get the status byte
  and %01111111         ; Remove the used flag
  sub TIMER_MAX_DELTA   ; Convert to a signed BCD value
  daa
  ld (TIMER_LAST_DELTA_BCD),a ; Save it
  jr nc,_PositiveDelta

  ld b,a                ; If it was negative, invert the sign around conversion to hex
  xor a
  sub b
  daa
  call BCDtoHex
  neg
  ld (TIMER_LAST_DELTA_HEX),a ; ...and save
  jr +
_PositiveDelta:
  call BCDtoHex
  ld (TIMER_LAST_DELTA_HEX),a ; Else just convert straight to hex

+:ld a,(TIMER_LAST_DELTA_BCD) ; Get deltas in b, c
  ld b,a
  ld a,(TIMER_DELTA_BCD)
  ld c,a
  ld a,(hl)             ; Get saved value
  sub b                 ; Remove previously used delta
  daa
  add a,c               ; Add new delta
  daa
  ex de,hl              ; Compare to RAM
  cp (hl)
  ex de,hl
  jr z,_Pass

  ld a,(TIMER_LAST_DELTA_HEX) ; Failed: try in hex
  ld b,a
  ld a,(TIMER_DELTA_HEX)
  ld c,a
  ld a,(hl)
  sub b
  add a,c
  ex de,hl
  cp (hl)
  ex de,hl
  jr nz,_Fail
_Pass:
  jp _Loop

_Fail:
  ld hl,(TRAINER_CURRENT_CODES_POINTER) ; Mark code as unused
  xor a
  ld (hl),a
  jp _Loop

_StopSearching:
  ret
.ends

.section "Check current codes against SRAM for known value" force
CheckForFailedMatches_LivesTechnique: ; $14f5
  ld hl,TRAINER_CODES - 4 ; Initialise pointer
  ld (TRAINER_CURRENT_CODES_POINTER),hl

_Loop:
  ld hl,(TRAINER_CURRENT_CODES_POINTER) ; p += 4
  ld a,4
  add a,l
  jr nc,+
  inc h
+:ld l,a
  ld (TRAINER_CURRENT_CODES_POINTER),hl

  ld de,TRAINER_CODES_END - 1
  xor a
  sbc hl,de
  jr nc,_StopSearching

  ld hl,(TRAINER_CURRENT_CODES_POINTER)
  bit 7,(hl)            ; Check high bit
  jr z,_Loop            ; Loop until we find one that is set

  ld b,(hl)             ; Get index/flag in b
  inc hl
  ld e,(hl)             ; Address in de
  inc hl
  ld d,(hl)
  ld a,b
  and %01111111         ; Mask to just the index

  ld hl,TRAINER_VALUES_TO_LOOK_FOR
  add a,l               ; Look up the indexed value, so the comparison is like-for-like
  jr nc,+
  inc h
+:ld l,a
  ld a,(de)             ; Get SRAM value
  cp (hl)               ; Compare to expected
  jr nz,+               ; No match -> we want to discard it
  jr _Loop

+:ld hl,(TRAINER_CURRENT_CODES_POINTER)
  xor a                 ; Zero the status byte so the slot can be reused
  ld (hl),a
  jr _Loop

_StopSearching:
  ret
.ends

.section "Clear trainer codes RAM" force
ClearCodes: ; $1535
  ld hl,TRAINER_CODES
  ld de,TRAINER_CODES + 1
  ld bc,TRAINER_CODES_SIZE - 1
  ld (hl),0
  ldir
  ret
.ends

.section "Set values to search for" force
SetValuesToSearchFor_LivesTechnique: ; $1543
  ld b,a                ; Save param

  call BCDtoHex         ; For hex value, save:
  ld (TRAINER_VALUES_TO_LOOK_FOR + 0),a ; n
  dec a
  ld (TRAINER_VALUES_TO_LOOK_FOR + 1),a ; n - 1
  inc a
  inc a
  ld (TRAINER_VALUES_TO_LOOK_FOR + 2),a ; n + 1

  ld a,b                ; For BCD value, save:
  ld (TRAINER_VALUES_TO_LOOK_FOR + 3),a ; n
  add a,1
  daa
  ld (TRAINER_VALUES_TO_LOOK_FOR + 4),a ; n + 1
  ld a,b
  sub 1
  daa
  ld (TRAINER_VALUES_TO_LOOK_FOR + 5),a ; n - 1
  ret
.ends

.section "BCD to hex convertor" force
BCDtoHex: ; $1565
; Input: (positive) BCD number in a
; Output: regular number in a
; Trashes flags
  push bc
    ld b,a              ; n
    and $f0             ; High nibble is number of 10s
    srl a
    ld c,a              ; Now it's that many 8s

    srl a               ; Now it's that many 2s
    srl a
    add a,c             ; Add them together
    ld c,a
    ld a,b
    and $0f             ; Add to the low nibble (which is OK)
    add a,c
  pop bc
  ret
.ends

.section "Input BCD number" force
; Get a BCD byte from the user. Draws to hl = xy, returns in a. U/D = change value, L/R = choose nibble, 1/2/reset = done
InputHexNumber: ; $1578
  ld (HEX_ENTRY_XY),hl   ; Save param

  ld a,1
  ld (CURSOR_X),a        ; Cursor -> 1, 0
  dec a
  ld (CURSOR_Y),a

  ld (HEX_ENTRY_VALUE),a ; value = 0

_ValueEntryLoop:
  ld hl,(HEX_ENTRY_XY)
  setxyfromhl

  ld a,(HEX_ENTRY_VALUE) ; Draw current value
  call DrawHexByte

  call UpdateEntryCursor ; Draw underline under the current digit

  call WaitForVBlank
  ld a,(MENU_WAIT_FRAMES) ; Delay between taking inputs
  or a
  jr z, _EndWait
  dec a
  ld (MENU_WAIT_FRAMES),a
  jr _ValueEntryLoop

_EndWait:
  call GetInputs
  ld b,a
  and BUTTON_L | BUTTON_R
  jr z,_NotLR

  ld a,(CURSOR_X)        ; Left or right pressed: toggle X position low bit (to switch digit)
  xor 1
  ld (CURSOR_X),a
  jr _InputsEnd

_NotLR:
  bit 0, b
  jr z, _NotU
  ld l,$11               ; Up pressed: add 1
  jr _ChangeValue

_NotU:
  bit 1,b
  jr z,_NotD
  ld l,$99               ; Down pressed: add 9
  ; fall through

_ChangeValue:
  ld a,(CURSOR_X)        ; Generate mask from digit
  bit 0,a
  jr z,+
  ld h,$0f
  jr ++
+:ld h,$f0
++:
  ld a,(HEX_ENTRY_VALUE)
  and h                  ; Mask
  add a,l                ; Add
  daa                    ; BCD adjust
  and h                  ; Mask
  ld b,a                 ; Save

  ld a,h                 ; Invert the mask
  cpl
  ld h,a
  ld a,(HEX_ENTRY_VALUE) ; Get the unchanged digit
  and h
  or b                   ; Merge
  ld (HEX_ENTRY_VALUE),a ; Save
  jr _InputsEnd          ; Done

_NotD:
  ld a,b
  and BUTTON_1 | BUTTON_2 | BUTTON_RESET
  jr z, _Not12Reset

-:call WaitForVBlank     ; Wait for button release
  call GetInputs
  or a
  jr nz,-

  ld a,(HEX_ENTRY_VALUE) ; Return with current value
  ret

_Not12Reset:
  jr + ; Skip the delay as there was no button

_InputsEnd:
  ld a, 8
  ld (MENU_WAIT_FRAMES),a

+:jr _ValueEntryLoop

UpdateEntryCursor: ; $1605
  ld de, SPRITE_TABLE | VRAM_WRITE
  rst SetVRAMAddressToDE

  ld a,(HEX_ENTRY_XY)        ; Get y
  sla a                     ; Convert to sprite coordinates (x8)
  sla a
  sla a
  out (VDP_DATA),a          ; Set sprite Y

  ld de, SPRITE_TABLE | VRAM_WRITE + 128 ; Sprite X
  rst SetVRAMAddressToDE

  ld a,(HEX_ENTRY_XY + 1)    ; Get X
  ld e,a
  ld a,(CURSOR_X)
  add a,e                   ; Add relative position
  sla a                     ; Convert to sprite coordinates
  sla a
  sla a
  out (VDP_DATA), a

  ld a, $57                 ; Underscore tile
  out (VDP_DATA), a
  ret
.ends

.section "Parameters screen" force
ParametersScreen: ; 162d
  call ClearNameTable
  call DrawCodes

_Loop: ; 1633
  call WaitForVBlank
  call GetInputs
  ld de, VRAM_WRITE | NAME_TABLE + 8 * 2 ; Draw position 8, 0
  rst SetVRAMAddressToDE
  ld a,(MENU_WAIT_FRAMES)
  or a
  jr z,+
  dec a
  ld (MENU_WAIT_FRAMES),a
  jp _LoopEnd                      ; Loop for delay

+:ld a,(BUTTONS_PRESSED)           ; Check inputs
  ld b,a
  and BUTTON_1 | BUTTON_2          ; Both buttons -> clear code
  cp BUTTON_1 | BUTTON_2
  jr nz,++

_ClearCode:
  ld a,(CLEAR_CODE_DEBOUNCE)       ; Check debounce flag - if zero (usually is), we want to wait...
  or a
  jr z,+
  ld a,(CODE_ENTRY_CURRENT_INDEX)  ; If the wait is up, we clear the code
  call ZeroCode
  xor a
  ld (CLEAR_CODE_DEBOUNCE),a
  ld a,16
  ld (MENU_WAIT_FRAMES),a
  jp _LoopEnd

+:cpl                              ; Set the flag to non-zero to signal the start of the wait
  ld (CLEAR_CODE_DEBOUNCE),a
  jp _LoopEndWithPause

++:
  bit 2,b                          ; Left
  jr z,++
  ld a,(CODE_ENTRY_CURRENT_CHAR)
  dec a                            ; Decrement char pointer
  cp -1
  jr nz,+
  ld a,7                           ; Wrap left -> right
+:ld (CODE_ENTRY_CURRENT_CHAR),a
  jr _LoopEndWithPause

++:
  bit 3,b                          ; Right
  jr z,++
  ld a,(CODE_ENTRY_CURRENT_CHAR)
  inc a                            ; Increment char pointer
  cp 8
  jr nz,+
  xor a                            ; Wrap right -> left
+:ld (CODE_ENTRY_CURRENT_CHAR),a
  jr _LoopEndWithPause

++:
  bit 1,b                          ; Down
  jr z,++
  ld a,(CODE_ENTRY_CURRENT_INDEX)
  inc a                            ; Next code
  cp 4
  jr c,+
  jp MainMenu                      ; Move past the bottom to return to the menu
+:ld (CODE_ENTRY_CURRENT_INDEX),a
  jr _LoopEndWithPause

++:
  bit 0,b                          ; Up
  jr z,++
  ld a,(CODE_ENTRY_CURRENT_INDEX)
  dec a                            ; Previous code
  cp -1
  jr nz,+
  ld a,3                           ; Wrap top -> bottom
+:ld (CODE_ENTRY_CURRENT_INDEX),a
  jr _LoopEndWithPause

++:
  bit 4,b                          ; Button 1
  jr z,++
  ld a,(CODE_ENTRY_CURRENT_INDEX)
  ld b,a
  ld a,(CODE_ENTRY_CURRENT_CHAR)
  ld c,a
  push bc
    call GetNibble
  pop bc
  inc a                            ; Increment value ($10 will be treated as $00)
  call SetNibble
  jr _LoopEndWithPause

++:
  bit 5,b                          ; Button 2
  jr z,++
  ld a,(CODE_ENTRY_CURRENT_INDEX)
  ld b,a
  ld a,(CODE_ENTRY_CURRENT_CHAR)
  ld c,a
  push bc
    call GetNibble
  pop bc
  dec a                            ; Decrement value ($ff will be treated as $0f)
  call SetNibble
  jr _LoopEndWithPause

++:
  bit 6,b                          ; Reset
  jr z,+

ReturnToGame:
  call GenerateCode
  jp ShowReturnToGameMenu

+:xor a
  ld (CLEAR_CODE_DEBOUNCE),a
  jr _LoopEnd

_LoopEndWithPause; ; 16fd
  ld a,8
  ld (MENU_WAIT_FRAMES),a

_LoopEnd: ; 1702
  ld a,(CODE_ENTRY_CURRENT_INDEX)
  call DrawCode_3x3
  call DrawEntryCursor
  jp _Loop
.ends

.section "Menu handler" force
MenuHandler: ; 170e
  ld de, $0000 ; 0, 0
  ld (DRAW_XY), de
  call SetVRAMWriteAddressXY

  ; get menu data pointer in ix
  push hl
  pop ix

  ; byte 2 = number of options; skip n * 2 + 3 bytes to find text. But for some reason we calculate it as (n-1) * 2 + 5...
  ld a, (ix+2)
  dec a
  sla a
  add a, 5
  add a, l
  jr nc, +
  inc h
+:ld l, a
  call DrawText

  ld a, (ix+0)
  ld (CURSOR_X), a
  ld a, 0
  ld (CURSOR_Y), a
  ld a, $42     ; '>'
  ld (CURSOR_TILE), a
  call _UpdateCursor

  xor a
  ld (MENU_WAIT_FRAMES), a

_MenuLoop:
  call WaitForVBlank
  call GetInputs

  ld a, (MENU_WAIT_FRAMES)
  or a
  jr z, +

  dec a         ; if non-zero, decrement and loop until it is
  ld (MENU_WAIT_FRAMES), a

  jr _MenuLoop_End

+:ld a, $00 ; ' ' Turn cursor off
  ld (CURSOR_TILE), a
  call _UpdateCursor

  ld a, (BUTTONS_PRESSED)
  ld b, a

  bit 0, b ; Up
  jr z, ++

  ld a, (CURSOR_Y) ; Decrement Y
  dec a
  cp -1
  jr nz, +
  ld a, (ix+2)     ; -1 -> choose last option
  dec a
+:ld (CURSOR_Y), a
  jr _MenuLoop_SmallPause

++:
  bit 1, b ; Down
  jr z, ++

  ld a, (CURSOR_Y) ; Increment Y
  inc a
  cp (ix+2)
  jr nz, +
  ld a, 0
+:ld (CURSOR_Y), a
  jr _MenuLoop_SmallPause

++:
  ld a, b
  and BUTTON_1 | BUTTON_2
  jr z, ++

-:call WaitForVBlank ; Wait for button release
  call GetInputs
  or a
  jr nz, -

  ld a, (CURSOR_Y) ; Figure out the menu item
  sla a
  add a, $03
  push ix          ; Look up function pointer
  pop hl
  add a, l
  jr nc, +
  inc h
+:ld l, a
  ld a, (hl)
  inc hl
  ld e, a
  ld a, (hl)
  ld d, a
  ex de, hl        ; ...into hl
  ld de, $7810     ; Name table address for 8, 0 ???
  rst SetVRAMAddressToDE
  jp (hl)          ; Jump to function

++:
  jr _MenuLoop_End

_MenuLoop_SmallPause:
  ld a, 8
  ld (MENU_WAIT_FRAMES), a
  ; fall through

_MenuLoop_End:
  ld a, $42 ; '>'
  ld (CURSOR_TILE), a
  call _UpdateCursor
  jp _MenuLoop

_UpdateCursor: ; 17c3
  ld de, SPRITE_TABLE | VRAM_WRITE ; y[0]
  rst SetVRAMAddressToDE
  ld a, (CURSOR_Y)
  add a, (ix+1) ; + original value..?
  sla a         ; x 8 to get pixel coords
  sla a
  sla a
  out (VDP_DATA), a  ; Y position
  ld de, SPRITE_TABLE | VRAM_WRITE + 128  ; sprite table: xn[0]
  rst SetVRAMAddressToDE
  ld a, (CURSOR_X)
  sla a         ; x8 to get pixel coords
  sla a
  sla a
  out (VDP_DATA), a
  ld a, (CURSOR_TILE) ; Tile number
  out (VDP_DATA), a
  ret
.ends

.section "Set VRAM address based on DRAW_XY" force
SetVRAMWriteAddressXY: ; 17ea
  push hl
  push de
    ld a, (DRAW_XY) ; get low byte (y)
    ld l, a
    xor a
    sla l
    rla
    sla l
    rla
    sla l
    rla
    sla l
    rla
    sla l
    rla
    sla l
    rla
    ld h, a       ; x64
    ld a, (DRAW_XY+1) ; get high byte (x)
    sla a
    add a, l
    ld l, a       ; x2 (+ add?!)
    ld de, NAME_TABLE | VRAM_WRITE
    add hl, de    ; add
    ex de, hl
    rst SetVRAMAddressToDE
  pop de
  pop hl
  ret

.ends

.section "Generate code" force
GenerateCode: ; 1814
  ld ix, GENERATED_CODE
  ld iy, CODE_1 ; codes

  ld a,$f5     ; > push af
  ld (ix+0),a
  inc ix

  ld b,4       ; four codes
-:
  ld a,(iy+3)  ; Check for blank code
  or (iy+2)
  or (iy+1)
  or (iy+0)
  jr z,+       ; Do nothing if blank

  ld a,$3e     ; > ld a, nn
  ld (ix+0),a
  ld a,(iy+0)
  ld (ix+1),a
  ld a,$32     ; > ld (nnnn), a
  ld (ix+2), a
  ld a, (iy+1)
  ld (ix+3), a
  ld a, (iy+2)
  ld (ix+4), a
  ld de, 5     ; Move on
  add ix,de
+:
  ld de, 4
  add iy,de
  djnz -

  ld a, $f1    ; > pop af
  ld (ix+0),a
  ld a, $c3    ; > jp $0035 ; This is aligned to ReturnToGameInterruptHandler
  ld (ix+1),a
  ld a, $35
  ld (ix+2),a
  ld a, $00
  ld (ix+3),a
  ret
.ends

.section "Show return to game menu" force
ShowReturnToGameMenu: ; 1870
  call ClearNameTable
  ld hl,MenuData_ReturnToGame
  jp MenuHandler

SlowRestart: ; 1879
  ld hl,INITIALISED_MARKER_VALUE
  ld (INITIALISED_MARKER),hl

  ld hl,MapToGameAndBoot_Slow
  ld de,SRAM_CODE_BOOTGAME ; Dest
  ld bc,MapToGameAndBoot_SlowEnd - MapToGameAndBoot_Slow
  ldir        ; Copy

  ld hl,ContinueTrainer ; Source ??? Why copy this?
  ld de,SRAM_DATA_NOTUSED ; Dest
  ld bc,$00dd ; Count (a bunch of random functions that don't have relative jumps)
  ldir

  jp SRAM_CODE_BOOTGAME

FastRestart: ; $1898
  ; Same as slow except for the function copied to RAM...
  ld hl,INITIALISED_MARKER_VALUE
  ld (INITIALISED_MARKER),hl

  ld hl,MapToGameAndBoot_Fast
  ld de,SRAM_CODE_BOOTGAME ; Dest
  ld bc,MapToGameAndBoot_FastEnd - MapToGameAndBoot_Fast
  ldir        ; Copy

  ld hl,ContinueTrainer ; see above
  ld de,SRAM_DATA_NOTUSED
  ld bc,$00dd
  ldir

  jp SRAM_CODE_BOOTGAME

MenuData_ReturnToGame: ; 18b7
.db 6, 7 ; x, y
.db 2 ; options
.dw FastRestart
.dw SlowRestart
.db STARTOFLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db NEWLINE
.db "        1)  Fast restart", NEWLINE
.db "        2)  Slow restart"
.db STRING_TERMINATOR
.ends

.section "Zero out one of the four codes" force
ZeroCode: ; 18f8
  sla a          ; Look up a'th code
  sla a
  ld hl,CODE_1
  add a,l
  jr nc,+
  inc h
+:ld l,a
  xor a          ; Zero it
  ld (hl),0
  inc hl
  ld (hl),0
  inc hl                      
  ld (hl),0
  inc hl                      
  ld (hl),0
  ret
.ends

.section "Blank codes" force
BlankCodes:
  ; Looks like some easily-patched values for the first two codes...
  ld hl, $0000
  ld (CODE_1 + 2), hl
  ld hl, $0000
  ld (CODE_1 + 0), hl
  ld hl, $0000
  ld (CODE_2 + 2), hl
  ld hl, $0000
  ld (CODE_2 + 0), hl
  ld hl, $0000
  ld (CODE_3 + 0), hl
  ld (CODE_3 + 2), hl
  ld (CODE_4 + 0), hl
  ld (CODE_4 + 2), hl
  ret
.ends

.section "Get nibble for code b, char c" force
GetNibble: ; 1939
  ; Params: b = code number (0-3), c = char number (0-7)
  ; Returns: nibble in low half of a
  ld a,b      ; b *= 4 -> byte offset of code
  sla a
  sla a
  ld b,a

  ld a,7      ; c = 7 - c
  sub c
  ld c,a

  sra a       ; Divide by 2 to get byte index
  add a,b     ; Add to offset

  ld hl,CODE_1 ; Point to the relevant byte
  add a,l
  jr nc,+
  inc h
+:ld l,a
  ld a,(hl)   ; Get byte

  bit 0,c     ; Which nibble?
  jr z,_LowNibble

_HighNibble:
  sra a
  sra a
  sra a
  sra a
_LowNibble:
  and $0f
  ret
.ends

.section "Set nibble for code b, char c" force
SetNibble: ; 195e
  ; Params: b = code number (0-3), c = char number (0-7), a = value to set (ignore high nibble)
  ld (HEX_ENTRY_VALUE),a ; Backup

  ld a,b ; Point hl at the relevant byte - see above
  sla a
  sla a
  ld b,a
  ld a,7
  sub c
  ld c,a
  sra a
  add a,b
  ld hl,CODE_1
  add a,l
  jr nc,+
  inc h
+:ld l,a

  bit 0,c     ; Which nibble?
  jr z,_LowNibble

_HighNibble:
  ld a,(HEX_ENTRY_VALUE) ; Restore value
  sla a                  ; Shift into high nibble
  sla a
  sla a
  sla a
  and $f0                ; Mask to high nibble
  ld b,a
  ld a,(hl)              ; Merge to existing byte
  and $0f
  or b
  ld (hl),a              ; Save
  ret

_LowNibble:
  ld a,(HEX_ENTRY_VALUE)
  and $0f                ; Mask
  ld b,a
  ld a,(hl)
  and $f0                ; Merge
  or b
  ld (hl),a              ; Save
  ret
.ends

.section "Code entry cursor" force
DrawEntryCursor: ; 199a
  ld a,(CODE_ENTRY_CURRENT_INDEX) ; Calculate Y offset for cursor
  sla a
  ld b,a          ; 2n
  sla a           ; 4n
  add a,b         ; 6n
  sla a           ; 12n
  sla a           ; 24n
  sla a           ; 48n
  add a, 23       ; 48n + 23
  ld b,a
  ld de, SPRITE_TABLE | VRAM_WRITE
  rst SetVRAMAddressToDE
  ld a,b
  out (VDP_DATA),a ; Write 3 times
  push af
  pop af
  out (VDP_DATA),a
  push af
  pop af
  out (VDP_DATA),a
  ld a,(CODE_ENTRY_CURRENT_CHAR) ; Now the X coordinate
  ld c,24         ; offset for first 4 chars
  cp 4
  jr c,+
  ld c,42         ; offset for last 4 chars
+:ld b,a          ; n
  sla a
  add a,b         ; 3n
  sla a           ; 6n
  sla a           ; 12n
  sla a           ; 24n
  add a,c         ; 24n + offset
  ld b,a
  ld c,$57        ; Underscore tile
  ld de, SPRITE_TABLE | VRAM_WRITE + 128 ; Sprite X
  rst SetVRAMAddressToDE
  ld a,b          ; X
  out (VDP_DATA),a
  push af
  pop af
  ld a,c          ; n
  out (VDP_DATA),a
  push af
  pop af
  ld a,b
  add a,8         ; x + 8
  out (VDP_DATA),a
  push af
  pop af
  ld a,c          ; n
  out (VDP_DATA),a
  push af
  pop af
  ld a,b
  add a,16        ; x + 16
  out (VDP_DATA),a
  push af
  pop af
  ld a,c          ; n
  out (VDP_DATA),a
  ret
.ends

.section "Text drawing" force
; Writes text from hl to name table
DrawText: ; 19f9
-:ld a, (hl) ; get byte
  cp STRING_TERMINATOR
  jr z, _DrawTextDone
  call DrawTextChar
  inc hl
  jr -

_DrawTextDone:
  ret

DrawTextChar: ; 1a05
  push af
  push de
  push hl
    cp $1F
    jr nc, DrawTextChar_ASCII
    cp NEWLINE
    jr nz, +

DrawTextChar_NEWLINE:
    ; If we get here, it's a newline.
    ld hl, (DRAW_XY)
    inc l
    ld (DRAW_XY), hl
    call SetVRAMWriteAddressXY

    jr DrawTextChar_Done

+:  cp STARTOFLINE
    jr nz, +

DrawTextChar_STARTOFLINE:
    ld hl, (DRAW_XY) ; set x = 0
    ld h, 0
    ld (DRAW_XY), hl
    call SetVRAMWriteAddressXY
    jr DrawTextChar_Done
+:
DrawTextChar_ASCII:
    ld e, a      ; get char in de
    ld d, $00
    ld hl, AsciiToTilemapTable
    add hl, de   ; look up tile number
    ld a, (hl)
    rst WriteAToVDP

DrawTextChar_Done:
  pop hl
  pop de
  pop af
  ret

AsciiToTilemapTable: ; Real ASCII?
.db $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45 ; Control chars
.db $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45 ; Control chars
.db $00, $1B, $1C, $1D, $1E, $1F, $21, $5A, $23, $24, $22, $47, $43, $56, $44, $46 ; Punctuation
.db $49, $4A, $4B, $4C, $4D, $4E, $4F, $50, $51, $52, $5B, $5C, $41, $48, $42, $45 ; 0-9, punctuation
.db $45, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F ; A-P
.db $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1A, $58, $56, $59, $20, $57 ; Q-Z, punctuation
.db $1D, $25, $26, $27, $28, $29, $2A, $2B, $2C, $2D, $2E, $2F, $30, $31, $32, $33 ; a-p
.db $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E, $3F, $40                ; q-z, punctuation

.ends

.section "Draw codes (large)" force
DrawCodes: ; 1ab7
  ld  b, 4 ; Number of colours, also the colour index
-:push bc
    ld a,b
    dec a
    call DrawCodeBackground
  pop bc ; unnecessary - a is still the right number
  push bc
    ld a,b
    dec a
    call DrawCode_3x3
  pop bc
  djnz -

  xor a ; Reset the code entry point
  ld (CODE_ENTRY_CURRENT_INDEX),a
  ld (CODE_ENTRY_CURRENT_CHAR),a
  ld (MENU_WAIT_FRAMES),a
  ret
.ends

.section "Palette loader" force
LoadPalette: ; 1ad4
  ld de, $C000 ; Palette
  rst SetVRAMAddressToDE
  ld hl, PaletteData
  ld bc, (PaletteDataEnd - PaletteData) << 8 | VDP_DATA ; count (64 - GG?), dest
-:outi
  push af ; delay
  pop af
  djnz -
  ret
.ends

.section "Draw coloured code background" force
DrawCodeBackground: ; 1ae5
  ; Draws 5 rows of tile 256+a at row 6a
  push af
    sla a     ; a * 2
    ld l,a
    sla a
    add a,l   ; + a * 4
    ld l,a
    xor a
    ld h,a
    sla l     ; * 64
    rl h
    sla l
    rl h
    sla l
    rl h
    sla l
    rl h
    sla l
    rl h
    sla l
    rl h
    ld de, NAME_TABLE | VRAM_WRITE
    xor a
    adc hl,de ; Set VRAM address to row 6 * a
    ex de,hl
    rst SetVRAMAddressToDE
  pop af

  ld b, 5 * 32 ; = 5 rows
-:out (VDP_DATA),a   ; Tile index 256 + a
  push af
    ld a, 1
    out (VDP_DATA),a
  pop af
  djnz -
  ret
.ends

.section "Draw code (3x3)" force
DrawCode_3x3: ; 1b1d
  ld (HEX_ENTRY_VALUE),a ; Save code index
  sla a              ; * 4 + 3
  sla a
  add a,3
  ld c,a             ; -> bc
  xor a
  ld b,a             ; Point iy at 3rd byte of code n
  ld iy,CODE_1
  add iy,bc

  ld a,(HEX_ENTRY_VALUE) ; Get back index
  sla a              ; * 3
  ld l,a
  sla a
  add a,l
  ld l,a
  xor a
  ld h,a             ; -> hl
  sla l
  rl h
  sla l
  rl h
  sla l
  rl h
  sla l
  rl h
  sla l
  rl h
  sla l
  rl h                ; * 64

  ld de, VRAM_WRITE | NAME_TABLE + 2 * (3 + 1 * 32) ; Get name table address for the start of the code (3, 1 offset)
  xor a
  adc hl,de
  ex de,hl

  ld b,2             ; Outer counter
--:
  ld c,2             ; Inner counter
-:push bc
    ld a,(iy+0)      ; Get byte
    srl a            ; Get high nibble
    srl a
    srl a
    srl a
    call DrawHexChar_3x3
    ld a,(iy+0)      ; Low nibble
    and $0f
    call DrawHexChar_3x3
    dec iy           ; Previous byte
  pop bc
  dec c
  jr nz,-

  ld hl,4            ; hl += 4
  xor a
  adc hl,de
  ex de,hl
  djnz --            ; Two tile gap in the middle
  ret
.ends

.section "Draw a large hex digit" force
DrawHexChar_3x3: ; 1b85
  push de
    ld ix,DRAW_LARGE_SCRATCH ; Pointer to a bit of scratch RAM
    ld c,$5e     ; Tile index for start of big '0'
    ld (ix+0), 3 * 11 - 2 ; Delta to next row
    cp 10        ; A-F?
    jr c,+
    ld c,$be - 3 * 10 ; Tile index for start of big 'A' - adjustment to look up 10 = 'A'
    ld (ix+0), 3 * 8 - 2 ; Delta to next row
+:  ld b,a       ; Save value to draw
    rst SetVRAMAddressToDE
    ld a,b       ; n*3 + base
    sla a
    add a,b
    add a,c
    rst WriteAToVDP
    inc a        ; Plus the next two tiles
    rst WriteAToVDP
    inc a
    rst WriteAToVDP

    add a,(ix+0) ; Add delta to get to next row's tiles

    ld hl, 32 * 2 ; Delta 1 row
    or a
    adc hl,de
    ex de,hl

    ld b,a       ; Save a
    rst SetVRAMAddressToDE
    ld a,b
    rst WriteAToVDP  ; Next row of tiles
    inc a
    rst WriteAToVDP
    inc a
    rst WriteAToVDP

    ; Do it all again for the third row
    add a,(ix+0)
    ld hl, 32 * 2
    or a
    adc hl,de
    ex de,hl
    ld b,a
    rst SetVRAMAddressToDE
    ld a,b
    rst WriteAToVDP
    inc a
    rst WriteAToVDP
    inc a
    rst WriteAToVDP
  pop hl
  ld de,3 * 2    ; Move VRAM address right by 3 tiles
  xor a
  adc hl,de
  ex de,hl
  ret
.ends

.section "1bpp tile loader" force
LoadTiles: ; 1bd3
  ld de, VRAM_WRITE | 0 ; VRAM address 0 - low tiles
  rst SetVRAMAddressToDE
  ld bc, FontDataEnd - FontData ; Count
  ld hl, FontData ; Address

  ld a, c ; fix counter for multiples of 256
  or a
  jr z, +
  inc b
+:
-:ld a, (hl)   ; get byte
  inc hl
  out (VDP_DATA), a ; write
  push af      ; delay
  pop af
  out (VDP_DATA), a ; Repeat - so black is 0, white is 15
  push af
  pop af
  out (VDP_DATA), a
  push af
  pop af
  out (VDP_DATA), a
  dec c
  jr nz, -
  djnz -

  ld de, VRAM_WRITE | (32 * 256) ; high tiles
  rst SetVRAMAddressToDE
  ld bc, ColouredTileDataSize
  ld hl, ColouredTileData
  ld a, c
  or a
  jr z, +
  inc b
+:
-:ld a, (hl)
  inc hl
  out (VDP_DATA), a
  dec c
  jr nz, -
  djnz -
  ret
.ends

.section "Flash border" force
; Flash border forever
FlashBorder: ; 1c10
  ld b,0
--:
-:in a,(VDP_STATUS) ; Wait for VBlank
  rlca
  jr nc,-
  ld a,3
  out (VDP_REGISTER),a
  ld a,$87  ; Border colour
  out (VDP_REGISTER),a
-:in a,(VDP_STATUS) ; Wait again
  rlca
  jr c,-
  ld a,$f
  out (VDP_REGISTER),a
  ld a,$87
  out (VDP_REGISTER),a
  jp --
.ends

.section "Wait for VBlank" force
WaitForVBlank: ; $1c2f
-:in a, (VDP_STATUS)
  rlca
  jr nc, -
  ret
.ends

.section "Unused tile-drawing routines" force
DrawTestPattern1: ; 1c35
  ; Writes tile c to VRAM in a column, b rows long (test pattern)
  ld a,c             ; a gets what was in c
  ld c,b             ; c gets what was in b
  ld b,a             ; Not used
-:push bc
    rst WriteAToVDP
    ex de,hl         ; Next row
    ld bc,32 * 2
    add hl,bc
    ex de,hl
  pop bc
  dec c
  jr nz, -
  ret

CopyToVRAMWithMask: ; 1c45
  ; args: de = VRAM address, hl = pointer, a = bitmask, bc = counter
  ; Writes bc bytes from hl to VRAM address de; but uses a as a bitmask for each 4 bytes. 1 = use data, 0 = zero
  ld ($c201),a       ; Backup
  rst SetVRAMAddressToDE
--:
  ld a,(hl)          ; Get value
  exx                ; Swap registers
    ld bc, (4 << 8) | VDP_DATA ; counter
    ld h,a

    ld a,($c201)     ; Restore
-:  rra              ; Get a bit onto the carry flag
    ld d,h           ; d = value
    jr c,+
    ld d,0           ; or d = 0
+:  out (c),d        ; Write to VDP
    djnz -           ; 4 times
  exx

  inc hl             ; Next byte
  dec bc
  ld a,b
  or c
  jr nz,--
  ret

SpriteTestPattern: ; 1c64
  ; y coords = 4, 10, 16, ... for 32 sprites
  ld de,VRAM_WRITE | SPRITE_TABLE
  rst SetVRAMAddressToDE
  ld b,32            ; count
  ld a,4             ; initial value
-:out (VDP_DATA),a
  push af
  pop af
  add a,6
  djnz -

  ; x, n = 4, 10, 16, ... for 32 sprites
  ld de,VRAM_WRITE | SPRITE_TABLE + 128
  rst SetVRAMAddressToDE
  ld b,32            ; count
  ld a,4             ; initial value
-:out (VDP_DATA),a
  push af
  pop af
  out (VDP_DATA),a
  push af
  pop af
  add a,6
  djnz -
  ret    

BlankFirstTile: ; 1c89
  ld de,VRAM_WRITE | 0 ; start of VRAM
  ld bc,32           ; one tile
  ld l,0             ; Value
  rst FillVRAM
  ret
.ends

.section "Name table clearer" force
ClearNameTable: ; 1c93
-:in a, (VDP_STATUS)  ; read VDP status register
  rlca         ; interrupt bit to c
  jr nc, -     ; wait
  ld de, NAME_TABLE | VRAM_WRITE
  ld bc, $0700 ; Size
  ld l, $00    ; Value to write
  rst FillVRAM
  ld de, NAME_TABLE | VRAM_WRITE ; Why?
  rst SetVRAMAddressToDE
  ld de, $0000 ; 0, 0
  ld (DRAW_XY), de ; set text cursor position to 0,0
  ret
.ends

.section "Test pattern 2 - unused" force
; Test pattern - unused
DrawTestPattern2: ; 1cad
  ld de,NAME_TABLE | VRAM_WRITE
  ld bc,$0300 ; Count
  rst SetVRAMAddressToDE
-:ld a,c      ; value
  cpl         ; ...inverted so it's incrementing
  out (VDP_DATA),a
  xor a       ; Low tiles
  push af     ; delay
  pop af
  out (VDP_DATA),a
  push af     ; delay
  pop af
  dec c
  jr nz,-     ; Loop
  djnz -
  ret

  ret         ; Unused?
.ends

.section "Read joypads" force
GetInputs: ; ORs P1 and P2, returns value in a, stores it in BUTTONS_PRESSED too
  push hl
  push bc
    in a, (IO_PORT_A) ; get P1 inputs
    xor $ff        ; invert (NEG)
    ld b, a        ; -> b

    ld hl, BUTTONS_PRESSED ; store here
    and BUTTON_U | BUTTON_D | BUTTON_L | BUTTON_R | BUTTON_1 | BUTTON_2 ; mask to P1
    ld (hl), a     ; store

    ld a, b        ; get back all bits
    rlca           ; get P2 bits
    rlca
    and BUTTON_U | BUTTON_D
    or (hl)        ; OR into the value
    ld (hl), a
    in a, (IO_PORT_B)    ; Get rest of P2 inputs
    xor $ff
    sla a
    sla a
    and BUTTON_L | BUTTON_R | BUTTON_1 | BUTTON_2 | BUTTON_RESET
    or (hl)        ; OR in
    ld (hl), a
  pop bc
  pop hl
  ret
.ends

.section "Draw hex byte" force
DrawHexByte: ; 1ce9
  push af
    srl a
    srl a
    srl a
    srl a
    cp $0a
    jr c,+
    add a,7
+:  add a,$30
    call DrawTextChar
  pop af
  and $0f
  cp $0a
  jr c,+
  add a,7
+:add a,$30
  call DrawTextChar
  ret
.ends

.section "Draw hex word" force
DrawHexWord: ; 1d0c
  push af
  push hl
    ld a,h
    call DrawHexByte
    ld a,l
    call DrawHexByte
  pop hl
  pop af
  ret
.ends

.section "RAM code: map to game and boot via BIOS" force
MapToGameAndBoot_Slow: ; 1d19
  ld (PAR_REGISTER_ENABLE_GAME_ROM),a         ; Switch back to game ROM
  ld (PAR_REGISTER_ENABLE_UPPER_ROM_BANK),a   ; Enable "cheat application mode"
  in a,(IO_PORT_A) ; Not used - delay?
  ld a,%11101011   ; Disable cart ROM
  ;        | `- I/O chip
  ;        `--- System RAM
  out (MEMORY_CONTROL),a
  ld a,%11100011 ; Enable BIOS ROM
  ;        ||`- I/O chip
  ;        |`-- BIOS ROM
  ;        `--- System RAM
  out (MEMORY_CONTROL),a
  jp $0000
  nop
  nop
  nop
MapToGameAndBoot_SlowEnd:
.ends

.section "RAM code: map to game and boot directly" force
MapToGameAndBoot_Fast: ; 1d2f
  ld (PAR_REGISTER_ENABLE_GAME_ROM),a         ; Switch back to game ROM
  ld (PAR_REGISTER_ENABLE_UPPER_ROM_BANK),a   ; Enable "cheat application mode"
  jp $0000 ; start of game
  nop
  nop
  nop
MapToGameAndBoot_FastEnd:
.ends

.section "RAM/ROM dump + boot game - unused" force
MemoryDump:
  ld de,8            ; Movement delta
  ld (MEMORY_DUMP_ADDRESS_DELTA),de
  ld hl,RAM_START    ; RAM dump start position
  ld (MEMORY_DUMP_CURRENT_ADDRESS),hl

_Loop:
-:in a,(VDP_STATUS)  ; Wait for VBlank
  rlca
  jr nc,-

  call GetInputs     ; Used later

  ; Draw hex dump
  setxy 2,4
  ld hl,(MEMORY_DUMP_CURRENT_ADDRESS)
  ld c,8             ; 8 rows
--:
  call DrawHexWord   ; word (4 chars)
  ld a,' '
  call DrawTextChar  ; 2 spaces
  call DrawTextChar

  ld b,8             ; then draw 8 bytes with spaces between them (3 * 8 = 24 chars)
-:ld a,(hl)
  ld (HEX_ENTRY_XY),hl
  ld hl,0
  call DrawHexByte
  ld hl,(HEX_ENTRY_XY)
  inc hl
  ld a,' '
  call DrawTextChar
  djnz -

  ld a,' '
  call DrawTextChar  ; 2 spaces makes it 32 chars total -> one row
  call DrawTextChar

  dec c              ; Next row
  ld a,c
  or a
  jr nz,--

  ; Process inputs
  ld a,(BUTTONS_PRESSED)
  bit 0,a            ; Up
  jr z,+
  ld de,(MEMORY_DUMP_ADDRESS_DELTA) ; Subtract delta from address
  ld hl,(MEMORY_DUMP_CURRENT_ADDRESS)
  xor a
  sbc hl,de
  ld (MEMORY_DUMP_CURRENT_ADDRESS),hl

+:ld a,(BUTTONS_PRESSED)
  bit 1,a            ; Down
  jr z,+
  ld de,(MEMORY_DUMP_ADDRESS_DELTA) ; Add delta
  ld hl,(MEMORY_DUMP_CURRENT_ADDRESS)
  add hl,de
  ld (MEMORY_DUMP_CURRENT_ADDRESS),hl

+:ld a,(BUTTONS_PRESSED)
  bit 4,a            ; Button 1 -> delta is 256 bytes
  jr z,+
  ld de,256
  jr ++
+:ld de,8
++:
  ld (MEMORY_DUMP_ADDRESS_DELTA),de

  ld a,(BUTTONS_PRESSED)
  bit 5,a             ; Button 2 -> boot game
  jr z,+
  ld hl,MapToGameAndBoot_Slow
  ld de,SRAM_START
  ld bc,MapToGameAndBoot_SlowEnd - MapToGameAndBoot_Slow
  ldir

  ld hl,INITIALISED_MARKER_VALUE
  ld (INITIALISED_MARKER),hl
  jp SRAM_START

+:jp _Loop

-: jp - ; Not sure what this is for
.ends

.section "Test data - unused" force
; 1de7
.db "The quick brown fox."
.db STRING_TERMINATOR
.ends

.section "Unused fragment" force
  ; Looks like the end of something? The djnz goes to an invalid place, rst $38 will not work...
  ld (hl),$80
  and b
  add a,c
  rst $38
  add a,d
  rst $38
  add a,e
  rst $38
  add a,h
  rst $38
  add a,l
  ei
  add a,(hl)
  nop
  adc a,b
  nop
  adc a,c
  rst $38
  adc a,d
  nop
  add a,a
  djnz -64 ; $1dd4
.ends

.section "PSG initialisation data" force
PSGSilence: ; $1e14
.db %10011111 | (0 << 5)
.db %10011111 | (1 << 5)
.db %10011111 | (2 << 5)
.db %10011111 | (3 << 5)
PSGSilenceEnd:
.ends

.section "Unused data" force
; 1e18
.dw $0064, $0065, $0066, $0067, $0068, $0069, $006a
.dw $008c, $008d, $008e, $008f, $0090, $0091
.dw $00b4, $00b5, $00b6, $00b7, $00b8, $00b9
.ends

.section "Palette" force
PaletteData: ; $1e3e
.db $00 $02 $08 $20 $22 $05 $06 $07 $08 $09 $0a $0b $0c $0d $0e $ff ; unused - last 32 bytes force (identical values)
.db $00 $01 $02 $03 $04 $05 $06 $07 $08 $09 $0a $0b $0c $0d $0e $0f ; unused
.db $00 $02 $08 $20 $22 $05 $06 $07 $08 $09 $0a $0b $0c $0d $0e $ff ; tiles
;   ----used-----------                                         ---
.db $00 $01 $02 $03 $04 $05 $06 $07 $08 $09 $0a $0b $0c $0d $0e $0f ; sprites (unused?)
PaletteDataEnd:
.ends

.section "BIOS data - unused" force
.incbin "bios13fx.sms" skip $0fba read $446 ; ? TODO: look at BIOS disassembly
.incbin "bios13fx.sms" skip $1704 read 69*8 + 12 ; Font + 12 extra (?) bytes
.ends

.section "Blank" force
.dsb 32 $ff
.ends

.section "Tiles" force
FontData: ; $2518
.incbin "font-small.1bpp" read 94*8 ; 94 tiles
.incbin "font-large-digits.1bpp" read (11*9 - 3)*8 ; 10 digits plus 1 space except at end
.incbin "font-large-alpha.1bpp" read (8*9 - 6)*8 ; A-F plus 2 spaces except at end
FontDataEnd:

ColouredTileData:
.incbin "coloured-tiles.bin" fsize ColouredTileDataSize
.ends

.section "More unused data" force
; 2d98
.db $03 $03 $03 $03 $ff $ff $00 $ff
.db $ff $ff $ff $00 $00 $00 $ff $ff
.db $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $ff $00 $00 $00
.db $00
.ends

; Blank to $4000

.bank 1 slot 0

; This bank seems to be mapped into the lower 16KB when cheats are active.

.orga $0035
.section "Cheat application INT handler" force
ReturnToGameInterruptHandler: 
  ld (PAR_REGISTER_ENABLE_GAME_ROM), a ; Page game ROM back in. This is jumped to from the generated code.
  ; This is at $0038:
  jp GENERATED_CODE
  ; unreachable?
  jp ReturnToGameInterruptHandler
.ends

.orga $0066
.section "Cheat application NMI handler" force
  reti
.ends

.orga $0068
.section "Unused debug helper" force
; 4048 unused - when mapped to the lower 16KB these function calls will fail anyway.
  exx
    call ClearNameTable
    setxy 2,2
  exx
  ld a, (TIMER_LAST_DELTA_BCD)
  call DrawHexByte
  ld a, ' '
  call DrawTextChar
  ld a, (TIMER_LAST_DELTA_HEX)
  call DrawHexByte
  ld a, ' '
  call DrawTextChar
  ld a, (TIMER_DELTA_BCD)
  call DrawHexByte
  ld a, ' '
  call DrawTextChar
  ld a, (TIMER_DELTA_HEX)
  call DrawHexByte
  ld a, ' '
  call DrawTextChar
  ld a, (hl)
  call DrawHexByte
  ld a, ' '
  call DrawTextChar
  call DrawHexWord
  ld bc, $0203 ; 2, 3
  ld (DRAW_XY), bc
  call SetVRAMWriteAddressXY
  ex de, hl
  ld a, (hl)
  call DrawHexByte
  ld a, ' '
  call DrawTextChar
  call DrawHexWord
  jp FlashBorder
.ends

.section "ROM fill" force
.repeat 16184
.db $31 ; ld sp, $3131 - seems useless?
.endr
.ends
