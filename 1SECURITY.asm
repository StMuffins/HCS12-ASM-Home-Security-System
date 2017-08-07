#include "hcs12.inc"
lcd_dat         equ     portk
lcd_dir         equ     ddrk
lcd_E           equ     $02
lcd_RS          equ     $01

                org     $10ff            ;loads RMB safely away from stak

hrs2            rmb     1                ;reserves space for hours main clock
hrs1            rmb     1                ;reserves space for hours main clock
                fcb     $3A              ;forces ASCII value for ":"
min2            rmb     1                ;reserves space for minute main clk
min1            rmb     1                ;reserves space for minute main clk
                fcb     $3A
scn2            rmb     1                ;reserves space for sec main clk
scn1            rmb     1                ;reserves space for sec main clk
ampm            rmb     1                ;reserves either A or P for A.M. P.M.
                fcb     $4D              ; ASCII value for "M"
                fcb     $20              ;blank
                fcb     $20              ;blank
d               rmb     1                ;D or space
o               rmb     1                ;0 or space
n               rmb     1                ;N, F, or space
e               rmb     1                ;F or space
last            rmb     1

milsec          rmb     1               ;miliseconds

screen1         fcb     $53              ;Status: Ready
                fcb     $54
                fcb     $41
                fcb     $54
                fcb     $55
                fcb     $53
                fcb     $3A
st              rmb     1             ;DEPENDS ON THE MODE
ta              rmb     1
at              rmb     1
tu              rmb     1
us              rmb     1
sa              rmb     1
sb              rmb     1
sc              rmb     1
blast           rmb     1

screen2         fcb     $53              ;characters for screen 2
                fcb     $54              ;"S1:RIGHT S2: INCR"
                fcb     $41
                fcb     $54
                fcb     $55
                fcb     $53
                fcb     $3A
                fcb     $4E
                fcb     $4F
                fcb     $54
                fcb     $20
                fcb     $52
                fcb     $45
                fcb     $41
                fcb     $44
                fcb     $59
clast           rmb     1

screen3         fcb     $53              ;Characters for screen 3
                fcb     $31              ;"S1: SILENCE ALARM
                fcb     $3A
                fcb     $53
                fcb     $49
                fcb     $4C
                fcb     $45
                fcb     $4E
                fcb     $43
                fcb     $45
dlast           rmb     1

chk12           rmb     2                ;rmb for checking 12:00 or 13:00
chk13           rmb     2

unit            rmb     1                ;all of these are units that affect
unit2           rmb     1                ;the functions of the clock
unit3           rmb     1                ;used so the clock knows what the user
unita           rmb     1                ;is currently doing and functions
unitaa          rmb     1                ;accordingly
unitaaa         rmb     1                ;being used to count 4 key presses

zero            rmb     1                 ;rmbs just for 0
zero1           rmb     1

pin1            rmb     1                 ;passssssssword
pin2            rmb     1
pin3            rmb     1
pin4            rmb     1

e1              rmb     1
e2              rmb     1
e3              rmb     1
e4              rmb     1

num1            rmb     1
num2            rmb     1
num3            rmb     1
num4            rmb     1
num12           rmb     1

byp1            rmb     1
byp2            rmb     1
byp3            rmb     1
byp4            rmb     1
byp5            rmb     1
byp6            rmb     1

zone1           rmb     1
zone2           rmb     1
zone3           rmb     1
zone4           rmb     1
zone5           rmb     1
zone6           rmb     1

staup           rmb     1

alarmon         rmb     1
amode           rmb     1

count           rmb     1

                org     $2000                 ;starts at 2000
                lds     #$1999           ; stack
                movb    #$00,ATD0DIEN         ;starts atd0
                movb    #$07,ddrh         ;turns on portm pins for output
                movb    #$01,pucr
                movb    #$00,ddra
                movb    #$F0,ddrp
                movb    #$00,ddrb
                jsr     openLCD                 ;starts lcd
                ldx     #$3132                 ;everything below here is data
                ldy     #$3133                 ;storing for the RMB's above
                stx     chk12
                sty     chk13
                ldaa    #0                ; load A w/ 0
                staa    last             ;stores 0 so putslcd stops
                staa    blast
                staa    clast
                staa    dlast
                staa    zero
                staa    zero1
                staa    unit2
                staa    unita
                staa    unitaa

                staa    unitaaa
                staa    count
                staa    milsec
                staa    d
                staa    byp1
                staa    byp2
                staa    byp3
                staa    byp4
                staa    byp5
                staa    byp6
                staa    zone1
                staa    zone2
                staa    zone3
                staa    zone4
                staa    zone5
                staa    zone6
                ldaa    #1
                staa    unit
                staa    unita
                ldaa    #$30
                staa    min2
                staa    min1
                staa    scn2
                staa    scn1
                ldaa    #$31
                staa    hrs2
                ldaa    #$31
                staa    hrs1
                ldaa    #$41
                staa    ampm
                ;;;;;;;;;;;;;;;
                ldaa        #$52
                staa        st
                ldaa        #$45
                staa        ta
                ldaa        #$41
                staa        at
                ldaa        #$44
                staa        tu
                ldaa        #$59
                staa        us
                clr        sa
                clr        sb
                clr        sc
                ;              ;              ;
                ldaa    #3
                staa    pin1
                ldaa    #5
                staa    pin2
                ldaa    #7
                staa    pin3
                ldaa    #10
                staa    pin4
                ;;;;;;;;;;;;;;
                ldaa    #1
                staa    num1
                ldaa    #2
                staa    num2
                ldaa    #3
                staa    num3
                ldaa    #4
                staa    num4
                ldaa    #12
                staa    num12
                ;;;;;;;;;;;;;;
                clr     alarmon
                clr     amode
                ;;;;;;;;;;;;;;
                jsr     lcd1                ;updates LCD after all values
                bra     apple                ;stored


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this is the master branch checkpoint, gets rmb's and branches to specified
;subroutines!
yo              ldab    #0
                cmpb    unitaa
                lbeq    yorp
                ldab    #1
                cmpb    unitaa
                lbeq    yorc
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;



expire          movb        #$50,PTP
                ldy     #3
no              jsr        delay32nd
                dbne    y,no
                movb        #$00,PTP
                ldy     #3
nu              jsr     delay32nd
                dbne    y,nu
                movb        #$50,PTP
                ldy     #3
na              jsr        delay32nd
                dbne    y,na
                movb        #$00,PTP
               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
apple                                      ;password try
                clr     count
                clr     unit2        ;being used for 2 sec limit between buttons
                inc     unit2
                clr     unit3
                ldab    #4
                stab    unitaaa
                
                ldaa    #0
                cmpa    alarmon
                lbeq    nax
                movb    #$10,PTP
nax             ldaa    #0
		cmpa    amode
                lbeq    loop1
                movb    #$50,PTP
                
loop1           jsr     get_char       ;fetches values
yorp            ldab    unitaaa
                cmpb    num4          ;and pushes them on the stack
                bne     ay1
                staa    e1
                bra     here
ay1             cmpb    num3
                bne     ay2
                staa    e2
                bra     here
ay2             cmpb    num2
                bne     ay3
                staa    e3
                bra     here
ay3             staa    e4

here            movb    #$20,PTP
                jsr     delay32nd
                movb   	#$00,PTP
                ldaa    unit2
                inca
                inca
                staa    unit3           ;using UNIT3 NOW
                dec     unitaaa
                ldab    unitaaa
                cmpb    zero
                bne     loop1


again           ldaa	e1              ;pulls numbers off the stack
                cmpa	pin1        ;and compares to master password
                bne     a             ;skips to next comparison if number doesnt match
                inc     count         ;increments count if numbers match
a               ldaa    e2
                cmpa    pin2
                bne     b
                inc     count
b               ldaa    e3
                cmpa    pin3
                bne     c
                inc     count
c               ldaa    e4
                cmpa    pin4
                bne     NotEqual
                inc     count
NotEqual
                ldaa    count            ;loads the count
                cmpa    #4               ;count value is 4, if all four numbers match master password
                beq     Correct           ;goes to correct if correct


Wrong           clr     unitaa
                jsr     delay32nd
                movb    #$10,PTP
                ldy     #20
red             jsr     delay32nd
                dbne    y,red
                movb    #$00,PTP
                lbra    apple             ;goes back to 'apple' to fetch new batch of numbers

Correct
On              jsr     delay32nd
                movb    #$40,PTP
                ldy     #20
green           jsr     delay32nd
                dbne    y,green
                movb    #$00,PTP
                ldaa    #1
                staa    unitaa
                cmpa    unita
                bne     disarmed
                jsr     get_char
yorc
                staa    staup
                ldaa    #1
                cmpa    staup
                beq     disarmed
                ldaa    #4
                cmpa    staup
                beq     Wrong
                ldaa    #5
                cmpa    staup
                beq     Wrong
                ldaa    #7
                cmpa    staup
                beq     Wrong
                ldaa    #8
                cmpa    staup
                beq     Wrong
                ldaa    #10
                cmpa    staup
                beq     Wrong
                ldaa    #12
                cmpa    staup
                beq     Wrong
                ldaa    staup
                staa    unita
                bra     nexti;;;;;;;;;;;;;;;;

disarmed        ldaa    #1
                staa    unita
                ldaa	#$52
                staa	st
                ldaa	#$45
                staa	ta
                ldaa	#$41
                staa	at
                ldaa	#$44
                staa	tu
                ldaa	#$59
                staa	us
                clr     sa
                clr     sb
                clr     sc
                clr     byp1
                clr     byp2
                clr     byp3
                clr     byp4
                clr     byp5
                clr     byp6
                jsr     lcd1
                clr     unitaa
                clr     alarmon
                clr     amode
                lbra    apple
                
nexti           ldaa    #2
                staa    amode
                cmpa    unita
                bne     away
                ldaa    #$53
                staa    st
                ldaa    #$54
                staa    ta
                ldaa    #$41
                staa    at
                ldaa    #$59
                staa    tu
                clr     us
                clr     sa
                clr     sb
                clr     sc
                jsr     lcd1
                clr     unitaa
                lbra    apple

away            ldaa    #3
                staa    amode
                cmpa    unita
                bne     bypass
                ldaa    #$41
                staa    st
                ldaa    #$57
                staa    ta
                ldaa    #$41
                staa    at
                ldaa    #$59
                staa    tu
                clr     us
                clr     sa
                clr     sb
                clr     sc
                jsr     lcd1
                clr     unitaa
                lbra    apple
                
bypass          ldaa    #6
                cmpa    unita
                swi
                end


get_char
seconds         jsr     lcdin
                ldaa    unit3      ;expireing brenchie
                cmpa    unit2
                lbeq    expire
                
                
                ldab    #1
                cmpb    zone1
                beq     zd1
                cmpb    zone2
                beq     zd2
                cmpb    zone3
                beq     zd3
                cmpb    zone4
                beq     zd4
                cmpb    zone5
                beq     zd5
                cmpb    zone6
                beq     zd6
                bra     zzz
zd6             incb
zd5             incb
zd4             incb
zd3             incb
zd2             incb
zd1
                cmpb    st
                lbeq    c1
                ldaa    #$5A
                staa    st
                ldaa    #$4F
                staa    ta
                ldaa    #$4E
                staa    at
                ldaa    #$45
                staa    tu
                ldaa    #$20
                staa    us
                ldaa    #$30
                aba
                staa    sa
                clr     sb
                clr     sc
                jsr     lcd1
                bra     c1
                

zzz             ldaa    #1
                cmpa    alarmon
                beq     c1
		ldaa    #$5A
                cmpa    st
                bne     c1
                ldaa    #$52
                staa    st
                ldaa    #$45
                staa    ta
                ldaa    #$41
                staa    at
                ldaa    #$44
                staa    tu
                ldaa    #$59
                staa    us
                ldaa    #$20
                staa    sa
                staa    sb
                staa    sc
                jsr     lcd1
                bra     c1
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
c1              movb    #$06,PTH
n1              brset   PTA,$01,n4
                brclr   PTA,$01,*
                ldaa    #1
                lbra    yo
                
n4              brset   PTA,$02,n7
                brclr   PTA,$02,*
                ldaa    #4
                lbra    yo
                
n7              brset   PTA,$04,nst
                brclr   PTA,$04,*
                ldaa    #7
                lbra     yo
                
nst             brset   PTA,$08,c2
                brclr   PTA,$08,*
                ldaa    unit2
                cmpa    zero
                lbne     expire
                ldaa    #11
                lbra     yo
                
c2              movb    #$05,PTH
n2              brset   PTA,$01,n5
                brclr   PTA,$01,*
                ldaa    #2
                lbra     yo
                
n5              brset   PTA,$02,n8
                brclr   PTA,$02,*
                ldaa    #5
                lbra     yo
                
n8              brset   PTA,$04,n0
                brclr   PTA,$04,*
                ldaa    #8
                lbra     yo
                
n0              brset   PTA,$08,c3
                brclr   PTA,$08,*
                ldaa    #10
                lbra     yo
                
c3              movb    #$03,PTH
n3              brset   PTA,$01,n6
                brclr   PTA,$01,*
                ldaa    #3
                lbra     yo
                
n6              brset   PTA,$02,n9
                brclr   PTA,$02,*
                ldaa    #6
                lbra     yo
                
n9              brset   PTA,$04,nh
                brclr   PTA,$04,*
                ldaa    #9
                lbra     yo
                
nh              brset   PTA,$08,b0
                brclr   PTA,$08,*
                ldaa    #12
                lbra     yo
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this is poling the sensors (decided it to be port B because ezpz

b0              ldaa    #1
                cmpa    byp1
                beq     b1
                ldaa    #0
                staa    zone1
                brclr   PTB,$01,b1
                ldaa    #1
                staa    zone1
                ldaa    #0
                cmpa    amode
                lbeq    inmilsec
                ldaa    #1
                staa    alarmon
                lbra    inmilsec
                
b1              ldaa    #1
                cmpa    byp2
                beq     b2
                ldaa    #0
                staa    zone2
                brclr   PTB,$02,b2
                ldaa    #1
                staa    zone2
                ldaa    #0
                cmpa    amode
                lbeq    inmilsec
                ldaa    #1
                staa    alarmon
                lbra     inmilsec
                
b2              ldaa    #1
                cmpa    byp3
                beq     b3
                ldaa    #0
                staa    zone3
                brclr   PTB,$04,b3
                ldaa    #1
                staa    zone3
        	ldaa    #0
                cmpa    amode
                lbeq    inmilsec
                ldaa    #1
                staa    alarmon
                lbra     inmilsec
                
b3              ldaa    #1
                cmpa    byp4
                beq     b4
                ldaa    #0
                staa    zone4
                brclr   PTB,$08,b4
                ldaa    #1
                staa    zone4
              	ldaa    #0
                cmpa    amode
                lbeq    inmilsec
                ldaa    #1
                staa    alarmon
                lbra     inmilsec
                
b4              ldaa    #1
                cmpa    byp5
                beq     b5
                ldaa    #0
                staa    zone5
                brclr   PTB,$10,b5
                ldaa    #1
                staa    zone5
        	ldaa    #0
                cmpa    amode
                lbeq    inmilsec
                ldaa    #1
                staa    alarmon
                bra     inmilsec
                
b5              ldaa    #1
                cmpa    byp6
                beq     inmilsec
                ldaa    #0
                staa    zone6
                brclr   PTB,$20,inmilsec
                ldaa    #1
                staa    zone6
        	ldaa    #0
                cmpa    amode
                lbeq    inmilsec
                ldaa    #1
                staa    alarmon
                bra     inmilsec
                

                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this is the normal time-running operation, seconds, minutes, hours ticking by
inmilsec        inc     milsec          ;MILISECONDS NOT WORKING< NEED TO FIX
                ldaa    #30
                cmpa    milsec
                lbne    seconds
                clr     milsec
insc1
                inc     scn1
                ldaa    #0
                cmpa    unit3
                beq     noex
                inc     unit2          ;more password expire
noex            ldaa    #$3A
                cmpa    scn1
                lbgt    seconds
                ldaa    #$30
                staa    scn1
                
insc2           inc     scn2
                ldab    #$36
                cmpb    scn2
                lbgt    seconds
                ldaa    #$30
                staa    scn1
ss2             staa    scn2

inmn1           inc     min1
                ldaa    #$3A
                cmpa    min1
                lbgt    seconds
                ldaa    #$30
                staa    scn1
                staa    scn2
sm1             staa    min1

                
inmn2           inc     min2
                ldab    #$36
                cmpb    min2
                lbgt    seconds
                ldaa    #$30
                staa    scn1
                staa    scn2
                staa    min1
sm2             staa    min2
                lbeq    seconds
                
inhr1           inc     hrs1
                ldaa    hrs2
                ldab    hrs1
                cpd     chk12
                beq     taco

                ldaa    hrs2
                ldab    hrs1
                cpd     chk13
                beq     tacor
                
                ldab    #$3A
                cmpb    hrs1
                lbne    seconds
                ldaa    #$30
                staa    scn1
                staa    scn2
                staa    min1
                staa    min2
sh1             staa    hrs1
                
inhr2           inc     hrs2
                jmp     seconds

tacor           ldaa    #$31
                staa    hrs1
                ldab    #$30
                stab    hrs2
                stab    min2
                stab    min1
                stab    scn2
                stab    scn1
                jmp     seconds

taco
                ldab    #$41
                cmpb    ampm
                beq     pm
                stab    ampm
                jmp     seconds
pm              ldab    #$50
                stab    ampm
                jmp     seconds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;when alarm is "on" branches here and compares current time to alarm time





;calculated values to LCD
lcdin
                ldaa    #$80            ;value specifies top line of lcd
                jsr     cmd2LCD         ;uses value in LCD cmd
                ldx     #hrs2           ;loads inch ASCII value
                jsr     putsLCD         ;uses value and displays it on LCD
                ldy     #$01            ;short delay
                jsr     delayby10ms
                rts

lcd1
                ldaa    #$C0            ;value specifies bottom line of lcd
                jsr     cmd2LCD         ;uses value in lcd cmd
                ldx     #screen1        ;loads cm ASCII value
                jsr     putsLCD         ;uses value and displays it on lcd
                ldy     #$28            ;delays $28 * 10ms
                jsr     delayby10ms
                rts

lcd2            ldaa    #$C0            ;value specifies bottom line of lcd
                jsr     cmd2LCD         ;uses value in lcd cmd
                ldx     #screen2        ;loads cm ASCII value
                jsr     putsLCD         ;uses value and displays it on lcd
                ldy     #$28            ;delays $28 * 10ms
                jsr     delayby10ms
                rts

lcd3
                ldaa    #$C0            ;value specifies bottom line of lcd
                jsr     cmd2LCD         ;uses value in lcd cmd
                ldx     #screen3        ;loads cm ASCII value
                jsr     putsLCD         ;uses value and displays it on lcd
                ldy     #$28            ;delays $28 * 10ms
                jsr     delayby10ms
                rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cmd2LCD     psha
            bclr    lcd_dat,lcd_RS
            bset    lcd_dat,lcd_E
            anda    #$F0
            lsra
            lsra
            oraa    #lcd_E
            staa    lcd_dat
            nop
            nop
            nop
            nop
            bclr    lcd_dat,lcd_E
            pula
            anda    #$0F
            lsla
            lsla
            bset    lcd_dat,lcd_E
            oraa    #lcd_e
            staa    lcd_dat
            nop
            nop
            nop
            bclr    lcd_dat,lcd_E
            ldy     #2
            jsr     delayby1ms
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
openLCD     movb    #$FF,lcd_dir
            ldy     #10
            jsr     delayby1ms
            ldaa    #$28
            jsr     cmd2lcd
            ldaa    #$01
            jsr     cmd2lcd
            ldaa    #$06
            jsr     cmd2lcd
            ldaa    #$01
            jsr     cmd2lcd
            ldy     #2
            jsr     delayby1ms
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
putcLCD     psha
            bset    lcd_dat,lcd_RS
            bset    lcd_dat,lcd_E
            anda    #$F0
            lsra
            lsra
            oraa    #$03
            staa    lcd_dat
            nop
            nop
            nop
            bclr    lcd_dat,lcd_E
            pula
            anda    #$0F
            lsla
            lsla
            bset    lcd_dat,lcd_E
            oraa    #$03
            staa    lcd_dat
            nop
            nop
            nop
            bclr    lcd_dat,lcd_E
            ldy     #1
            jsr     delayby1ms
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
putsLCD     ldaa    1,x+                    ; get one char from string
            beq     donePS                  ; reach NULL character
            jsr     putcLCD
            bra     putsLCD
donePS      rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
delayby1ms  movb    #$90,TSCR1              ; enable TCNT & ffclr
            movb    #$06,TSCR2              ; config prs factor to 64
            movb    #$01,TIOS               ; enable OC0
            ldd     TCNT
again0      addd    #375                    ; start an o/c operation
            std     TC0                     ; with 50 ms time delay
wait_lp0    brclr   TFLG1,$01,wait_lp0
            ldd     TC0
            dbne    y,again0
            rts

delayby10ms movb    #$90,TSCR1              ; enable TCNT & ffclr
            movb    #$06,TSCR2              ; config prs factor to 64
            movb    #$01,TIOS               ; enable OC0
            ldd     TCNT
again2      addd    #3750                   ; start an output compare operation
            std     TC0                     ; with 50 ms time delay
wait_lp     brclr   TFLG1,$01,wait_lp
            ldd     TC0
            dbne    y,again2
            rts

delay20us   movb    #$90,tscr1              ; enable timer and ffclr
            movb    #$00,tscr2              ; prescale set to 1
            movb    #$01,TIOS               ; enable OC0
            ldd     tcnt                    ; enable time counter
            addd    #480                    ; add 480 to register D
            std     tc0                     ; enable ch.0 for o/c
loop        brclr   tflg1,$01,loop          ; branch clear
            rts

delay32nd   movb    #$90,TSCR1         ; enable TCNT and fast flags clear
            movb    #$06,TSCR2         ; configure prescale factor to 8
            movb    #$01,TIOS          ; enable OC0
            ldd     TCNT                    ; enable time counter
            addd    #11700
            std     TC0
            brclr   TFLG1,$01,*
            rts