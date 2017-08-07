; ************************************************************************
; Prepared by Dr. Han-Way Huang
; Date: 12/31/2004
; HC12SDP256 I/O register locations
; HCS12 peripheral bits definitions
; D-Bug12 I/O functions calling address
; D-Bug12 SRAM interrupt vector table
; Flash and EEPROM commands
; ************************************************************************

PORTA           equ     0       ; port a = address lines a8 - a15
PTA             equ     0       ; alternate name for PORTA
PORTB           equ     1       ; port b = address lines a0 - a7
PTB             equ     1       ; alternate name for PORTB
DDRA            equ     2       ; port a direction register
DDRB            equ     3       ; port a direction register

PORTE           equ     8       ; port e = mode,irqandcontrolsignals
PTE             equ     8       ; alternate name for PORTE
DDRE            equ     9       ; port e direction register
PEAR            equ     $a      ; port e assignments
MODE            equ     $b      ; mode register
PUCR            equ     $c      ; port pull-up control register
RDRIV           equ     $d      ; port reduced drive control register
EBICTL          equ     $e      ; e stretch control

INITRM          equ     $10     ; ram location register
INITRG          equ     $11     ; register location register
INITEE          equ     $12     ; eeprom location register
MISC            equ     $13     ; miscellaneous mapping control
MTST0           equ     $14     ; reserved
ITCR            equ     $15     ; interrupt test control register
ITEST           equ     $16     ; interrupt test register
MTST1           equ     $17     ; reserved

PARTIDH         equ     $1a     ; part id high
PARTIDL         equ     $1b     ; part id low
MEMSIZ0         equ     $1c     ; memory size
MEMSIZ1         equ     $1d     ; memory size
INTCR           equ     $1e     ; interrupt control register
IRQCR           equ     $1e     ; interrupt control register
HPRIO           equ     $1f     ; high priority reg

BKPCT0          equ     $28     ; break control register
BKPCT1          equ     $29     ; break control register
BKP0X           equ     $2a     ; break 0 index register
BKP0H           equ     $2b     ; break 0 pointer high
BRP0L           equ     $2c     ; break 0 pointer low
BKP1X           equ     $2d     ; break 1 index register
BKP1H           equ     $2e     ; break 1 pointer high
BRP1L           equ     $2f     ; break 1 pointer low
PPAGE           equ     $30     ; program page register

PORTK           equ     $32     ; port k data
PTK             equ     $32     ; alternate name for PORTK
DDRK            equ     $33     ; port k direction
SYNR            equ     $34     ; synthesizer / multiplier register
REFDV           equ     $35     ; reference divider register
CTFLG           equ     $36     ; reserved
CRGFLG          equ     $37     ; pll flags register
CRGINT          equ     $38     ; pll interrupt register
CLKSEL          equ     $39     ; clock select register
PLLCTL          equ     $3a     ; pll control register
RTICTL          equ     $3b     ; real time interrupt control
COPCTL          equ     $3c     ; watchdog control
FORBYP          equ     $3d     ;
CTCTL           equ     $3e     ;
ARMCOP          equ     $3f     ; cop reset register

TIOS            equ     $40     ; timer input/output select
CFORC           equ     $41     ; timer compare force
OC7M            equ     $42     ; timer output compare 7 mask
OC7D            equ     $43     ; timer output compare 7 data
TCNT            equ     $44     ; timer counter register hi
TSCR1           equ     $46     ; timer system control register
TSCR            equ     $46     ; 
TTOV            equ     $47     ; reserved
TCTL1           equ     $48     ; timer control register 1
TCTL2           equ     $49     ; timer control register 2
TCTL3           equ     $4a     ; timer control register 3
TCTL4           equ     $4b     ; timer control register 4
TMSK1           equ     $4c     ; timer interrupt mask 1
TIE             equ     $4C     ; 
TSCR2           equ     $4d     ; timer system control register 2
TFLG1           equ     $4e     ; timer flags 1
TFLG2           equ     $4f     ; timer flags 2
TC0             equ     $50     ; timer capture/compare register 0
TC1             equ     $52     ; timer capture/compare register 1
TC2             equ     $54     ; timer capture/compare register 2
TC3             equ     $56     ; timer capture/compare register 3
TC4             equ     $58     ; timer capture/compare register 4
TC5             equ     $5a     ; timer capture/compare register 5
TC6             equ     $5c     ; timer capture/compare register 6
TC7             equ     $5e     ; timer capture/compare register 7
PACTL           equ     $60     ; pulse accumulator controls
PAFLG           equ     $61     ; pulse accumulator flags
PACNT           equ     $62     ; pulse accumulator A counter
PACN3           equ     $62     ; pulse accumulator counter 3
PACN2           equ     $63     ; pulse accumulator counter 2
PBCNT           equ     $64     ; pulse accumulator B counter 
PACN1           equ     $64     ; pulse accumulator counter 1
PACN0           equ     $65     ; pulse accumulator counter 0
MCCTL           equ     $66     ; modulus down conunter control
MCFLG           equ     $67     ; down counter flags
ICPAR           equ     $68     ; input pulse accumulator control
DLYCT           equ     $69     ; delay count to down counter
ICOVW           equ     $6a     ; input control overwrite register
ICSYS           equ     $6b     ; input control system control

TIMTST          equ     $6d     ; timer test register

PBCTL           equ     $70     ; pulse accumulator b control
PBFLG           equ     $71     ; pulse accumulator b flags
PA3H            equ     $72     ; pulse accumulator holding register 3
PA2H            equ     $73     ; pulse accumulator holding register 2
PA1H            equ     $74     ; pulse accumulator holding register 1
PA0H            equ     $75     ; pulse accumulator holding register 0
MCCNT           equ     $76     ; modulus down counter register
*MCCNTL         equ     $77     ; low byte
TCOH            equ     $78     ; capture 0 holding register
TC1H            equ     $7a     ; capture 1 holding register
TC2H            equ     $7c     ; capture 2 holding register
TC3H            equ     $7e     ; capture 3 holding register

ATD0CTL0        equ     $80     ; adc control 0 (reserved)
ATD0CTL1        equ     $81     ; adc control 1 (reserved)
ATD0CTL2        equ     $82     ; adc control 2
ATD0CTL3        equ     $83     ; adc control 3
ATD0CTL4        equ     $84     ; adc control 4
ATD0CTL5        equ     $85     ; adc control 5
ATD0STAT0       equ     $86     ; adc status register 0
ATD0TEST0       equ     $88     ; adc test 0(reserved)
ATD0TEST1       equ     $89     ; adc test 1(reserved)

ATD0DIEN        equ     $8d     ; adc0 input enable register
PORTAD0         equ     $8f     ; port adc = input only
PTAD0           equ     $8F
ATD0DR0         equ     $90     ; adc result 0 register
ATD0DR1         equ     $92     ; adc result 1 register
ATD0DR2         equ     $94     ; adc result 2 register
ATD0DR3         equ     $96     ; adc result 3 register
ATD0DR4         equ     $98     ; adc result 4 register
ATD0DR5         equ     $9a     ; adc result 5 register
ATD0DR6         equ     $9c     ; adc result 6 register
ATD0DR7         equ     $9e     ; adc result 7 register

PWME            equ     $a0     ; pwm enable
PWMPOL          equ     $a1     ; pwm polarity
PWMCLK          equ     $a2     ; pwm clock select register
PWMPRCLK        equ     $a3     ; pwm prescale clock select register
PWMCAE          equ     $a4     ; pwm center align select register
PWMCTL          equ     $a5     ; pwm control register
PWMTST          equ     $a6     ; reserved
PWMPRSC         equ     $a7     ; reserved
PWMSCLA         equ     $a8     ; pwm scale a
PWMSCLB         equ     $a9     ; pwm scale b
PWMSCNTA        equ     $aa     ; reserved
PWMSCNTB        equ     $ab     ; reserved
PWMCNT0         equ     $ac     ; pwm channel 0 counter
PWMCNT1         equ     $ad     ; pwm channel 1 counter
PWMCNT2         equ     $ae     ; pwm channel 2 counter
PWMCNT3         equ     $af     ; pwm channel 3 counter


PWMCNT4         equ     $b0     ; pwm channel 4 COUNTER
PWMCNT5         equ     $b1     ; pwm channel 5 counter
PWMCNT6         equ     $b2     ; pwm channel 6 counter
PWMCNT7         equ     $b3     ; pwm channel 7 counter
PWMPER0         equ     $b4     ; pwm channel 0 period
PWMPER1         equ     $b5     ; pwm channel 1 period
PWMPER2         equ     $b6     ; pwm channel 2 period
PWMPER3         equ     $b7     ; pwm channel 3 period
PWMPER4         equ     $b8     ; pwm channel 4 period
PWMPER5         equ     $b9     ; pwm channel 5 period
PWMPER6         equ     $ba     ; pwm channel 6 period
PWMPER7         equ     $bb     ; pwm channel 7 period
PWMDTY0         equ     $bc     ; pwm channel 0 duty cycle
PWMDTY1         equ     $bd     ; pwm channel 1 duty cycle
PWMDTY2         equ     $be     ; pwm channel 2 duty cycle
PWMDTY3         equ     $bf     ; pwm channel 3 duty cycle
PWMDTY4         equ     $c0     ; pwm channel 0 duty cycle
PWMDTY5         equ     $c1     ; pwm channel 1 duty cycle
PWMDTY6         equ     $c2     ; pwm channel 2 duty cycle
PWMDTY7         equ     $c3     ; pwm channel 3 duty cycle
PWMSDN          equ     $c4     ; pwm shutdown register

SCI0BDH         equ     $c8     ; sci 0 baud reg hi byte
SCI0BDL         equ     $c9     ; sci 0 baud reg lo byte
SCI0CR1         equ     $ca     ; sci 0 control1 reg
SCI0CR2         equ     $cb     ; sci 0 status reg 1
SCI0SR1         equ     $cc     ; sci 0 status reg 1
SCI0SR2         equ     $cd     ; sci 0 status reg 2
SCI0DRH         equ     $ce     ; sci 0 data reg hi
SCI0DRL         equ     $cf     ; sci 0 data reg lo
SCI1BDH         equ     $d0     ; sci 1 baud reg hi byte
SCI1BDL         equ     $d1     ; sci 1 baud reg lo byte
SCI1CR1         equ     $d2     ; sci 1 control1 reg
SCI1CR2         equ     $d3     ; sci 1 control2 reg
SCI1SR1         equ     $d4     ; sci 1 status reg 1
SCI1SR2         equ     $d5     ; sci 1 status reg 2
SCI1DRH         equ     $d6     ; sci 1 data reg lo
SPI0CR1         equ     $d8     ; spi 0 control1 reg
SPI0CR2         equ     $d9     ; spi 0 control2 reg
SPI0BR          equ     $da     ; spi 0 baud reg
SPI0SR          equ     $db     ; spi 0 status reg hi

SPI0DR          equ     $dd     ; spi 0 data reg

IBAD            equ     $e0     ; i2c bus address register
IBFD            equ     $e1     ; i2c bus frequency divider
IBCR            equ     $e2     ; i2c bus control register
IBSR            equ     $e3     ; i2c bus status register
IBDR            equ     $e4     ; i2c bus message data register

DLCBCR1         equ     $e8     ; bdlc control register 1
DLCBSVR         equ     $e9     ; bdlc state vector register
DLCBCR2         equ     $ea     ; bdlc control register 2
DLCBDR          equ     $eb     ; bdlc data register
DLCBARD         equ     $ec     ; bdlc analog delay register
DLCBRSR         equ     $ed     ; bdlc rate select register
DLCSCR          equ     $ee     ; bdlc control register
DLCBSTAT        equ     $ef     ; bdlc status register
SPI1CR1         equ     $f0     ; spi 1 control1 reg
SPI1CR2         equ     $f1     ; spi 1 control2 reg
SPI1BR          equ     $f2     ; spi 1 baud reg
SPI1SR          equ     $f3     ; spi 1 status reg hi

SP1DR           equ     $f5     ; spi 1 data reg

SPI2CR1         equ     $f8     ; spi 2 control1 reg
SPI2CR2         equ     $f9     ; spi 2 control2 reg
SPI2BR          equ     $fa     ; spi 2 baud reg
SPI2SR          equ     $fb     ; spi 2 status reg hi

SP2DR           equ     $fd     ; spi 2 data reg

FCLKDIV         equ     $100    ; flash clock divider
FSEC            equ     $101    ; flash security register
FTSTMOD         equ     $102
FCNFG           equ     $103    ; flash configuration register
FPROT           equ     $104    ; flash protection register
FSTAT           equ     $105    ; flash status register
FCMD            equ     $106    ; flash command register
FADDR           equ     $108    ; 16-bit address register
FDATA           equ     $10A    ; 16-bit data register

ECLKDIV         equ     $110    ; eeprom clock divider
ECNFG           equ     $113    ; eeprom configuration register
EPROT           equ     $114    ; eeprom protection register
ESTAT           equ     $115    ; eeprom status register
ECMD            equ     $116    ; eeprom command register

ATD1CTL0        equ     $120    ; adc1 control 0 (reserved)
ATD1CTL1        equ     $121    ; adc1 control 1 (reserved)
ATD1CTL2        equ     $122    ; adc1 control 2
ATD1CTL3        equ     $123    ; adc1 control 3
ATD1CTL4        equ     $124    ; adc1 control 4
ATD1CTL5        equ     $125    ; adc1 control 5
ATD1STAT0       equ     $126    ; adc1 status register
ATD1TEST0       equ     $128    ; adc1 test register 0 (reserved)
ATD1TEST1       equ     $129    ; adc1 test register 1 (reserved)
ATD1STAT1       equ     $12B
ATD1DIEN        equ     $12d    ; adc1 input enable register

PORTAD1         equ     $12f    ; port adc1 = input only
PTAD1           equ     $12F
ATD1DR0         equ     $130    ; adc1 result 0 register
ATD1DR1         equ     $132    ; adc1 result 1 register
ATD1DR2         equ     $134    ; adc1 result 2 register
ATD1DR3         equ     $136    ; adc1 result 3 register
ATD1DR4         equ     $138    ; adc1 result 4 register
ATD1DR5         equ     $13a    ; adc1 result 5 register
ATD1DR6         equ     $13c    ; adc1 result 6 register
ATD1DR7         equ     $13e    ; adc1 result 7 register
CAN0CTL0        equ     $140    ; can0 control register 0
CAN0CTL1        equ     $141    ; can0 control register 1
CAN0BTR0        equ     $142    ; can0 bus timing register 0
CAN0BTR1        equ     $143    ; can0 bus timing register 1
CAN0RFLG        equ     $144    ; can0 receiver flags
CAN0RIER        equ     $145    ; can0 receiver interrupt enables
CAN0TFLG        equ     $146    ; can0 transmit flags
CAN0TIER        equ     $147    ; can0 transmit interrupt enables
CAN0TARQ        equ     $148    ; can0 transmit message abort control
CAN0TAAK        equ     $149    ; can0 transmit message abort status
CAN0TBSEL       equ     $14a    ; can0 transmit buffer select
CAN0IDAC        equ     $14b    ; can0 identifier acceptance control

CAN0RXERR       equ     $14e    ; can0 receive error counter
CAN0TXERR       equ     $14f    ; can0 transmit error counter
CAN0IDAR0       equ     $150    ; can0 identifier acceptance register 0
CAN0IDAR1       equ     $151    ; can0 identifier acceptance register 1
CAN0IDAR2       equ     $152    ; can0 identifier acceptance register 2
CAN0IDAR3       equ     $153    ; can0 identifier acceptance register 3
CAN0IDMR0       equ     $154    ; can0 identifier mask register 0
CAN0IDMR1       equ     $155    ; can0 identifier mask register 1
CAN0IDMR2       equ     $156    ; can0 identifier mask register 2
CAN0IDMR3       equ     $157    ; can0 identifier mask register 3
CAN0IDAR4       equ     $158    ; can0 identifier acceptance register 4
CAN0IDAR5       equ     $159    ; can0 identifier acceptance register 5
CAN0IDAR6       equ     $15a    ; can0 identifier acceptance register 6
CAN0IDAR7       equ     $15b    ; can0 identifier acceptance register 7
CAN0IDMR4       equ     $15c    ; can0 identifier mask register 4
CAN0IDMR5       equ     $15d    ; can0 identifier mask register 5
CAN0IDMR6       equ     $15e    ; can0 identifier mask register 6
CAN0IDMR7       equ     $15f    ; can0 identifier mask register 7
CAN0RXFG        equ     $160    ; can0 rx foreground buffer thru +$16f
CAN0RIDR0       equ     $160    ; CAN0 rx foreground buffer identifier register 0
CAN0RIDR1       equ     $161    ; CAN0 rx foreground buffer identifier register 1
CAN0RIDR2       equ     $162    ; CAN0 rx foreground buffer identifier register 2
CAN0RIDR3       equ     $163    ; CAN0 rx foreground buffer identifier register 3
CAN0RDSR0       equ     $164    ; CAN0 rx foreground buffer data segment register 0
CAN0RDSR1       equ     $165    ; CAN0 rx foreground buffer data segment register 1
CAN0RDSR2       equ     $166    ; CAN0 rx foreground buffer data segment register 2
CAN0RDSR3       equ     $167    ; CAN0 rx foreground buffer data segment register 3
CAN0RDSR4       equ     $168    ; CAN0 rx foreground buffer data segment register 4
CAN0RDSR5       equ     $169    ; CAN0 rx foreground buffer data segment register 5
CAN0RDSR6       equ     $16A    ; CAN0 rx foreground buffer data segment register 6
CAN0RDSR7       equ     $16B    ; CAN0 rx foreground buffer data segment register 7
CAN0RDLR        equ     $16C    ; CAN0 rx foreground buffer data length register
CAN0TXFG        equ     $170    ; can0 tx foreground buffer thru +$17f
CAN0TIDR0       equ     $170    ; CAN0 tx foreground buffer identifier register 0
CAN0TIDR1       equ     $171    ; CAN0 tx foreground buffer identifier register 1
CAN0TIDR2       equ     $172    ; CAN0 tx foreground buffer identifier register 2
CAN0TIDR3       equ     $173    ; CAN0 tx foreground buffer identifier register 3
CAN0TDSR0       equ     $174    ; CAN0 tx foreground buffer data segment register 0
CAN0TDSR1       equ     $175    ; CAN0 tx foreground buffer data segment register 1
CAN0TDSR2       equ     $176    ; CAN0 tx foreground buffer data segment register 2
CAN0TDSR3       equ     $177    ; CAN0 tx foreground buffer data segment register 3
CAN0TDSR4       equ     $178    ; CAN0 tx foreground buffer data segment register 4
CAN0TDSR5       equ     $179    ; CAN0 tx foreground buffer data segment register 5
CAN0TDSR6       equ     $17A    ; CAN0 tx foreground buffer data segment register 6
CAN0TDSR7       equ     $17B    ; CAN0 tx foreground buffer data segment register 7
CAN0TDLR        equ     $17C    ; CAN0 tx foreground buffer data length register
CAN0TBPR        equ     $17D    ; CAN0 tx foreground buffer transmit buffer priority register
CAN0TSRH        equ     $17E    ; CAN0 tx foreground buffer transmit time stamp register high
CAN0TSRL        equ     $17F    ; CAN0 tx foreground buffer transmit time stamp register low

CAN1CTL0        equ     $180    ; can1 control register 0
CAN1CTL1        equ     $181    ; can1 control register 1
CAN1BTR0        equ     $182    ; can1 bus timing register 0
CAN1BTR1        equ     $183    ; can1 bus timing register 1
CAN1RFLG        equ     $184    ; can1 receiver flags
CAN1RIER        equ     $185    ; can1 receiver interrupt enables
CAN1TFLG        equ     $186    ; can1 transmit flags
CAN1TIER        equ     $187    ; can1 transmit interrupt enables
CAN1TARQ        equ     $188    ; can1 transmit message abort control
CAN1TAAK        equ     $189    ; can1 transmit message abort status
CAN1TBSEL       equ     $18a    ; can1 transmit buffer select
CAN1IDAC        equ     $18b    ; can1 identifier acceptance control

CAN1RXERR       equ     $18e    ; can1 receive error counter
CAN1TXERR       equ     $18f    ; can1 transmit error counter
CAN1IDAR0       equ     $190    ; can1 identifier acceptance register 0
CAN1IDAR1       equ     $191    ; can1 identifier acceptance register 1
CAN1IDAR2       equ     $192    ; can1 identifier acceptance register 2
CAN1IDAR3       equ     $193    ; can1 identifier acceptance register 3
CAN1IDMR0       equ     $194    ; can1 identifier mask register 0
CAN1IDMR1       equ     $195    ; can1 identifier mask register 1
CAN1IDMR2       equ     $196    ; can1 identifier mask register 2
CAN1IDMR3       equ     $197    ; can1 identifier mask register 3
CAN1IDAR4       equ     $198    ; can1 identifier acceptance register 4
CAN1IDAR5       equ     $199    ; can1 identifier acceptance register 5
CAN1IDAR6       equ     $19a    ; can1 identifier acceptance register 6
CAN1IDAR7       equ     $19b    ; can1 identifier acceptance register 7
CAN1IDMR4       equ     $19c    ; can1 identifier mask register 4
CAN1IDMR5       equ     $19d    ; can1 identifier mask register 5
CAN1IDMR6       equ     $19e    ; can1 identifier mask register 6
CAN1IDMR7       equ     $19f    ; can1 identifier mask register 7
CAN1RXFG        equ     $1a0    ; can1 rx foreground buffer thru +$1af
CAN1RIDR0       equ     $1a0    ; CAN1 rx foreground buffer identifier register 0
CAN1RIDR1       equ     $1a1    ; CAN1 rx foreground buffer identifier register 1
CAN1RIDR2       equ     $1a2    ; CAN1 rx foreground buffer identifier register 2
CAN1RIDR3       equ     $1a3    ; CAN1 rx foreground buffer identifier register 3
CAN1RDSR0       equ     $1a4    ; CAN1 rx foreground buffer data segment register 0
CAN1RDSR1       equ     $1a5    ; CAN1 rx foreground buffer data segment register 1
CAN1RDSR2       equ     $1a6    ; CAN1 rx foreground buffer data segment register 2
CAN1RDSR3       equ     $1a7    ; CAN1 rx foreground buffer data segment register 3
CAN1RDSR4       equ     $1a8    ; CAN1 rx foreground buffer data segment register 4
CAN1RDSR5       equ     $169    ; CAN1 rx foreground buffer data segment register 5
CAN1RDSR6       equ     $1aA    ; CAN1 rx foreground buffer data segment register 6
CAN1RDSR7       equ     $1aB    ; CAN1 rx foreground buffer data segment register 7
CAN1RDLR        equ     $1aC    ; CAN1 rx foreground buffer data length register
CAN1TXFG        equ     $1b0    ; can1 tx foreground buffer thru +$1bf
CAN1TIDR0       equ     $1b0    ; CAN1 tx foreground buffer identifier register 0
CAN1TIDR1       equ     $1b1    ; CAN1 tx foreground buffer identifier register 1
CAN1TIDR2       equ     $1b2    ; CAN1 tx foreground buffer identifier register 2
CAN1TIDR3       equ     $1b3    ; CAN1 tx foreground buffer identifier register 3
CAN1TDSR0       equ     $1b4    ; CAN1 tx foreground buffer data segment register 0
CAN1TDSR1       equ     $1b5    ; CAN1 tx foreground buffer data segment register 1
CAN1TDSR2       equ     $1b6    ; CAN1 tx foreground buffer data segment register 2
CAN1TDSR3       equ     $1b7    ; CAN1 tx foreground buffer data segment register 3
CAN1TDSR4       equ     $1b8    ; CAN1 tx foreground buffer data segment register 4
CAN1TDSR5       equ     $1b9    ; CAN1 tx foreground buffer data segment register 5
CAN1TDSR6       equ     $1bA    ; CAN1 tx foreground buffer data segment register 6
CAN1TDSR7       equ     $1bB    ; CAN1 tx foreground buffer data segment register 7
CAN1TDLR        equ     $1bC    ; CAN1 tx foreground buffer data length register
CAN1TBPR        equ     $1bD    ; CAN1 tx foreground buffer transmit buffer priority register
CAN1TSRH        equ     $1bE    ; CAN1 tx foreground buffer transmit time stamp register high
CAN1TSRL        equ     $1bF    ; CAN1 tx foreground buffer transmit time stamp register low

CAN2CTL0        equ     $1c0    ; can2 control register 0
CAN2CTL1        equ     $1c1    ; can2 control register 1
CAN2BTR0        equ     $1c2    ; can2 bus timing register 0
CAN2BTR1        equ     $1c3    ; can2 bus timing register 1
CAN2RFLG        equ     $1c4    ; can2 receiver flags
CAN2RIER        equ     $1c5    ; can2 receiver interrupt enables
CAN2TFLG        equ     $1c6    ; can2 transmit flags
CAN2TIER        equ     $1c7    ; can2 transmit interrupt enables
CAN2TARQ        equ     $1c8    ; can2 transmit message abort control
CAN2TAAK        equ     $1c9    ; can2 transmit message abort status
CAN2TBSEL       equ     $1ca    ; can2 transmit buffer select
CAN2IDAC        equ     $1cb    ; can2 identifier acceptance control

CAN2RXERR       equ     $1ce    ; can2 receive error counter
CAN2TXERR       equ     $1cf    ; can2 transmit error counter
CAN2IDAR0       equ     $1d0    ; can2 identifier acceptance register 0
CAN2IDAR1       equ     $1d1    ; can2 identifier acceptance register 1
CAN2IDAR2       equ     $1d2    ; can2 identifier acceptance register 2
CAN2IDAR3       equ     $1d3    ; can2 identifier acceptance register 3
CAN2IDMR0       equ     $1d4    ; can2 identifier mask register 0
CAN2IDMR1       equ     $1d5    ; can2 identifier mask register 1
CAN2IDMR2       equ     $1d6    ; can2 identifier mask register 2
CAN2IDMR3       equ     $1d7    ; can2 identifier mask register 3
CAN2IDAR4       equ     $1d8    ; can2 identifier acceptance register 4
CAN2IDAR5       equ     $1d9    ; can2 identifier acceptance register 5
CAN2IDAR6       equ     $1da    ; can2 identifier acceptance register 6
CAN2IDAR7       equ     $1db    ; can2 identifier acceptance register 7
CAN2IDMR4       equ     $1dc    ; can2 identifier mask register 4
CAN2IDMR5       equ     $1dd    ; can2 identifier mask register 5
CAN2IDMR6       equ     $1de    ; can2 identifier mask register 6
CAN2IDMR7       equ     $1df    ; can2 identifier mask register 7
CAN2RXFG        equ     $1e0    ; can2 rx foreground buffer thru +$1ef
CAN2RIDR0       equ     $1e0    ; CAN2 rx foreground buffer identifier register 0
CAN2RIDR1       equ     $1e1    ; CAN2 rx foreground buffer identifier register 1
CAN2RIDR2       equ     $1e2    ; CAN2 rx foreground buffer identifier register 2
CAN2RIDR3       equ     $1e3    ; CAN2 rx foreground buffer identifier register 3
CAN2RDSR0       equ     $1e4    ; CAN2 rx foreground buffer data segment register 0
CAN2RDSR1       equ     $1e5    ; CAN2 rx foreground buffer data segment register 1
CAN2RDSR2       equ     $1e6    ; CAN2 rx foreground buffer data segment register 2
CAN2RDSR3       equ     $1e7    ; CAN2 rx foreground buffer data segment register 3
CAN2RDSR4       equ     $1e8    ; CAN2 rx foreground buffer data segment register 4
CAN2RDSR5       equ     $1e9    ; CAN2 rx foreground buffer data segment register 5
CAN2RDSR6       equ     $1eA    ; CAN2 rx foreground buffer data segment register 6
CAN2RDSR7       equ     $1eB    ; CAN2 rx foreground buffer data segment register 7
CAN2RDLR        equ     $1eC    ; CAN2 rx foreground buffer data length register
CAN2TXFG        equ     $1f0    ; can2 tx foreground buffer thru +$1ff
CAN2TIDR0       equ     $1f0    ; CAN2 tx foreground buffer identifier register 0
CAN2TIDR1       equ     $1f1    ; CAN2 tx foreground buffer identifier register 1
CAN2TIDR2       equ     $1f2    ; CAN2 tx foreground buffer identifier register 2
CAN2TIDR3       equ     $1f3    ; CAN2 tx foreground buffer identifier register 3
CAN2TDSR0       equ     $1f4    ; CAN2 tx foreground buffer data segment register 0
CAN2TDSR1       equ     $1f5    ; CAN2 tx foreground buffer data segment register 1
CAN2TDSR2       equ     $1f6    ; CAN2 tx foreground buffer data segment register 2
CAN2TDSR3       equ     $1f7    ; CAN2 tx foreground buffer data segment register 3
CAN2TDSR4       equ     $1f8    ; CAN2 tx foreground buffer data segment register 4
CAN2TDSR5       equ     $1f9    ; CAN2 tx foreground buffer data segment register 5
CAN2TDSR6       equ     $1fA    ; CAN2 tx foreground buffer data segment register 6
CAN2TDSR7       equ     $1fB    ; CAN2 tx foreground buffer data segment register 7
CAN2TDLR        equ     $1fC    ; CAN2 tx foreground buffer data length register
CAN2TBPR        equ     $1fD    ; CAN2 tx foreground buffer transmit buffer priority register
CAN2TSRH        equ     $1fE    ; CAN2 tx foreground buffer transmit time stamp register high
CAN2TSRL        equ     $1fF    ; CAN2 tx foreground buffer transmit time stamp register low

CAN3CTL0        equ     $200    ; can3 control register 0
CAN3CTL1        equ     $201    ; can3 control register 1
CAN3BTR0        equ     $202    ; can3 bus timing register 0
CAN3BTR1        equ     $203    ; can3 bus timing register 1
CAN3RFLG        equ     $204    ; can3 receiver flags
CAN3RIER        equ     $205    ; can3 receiver interrupt enables
CAN3TFLG        equ     $206    ; can3 transmit flags
CAN3TIER        equ     $207    ; can3 transmit interrupt enables
CAN3TARQ        equ     $208    ; can3 transmit message abort control
CAN3TAAK        equ     $209    ; can3 transmit message abort status
CAN3TBSEL       equ     $20a    ; can3 transmit buffer select
CAN3IDAC        equ     $20b    ; can3 identifier acceptance control

CAN3RXERR       equ     $20e    ; can3 receive error counter
CAN3TXERR       equ     $20f    ; can3 transmit error counter
CAN3IDAR0       equ     $210    ; can3 identifier acceptance register 0
CAN3IDAR1       equ     $211    ; can3 identifier acceptance register 1
CAN3IDAR2       equ     $212    ; can3 identifier acceptance register 2
CAN3IDAR3       equ     $213    ; can3 identifier acceptance register 3
CAN3IDMR0       equ     $214    ; can3 identifier mask register 0
CAN3IDMR1       equ     $215    ; can3 identifier mask register 1
CAN3IDMR2       equ     $216    ; can3 identifier mask register 2
CAN3IDMR3       equ     $217    ; can3 identifier mask register 3
CAN3IDAR4       equ     $218    ; can3 identifier acceptance register 4
CAN3IDAR5       equ     $219    ; can3 identifier acceptance register 5
CAN3IDAR6       equ     $21a    ; can3 identifier acceptance register 6
CAN3IDAR7       equ     $21b    ; can3 identifier acceptance register 7
CAN3IDMR4       equ     $21c    ; can3 identifier mask register 4
CAN3IDMR5       equ     $21d    ; can3 identifier mask register 5
CAN3IDMR6       equ     $21e    ; can3 identifier mask register 6
CAN3IDMR7       equ     $21f    ; can3 identifier mask register 7
CAN3RXFG        equ     $220    ; can3 rx foreground buffer thru +$22f
CAN3RIDR0       equ     $220    ; CAN3 rx foreground buffer identifier register 0
CAN3RIDR1       equ     $221    ; CAN3 rx foreground buffer identifier register 1
CAN3RIDR2       equ     $222    ; CAN3 rx foreground buffer identifier register 2
CAN3RIDR3       equ     $223    ; CAN3 rx foreground buffer identifier register 3
CAN3RDSR0       equ     $224    ; CAN3 rx foreground buffer data segment register 0
CAN3RDSR1       equ     $225    ; CAN3 rx foreground buffer data segment register 1
CAN3RDSR2       equ     $226    ; CAN3 rx foreground buffer data segment register 2
CAN3RDSR3       equ     $227    ; CAN3 rx foreground buffer data segment register 3
CAN3RDSR4       equ     $228    ; CAN3 rx foreground buffer data segment register 4
CAN3RDSR5       equ     $229    ; CAN3 rx foreground buffer data segment register 5
CAN3RDSR6       equ     $22A    ; CAN3 rx foreground buffer data segment register 6
CAN3RDSR7       equ     $22B    ; CAN3 rx foreground buffer data segment register 7
CAN3RDLR        equ     $22C    ; CAN3 rx foreground buffer data length register
CAN3TXFG        equ     $230    ; can3 tx foreground buffer thru +$23f
CAN3TIDR0       equ     $230    ; CAN3 tx foreground buffer identifier register 0
CAN3TIDR1       equ     $231    ; CAN3 tx foreground buffer identifier register 1
CAN3TIDR2       equ     $232    ; CAN3 tx foreground buffer identifier register 2
CAN3TIDR3       equ     $233    ; CAN3 tx foreground buffer identifier register 3
CAN3TDSR0       equ     $234    ; CAN3 tx foreground buffer data segment register 0
CAN3TDSR1       equ     $235    ; CAN3 tx foreground buffer data segment register 1
CAN3TDSR2       equ     $236    ; CAN3 tx foreground buffer data segment register 2
CAN3TDSR3       equ     $237    ; CAN3 tx foreground buffer data segment register 3
CAN3TDSR4       equ     $238    ; CAN3 tx foreground buffer data segment register 4
CAN3TDSR5       equ     $239    ; CAN3 tx foreground buffer data segment register 5
CAN3TDSR6       equ     $23A    ; CAN3 tx foreground buffer data segment register 6
CAN3TDSR7       equ     $23B    ; CAN3 tx foreground buffer data segment register 7
CAN3TDLR        equ     $23C    ; CAN3 tx foreground buffer data length register
CAN3TBPR        equ     $23D    ; CAN3 tx foreground buffer transmit buffer priority register
CAN3TSRH        equ     $23E    ; CAN3 tx foreground buffer transmit time stamp register high
CAN3TSRL        equ     $23F    ; CAN3 tx foreground buffer transmit time stamp register low

PTT             equ     $240    ; portt data register
PTIT            equ     $241    ; portt input register
DDRT            equ     $242    ; portt direction register
RDRT            equ     $243    ; portt reduced drive register
PERT            equ     $244    ; portt pull device enable
PPST            equ     $245    ; portt pull polarity select

PTS             equ     $248    ; ports data register
PTIS            equ     $249    ; ports input register
DDRS            equ     $24a    ; ports direction register
RDRS            equ     $24b    ; ports reduced drive register
PERS            equ     $24c    ; ports pull device enable
PPSS            equ     $24d    ; ports pull polarity select
WOMS            equ     $24e    ; ports wired or mode register

PTM             equ     $250    ; portm data register
PTIM            equ     $251    ; portm input register
DDRM            equ     $252    ; portm direction register
RDRM            equ     $253    ; portm reduced drive register
PERM            equ     $254    ; portm pull device enable
PPSM            equ     $255    ; portm pull polarity select
WOMM            equ     $256    ; portm wired or mode register
MODRR           equ     $257    ; portm module routing register
PTP             equ     $258    ; portp data register
PTIP            equ     $259    ; portp input register
DDRP            equ     $25a    ; portp direction register
RDRP            equ     $25b    ; portp reduced drive register
PERP            equ     $25c    ; portp pull device enable
PPSP            equ     $25d    ; portp pull polarity select
PIEP            equ     $25e    ; portp interrupt enable register
PIFP            equ     $25f    ; portp interrupt flag register
PTH             equ     $260    ; porth data register
PTIH            equ     $261    ; porth input register
DDRH            equ     $262    ; porth direction register
RDRH            equ     $263    ; porth reduced drive register
PERH            equ     $264    ; porth pull device enable
PPSH            equ     $265    ; porth pull polarity select
PIEH            equ     $266    ; porth interrupt enable register
PIFH            equ     $267    ; porth interrupt flag register
PTJ             equ     $268    ; portp data register
PTIJ            equ     $269    ; portp input register
DDRJ            equ     $26a    ; portp direction register
RDRJ            equ     $26b    ; portp reduced drive register
PERJ            equ     $26c    ; portp pull device enable
PPSJ            equ     $26d    ; portp pull polarity select
PIEJ            equ     $26e    ; portp interrupt enable register
PIFJ            equ     $26f    ; portp interrupt flag register

CAN4CTL0        equ     $280    ; can4 control register 0
CAN4CTL1        equ     $281    ; can4 control register 1
CAN4BTR0        equ     $282    ; can4 bus timing register 0
CAN4BTR1        equ     $283    ; can4 bus timing register 1
CAN4RFLG        equ     $284    ; can4 receiver flags
CAN4RIER        equ     $285    ; can4 receiver interrupt enables
CAN4TFLG        equ     $286    ; can4 transmit flags
CAN4TIER        equ     $287    ; can4 transmit interrupt enables
CAN4TARQ        equ     $288    ; can4 transmit message abort control
CAN4TAAK        equ     $289    ; can4 transmit message abort status
CAN4TBSEL       equ     $28a    ; can4 transmit buffer select
CAN4IDAC        equ     $28b    ; can4 identifier acceptance control

CAN4RXERR       equ     $28e    ; can4 receive error counter
CAN4TXERR       equ     $28f    ; can4 transmit error counter
CAN4IDAR0       equ     $290    ; can4 identifier acceptance register 0
CAN4IDAR1       equ     $291    ; can4 identifier acceptance register 1
CAN4IDAR2       equ     $292    ; can4 identifier acceptance register 2
CAN4IDAR3       equ     $293    ; can4 identifier acceptance register 3
CAN4IDMR0       equ     $294    ; can4 identifier mask register 0
CAN4IDMR1       equ     $295    ; can4 identifier mask register 1
CAN4IDMR2       equ     $296    ; can4 identifier mask register 2
CAN4IDMR3       equ     $297    ; can4 identifier mask register 3
CAN4IDAR4       equ     $298    ; can4 identifier acceptance register 4
CAN4IDAR5       equ     $299    ; can4 identifier acceptance register 5
CAN4IDAR6       equ     $29a    ; can4 identifier acceptance register 6
CAN4IDAR7       equ     $29b    ; can4 identifier acceptance register 7
CAN4IDMR4       equ     $29c    ; can4 identifier mask register 4
CAN4IDMR5       equ     $29d    ; can4 identifier mask register 5
CAN4IDMR6       equ     $29e    ; can4 identifier mask register 6
CAN4IDMR7       equ     $29f    ; can4 identifier mask register 7
CAN4RXFG        equ     $2a0    ; can4 rx foreground buffer thru +$2af
CAN4RIDR0       equ     $2a0    ; CAN4 rx foreground buffer identifier register 0
CAN4RIDR1       equ     $2a1    ; CAN4 rx foreground buffer identifier register 1
CAN4RIDR2       equ     $2a2    ; CAN4 rx foreground buffer identifier register 2
CAN4RIDR3       equ     $2a3    ; CAN4 rx foreground buffer identifier register 3
CAN4RDSR0       equ     $2a4    ; CAN4 rx foreground buffer data segment register 0
CAN4RDSR1       equ     $2a5    ; CAN4 rx foreground buffer data segment register 1
CAN4RDSR2       equ     $2a6    ; CAN4 rx foreground buffer data segment register 2
CAN4RDSR3       equ     $2a7    ; CAN4 rx foreground buffer data segment register 3
CAN4RDSR4       equ     $2a8    ; CAN4 rx foreground buffer data segment register 4
CAN4RDSR5       equ     $2a9    ; CAN4 rx foreground buffer data segment register 5
CAN4RDSR6       equ     $2aA    ; CAN4 rx foreground buffer data segment register 6
CAN4RDSR7       equ     $2aB    ; CAN4 rx foreground buffer data segment register 7
CAN4RDLR        equ     $2aC    ; CAN4 rx foreground buffer data length register
CAN4TXFG        equ     $2b0    ; can4 tx foreground buffer thru +$2bf
CAN4TIDR0       equ     $2b0    ; CAN4 tx foreground buffer identifier register 0
CAN4TIDR1       equ     $2b1    ; CAN4 tx foreground buffer identifier register 1
CAN4TIDR2       equ     $2b2    ; CAN4 tx foreground buffer identifier register 2
CAN4TIDR3       equ     $2b3    ; CAN4 tx foreground buffer identifier register 3
CAN4TDSR0       equ     $2b4    ; CAN4 tx foreground buffer data segment register 0
CAN4TDSR1       equ     $2b5    ; CAN4 tx foreground buffer data segment register 1
CAN4TDSR2       equ     $2b6    ; CAN4 tx foreground buffer data segment register 2
CAN4TDSR3       equ     $2b7    ; CAN4 tx foreground buffer data segment register 3
CAN4TDSR4       equ     $2b8    ; CAN4 tx foreground buffer data segment register 4
CAN4TDSR5       equ     $2b9    ; CAN4 tx foreground buffer data segment register 5
CAN4TDSR6       equ     $2bA    ; CAN4 tx foreground buffer data segment register 6
CAN4TDSR7       equ     $2bB    ; CAN4 tx foreground buffer data segment register 7
CAN4TDLR        equ     $2bC    ; CAN4 tx foreground buffer data length register
CAN4TBPR        equ     $2bD    ; CAN4 tx foreground buffer transmit buffer priority register
CAN4TSRH        equ     $2bE    ; CAN4 tx foreground buffer transmit time stamp register high
CAN4TSRL        equ     $2bF    ; CAN4 tx foreground buffer transmit time stamp register low
* end of register definitions
; *************************************************************************************
; flash and EEPROM memory command
; *************************************************************************************
Program         equ     $20             ; program a flash or EEPROM word
EraseVerify     equ     $05             ; Erase and verify flash and EEPROM, BLANK bit will be set
SectorErase     equ     $40             ; Erase a sector of flash or EEPROM
BulkErase       equ     $41             ; Bulk erase the flash or EEPROM
SectorModify    equ     $60             ; Erase a sector (4 bytes), program a word (2 bytes)
; *************************************************************************************
; definitions of bits
; *************************************************************************************
BIT7            equ     $80
BIT6            equ     $40
BIT5            equ     $20
BIT4            equ     $10
BIT3            equ     $08
BIT2            equ     $04
BIT1            equ     $02
BIT0            equ     $01
NOACCE          equ     $80
PIPOE           equ     $20
NECLK           equ     $10
LSTRE           equ     $08
RDWE            equ     $04
MODC            equ     $80
MODB            equ     $40
MODA            equ     $20
IVIS            equ     $08
EMK             equ     $02
EME             equ     $01
PUPKE           equ     $80
PUPEE           equ     $10
PUPBE           equ     $02
PUPAE           equ     $01
RDPK            equ     $80
RDPE            equ     $10
RDPB            equ     $02
RDPA            equ     $01
ESTR            equ     $01
EXSTR1          equ     $08
EXSTR0          equ     $04
ROMHM           equ     $02
ROMON           equ     $01
WRINT           equ     $10
INTE            equ     $80
INTC            equ     $40
INTA            equ     $20
INT8            equ     $10
INT6            equ     $08
INT4            equ     $04
INT2            equ     $02
INT0            equ     $01
IRQE            equ     $80
IRQEN           equ     $40
BKEN            equ     $80
BKFULL          equ     $40
BKBDM           equ     $20
BKTAG           equ     $10
BK0RWE          equ     $08
BK0RW           equ     $04
BK1RWE          equ     $02
BK1RW           equ     $01
RTIF            equ     $80
PROF            equ     $40
LOCKIF          equ     $10
LOCK            equ     $08
TRACK           equ     $04
SCMIF           equ     $02
SCM             equ     $01
RTIE            equ     $80
LOCKIE          equ     $10
SCMIE           equ     $02
PLLSEL          equ     $80
PSTP            equ     $40
SYSWAI          equ     $20
ROAWAI          equ     $10
PLLWAI          equ     $08
CWAI            equ     $04
RTIWAI          equ     $02
COPWAI          equ     $01
CME             equ     $80
PLLON           equ     $40
AUTO            equ     $20
ACQ             equ     $10
PRE             equ     $04
PCE             equ     $02
SCME            equ     $01
WCOP            equ     $80
RSBCK           equ     $40
RTIBYP          equ     $80
COPBYP          equ     $40
PLLBYP          equ     $10
FCM             equ     $02
TEN             equ     $80
TSWAI           equ     $40
TSFRZ           equ     $20
TFFCA           equ     $10
C7I             equ     $80
C6I             equ     $40
C5I             equ     $20
C4I             equ     $10
C3I             equ     $08
C2I             equ     $04
C1I             equ     $02
C0I             equ     $01
C7F             equ     $80
C6F             equ     $40
C5F             equ     $20
C4F             equ     $10
C3F             equ     $08
C2F             equ     $04
C1F             equ     $02
C0F             equ     $01
OC7             equ     $80
OC6             equ     $40
OC5             equ     $20
OC4             equ     $10
OC3             equ     $08
OC2             equ     $04
OC1             equ     $02
OC0             equ     $01
IC7             equ     $80
IC6             equ     $40
IC5             equ     $20
IC4             equ     $10
IC3             equ     $08
IC2             equ     $04
IC1             equ     $02
IC0             equ     $01
IOS7            equ     $80
IOS6            equ     $40
IOS5            equ     $20
IOS4            equ     $10
IOS3            equ     $08
IOS2            equ     $04
IOS1            equ     $02
IOS0            equ     $01
NOVW7           equ     $80
NOVW6           equ     $40
NOVW5           equ     $20
NOVW4           equ     $10
NOVW3           equ     $08
NOVW2           equ     $04
NOVW1           equ     $02
NOVW0           equ     $01
TCRE            equ     $08
PAEN            equ     $40
PAMOD           equ     $20
PEDGE           equ     $10
CLK1            equ     $08
CLK0            equ     $04
PAOVI           equ     $02
PAI             equ     $01
PAOVF           equ     $02
PAIF            equ     $01
MCZI            equ     $80
MODMC           equ     $40
RDMCL           equ     $20
ICLAT           equ     $10
FLMC            equ     $08
MCEN            equ     $04
MCPR1           equ     $02
MCPR0           equ     $01
MCZF            equ     $80
POLF3           equ     $08
POLF2           equ     $04
POLF1           equ     $02
POLF0           equ     $01
PAEN3           equ     $08
PAEN2           equ     $04
PAEN1           equ     $02
PAEN0           equ     $01
TFMOD           equ     $08
PACMX           equ     $04
BUFEN           equ     $02
LATQ            equ     $01
TCBYP           equ     $02
PBEN            equ     $40
PBOVI           equ     $02
PBOVF           equ     $02
ADPU            equ     $80
AFFC            equ     $40
AWAI            equ     $20
ETRIGLE         equ     $10
ETRIGP          equ     $08
ETRIG           equ     $04
ASCIE           equ     $02
ASCIF           equ     $01
SCF             equ     $80
ETORF           equ     $20
FIFOR           equ     $10
CON67           equ     $80
CON45           equ     $40
CON23           equ     $20
CON01           equ     $10
PSWAI           equ     $08
PFRZ            equ     $04
PWMIF           equ     $80
PWMIE           equ     $40
PWMRSTRT        equ     $20
PWMLVL          equ     $10
PWM7IN          equ     $04
PWM7INL         equ     $02
PWM7ENA         equ     $01
PWME7           equ     $80
PWME6           equ     $40
PWME5           equ     $20
PWME4           equ     $10
PWME3           equ     $08
PWME2           equ     $04
PWME1           equ     $02
PWME0           equ     $01
PCLK7           equ     $80
PCLK6           equ     $40
PCLK5           equ     $20
PCLK4           equ     $10
PCLK3           equ     $08
PCLK2           equ     $04
PCLK1           equ     $02
PCLK0           equ     $01
PPOL7           equ     $80
PPOL6           equ     $40
PPOL5           equ     $20
PPOL4           equ     $10
PPOL3           equ     $08
PPOL2           equ     $04
PPOL1           equ     $02
PPOL0           equ     $01
CAE7            equ     $80
CAE6            equ     $40
CAE5            equ     $20
CAE4            equ     $10
CAE3            equ     $08
CAE2            equ     $04
CAE1            equ     $02
CAE0            equ     $01
TIEN            equ     $80
TCIE            equ     $40
RIE             equ     $20
ILIE            equ     $10
TE              equ     $08
RE              equ     $04
RWU             equ     $02
SBK             equ     $01
TDRE            equ     $80
TC              equ     $40
RDRF            equ     $20
IDLE            equ     $10
OR              equ     $08
NF              equ     $04
FE              equ     $02
PF              equ     $01
BRK13           equ     $04
TXDIR           equ     $02
RAF             equ     $01
R8              equ     $80
T8              equ     $40
SPIF            equ     $80
SPTEF           equ     $20
MODF            equ     $10
IBEN            equ     $80
IBIE            equ     $40
MSSL            equ     $20
TXRX            equ     $10
TXAK            equ     $08
RSTA            equ     $04
IBSWAI          equ     $01
TCF             equ     $80
IAAS            equ     $40
IBB             equ     $20
IBAL            equ     $10
SRW             equ     $04
IBIF            equ     $02
RXAK            equ     $01
IMSG            equ     $80
CLKS            equ     $40
IE              equ     $02
WCM             equ     $01
BDLCE           equ     $10
BIDLE           equ     $01           ;idle bit of BDLC
WUPIF           equ     $80
CSCIF           equ     $40
RSTAT1          equ     $20
RSTAT0          equ     $10
TSTAT1          equ     $08
TSTAT0          equ     $04
CANE            equ     $80
OVRIF           equ     $02
RXF             equ     $01
INITRQ          equ     $01
INITAK          equ     $01
SLPRQ           equ     $02
SLPAK           equ     $02
WUPIE           equ     $80
CSCIE           equ     $40
RSTATE1         equ     $20
RSTATE0         equ     $10
TSTATE1         equ     $08
TSTATE0         equ     $04
OVRIE           equ     $02
RXFIE           equ     $01
TXE2            equ     $04
TXE1            equ     $02
TXE0            equ     $01
TXEIE2          equ     $04
TXEIE1          equ     $02
TXEIE0          equ     $01
TX2             equ     $04
TX1             equ     $02
TX0             equ     $01
FDIVLD          equ     $80     ; clock divider loaded
EDIVLD          equ     $80     ;       "
PRDIV8          equ     $40     ; enable divider by 8 bit
KEYEN           equ     $80     ; enable backdoor key to security
WRALL           equ     $10     ; write to all register banks
CBEIE           equ     $80     ; command buffer empty interrupt enable
CCIE            equ     $40     ; command completion interrupt enable
KEYACC          equ     $20     ; enable security key writing
FPOPEN          equ     $80     ; Opens the flash for program or erase
EPOPEN          equ     $80     ; Opens the EEPROM for program or erase
EPDIS           equ     $08     ; EEPROM protection address range disable
FPHDIS          equ     $20     ; flash protection higher address range disable
FPLDIS          equ     $04     ; flash protection lower address range disable
CBEIF           equ     $80     ; command buffer empty interrupt flag
CCIF            equ     $40     ; command complete interrupt flag
PVIOL           equ     $20     ; protection violation interrupt flag
ACCERR          equ     $10     ; access error flag
BLANK           equ     $04     ; array has been verified as erased

; D-Bug12 functions addresses
main            equ     $EE80
getchar         equ     $EE84
putchar         equ     $EE86
printf          equ     $EE88
getcmdline      equ     $EE8A
sscanhex        equ     $EE8E
isxdigit        equ     $EE92
toupper         equ     $EE94
isalpha         equ     $EE96
strlen          equ     $EE98
strcpy          equ     $EE9A
out2hex         equ     $EE9C
out4hex         equ     $EEA0
setuservector   equ     $EEA4
writeeebyte     equ     $EEA6
eraseee         equ     $EEAA
readmem         equ     $EEAE
writemem        equ     $EEB2
; D-Bug12 interrut vector numbers
;userpwmshdn    equ     6
;userportp      equ     7
;usermscan4tx   equ     8
;usermscan4rx   equ     9
;usermscan4errs equ     10
;usermscan4wake equ     11
;usermscan3tx   equ     12
;usermscan3rx   equ     13
;usermscan3errs equ     14
;usermscan3wake equ     15
;usermscan2tx   equ     16
;usermscan2rx   equ     17
;usermscan2errs equ     18
;usermscan2wake equ     19
;usermscan1tx   equ     20
;usermscan1rx   equ     21
;usermscan1errs equ     22
;usermscan1wake equ     23
;usermscan0tx   equ     24
;usermscan0rx   equ     25
;usermscan0errs equ     26
;usermscan0wake equ     27
;userflash      equ     28
;usereeprom     equ     29
;userspi2       equ     30
;userspi1       equ     31
;useriic        equ     32
;userdlc        equ     33
;userscme       equ     34
;usercrg        equ     35
;userpaccbov    equ     36
;usermoddwnctr  equ     37
;userporth      equ     38
;userportj      equ     39
;useratod1      equ     40
;useratod0      equ     41
;usersci1       equ     42
;usersci0       equ     43
;userspi0       equ     44
;userpaccedge   equ     45
;userpaccovf    equ     46
;usertimerovf   equ     47
;usertimerch7   equ     48
;usertimerch6   equ     49
;usertimerch5   equ     50
;usertimerch4   equ     51
;usertimerch3   equ     52
;usertimerch2   equ     53
;usertimerch1   equ     54
;usertimerch0   equ     55
;userrti        equ     56
;userirq        equ     57
;userxirq       equ     58
;userswi        equ     59

;usertrap       equ     60
; end of D-Bug12 interrupt vector number table
; D-Bug12 SRAM vector table
UserRsrv$80     equ     $3E00
UserRsrv$82     equ     $3E02
UserRsrv$84     equ     $3E04
UserRsrv$86     equ     $3E06
UserRsrv$88     equ     $3E08
UserRsrv$8a     equ     $3E0A
UserPWMShDn     equ     $3E0C
UserPortP       equ     $3E0E
UserMSCAN4Tx    equ     $3E10
UserMSCAN4Rx    equ     $3E12
UserMSCAN4Errs  equ     $3E14
UserMSCAN4Wake  equ     $3E16
UserMSCAN3Tx    equ     $3E18
UserMSCAN3Rx    equ     $3E1A
UserMSCAN3Errs  equ     $3E1C
UserMSCAN3Wake  equ     $3E1E
UserMSCAN2Tx    equ     $3E20
UserMSCAN2Rx    equ     $3E22
UserMSCAN2Errs  equ     $3E24
UserMSCAN2Wake  equ     $3E26
UserMSCAN1Tx    equ     $3E28
UserMSCAN1Rx    equ     $3E2A
UserMSCAN1Errs  equ     $3E2C
UserMSCAN1Wake  equ     $3E2E
UserMSCAN0Tx    equ     $3E30
UserMSCAN0Rx    equ     $3E32
UserMSCAN0Errs  equ     $3E34
UserMSCAN0Wake  equ     $3E36
UserFlash       equ     $3E38
UserEEPROM      equ     $3E3A
UserSPI2        equ     $3E3C
UserSPI1        equ     $3E3E
UserIIC         equ     $3E40
UserDLC         equ     $3E42
UserSCME        equ     $3E44
UserCRG         equ     $3E46
UserPAccBOv     equ     $3E48
UserModDwnCtr   equ     $3E4A
UserPortH       equ     $3E4C
UserPortJ       equ     $3E4E
UserAtoD1       equ     $3E50
UserAtoD0       equ     $3E52
UserSCI1        equ     $3E54
UserSCI0        equ     $3E56
UserSPI0        equ     $3E58
UserPAccEdge    equ     $3E5A
UserPAccOvf     equ     $3E5C
UserTimerOvf    equ     $3E5E
UserTimerCh7    equ     $3E60
UserTimerCh6    equ     $3E62
UserTimerCh5    equ     $3E64
UserTimerCh4    equ     $3E66
UserTimerCh3    equ     $3E68
UserTimerCh2    equ     $3E6A
UserTimerCh1    equ     $3E6C
UserTimerCh0    equ     $3E6E
UserRTI         equ     $3E70
UserIRQ         equ     $3E72
UserXIRQ        equ     $3E74
UserSWI         equ     $3E76
UserTrap        equ     $3E78
; end of user SRAM interrupt vector table
; Axiom CML12SDP256 demo board utils
rprint          equ     $FF16   ; display user registers
outa            equ     $FF4F   ; output ascii character in A
out1byt         equ     $FF52   ; display the hex value pointed to by X
out1bsp         equ     $FF55   ; out1byt followed by a space
out2bsp         equ     $FF58   ; display 2 hex bytes pointed to by X
outcrlf         equ     $FF5B   ; output a carriage return and a line feed to terminal
outstrg         equ     $FF5E   ; display the string pointed to by X (terminated by $04) preceded by CR/LF
outstrg0        equ     $FF5E   ; display the string pointed to by X (terminated by $04)without initial CR/LF
inchar          equ     $FF64   ; wait for and input a char from terminal