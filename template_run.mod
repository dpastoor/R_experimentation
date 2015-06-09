$PROB emax model changing EC50
$INPUT C ID PERIOD ARM DV MDV IPLC IEMAX IEC50 IBASE ICL IVV IVKA  AMT PLC_P EM_P c50_p BP_P CL_P V_P KA_P BSVPLC BSVEM BSVC50 BSVCL BSVV BSVKA BOVCL BOVPLC
$DATA olmdata.csv
IGNORE= C

$PRED
;------------BOVCL-------------------
OCCL1=0
IF(PERIOD.EQ.1.AND.ARM.EQ.2) OCCL1=1
OCCL2=0
IF(PERIOD.EQ.2.AND.ARM.EQ.2) OCCL2=1
;-----------BOVPL
OCPL1=0
IF(PERIOD.EQ.1.AND.ARM.EQ.1) OCPL1=1
OCPL2=0
IF(PERIOD.EQ.2.AND.ARM.EQ.1) OCPL2=1

;------------BOVCL--------------------
OCCCL=OCCL1*ETA(8)+OCCL2*ETA(9)
;-------------------------------------
;------------BOVPL--------------------
OCCPL=OCPL1*ETA(10)+OCPL2*ETA(11)
;-------------------------------------

TVPLC=THETA(1)
TVEMAX=THETA(2)
TVEC50=THETA(3)
TVBASE=THETA(4)
TVCL=THETA(5)
TVV=THETA(6)
TVKA=THETA(7)

PLC=TVPLC*EXP(ETA(1)+OCCPL)
EMAX=TVEMAX+ETA(2)
EC50=TVEC50*EXP(ETA(3))
BASE=TVBASE+ETA(4)
CL=TVCL*EXP(ETA(5)+OCCCL)
V=TVV*EXP(ETA(6))
KA=TVKA*EXP(ETA(7))

;----------------PK model-----------------------------------------------------
DOSE=AMT               ; AMT=40mg
AUC=DOSE/CL+ERR(1)     ;
;-----------------------------------------------------------------------------
;-----------------PD model ---------------------------------------------------
IF(PERIOD.EQ.1) THEN
Y=-PLC*((BASE/TVBASE)**3.19)+ERR(2)
ELSE
Y=-PLC*((BASE/TVBASE)**3.19)-((EMAX*AUC/(EC50+AUC))*(BASE/TVBASE)**2.46)+ERR(2)
ENDIF
;-----------------------------------------------------------------------------

;------------------------truncate simulated ----------------------------------
IF(ICALL.EQ.4) THEN
DO WHILE (ABS(ETA(5)).GT.10)
CALL SIMETA(ETA)
END DO
DO WHILE(ABS(EPS(1)).GT.8)
CALL SIMEPS(EPS)
END DO
ENDIF

;----------------------Code for output of simulated parameter values------------
IF(ICALL.EQ.4) THEN
DV=Y
IEMAX=EMAX
IEC50=EC50
IBASE=BASE
ICL=CL
IVV=V
IKA=KA
;-Flagging the used PK PD scenario in the output-----------------------------
PLC_P = <PLC_P>
EM_P = <EM_P>
c50_p = <c50_p>
BP_P = <BP_P>
CL_P = <CL_P>
V_P = <V_P>
KA_P = <KA_P>
BSVPLC = <BSVPLC>
BSVEM = <BSVEM>
BSVC50 = <BSVC50>
BSVL = <BSVL>
BSVCL = <BSVCL>
BSVV = <BSVV>
BSVKA = <BSVKA>
BOVCL = <BOVCL>
BOVPLC = <BOVPLC>
ERR1 = <ERR1>
ERR2 = <ERR2>
ENDIF
;------------------------------------------------------------------------------
REP=IREP

;---------------------Give initial estimates-----------------------------------
$THETA
3.8   ;Placebo effect
10     ; Emax
0.01  ; EC50 mg/L
101   ; Blood pressure
6.32  ; clearence L/h in adult
36.8 FIX ; Volume not used in this this exercise
1.25 FIX ; absorption rate also not used in this exercise

$OMEGA BLOCK(1) 0.01 FIX  ; placebo effect not used in this case
$OMEGA BLOCK(1) 0.01   ; emax calculated from %cv=10%
$OMEGA BLOCK(1) 0.01   ; AUC 50  calculated from %cv=10%
$OMEGA BLOCK(1) 25     ; DBP standard deviation 5%
$OMEGA BLOCK(1) 0.01   ; CL calculated from %cv=10%
$OMEGA BLOCK(1) 0.01 FIX  ; volume not used in this exercise
$OMEGA BLOCK(1) 0.01 FIX  ; Ka not used in this exercise
$OMEGA BLOCK(1) 0.01   ; calculated from %cv=10% FOR BOV
$OMEGA BLOCK(1) SAME   ; calculated from %cv=10% FPR BOV
$OMEGA BLOCK(1) 0.01 FIX  ; Between occassion variation in placebo effect not used in this exercise
$OMEGA BLOCK(1) SAME

$SIGMA
0 FIX  ; Residual variability in PK, not used in this example
16     ; Residual variability in PD
;-------------------------------------------------------------------------------


$SIM (1234 NEW) (45678 UNI) ONLYSIM NSUB=1

$TABLE ID PERIOD BASE DV DOSE CL EMAX EC50 PLC NOAPPEND NOPRINT ONEHEADER FILE=simtab3
