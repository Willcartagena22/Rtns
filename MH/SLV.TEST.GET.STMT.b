*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.TEST.GET.STMT
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------
Y.AC.DUP.VAR  = '10000000000165'
DATE.FROM = '20150101'
DATE.TO = '20161231'
YID.LIST = ''
OPENING.BAL = ''
ER = ''
FN.STMT.ENTRY = 'F.STMT.ENTRY'
F.STMT.ENTRY = ''
FECHA=TODAY

CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)

 
 CALL EB.ACCT.ENTRY.LIST(Y.AC.DUP.VAR,DATE.FROM,DATE.TO,YID.LIST,OPENING.BAL,ER)
 CRT "IDS:":YID.LIST
 LOOP
  REMOVE Y.ID FROM YID.LIST SETTING POSS
  WHILE Y.ID NE ''
   CALL F.READ(FN.STMT.ENTRY,Y.ID,R.STMT,F.STMT.ENTRY,ERROR)
   CRT R.STMT
  REPEAT
 END
