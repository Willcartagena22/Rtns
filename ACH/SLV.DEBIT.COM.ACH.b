*-----------------------------------------------------------------------------
* <Rating>-51</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.DEBIT.COM.ACH
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_System
$INSERT I_GTS.COMMON 
$INSERT I_F.ACCOUNT
$INSERT I_F.AC.CHARGE.REQUEST
*-----------------------------------------------------------------------------

;*IF OFS$OPERATION EQ 'PROCESS' THEN
	GOSUB INIT
	GOSUB OPENFILE
	GOSUB PROCESS
;*END
	
RETURN

INIT:		
	FN.ACC	= 'F.ACCOUNT'
	F.ACC	= ''
	
	;*Constantes	
	EQU ID.PARAM.OFS TO 'FT.COMMISSION.TYPE.ACH'
	GOSUB GET.MULTI.LOC.REF
RETURN

OPENFILE:
	CALL OPF(FN.ACC,F.ACC)
RETURN 

PROCESS:
		TRANS.ID = ''
		R.COMI.TR.INT = '' 
	CALL F.READ(FN.ACC,R.NEW(FT.DEBIT.ACCT.NO),R.ACC,F.ACC,E.ACC)	 
	
		Y.CUSTOMER			= R.ACC<AC.CUSTOMER>
		TEXTO.ARCHIVO = 'Y.CUSTOMER -> ':Y.CUSTOMER
		GOSUB ESCRIBIR.ARCHIVO
	
		R.COMI.TR.INT<CHG.DEBIT.ACCOUNT> = R.NEW(FT.DEBIT.ACCT.NO)
		R.COMI.TR.INT<CHG.CUSTOMER.NO>   = Y.CUSTOMER
		R.COMI.TR.INT<CHG.CHARGE.AMOUNT> = R.NEW(FT.LOCAL.REF)<1,Y.FT.COMISION.ACH>
		
		TEXTO.ARCHIVO = 'R.COMI.TR.INT -> ':R.COMI.TR.INT
		GOSUB ESCRIBIR.ARCHIVO
		
		CALL SLV.UTIL.OFS.TRX(TRANS.ID,R.COMI.TR.INT,ID.PARAM.OFS,Y.OUT)
RETURN

GET.MULTI.LOC.REF:
    Y.APPL = "FUNDS.TRANSFER"
    Y.FIELD = "LF.TCE.NARR": VM :"LF.AMOUNT":VM:"ACH.BANCO.BENEF":VM:"ACH.PROP":VM:"ACH.BANCO.AZUL":VM:"ACH.TIPO.TXN":VM:"LF.ID.COL":VM:"ACH.BANCO.AZUL":VM:"ID.BENEF.ACH":VM:"ACCT.BENEF.ACH":VM:"FT.COMISION.ACH":VM:"LF.IVA" 
    Y.POS = ""
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FIELD,Y.POS)

    Y.LF.TCE.NARR 			= Y.POS<1,1>
    Y.LF.AMOUNT 			= Y.POS<1,2>
    Y.ACH.BANCO.BENEF		= Y.POS<1,3>
    Y.ACH.PROP 				= Y.POS<1,4>
   	Y.ACH.BANCO.AZUL		= Y.POS<1,5>
    Y.ACH.TIPO.TXN			= Y.POS<1,6>
    Y.LF.ID.COL				= Y.POS<1,7>
    Y.ID.BANCO.AZUL			= Y.POS<1,8>
    Y.BENEF.ACH				= Y.POS<1,9>
    Y.ACCT.BENEF.ACH		= Y.POS<1,10> 
    Y.FT.COMISION.ACH		= Y.POS<1,11>  
    Y.LF.IVA				= Y.POS<1,12> 
RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'ACH'
    R.ID   = 'ACH.PAGO.COMISION.':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN


END
