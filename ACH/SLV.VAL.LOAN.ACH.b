*-----------------------------------------------------------------------------
* <Rating>-61</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.VAL.LOAN.ACH
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.ACCOUNT
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB GET.LOCAL.FIELD
GOSUB PROCESS
RETURN

INIT:
	FN.FT  = "F.FUNDS.TRANSFER"
    F.FT   = ""	
    CALL OPF(FN.FT,F.FT) 
    
   	FN.ACC	= 'F.ACCOUNT'
	F.ACC	= '' 
    CALL OPF(FN.ACC,F.ACC)
    
    EQU STATUS.NO.VALID TO '3':FM:'4':FM:'5':FM:'6'

RETURN


PROCESS:
	
	CALL F.READ(FN.ACC,R.NEW(FT.CREDIT.ACCT.NO),R.ACC,F.ACC,E.ACC)	
	
	;************************************
	TEXTO.ARCHIVO = 'R.ACC -> ':R.ACC<AC.LOCAL.REF><1,Y.LF.LOAN.STATUS>
	GOSUB ESCRIBIR.ARCHIVO
	;*-----------------------------------
	
	FIND R.ACC<AC.LOCAL.REF><1,Y.LF.LOAN.STATUS> IN STATUS.NO.VALID SETTING Y.FIELD,Y.VAL THEN
		Y.KEY.ERROR = 'AA-SLV.VAL.PAGO.ESTADO'
		
		;************************************
		TEXTO.ARCHIVO = 'R.ACC -> ':Y.KEY.ERROR
		GOSUB ESCRIBIR.ARCHIVO
		;*-----------------------------------
		
		GOSUB SEND.ERROR
	END
	
RETURN

GET.LOCAL.FIELD:
	 Y.APPL = "ACCOUNT"
    Y.FIELD = "LF.LOAN.STATUS"
    Y.POS = ""
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FIELD,Y.POS)

    Y.LF.LOAN.STATUS 			= Y.POS<1,1>
	
RETURN

SEND.ERROR:
   ETEXT = Y.KEY.ERROR
   CALL STORE.END.ERROR
RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'ACH'
    R.ID   = 'ACH.PAGO.ACRP.':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN

END
