*-----------------------------------------------------------------------------
* <Rating>4</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.GET.COM.ACH
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.ACCOUNT
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.CAT.COL.VEN
$INSERT I_F.EB.SLV.RANG.PARAM
$INSERT I_F.BENEFICIARY
$INSERT I_F.EB.SLV.TIPO.ACC.ACH 
$INSERT I_F.EB.SLV.TRANSACTION.ACH
$INSERT I_System
$INSERT I_GTS.COMMON 
*-----------------------------------------------------------------------------

IF OFS$OPERATION EQ 'VALIDATE' THEN
	GOSUB INIT
	GOSUB OPENFILE
	GOSUB PROCESS
END
	
RETURN


INIT:		
	FN.ACC	= 'F.ACCOUNT'
	F.ACC	= ''
	
	GOSUB GET.MULTI.LOC.REF
RETURN

OPENFILE:
	CALL OPF(FN.ACC,F.ACC)
RETURN 
		
		
PROCESS:
	CALL F.READ(FN.ACC,R.NEW(FT.DEBIT.ACCT.NO),R.ACC,F.ACC,E.ACC)	 
	Y.CUSTOMER			= R.ACC<AC.CUSTOMER>
	
	CALL SLV.UTIL.GET.COM.VALUE(Y.CUSTOMER,Y.COMI,Y.IVA)
	
	R.NEW(FT.LOCAL.REF)<1,Y.FT.COMISION.ACH> 	= FMT(Y.COMI , 'L2') 
	R.NEW(FT.LOCAL.REF)<1,Y.LF.IVA>				= FMT(Y.IVA , 'L2') 
	
	Y.TOTAL.COM = R.NEW(FT.DEBIT.AMOUNT) + Y.COMI + Y.IVA
	
	IF NUM(Y.TOTAL.COM) THEN
		R.NEW(FT.DEBIT.AMOUNT) = Y.TOTAL.COM
	END
	ELSE
		;* CALL ERROR PARSE NUMERIC AMOUNT
	END	 
	
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
	
END
