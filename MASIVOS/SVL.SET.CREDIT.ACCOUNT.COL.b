*-----------------------------------------------------------------------------
* <Rating>56</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SVL.SET.CREDIT.ACCOUNT.COL
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :   
* Version	Autor		Fecha			Comentario 
*-------------------------------------------------------------------------------------------*
* 1.0
* 2.0		RCORTES		2018.02.19		- Realizacion de las validaciones del campo Cuenta de Crédito
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_GTS.COMMON
$INSERT I_TSS.COMMON
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.ACCOUNT
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPEN.FILE
GOSUB PROCESS
RETURN

INIT:
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''

	CREDIT.ACCOUNT 	= COMI
	ACCOUNT.NUMBER 	= FIELD(CREDIT.ACCOUNT,'-',1)
	DEBIT.ACC 		= R.NEW(FT.DEBIT.ACCT.NO) 
	CALL GET.LOC.REF('FUNDS.TRANSFER','LF.COL.ACC',POS.CMP1)
RETURN

OPEN.FILE:
	CALL OPF(FN.ACC,F.ACC)
RETURN

PROCESS:
	;* validacion si el campo tiene valor
	IF ACCOUNT.NUMBER THEN 
		CALL F.READ(FN.ACC, ACCOUNT.NUMBER, RS.ACC, F.ACC, ACC.ERR)
		;* validacion si la cuenta ingresada existe
		IF RS.ACC THEN
			;* validacion si la cuenta de credito no se igual que la de debito 
			IF ACCOUNT.NUMBER NE DEBIT.ACC THEN  
				;* validacion si la cuenta posee restriccion 
				Y.POST.REST = RS.ACC<AC.POSTING.RESTRICT> 
				IF Y.POST.REST NE 20 OR Y.POST.REST NE 21 OR Y.POST.REST NE 24 OR Y.POST.REST NE 26 THEN 
					R.NEW(FT.CREDIT.ACCT.NO)  	= ACCOUNT.NUMBER 
					R.NEW(FT.CHARGED.CUSTOMER)  = RS.ACC<AC.CUSTOMER> 
				END ELSE 
					STRERR = 'EB-CINTEX.CTA.RESTRINGIDA' 
					GOSUB CRT_ERROR	
				END
			END ELSE 
				STRERR = 'EB-CINTEX.CTA.CR.IGUAL'
				GOSUB CRT_ERROR	
			END 			
		END ELSE
			STRERR = 'EB-CINTEX.NO.EXISTE'
			GOSUB CRT_ERROR			    	
	    END
	END ELSE
		STRERR = 'EB-CINTEX.NO.EXISTE.INFO'
		GOSUB CRT_ERROR	
	END

RETURN

CRT_ERROR:
    AF  	= FT.LOCAL.REF
    AV 		= POS.CMP1
    ETEXT	= STRERR    	
	CALL STORE.END.ERROR	
RETURN

END
