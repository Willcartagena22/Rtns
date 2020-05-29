*------------------------------------------------------------------------------------------
* <Rating>-68</Rating>
*------------------------------------------------------------------------------------------
    SUBROUTINE SLV.VAL.PAGOS.CINTEX
*------------------------------------------------------------------------------------------
* RUTINA QUE REALIZA VALIDACIONES PARA LA FUNCIONALIDAD DE SEGUROS
*------------------------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version.
* SFUSILLO 	11.09.2017 		Initial Code
* SFUSILLO 					Rutina para validacion de pagos que vienen de Cintex
*							Inicialmente se agregan validaciones relacionados a los pagos
*							de primas de Seguros.
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_EQUATE
	$INSERT I_F.EB.SLV.ALTA.SEGURO
	$INSERT I_F.CUSTOMER
	$INSERT I_F.CUSTOMER.ACCOUNT
	$INSERT I_F.ACCOUNT
	$INSERT I_F.FUNDS.TRANSFER
	$INSERT I_F.AC.LOCKED.EVENTS


    GOSUB INIT
    GOSUB OPENFILE
    
    BEGIN CASE 
    CASE VAR.TRANSAC.TYPE EQ 'AC33'
    GOSUB PROCESS
    END CASE

    RETURN

INIT:
;*ARCHIVOS
    FN.ALTA.SEG 	= 'F.EB.SLV.ALTA.SEGURO'
    F.ALTA.SEG	= ''
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER	= ''
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT	= ''
    FN.LCK = 'F.AC.LOCKED.EVENTS'
    F.LCK=''
    FN.CUSTOMER.ACCOUNT = 'F.CUSTOMER.ACCOUNT'
    F.CUSTOMER.ACCOUNT	= ''
    
    
	CALL GET.LOC.REF('FUNDS.TRANSFER','LF.CUS.ID',LOCPOS.CUS.ID)						
	N.CUSTOMER.ID=R.NEW(FT.LOCAL.REF)<1,LOCPOS.CUS.ID>
	VAR.FT.ACC=R.NEW(FT.DEBIT.ACCT.NO)

	
	ARR_LOCAL_FIELD_CUS	=''
	VAR.TRANSAC.TYPE=R.NEW(FT.TRANSACTION.TYPE)
	Y.AMT.TXN=R.NEW(FT.DEBIT.AMOUNT)
	;*N.CUSTOMER.ID='100944'
	;*VAR.TRANSAC.TYPE = 'AC'
	;*VAR.FT.ACC='10000000041899'
	;*Y.AMT.TXN=5000.00
;*-----------------------------------

;*CONSTANTES
;*-----------------------------------



;*PARAMETROS
;*-----------------------------------
   
    RETURN

*APERTURA DE ARCHIVOS A USAR
OPENFILE:
    CALL OPF(FN.ALTA.SEG,F.ALTA.SEG)
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.LCK,F.LCK)
    
  

RETURN


PROCESS:

    
;*registo del cliente
CALL F.READ (FN.CUSTOMER,N.CUSTOMER.ID,R_CUS,F.CUSTOMER,CUS_ERR)
CALL F.READ (FN.CUSTOMER.ACCOUNT,N.CUSTOMER.ID,R.CUS.ACC,F.CUSTOMER.ACCOUNT,CUS_ERR)
CALL F.READ (FN.ACCOUNT,VAR.FT.ACC,R.ACC,F.ACCOUNT,CUS_ERR)
ARR_LOCAL_FIELD_CUS = (R_CUS)

;*Obteniendo data de la cuenta
;*-----------------------------
*    Y.AMT.ACC = R.ACC<AC.WORKING.BALANCE>
*	
* 	;*Obteniendo el saldo Bloqueado de la cuenta
*;*-------------------------------------------
*    SMT = "SELECT ": FN.LCK : " WITH ACCOUNT.NUMBER EQ '":VAR.FT.ACC:"'"
*    CALL EB.READLIST (SMT, LIST.LCK, NAME.LCK, SELECTED.LCK, ERR.LCK)
*    Y.SUM.LCK = 0
*    FOR LCK = 1 TO SELECTED.LCK
*        CALL F.READ(FN.LCK,LIST.LCK<LCK>,RECORD.LCK,F.LCK,ERROR.LCK)
*        Y.SUM.LCK = Y.SUM.LCK + RECORD.LCK<AC.LCK.LOCKED.AMOUNT>
*    NEXT LCK

*;*Saldo disponible en la cuenta
*;*-----------------------------
*    Y.AMT.DISP.ACC = Y.AMT.ACC - Y.SUM.LCK
*       ;*validando que posee cupo de liberacion
*        ;*--------------------------------------
*  	IF Y.AMT.DISP.ACC LT Y.AMT.TXN THEN
*		V_NAME_FIELD= FT.DEBIT.AMOUNT
*		V.MENSAJE=VAR.FT.ACC
*		STRERR ='Saldo de la cuenta es insuficiente: ':FM:V.MENSAJE
*        GOSUB CRT_ERROR
*	END
	
	CALL GET.LOC.REF('ACCOUNT', 'LF.ESTADO.CTA', POS.ESTADO)
	VAR.STATUS = R.ACC<AC.LOCAL.REF, POS.ESTADO>
	IF VAR.STATUS NE '' AND VAR.STATUS NE 'ACT' THEN
		V_NAME_FIELD= FT.DEBIT.ACCT.NO
		V.MENSAJE=VAR.FT.ACC
		STRERR ='Cuenta esta Inactiva: ':FM:V.MENSAJE
		GOSUB CRT_ERROR
	END

VAR.POST.REST = R.ACC<AC.POSTING.RESTRICT>

BEGIN CASE
CASE VAR.POST.REST EQ 24 OR VAR.POST.REST EQ 26 OR VAR.POST.REST EQ 20 OR VAR.POST.REST EQ 23 OR VAR.POST.REST EQ 21
	V_NAME_FIELD= FT.DEBIT.ACCT.NO
	V.MENSAJE=VAR.FT.ACC
	STRERR ='Cuenta Presenta Bloqueo Debitos o Embargo: ':FM:V.MENSAJE
	GOSUB CRT_ERROR
END CASE

BEGIN CASE 
CASE R.CUS.ACC<EB.CAC.ACCOUNT.NUMBER> NE VAR.FT.ACC
	V_NAME_FIELD= FT.DEBIT.ACCT.NO
	V.MENSAJE=VAR.FT.ACC
	STRERR ='Cuenta no Pertenece al Cliente: ':FM:V.MENSAJE
	GOSUB CRT_ERROR
END CASE

RETURN

      
;*Lazadores de error
*-----------------------------------------------------------------------------------------------------------------------
CRT_ERROR:
    AF  = V_NAME_FIELD
    AV 	= 1
    ETEXT = STRERR
    CALL STORE.END.ERROR
    RETURN

CRT_OVERRIDE:
    TEXT =STRERR
    CURR.NO =V_NAME_FIELD
    CALL STORE.OVERRIDE(CURR.NO)
    RETURN

END
    
    
   
