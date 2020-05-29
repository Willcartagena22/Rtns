*------------------------------------------------------------------------------------------
* <Rating>-55</Rating>
*------------------------------------------------------------------------------------------
    SUBROUTINE SLV.VAL.SEG.REINT.COM
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
    GOSUB PROCESS


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

CALL GET.LOC.REF('FUNDS.TRANSFER','LF.FECHA.ABONO',LOCPOSFechaAbono)

*VAR.FECHA.VALOR.DIA=R.NEW(FT.DEBIT.VALUE.DATE)[1,2]
*VAR.FECHA.VALOR.ANIO=R.NEW(FT.DEBIT.VALUE.DATE)[8,4]
*VAR.FECHA.VALOR.MES=R.NEW(FT.DEBIT.VALUE.DATE)[4,3]
*
*VAR.FECHA.ABONO.DIA=R.NEW(FT.LOCAL.REF)<1,LOCPOSFechaAbono>[1,2]
*VAR.FECHA.ABONO.ANIO=R.NEW(FT.LOCAL.REF)<1,LOCPOSFechaAbono>[8,4]
*VAR.FECHA.ABONO.MES=R.NEW(FT.LOCAL.REF)<1,LOCPOSFechaAbono>[4,3]
*
*BEGIN CASE 
*CASE (VAR.FECHA.VALOR.DIA GT 31 OR VAR.FECHA.VALOR.DIA LT 0 OR ISDIGIT(VAR.FECHA.VALOR.DIA) EQ 0) OR ISALPHA(VAR.FECHA.VALOR.MES) EQ 0 OR ISDIGIT(VAR.FECHA.VALOR.ANIO) EQ 0 
*	V_NAME_FIELD= FT.DEBIT.VALUE.DATE
*	V.MENSAJE=VAR.FT.ACC
*	STRERR ='Fecha Invalida':FM:V.MENSAJE
*	GOSUB CRT_ERROR
*END CASE
*
*BEGIN CASE 
*CASE (VAR.FECHA.ABONO.DIA GT 31 OR VAR.FECHA.ABONO.DIA LT 0 OR ISDIGIT(VAR.FECHA.ABONO.DIA) EQ 0) OR ISALPHA(VAR.FECHA.ABONO.MES) EQ 0 OR ISDIGIT(VAR.FECHA.ABONO.ANIO) EQ 0
*	V_NAME_FIELD= FT.LOCAL.REF
*	VAR.POS= LOCPOSFechaAbono
*	V.MENSAJE=VAR.FT.ACC
*	STRERR ='Fecha Invalida':FM:V.MENSAJE
*	GOSUB CRT_ERROR
*END CASE

VAR.ACC.DEBIT=R.NEW(FT.DEBIT.ACCT.NO)
VAR.ACC.CREDIT=R.NEW(FT.CREDIT.ACCT.NO)
BEGIN CASE 
CASE VAR.ACC.CREDIT EQ VAR.ACC.DEBIT
	V_NAME_FIELD= FT.DEBIT.ACCT.NO
	V.MENSAJE=VAR.FT.ACC
	STRERR ='Cuenta de Credito debe ser diferente a Cuenta de Debito':FM:V.MENSAJE
	GOSUB CRT_ERROR
END CASE

VAR.MONTO=R.NEW(FT.DEBIT.AMOUNT)
BEGIN CASE 
CASE VAR.MONTO LE 0
	V_NAME_FIELD= FT.DEBIT.AMOUNT
	V.MENSAJE=VAR.FT.ACC
	STRERR ='Monto Debera ser Mayor a Cero':FM:V.MENSAJE
	GOSUB CRT_ERROR
END CASE

RETURN

      
;*Lazadores de error
*-----------------------------------------------------------------------------------------------------------------------
CRT_ERROR:
    AF  = V_NAME_FIELD
    AV 	= VAR.POS
    ETEXT = STRERR
    CALL STORE.END.ERROR
    RETURN

CRT_OVERRIDE:
    TEXT =STRERR
    CURR.NO =V_NAME_FIELD
    CALL STORE.OVERRIDE(CURR.NO)
    RETURN

END
    
    
   
