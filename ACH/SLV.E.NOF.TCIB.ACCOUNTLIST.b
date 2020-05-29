*------------------------------------------------------------------------------
* <Rating>-111</Rating>
*------------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.TCIB.ACCOUNTLIST(ENQ.DATA)
*------------------------------------------------------------------------------
* Nombre:		SLV.E.NOF.TCIB.ACCOUNTLIST(ENQ.DATA)
* Descripcion: 	Rutina para recuperar las cuentas de un cliente configuradas  
*				en Banca en Línea según el ISA.
*------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*------------------------------------------------------------------------------
* 1.0		JMCRUZ		26.04.2017	Version inicial
* 1.1		PSANCHEZ	08.01.2017	Correccion al validar el estado de la cuenta
* 1.2		RCORTES		13.12.2019	Correccion para verificar estado de cuenta 
*------------------------------------------------------------------------------
	$INSERT I_COMMON
	$INSERT I_EQUATE
	$INSERT I_ENQUIRY.COMMON
    $INSERT I_System

	$INSERT I_F.AA.ARRANGEMENT
	$INSERT I_F.AC.LOCKED.EVENTS
	$INSERT I_F.ACCOUNT
	$INSERT I_F.CUSTOMER
*------------------------------------------------------------------------------
	GOSUB INIT
	GOSUB GET_TIMEDATE
	GOSUB OPEN
	GOSUB PROCESS
	GOSUB GET_TIMEDATE
RETURN
*------------------------------------------------------------------------------
INIT:
	EQU RTN TO 'SLV_ACCOUNT_LIST'
	Y.TXT = ''

	FN.ARRANGEMENT = 'F.AA.ARRANGEMENT'
	F.ARRANGEMENT = ''
	FN.LCK = 'F.AC.LOCKED.EVENTS'
	F.LCK = ''
	FN.ACCOUNT = 'F.ACCOUNT'
	F.ACCOUNT = ''
	FN.CUSTOMER = 'F.CUSTOMER'
	F.CUSTOMER = ''

	EQU EXT.CUSTOMER TO 'EXT.CUSTOMER'
	EQU EXT.EXTERNAL.USER TO 'EXT.EXTERNAL.USER'
	EQU EXT.SMS.CUSTOMERS TO 'EXT.SMS.CUSTOMERS'
    EQU EXT.SMS.ACCOUNTS TO 'EXT.SMS.ACCOUNTS'

	EQU CUSTOMER TO 'CUSTOMER'
	EQU LF.RAZON.SOCIAL TO 'LF.RAZON.SOCIAL'

	EQU ACCOUNTS TO 'ACCOUNTS'
    EQU STATUS.UNAUTH TO 'UNAUTH'
    EQU STATUS.CANCELLED TO 'CANCELLED'
    EQU STATUS.MATURED TO 'MATURED'
    EQU STATUS.CLOSE TO 'CLOSE'

	EQU RANGO TO 'SLV.CAT.CUENTAS'
	EQU CTA.AHO TO 'CTA.AHO'
    EQU CTA.COR TO 'CTA.COR'
RETURN
*------------------------------------------------------------------------------
;* Identificación de la Ejecución Actual
GET_TIMEDATE:
	Y.TXT = RTN : ' = ' : TIMEDATE()
*	CRT RTN : ' = ' : TIMEDATE()
	GOSUB WRITE_LOG_FILE
RETURN
*------------------------------------------------------------------------------
;* Archivo para Revisión de Errores
WRITE_LOG_FILE:
*	DIR.NAME = 'CHQ.OUT'
*	R.ID = RTN : '_' : TODAY : '.txt'
*
*	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
*		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
*		END
*	CLOSESEQ SEQ.PTR 
RETURN
*------------------------------------------------------------------------------
OPEN:
	CALL OPF(FN.ARRANGEMENT, F.ARRANGEMENT)
	CALL OPF(FN.LCK, F.LCK)
	CALL OPF(FN.ACCOUNT, F.ACCOUNT)
	CALL OPF(FN.CUSTOMER, F.CUSTOMER)
RETURN
*------------------------------------------------------------------------------
PROCESS:
******************************************************
* DEBUG:
*
*	EXT.USER = 'INPUT116999'
*	EXT.PN.CUSTOMER = '116999'
*	EXT.PJ.CUSTOMER = '116999'
*	EXT.ACCOUNTS = '10000000000928':SM:'10000000297435':SM:'10000000297435'
	
******************************************************
	;* Obtener Nombre de Usuario
	EXT.USER = System.getVariable(EXT.EXTERNAL.USER)
	Y.TXT = 'EXT.USER = ' : EXT.USER
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	;* Obtener ID para Cliente Natural
	EXT.PN.CUSTOMER = System.getVariable(EXT.CUSTOMER)
	Y.TXT = 'EXT.PN.CUSTOMER = ' : EXT.PN.CUSTOMER
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	;* Obtener ID para Cliente Jurídico
	EXT.PJ.CUSTOMER = System.getVariable(EXT.SMS.CUSTOMERS)
	Y.TXT = 'EXT.PJ.CUSTOMER = ' : EXT.PJ.CUSTOMER
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	;* Obtener Listado de Cuentas
	EXT.ACCOUNTS = System.getVariable(EXT.SMS.ACCOUNTS)
	Y.TXT = 'EXT.ACCOUNTS = ' : EXT.ACCOUNTS
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	;* Cantidad de Cuentas
    Y.ACCOUNTS.NO = DCOUNT(EXT.ACCOUNTS, SM)
	Y.TXT = 'Y.ACCOUNTS.NO = ' : Y.ACCOUNTS.NO
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	GOSUB GET_ACCOUNTS_DATA
RETURN
*------------------------------------------------------------------------------
;* Detalles de Cuentas
GET_ACCOUNTS_DATA:
    Y.ACCOUNT = ''
    Y.ARRANGEMENT = '' 
    Y.PRODUCT.LINE = ''

    FOR Y.ACC.NO = 1 TO Y.ACCOUNTS.NO
		;* Recorriendo el Listado de Cuentas
        Y.ACCOUNT = TRIM(EXT.ACCOUNTS<1, 1, Y.ACC.NO>)
		Y.TXT = 'Y.ACCOUNT = ' : Y.ACCOUNT
*		CRT Y.TXT
		GOSUB WRITE_LOG_FILE

		;* Recuperando el Registro de la Cuenta
    	CALL F.READ(FN.ACCOUNT, Y.ACCOUNT, R.ACCOUNT, F.ACCOUNT, ERR.ACCOUNT)
		Y.TXT = 'R.ACCOUNT = ' : R.ACCOUNT
*		CRT Y.TXT
		GOSUB WRITE_LOG_FILE
		
			CALL GET.LOC.REF('ACCOUNT', 'LF.ESTADO.CTA', POS.ESTADO.CTA)
			ESTADO.CTA	= R.ACCOUNT<AC.LOCAL.REF><1 , POS.ESTADO.CTA>
			Y.TXT 		= 'Y.ACCOUNT = ' : Y.ACCOUNT : ' | ESTADO.CTA = ' : ESTADO.CTA
			GOSUB WRITE_LOG_FILE

		;* Acuerdo correspondiente a la Cuenta - AC.ARRANGEMENT.ID
		Y.ARRANGEMENT = TRIM(R.ACCOUNT<AC.ARRANGEMENT.ID>)
		Y.TXT = 'Y.ARRANGEMENT = ' : Y.ARRANGEMENT
*		CRT Y.TXT
		GOSUB WRITE_LOG_FILE

		;* Recuperando el Registro del Acuerdo
		CALL F.READ(FN.ARRANGEMENT, Y.ARRANGEMENT, R.ARRANGEMENT, F.ARRANGEMENT, ERR.ARRANGEMENT)
		Y.TXT = 'R.ARRANGEMENT = ' : R.ARRANGEMENT
*		CRT Y.TXT
		GOSUB WRITE_LOG_FILE

		;* Tipo de Producto configurado en el Acuerdo - AA.ARR.PRODUCT.LINE
		Y.PRODUCT.LINE = TRIM(R.ARRANGEMENT<AA.ARR.PRODUCT.LINE>)
		Y.TXT = 'Y.PRODUCT.LINE = ' : Y.PRODUCT.LINE
*		CRT Y.TXT
		GOSUB WRITE_LOG_FILE
		

    	IF Y.PRODUCT.LINE EQ ACCOUNTS AND Y.STATUS NE STATUS.UNAUTH AND Y.STATUS NE STATUS.CANCELLED AND Y.STATUS NE STATUS.MATURED AND Y.STATUS NE STATUS.CLOSE THEN
    		IF ESTADO.CTA = 'ACT' OR ESTADO.CTA = '' THEN
				Y.TXT 		= 'Y.ACCOUNT = ' : Y.ACCOUNT : ' | ESTADO.CTA = ' : ESTADO.CTA
				CRT   Y.TXT
				GOSUB WRITE_LOG_FILE
		
				GOSUB GET_PRODUCT_DATA
				GOSUB GET_ENQUIRY
			END    			
		END

		Y.TXT = 'Y.ACC.NO = ' : Y.ACC.NO
*		CRT Y.TXT
		GOSUB WRITE_LOG_FILE
    NEXT Y.ACC.NO
RETURN
*------------------------------------------------------------------------------
;* Información Solicitada
GET_PRODUCT_DATA:
	;* Persona Natural
	CALL F.READ(FN.CUSTOMER, EXT.PN.CUSTOMER, R.CUSTOMER, F.CUSTOMER, ERR.CUSTOMER)
	;* Nombre Corto
	Y.SHORT.NAME = TRIM(R.CUSTOMER<EB.CUS.SHORT.NAME, 1>)

	Y.TXT = 'Y.SHORT.NAME = ' : Y.SHORT.NAME
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	Y.RAZON.SOCIAL = ''
	IF EXT.PN.CUSTOMER NE EXT.PJ.CUSTOMER THEN
 		;* Persona Jurídica
 		CALL F.READ(FN.CUSTOMER, EXT.PJ.CUSTOMER, R.CUSTOMER, F.CUSTOMER, ERR.CUSTOMER)
		CALL GET.LOC.REF(CUSTOMER, LF.RAZON.SOCIAL, P.RAZON.SOCIAL)
		;* Razón Social
		Y.RAZON.SOCIAL = TRIM(R.CUSTOMER<EB.CUS.LOCAL.REF, P.RAZON.SOCIAL>)
 	END

	Y.TXT = 'Y.RAZON.SOCIAL = ' : Y.RAZON.SOCIAL
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	;* Categoría de la Cuenta - AC.CATEGORY
	Y.CATEGORY = TRIM(R.ACCOUNT<AC.CATEGORY>)
	Y.TXT = 'Y.CATEGORY = ' : Y.CATEGORY
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	;* Leyenda de la Cuenta - AC.ACCOUNT.TITLE.1
	Y.TITLE.1 = TRIM(R.ACCOUNT<AC.ACCOUNT.TITLE.1>)
	Y.TXT = 'Y.TITLE.1 = ' : Y.TITLE.1
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	;* Moneda de la Cuenta - AC.CURRENCY
	Y.CURRENCY = TRIM(R.ACCOUNT<AC.CURRENCY>)
	Y.TXT = 'Y.CURRENCY = ' : Y.CURRENCY
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	;* Saldo Total de la Cuenta - AC.ONLINE.ACTUAL.BAL
	Y.ACTUAL.BAL = TRIM(R.ACCOUNT<AC.ONLINE.ACTUAL.BAL>)
	Y.TXT = 'Y.ACTUAL.BAL = ' : Y.ACTUAL.BAL
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	;* Saldo Disponible de la Cuenta - AC.WORKING.BALANCE
	Y.WORKING.BAL = TRIM(R.ACCOUNT<AC.WORKING.BALANCE>)
	Y.TXT = 'Y.WORKING.BAL = ' : Y.WORKING.BAL
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	;* Calculando el Monto Bloqueado en la Cuenta
	SMT = "SELECT " : FN.LCK : " WITH ACCOUNT.NUMBER EQ '" : Y.ACCOUNT : "'"
	CALL EB.READLIST(SMT, LIST.LCK, NAME.LCK, SELECTED.LCK, ERR.LCK)
	Y.TXT = 'SELECTED.LCK = ' : SELECTED.LCK
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	Y.SUM.LCK = 0
	FOR LCK = 1 TO SELECTED.LCK
		CALL F.READ(FN.LCK, LIST.LCK<LCK>, RECORD.LCK, F.LCK, ERROR.LCK)
		Y.TXT = 'RECORD.LCK = ' : RECORD.LCK
*		CRT Y.TXT
		GOSUB WRITE_LOG_FILE

		Y.SUM.LCK = Y.SUM.LCK + RECORD.LCK<AC.LCK.LOCKED.AMOUNT>
	NEXT LCK
	Y.TXT = 'Y.SUM.LCK = ' : Y.SUM.LCK
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	;* Saldo Real de la Cuenta
	Y.REAL.BAL = Y.WORKING.BAL - Y.SUM.LCK
	Y.TXT = 'Y.REAL.BAL = ' : Y.REAL.BAL
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE
RETURN
*------------------------------------------------------------------------------
;* Enquiry
GET_ENQUIRY:
 	;* Construcción del Arreglo
 	STR.ARR = ''

	STR.ARR	:= EXT.USER : "*" ;*1
 	STR.ARR	:= EXT.PN.CUSTOMER : "*" ;*2
 	STR.ARR	:= Y.SHORT.NAME : "*" ;*3
 	STR.ARR	:= EXT.PJ.CUSTOMER : "*" ;*4
 	STR.ARR	:= Y.RAZON.SOCIAL : "*" ;*5

 	STR.ARR	:= Y.ACCOUNT : "*" ;*6
 	STR.ARR	:= Y.ARRANGEMENT : "*" ;*7
 	STR.ARR	:= Y.PRODUCT.LINE : "*" ;*8
 	STR.ARR	:= Y.CATEGORY : "*" ;*9
 	STR.ARR	:= Y.TITLE.1 : "*" ;*10

 	STR.ARR	:= Y.CURRENCY : "*" ;*11
 	STR.ARR	:= Y.ACTUAL.BAL : "*" ;*12
 	STR.ARR	:= Y.WORKING.BAL : "*" ;*13
 	STR.ARR	:= Y.SUM.LCK : "*" ;*14
 	STR.ARR	:= Y.REAL.BAL ;*15

	Y.TXT = 'STR.ARR = ' : STR.ARR
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE

	ENQ.DATA<-1> = STR.ARR
	Y.TXT = 'ENQ.DATA = ' : ENQ.DATA
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE
RETURN
*------------------------------------------------------------------------------
END
*------------------------------------------------------------------------------
