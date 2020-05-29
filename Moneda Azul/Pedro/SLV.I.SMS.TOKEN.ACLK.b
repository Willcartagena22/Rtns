*-----------------------------------------------------------------------------
* <Rating>-47</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.SMS.TOKEN.ACLK 
*-----------------------------------------------------------------------------
*
* Nombre: SLV.I.SMS.TOKEN.ACLK
* Descripción: Rutina para envio de SMS a Cliente con Token desde AC.LOCKED.EVENTS
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* OCornejo	09.10.2017	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AC.LOCKED.EVENTS
$INSERT I_F.EB.SLV.STM.FAVORITES
$INSERT I_F.ACCOUNT 
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

;*Obteniendo Posiciones de Campos Locales
Y.APPL = "AC.LOCKED.EVENTS"
Y.FIELD = "LF.MOTIVO.CHQ" : VM : "LF.CHQ.BENEFICI" : VM : "LF.TRAN.TIME"
Y.POS = ""
CALL MULTI.GET.LOC.REF(Y.APPL,Y.FIELD,Y.POS)
LF.MOTIVO       = Y.POS<1,1>
LF.CHQ.BENEFICI = Y.POS<1,2>
LF.TRAN.TIME    = Y.POS<1,3> 
	    
;*Ejecutar Rutina solo Cuando sea para el Servicio de Transferencia Movil
IF R.NEW(AC.LCK.LOCAL.REF)<1,LF.MOTIVO> NE 'STM' THEN
	RETURN
END

GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
RETURN
	
	INIT:
		FN.FAV = 'F.EB.SLV.STM.FAVORITES'
		F.FAV  = ''
		FN.ACC = 'F.ACCOUNT'
		F.ACC  = ''
		FN.CUS = 'F.CUSTOMER'
		F.CUS  = ''    
	RETURN
	
	OPENFILE:
		CALL OPF(FN.FAV, F.FAV)
		CALL OPF(FN.ACC, F.ACC)
		CALL OPF(FN.CUS, F.CUS)
	RETURN
	
	PROCESS:
		PACKAGE.NAME 		= "t24broadcasterutilnotifications.T24BroadCasterUtilNotifications"
		THIS.METHOD.NAME 	= ""
		METHOD.NAME			= "notificationHalCash"
		
		;*Obtener Registro de Favorito Asociado en Transaccion
		Y.FAVORITO = R.NEW(AC.LCK.LOCAL.REF)<1, LF.CHQ.BENEFICI>
		CALL F.READ(FN.FAV, Y.FAVORITO, R.FAV, F.FAV, E.FAV)
		
		;*Generar Token Unico
		Y.ID    = 'STM'
		Y.SEEDS  = R.FAV<EB.SLV83.PHONE> : @VM : R.FAV<EB.SLV83.DOCUMENT>
		Y.TOKEN = ''
		Y.ERROR = ''
		CALL SLV.UTIL.STM.TOKEN(Y.ID, Y.SEEDS, Y.TOKEN, Y.ERROR)
		
		;*Si se produjo un error en la generacion del Token retornar error
		IF Y.ERROR NE '' THEN
			E = Y.ERROR
			CALL ERR
			RETURN
		END
		
		;*Almacenar Hora de Generacion del Token
		R.NEW(AC.LCK.LOCAL.REF)<1, LF.TRAN.TIME> = FIELD(Y.TOKEN, @VM, 2)
		Y.TOKEN = FIELD(Y.TOKEN, @VM, 1)
		
		;*Obtener Correo del Cliente
		CALL CACHE.READ(FN.ACC, R.NEW(AC.LCK.ACCOUNT.NUMBER), R.ACC, E.ACC)
		CALL CACHE.READ(FN.CUS, R.ACC<AC.CUSTOMER>, R.CUS, E.CUS)
		
		;*Envio de SMS a Beneficiario de Transferencia
		ARR.SMS = ''
		ARR.SMS	:= Y.TOKEN						: "%%"
		ARR.SMS	:= 'USD'						: "%%"
		ARR.SMS	:= '$'							: "%%"
		ARR.SMS	:= R.NEW(AC.LCK.LOCKED.AMOUNT)	: "%%"
		ARR.SMS	:= R.FAV<EB.SLV83.PHONE>		: "%%"
		ARR.SMS	:= R.CUS<EB.CUS.EMAIL.1>		: "%%"
		ARR.SMS	:= R.NEW(AC.LCK.TO.DATE)[7,2] : '/' : R.NEW(AC.LCK.TO.DATE)[5,2] : '/' : R.NEW(AC.LCK.TO.DATE)[1,4]	    
		GOSUB PROCESS.SEND.NOTIFICACION
	RETURN

	PROCESS.SEND.NOTIFICACION:
		THIS.PACKAGE.CLASS 				= PACKAGE.NAME
	    THIS.METHOD.SMS					= METHOD.NAME
	    CALLJ.ARGUMENTS.SMS 			= ARR.SMS
		CALLJ.ERROR.SMS 				= " "
		CALLJ.RESPONSE.SMS 				= " "	
		CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.SMS,CALLJ.ARGUMENTS.SMS,CALLJ.RESPONSE.SMS,CALLJ.ERROR.SMS)
		IF CALLJ.ERROR.SMS EQ '0' AND CALLJ.ARGUMENTS.NOLIOF[1,3]!='(-1'THEN
			 ETEXT = "No se puede enviar el ARR.SMS":CALLJ.RESPONSE.SMS:"EMAILNOLIOF:": CALLJ.ARGUMENTS.SMS
		END
	RETURN
END
