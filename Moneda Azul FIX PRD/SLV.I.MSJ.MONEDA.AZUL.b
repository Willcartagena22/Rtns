*-----------------------------------------------------------------------------
* <Rating>-58</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.MSJ.MONEDA.AZUL
*--------------------------------------------------------------------------------------------------------------
*
* Nombre: SLV.I.MSJ.MONEDA.AZUL
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
Y.FIELD = "LF.MOTIVO.CHQ" : VM : "LF.CHQ.BENEFICI" : VM : "LF.TRAN.TIME" : VM : "LF.AMT.BLUE"
Y.POS = ""
CALL MULTI.GET.LOC.REF(Y.APPL,Y.FIELD,Y.POS)
LF.MOTIVO       = Y.POS<1,1>
LF.CHQ.BENEFICI = Y.POS<1,2>
LF.TRAN.TIME    = Y.POS<1,3> 
LF.AMT.BLUE		= Y.POS<1,4>

		TEXTO.ARCHIVO='LF.MOTIVO : ': LF.MOTIVO:' LF.CHQ.BENEFICI : ':LF.CHQ.BENEFICI:' LF.TRAN.TIME : ' :LF.TRAN.TIME : ' LF.AMT.BLUE':LF.AMT.BLUE
		GOSUB ESCRIBIR.ARCHIVO
	    
	   MOTIVO= R.NEW(AC.LCK.LOCAL.REF)<1,LF.MOTIVO>
	   
TEXTO.ARCHIVO='MOTIVO : ': MOTIVO
GOSUB ESCRIBIR.ARCHIVO
;*Ejecutar Rutina solo Cuando sea para el Servicio de Transferencia Movil
IF R.NEW(AC.LCK.LOCAL.REF)<1,LF.MOTIVO> NE 'STM' THEN
TEXTO.ARCHIVO='ENTRO AL IF R.NEW(AC.LCK.LOCAL.REF)<1,LF.MOTIVO> NE STM'
GOSUB ESCRIBIR.ARCHIVO
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
		
		TEXTO.ARCHIVO='Y.FAVORITO : ': Y.FAVORITO
		GOSUB ESCRIBIR.ARCHIVO
		
		
		
		CALL F.READ(FN.FAV, Y.FAVORITO, R.FAV, F.FAV, E.FAV)
		
		;*Generar Token Unico
		Y.ID    = 'STM'
		Y.SEEDS  = R.FAV<EB.SLV83.PHONE> : @VM : R.FAV<EB.SLV83.DOCUMENT>
		Y.TOKEN = ''
		Y.ERROR = ''
		CALL SLV.UTIL.STM.TOKEN(Y.ID, Y.SEEDS, Y.TOKEN, Y.ERROR)
		
		TEXTO.ARCHIVO='Y.SEEDS : ': Y.SEEDS
		GOSUB ESCRIBIR.ARCHIVO
		
		
		;*Si se produjo un error en la generacion del Token retornar error
		IF Y.ERROR NE '' THEN
			E = Y.ERROR
			CALL ERR
			RETURN
		END
		
		;*Almacenar Hora de Generacion del Token
		R.NEW(AC.LCK.LOCAL.REF)<1, LF.TRAN.TIME> = FIELD(Y.TOKEN, @VM, 2)
		Y.TOKEN = FIELD(Y.TOKEN, @VM, 1)
		
		TEXTO.ARCHIVO='Y.TOKEN : ': Y.TOKEN
		GOSUB ESCRIBIR.ARCHIVO
		
		;*Obtener Correo del Cliente
		CALL CACHE.READ(FN.ACC, R.NEW(AC.LCK.ACCOUNT.NUMBER), R.ACC, E.ACC)
		CALL CACHE.READ(FN.CUS, R.ACC<AC.CUSTOMER>, R.CUS, E.CUS)
		
		COMISION= R.NEW(AC.LCK.LOCAL.REF)<1, LF.AMT.BLUE>
		MONTO=R.NEW(AC.LCK.LOCKED.AMOUNT)
		
		MONTOSINCOMISION=MONTO-COMISION
		
		TEXTO.ARCHIVO='COMISION : ': COMISION:' MONTO :':MONTO: ' MONTOSINCOMISION: ':MONTOSINCOMISION
		GOSUB ESCRIBIR.ARCHIVO
		
		;*Envio de SMS a Beneficiario de Transferencia
		ARR.SMS = ''
		ARR.SMS	:= Y.TOKEN						: "%%"
		ARR.SMS	:= 'USD'						: "%%"
		ARR.SMS	:= '$'							: "%%"
		ARR.SMS	:= MONTOSINCOMISION	: "%%"
		ARR.SMS	:= R.FAV<EB.SLV83.PHONE>		: "%%"
		ARR.SMS	:= R.CUS<EB.CUS.EMAIL.1>		: "%%"
		ARR.SMS	:= R.NEW(AC.LCK.TO.DATE)[7,2] : '/' : R.NEW(AC.LCK.TO.DATE)[5,2] : '/' : R.NEW(AC.LCK.TO.DATE)[1,4]	    
		
		
		

		Y.OPERACION=R.NEW(AC.LCK.RECORD.STATUS)
		Y.OPERACION=Y.OPERACION[1,1]
		
		IF Y.OPERACION NE 'R' 
		THEN
		GOSUB PROCESS.SEND.NOTIFICACION
		END
		
		
	RETURN

	PROCESS.SEND.NOTIFICACION:
		THIS.PACKAGE.CLASS 				= PACKAGE.NAME
	    THIS.METHOD.SMS					= METHOD.NAME
	    CALLJ.ARGUMENTS.SMS 			= ARR.SMS
		CALLJ.ERROR.SMS 				= " "
		CALLJ.RESPONSE.SMS 				= " "	
		CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.SMS,CALLJ.ARGUMENTS.SMS,CALLJ.RESPONSE.SMS,CALLJ.ERROR.SMS)
		IF CALLJ.ERROR.SMS EQ '0' THEN
			 ETEXT = "No se puede enviar el ARR.SMS":CALLJ.RESPONSE.SMS:"EMAILNOLIOF:": CALLJ.ARGUMENTS.SMS
		END
		
		TEXTO.ARCHIVO='CALLJ.ARGUMENTS : ': CALLJ.ARGUMENTS.SMS
		GOSUB ESCRIBIR.ARCHIVO
		TEXTO.ARCHIVO='CALLJ.RESPONSE.SMS : ': CALLJ.RESPONSE.SMS
		GOSUB ESCRIBIR.ARCHIVO
		TEXTO.ARCHIVO='CALLJ.ERROR.SMS : ': CALLJ.ERROR.SMS
		GOSUB ESCRIBIR.ARCHIVO
		
		
	RETURN
	
	
	
	ESCRIBIR.ARCHIVO:
    DIR.NAME= 'MonedaAzul1'
    R.ID   = 'Moneda_Azul ':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN
    END

	
	
END
