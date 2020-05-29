*-----------------------------------------------------------------------------
* <Rating>-32</Rating>
*-----------------------------------------------------------------------------
 SUBROUTINE PRUEBA.MONEDA.AZUL.CALLJ
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*-----------------------------------------------------------------------------



GOSUB INIT
GOSUB PROCESS.SEND.NOTIFICACION

INIT:
    PACKAGE.NAME 		= "t24broadcasterutilnotifications.T24BroadCasterUtilNotifications"
    THIS.METHOD.NAME 	= ""
    METHOD.NAME			= "notificationHalCash"
    RETURN


CALL JF.INITIALISE.CONNECTION


PROCESS.SEND.NOTIFICACION:
		;*Envio de SMS a Beneficiario de Transferencia
		ARR.SMS = ''
		ARR.SMS	:= '1719651651'						: "%%"
		ARR.SMS	:= 'USD'						: "%%"
		ARR.SMS	:= '$'							: "%%"
		ARR.SMS	:= 1	: "%%"
		ARR.SMS	:= '71055581'		: "%%"
		ARR.SMS	:= 'calvarado@bancoazul.com'		: "%%"
		ARR.SMS	:= '02/10/2018'	    


    THIS.PACKAGE.CLASS 				= PACKAGE.NAME
    THIS.METHOD.SMS					= METHOD.NAME
    CALLJ.ARGUMENTS.SMS 			= ARR.SMS
    CALLJ.ERROR.SMS 				= " "
    CALLJ.RESPONSE.SMS 				= " "
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.SMS,CALLJ.ARGUMENTS.SMS,CALLJ.RESPONSE.SMS,CALLJ.ERROR.SMS)
    IF CALLJ.ERROR.SMS EQ '0' AND CALLJ.ARGUMENTS.NOLIOF[1,3]!='(-1'THEN
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
