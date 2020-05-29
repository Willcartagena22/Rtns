*-----------------------------------------------------------------------------
* <Rating>-12</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.ENVIO.MENSAJES.HDM(TELEFONO,MENSAJE)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------

PROCESS:
    
    ;* idTransaccion~nombreAplicacion~idColector~idCanal~idServicio~idAgencia~tipoPlantilla~idOperacion~RESTO DE DATOS PROPIOS DEL SERVICIO
	PARAMETROS=TELEFONO:"~":MENSAJE
	Y.PARAMETRO.ARGUMENT = ID.TRANSACTION:'~':'MENSAJES':'~':'07':'~':'OTRO':'~':'MENSAJERIA':'~':'null':'~':'XML':'~':'2':'~':PARAMETROS
	
	CRT 'ENVIO >>> ':Y.PARAMETRO.ARGUMENT
	
	THIS.PACKAGE.CLASS 	= "com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT		= "setConnect"
    CALLJ.ARGUMENTS.CLT = Y.PARAMETRO.ARGUMENT
    CALLJ.ERROR.SMS 	= " "
    CALLJ.RESPONSE.CLT 	= " "
	;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA=CALLJ.RESPONSE.CLT

RETURN

