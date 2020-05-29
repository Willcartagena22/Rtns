*-----------------------------------------------------------------------------
* <Rating>-6</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE BASIC.CINTEX
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
	$INSERT I_COMMON
	$INSERT I_EQUATE
*-----------------------------------------------------------------------------
	
	;* Primero hay que obtener el Id de la transacción para CINTEX
	
	;* idColector~idCanal
	Y.PARAMETRO.ARGUMENT = '06':'~':'VENTANILLA'
	THIS.PACKAGE.CLASS 	= "com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT		= "getID"
    CALLJ.ARGUMENTS.CLT = Y.PARAMETRO.ARGUMENT
    CALLJ.ERROR.SMS 	= " "
    CALLJ.RESPONSE.CLT 	= " "
	;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    
    ;* Asignar el Id recuperado
    ID.TRANSACTION = FIELD(CALLJ.RESPONSE.CLT,'|',3)
    
    ;* Ahora Enviar la petición hacia CINTEX
    
    ;* idTransaccion~nombreAplicacion~idColector~idCanal~idServicio~idAgencia~tipoPlantilla~idOperacion~RESTO DE DATOS PROPIOS DEL SERVICIO
	Y.PARAMETRO.ARGUMENT = ID.TRANSACTION:'~':'REMESAS APP':'~':'06':'~':'VENTANILLA':'~':'REMESAS':'~':'SV0010001':'~':'XML':'~':'1':'~':'test':'~':'VPN':'~':'BANCOAZUL00001':'~':'287600000015':'~':'~':'~':'~':'~':'BA6500'
	
	CRT 'ENVIO >>> ':Y.PARAMETRO.ARGUMENT
	
	THIS.PACKAGE.CLASS 	= "com.bancoazul.collector.Collector"
    THIS.METHOD.CLT		= "setConnect"
    CALLJ.ARGUMENTS.CLT = Y.PARAMETRO.ARGUMENT
    CALLJ.ERROR.SMS 	= " "
    CALLJ.RESPONSE.CLT 	= " "
	;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    
    CRT 'RESPUESTA CALLJ >>> ':CALLJ.RESPONSE.CLT
    
END
