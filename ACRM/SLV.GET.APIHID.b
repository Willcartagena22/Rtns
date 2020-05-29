*----------------------------------------------------------------------------
* <Rating>-136</Rating>
*---------------------------------------------------------------------------
SUBROUTINE SLV.GET.APIHID(METOD, PARAM, OUT.DATA)
*-----------------------------------------------------------------------------
*
*----------------------------------------------------------------------------
* Modification History :
*----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE


*---------------------------------------------------------------------------

Y.METODO = METOD
Y.IN.PARAMETER = PARAM
Y.PAQUETE =  'com.mad.banco.azul.hid.t24.T24HID'
 
BEGIN CASE
	
	CASE Y.METODO = 'requestOTPUsingSMS_V2'
    GOSUB requestOTPUsingSMS_V2

	CASE Y.METODO = 'createSoftToken'
    GOSUB createSoftToken
    
    CASE Y.METODO = 'buscarTokenAsignado'
    GOSUB buscarTokenAsignado

    CASE Y.METODO = 'registrarSoftToken'
    GOSUB registrarSoftToken
    
    CASE Y.METODO = 'updateBloqueoCanal'
	GOSUB updateBloqueoCanal
	
	CASE Y.METODO = 'asignarHardToken'
	GOSUB asignarHardToken
	
	CASE Y.METODO = 'blockedChannelSms'
	GOSUB blockedChannelSms
	
	CASE Y.METODO = 'blockedChannelSoftToken'
	GOSUB blockedChannelSoftToken
	
	CASE Y.METODO = 'blockedChannelHardToken'
	GOSUB blockedChannelSoftToken
	
	CASE Y.METODO = 'deleteToken'
	GOSUB deleteToken
	
	CASE Y.METODO = 'unregisterUserForOutOfBand'
	GOSUB unregisterUserForOutOfBand 
	
	CASE Y.METODO = 'updatePhone'
	GOSUB updatePhone 
	
	CASE Y.METODO = 'borrarUsuario'
	GOSUB borrarUsuario 
	
	
END CASE

RETURN

requestOTPUsingSMS_V2:	
*	Y.PAQUETE 			= "com.mad.banco.azul.hid.t24.T24HID"
*	Y.METODO  			= "requestOTPUsingSMS_V2"
*  	Y.IN.PARAMETER 		=  Y.USER.ID
   	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
;*LLamada createSoftToken en HIDBancoAzulAPI
   	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
*  	OUT.DATA = Y.OUT.PARAMETER
   	IF Y.ERROR.RESPONSE THEN
	  	E = "Error al Solicitar SoftToken - no se pudo invocar libreria: ":Y.ERROR.RESPONSE
       	CALL ERR
    END
    ELSE
    	OUT.DATA = Y.OUT.PARAMETER
*   	R.NEW(EB.EXDEV.CODE.ACTIVATION) = Y.OUT.PARAMETER
    END		
RETURN

createSoftToken:
	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
	
	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
    
    IF Y.ERROR.RESPONSE THEN
*		CALL SLV.VERIFICACION.TOKEN
        E = "Error al Solicitar SoftToken - no se pudo invocar libreria: ":Y.ERROR.RESPONSE
        CALL ERR
    END
    ELSE
    	OUT.DATA = Y.OUT.PARAMETER
*    	R.NEW(EB.EXDEV.CODE.ACTIVATION) = Y.OUT.PARAMETER
   	END		
RETURN

buscarTokenAsignado:
	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
	
	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
    
    IF Y.ERROR.RESPONSE THEN
*		CALL SLV.VERIFICACION.TOKEN
        E = "Error al Solicitar SoftToken - no se pudo invocar libreria: ":Y.ERROR.RESPONSE
        CALL ERR
    END
    ELSE
    	OUT.DATA = Y.OUT.PARAMETER
*    	R.NEW(EB.EXDEV.CODE.ACTIVATION) = Y.OUT.PARAMETER
   	END		


RETURN

registrarSoftToken: 
	
	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
	IF Y.ERROR.RESPONSE THEN
		E = "Error al Registrar SoftToken - no se pudo invocar libreria: ":Y.ERROR.RESPONSE
	    CALL ERR
	END
	ELSE
		OUT.DATA = Y.OUT.PARAMETER
	END

RETURN

updateBloqueoCanal:
	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
	IF Y.ERROR.RESPONSE THEN
		E = "Error al Asignar Dispositivo- ":Y.ERROR.RESPONSE
	    CALL ERR
	END
	ELSE
		OUT.DATA = Y.OUT.PARAMETER
	END

RETURN

asignarHardToken:
	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
	IF Y.ERROR.RESPONSE THEN
		E = "Error al Asignar HardToken - no se pudo invocar libreria: ":Y.ERROR.RESPONSE
        CALL ERR
	END
	ELSE
		OUT.DATA = Y.OUT.PARAMETER
	END

RETURN

blockedChannelSms:
	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
	IF Y.ERROR.RESPONSE THEN
		E = "Error al aplicar bloqueo de canal para SMS Token- ":Y.ERROR.RESPONSE
	    CALL ERR
	END
	ELSE
		OUT.DATA = Y.OUT.PARAMETER
	END

RETURN

blockedChannelSoftToken:
	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
	IF Y.ERROR.RESPONSE THEN
		E = "Error al aplicar bloqueo de canal para SMS Token- ":Y.ERROR.RESPONSE
	    CALL ERR
	END
	ELSE
		OUT.DATA = Y.OUT.PARAMETER
	END

RETURN

blockedChannelHardToken:
	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
	IF Y.ERROR.RESPONSE THEN
		E = "Error al aplicar bloqueo de canal para SMS Token- ":Y.ERROR.RESPONSE
	    CALL ERR
	END
	ELSE
		OUT.DATA = Y.OUT.PARAMETER
	END

RETURN

deleteToken:
	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
	IF Y.ERROR.RESPONSE THEN
		E = "Error al intentar eliminar el dispositivo - ":Y.ERROR.RESPONSE
	    CALL ERR
	END
	ELSE
		OUT.DATA = Y.OUT.PARAMETER
	END

RETURN

unregisterUserForOutOfBand:
	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
	IF Y.ERROR.RESPONSE THEN
		E = "Error al intentar eliminar la autenticacion SMS Token - ":Y.ERROR.RESPONSE
	    CALL ERR
	END
	ELSE
		OUT.DATA = Y.OUT.PARAMETER
	END

RETURN

updatePhone:
	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
	IF Y.ERROR.RESPONSE THEN
		E = "Error al intentar actualizar el telefono del cliente- ":Y.ERROR.RESPONSE
	    CALL ERR
	END
	ELSE
		OUT.DATA = Y.OUT.PARAMETER
	END

RETURN

borrarUsuario:
	Y.OUT.PARAMETER		= ''
   	Y.ERROR.RESPONSE 	= ''
	CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
	IF Y.ERROR.RESPONSE THEN
		E = "Error al intentar eliminar el usr - ":Y.ERROR.RESPONSE
	    CALL ERR
	END
	ELSE
		OUT.DATA = Y.OUT.PARAMETER
	END

RETURN

END
