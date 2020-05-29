*-----------------------------------------------------------------------------
* <Rating>-11</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE PRUEBA.NPE.VALIDACION
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------

    GOSUB CALL.J
    
    
CALL.J:
;*ASIGNACION DE PARAMETROS PARA EL CALLJ
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";
    THIS.METHOD.CLT= "getCollectorName"
    CALLJ.ARGUMENTS.CLT = "01"
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
    
    
   CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
CRT 'RESPUESTA':CALLJ.RESPONSE
    
    


END
