*-----------------------------------------------------------------------------
* <Rating>-12</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.TSA.ARCHIVOS.ISSS
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------


GOSUB PROCESS
RETURN




PROCESS:
;*Generacion de Archivos ISSS
THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector"
THIS.METHOD.CLT= "setExecuteProcedure"
CALLJ.ARGUMENTS.CLT = "ArchivosISSS_Manual"
CALLJ.ERROR.SMS = " "
CALLJ.RESPONSE.CLT = " "
;*LLAMADA AL METODO CALLJ
CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
RESPUESTA.SERVICIO.LISTADO.COLECTORES = CHANGE(CALLJ.RESPONSE.CLT,'"','') 

RETURN



END
