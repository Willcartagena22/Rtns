*-----------------------------------------------------------------------------
* <Rating>-11</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.I.LOAD.COLLECTOR(ENQ.DATA)
*-----------------------------------------------------------------------------
*
* Descripcion: RTN que obtiene un listado del catalogo de colectores almacenados en SQLSERVER
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*-----------------------------------------------------------------------------


;*LLAMADA AL CALLJ PARA OBTENER CUENTAS DEL COLECTOR SELECCIONADO
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector"
    THIS.METHOD.CLT= "getCollectors"
    CALLJ.ARGUMENTS.CLT = "Este parametro solo es para que detecte la funcion"
    CALLJ.ERROR.SMS = ""
    CALLJ.RESPONSE.CLT = ""
;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    STRING.RESPUESTA = CALLJ.RESPONSE.CLT
    TAMANIO = DCOUNT(STRING.RESPUESTA,'|') -2
    CRT TAMANIO
    SEGMENTACION = FIELD(STRING.RESPUESTA,'|',2)
    IF SEGMENTACION EQ 'OK' THEN
        FOR J = 3 TO TAMANIO
            SEGMENTACION = FIELD(STRING.RESPUESTA,'|',J)
            STR.ARR<-1> = SEGMENTACION
        NEXT J
        ENQ.DATA = STR.ARR
    END
    ELSE
    STRERR = STRING.RESPUESTA
    GOSUB CRT_ERROR
    END

CRT_ERROR:
    ETEXT = STRERR
    AS = 1
    CALL STORE.END.ERROR
    RETURN

    END
