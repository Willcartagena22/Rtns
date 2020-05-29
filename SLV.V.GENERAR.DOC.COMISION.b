*-----------------------------------------------------------------------------
* <Rating>-66</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.GENERAR.DOC.COMISION
*-----------------------------------------------------------------------------
* 
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AC.CHARGE.REQUEST
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
    EQU POSICION.MONTO.COMISION  TO 1
    EQU POSICION.TOTAL.PAGOS     TO 2
    EQU POSICION.MONTO.IVA       TO 3
    EQU POSICION.MONTO.PAGOS     TO 4
    EQU POSICION.MONTO.IVA.PAGOS TO 5
    EQU POSICION.MONTO.NPE       TO 6
    EQU POSICION.FECHA.GENERICA  TO 7
    EQU POSICION.PERIODO.PAGO    TO 8
    RETURN

PROCESS:
*   CALL GET.LOC.REF ('AC.CHARGE.REQUEST', 'LF.COD.CL', POS)
*   COLECTOR = R.NEW(CHG.LOCAL.REF)<1,POS>

    COLECTOR = '02'

    IF COLECTOR EQ '02' THEN
        GOSUB CREATE.DOC.COMMISSION
    END

    TEXTO.ARCHIVO = 'COLECTOR > ':COLECTOR
    GOSUB ESCRIBIR.ARCHIVO

    RETURN

CREATE.DOC.COMMISSION:
    GOSUB GET.INFORMATION.COLECTOR
    LST.VALUES = CHANGE(RESPUESTA.SERVICIO.EXTERNO,'|',FM)

    MONTO.COMISION  = FMT(LST.VALUES<POSICION.MONTO.COMISION>,"R2")
    CANTIDAD.PAGOS  = LST.VALUES<POSICION.TOTAL.PAGOS>
    MONTO.IVA       = FMT(LST.VALUES<POSICION.MONTO.IVA>,"R2")
    MONTO.PAGOS     = FMT(LST.VALUES<POSICION.MONTO.PAGOS>,"R2")
    MONTO.TOTAL.IVA = FMT(LST.VALUES<POSICION.MONTO.IVA.PAGOS>,"R2")
    MONTO.NPE       = FMT(LST.VALUES<POSICION.MONTO.NPE>,"R2")
    MONTO.TOTAL.ARC = MONTO.NPE
    FECHA.GENERICA  = LST.VALUES<POSICION.FECHA.GENERICA>
    PERIODO.PAGO    = LST.VALUES<POSICION.PERIODO.PAGO>
    ;*INVOCAMOS LA RUTINA PARA CONVERTIR EL MONTO TOTAL A LETRAS
    CALL SLV.UTIL.IMP.GET.NUMLETRAS(MONTO.NPE)
    MONTO.LETRAS = MONTO.NPE    
    
    GOSUB GENERATE.SPOOL
    
    RETURN

GENERATE.SPOOL:
    FECHA.COMPROBANTE = OCONV(ICONV(TODAY, "D"),"D4/E"):' ':OCONV(TIME(), "MTS")
    stringData  = ''
    stringData := 'FechaGeneracion=':FECHA.GENERICA:';'
    stringData := 'MesGeneracion=':PERIODO.PAGO:';'
    stringData := 'MontoComision=$':MONTO.COMISION:';'
    stringData := 'TotalPlanilla=':CANTIDAD.PAGOS:';'
    stringData := 'Monto1=$':MONTO.PAGOS:';'
    stringData := 'MontoIVA=$':MONTO.TOTAL.IVA:';'
    stringData := 'Monto2=$':MONTO.TOTAL.ARC:';'
    stringData := 'MontoTotal=$':MONTO.TOTAL.ARC:';'
    stringData := 'MontoLetras=':MONTO.LETRAS:';'
    
    idArchivo    = 'CARTACOMISION-':TODAY:'.txt'
*    direcArchivo = 'C:\APDF\spool\'
    direcArchivo = 'SPOOL.FILES'
    OPENSEQ direcArchivo, idArchivo TO SEQ.PTR ELSE
        CREATE SEQ.PTR ELSE
        CRT 'No se puede crear el archivo.'
    END

    WRITESEQ stringData ON SEQ.PTR ELSE
    CRT 'No se pueden escribir los datos en el archivo.'

    END

    RETURN

GET.INFORMATION.COLECTOR:
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT= "getInformationDocCommission"
    CALLJ.ARGUMENTS.CLT = COLECTOR
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA.SERVICIO.EXTERNO = CHANGE(CALLJ.RESPONSE.CLT,'"','')
    RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'COLECTORES'
    R.ID   = 'SLV_V_GENERAR_DOC_COMISION_':TODAY:'.txt'
;* hacer que escriba un archivo
    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN

    END
