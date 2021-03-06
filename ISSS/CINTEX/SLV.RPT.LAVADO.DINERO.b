*-----------------------------------------------------------------------------
* <Rating>221</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.RPT.LAVADO.DINERO
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.EB.EXTERNAL.USER
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
    FN.APP = 'F.CUSTOMER'
    F.APP  = ''
    
    FN.APP.EXTERNAL.USER = 'F.EB.EXTERNAL.USER'
    F.APP.EXTERNAL.USER  = ''
    
    EQU CODIGO.COLECTOR TO '02'
    RETURN

OPEN.FILE:
    CALL OPF(FN.APP,F.APP)
    CALL OPF(FN.APP.EXTERNAL.USER,F.APP.EXTERNAL.USER)
    RETURN

PROCESS:
    GOSUB GET.INFORMATION.DBCOLLECTORS
    
    IF FLAG.REPORTE EQ 'T' THEN
    
    LST.VALUES = CHANGE(RESPUESTA.SERVICIO,'|',FM)
    RECORD.COUNT.VALUES = DCOUNT(LST.VALUES,FM)

    FOR I=2 TO RECORD.COUNT.VALUES
        SUBVALUES = LST.VALUES<I>
        LST.SUBVALUES = CHANGE(SUBVALUES,'-',FM)

        ID.USUARIO = LST.SUBVALUES<1>
        ID.FT      = LST.SUBVALUES<2>
        MONTO      = FMT(LST.SUBVALUES<3>,"R2")
        
        ;*OBTENEMOS EL CODIGO DE CLIENTE MEDIANTE LA APLICACION EB.EXTERNAL.USER
        CALL F.READ(FN.APP.EXTERNAL.USER,ID.USUARIO,A.EXT,F.APP.EXTERNAL.USER,ERR.APP.EXTERNAL.USR)
        
        CUSTOMER.CODE = A.EXT<EB.XU.CUSTOMER>
        
        ;*CONSULTAMOS LA INFORMACION DEL CLIENTE QUE OBTENEMOS DE CADA
        CALL F.READ(FN.APP,CUSTOMER.CODE,RESPUESTA,F.APP,F.ERR)

        CALL GET.LOC.REF("CUSTOMER","SEGMENT",SEG.POS)
        CALL GET.LOC.REF("CUSTOMER","LF.RAZON.SOCIAL",RAZ.POS)
        CALL GET.LOC.REF("CUSTOMER","LF.NIT",NIT.POS)

        CUSTOMER.TYPE = RESPUESTA<EB.CUS.LOCAL.REF, SEG.POS>
        
        IF CUSTOMER.TYPE EQ "1" THEN
            RAZON.SOCIAL = RESPUESTA<EB.CUS.NAME.1>:" ":RESPUESTA<EB.CUS.NAME.2>:" ":RESPUESTA<EB.CUS.TEXT>:" ":RESPUESTA<EB.CUS.FAMILY.NAME>
        END
        ELSE
        RAZON.SOCIAL = RESPUESTA<EB.CUS.LOCAL.REF,RAZ.POS>
        END
        
        NIT = RESPUESTA<EB.CUS.LOCAL.REF,NIT.POS>
        
        CARACTERES.RAZON.SOCIAL = LEN(RAZON.SOCIAL)
        CARACTERES.MONTO = LEN(CHANGE(MONTO,'.',''))
        
        IF CARACTERES.RAZON.SOCIAL < 40 THEN
           CARACTERES.FALTANTES.RS = 40 - CARACTERES.RAZON.SOCIAL
           RAZON.SOCIAL.FILE = RAZON.SOCIAL:SPACES(CARACTERES.FALTANTES.RS)
        END
        ELSE
           RAZON.SOCIAL.FILE = SUBSTRINGS(RAZON.SOCIAL,0,40)
        END

        IF CARACTERES.MONTO < 12 THEN
           CARACTERES.FALTANTES.MN = 12 - CARACTERES.MONTO
           FORMATO.MONTO = 'R%12'
           MONTO.FILE = FMT(CHANGE(MONTO,'.',''),FORMATO.MONTO)
        END
        
        CRT 'RAZON SOCIAL >>> ':RAZON.SOCIAL.FILE
        CRT 'MONTO >>>> ':MONTO.FILE
        
        TEXTO.ARCHIVO = RAZON.SOCIAL.FILE:NIT:MONTO.FILE:SPACES(12):SPACES(12):'WEB':SPACES(9)
        GOSUB ESCRIBIR.ARCHIVO
        
    NEXT I
    END
    ELSE
        TEXTO.ARCHIVO = ''
        GOSUB ESCRIBIR.ARCHIVO
    END
    
        CRT 'ARCHIVO GENERADO EXISTOSAMENTE'
    RETURN


GET.INFORMATION.DBCOLLECTORS:
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT= "getInformationLavadoDineroFile"
    CALLJ.ARGUMENTS.CLT = CODIGO.COLECTOR
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA.SERVICIO = CHANGE(CALLJ.RESPONSE.CLT,'"','')
    CRT 'RESPUESTA SERVICIO................................'
    CRT RESPUESTA.SERVICIO
    
    IF LEFT(RESPUESTA.SERVICIO,2) NE 'OK' THEN
       FLAG.REPORTE = 'T'
    END
    ELSE
       FLAG.REPORTE = 'F'
    END
    
    RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'AFPCRECER'
    R.ID   = 'L':TODAY:'.txt'
;* hacer que escriba un archivo
    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN

    END
