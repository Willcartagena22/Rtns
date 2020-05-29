*-----------------------------------------------------------------------------
* <Rating>-55</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.ID.COL.FRONT.END
*-----------------------------------------------------------------------------
*Descripcion RTN que obtiene el id para para appLocal
*
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.COL.FRONT.END
    $INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------
;*validar si la entrada es VALIDATE O PROCESS=COMI
    GOSUB INICIALIZAR
    GOSUB OPENFILE
    GOSUB PROCESS


INICIALIZAR:

    FN.COLECTOR	= 'F.EB.SLV.COL.FRONT.END'
    F.COLECTOR	= ''

    RETURN

OPENFILE:
    CALL OPF(FN.COLECTOR, F.COLECTOR)
    RETURN

PROCESS:
;*DEFINICIO DE PARAMETROS PARA OBTENER EL ID DE VERSION
   
    ID.APPLOCAL = COMI
    COLECTOR = FIELD(ID.APPLOCAL,'-',1)
    CHANNEL = FIELD(ID.APPLOCAL,'-',2)
    CRT COLECTOR
    CRT CHANNEL
    CALL F.READ(FN.COLECTOR,ID.APPLOCAL,R.COLECTOR,F.COLECTOR,Y.ERR)
    EXISTE.ID = COUNT(R.COLECTOR,1)
    IF EXISTE.ID EQ '0' THEN
        GOSUB ASIGNAR.ID
    END
    RETURN

ASIGNAR.ID:
;*ASIGNACION DE PARAMETROS PARA EL CALLJ
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT= "getID"
    CALLJ.ARGUMENTS.CLT = COLECTOR:'~':CHANNEL
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "

;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    
;*ASIGNANDO EL NUEVO ID  APPLOCAL
    OUTPUT.VALUE = CALLJ.RESPONSE.CLT
    SEGMENTO.ESTADO = FIELD(OUTPUT.VALUE,'|',2)
    IF SEGMENTO.ESTADO EQ 'OK' THEN
        COMI = FIELD(OUTPUT.VALUE,'|',3)
    END
    ELSE
    E = 'EB-COLLECTOR.ID.VERSION'
    CALL ERR
    END
    RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'MHLogs'
    R.ID   = 'ID.COL.FRONT.END':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN

    END
