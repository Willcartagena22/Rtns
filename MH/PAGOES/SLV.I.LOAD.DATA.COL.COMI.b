*-----------------------------------------------------------------------------
* <Rating>-59</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.I.LOAD.DATA.COL.COMI
*-----------------------------------------------------------------------------
* Descripcion: RTN de carga de data(# Cta. Retencion,# Cta.Comision, Saldo de comision)
*              del colector en el AC.CHARGE REQUEST
*
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AC.CHARGE.REQUEST
    $INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------
        GOSUB INICIALIZAR
        GOSUB OPENFILE
        GOSUB PROCESS

INICIALIZAR:
    FN.AC.CHARGE = 'F.AC.CHARGE.REQUEST'
    F.AC.CHARGE = ''
    RETURN

OPENFILE:
    CALL OPF(FN.AC.CHARGE,F.AC.CHARGE)
    RETURN
PROCESS:
   ;*CALL GET.LOC.REF ('AC.CHARGE.REQUEST', 'LF.COD.CL', POS)
    ;*LS.PERSONERIA = R.CUST<CHG.LOCAL.REF, POS>
    COLECTOR  = COMI;*'01-MH|'R.NEW(CHG.LOCAL.REF,POS)
;*LLAMADA AL CALLJ PARA OBTENER CUENTAS DEL COLECTOR SELECCIONADO
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector"
    THIS.METHOD.CLT= "getAccounts"
    CALLJ.ARGUMENTS.CLT = COLECTOR
    CALLJ.ERROR.SMS = ""
    CALLJ.RESPONSE.CLT = " "
;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    STRING.RESPUESTA = CALLJ.RESPONSE.CLT
    SEGMENTACION = FIELD(STRING.RESPUESTA,'|',2)
    IF SEGMENTACION EQ 'OK' THEN
        ;*CALCULO DE RETENCION DEL 1% AL VALOR SIN IVA

        ;*              SEGMENTO.COBRAR = FIELD(STRING.RESPUESTA,'|',3)
        ;*             CC.COMISION = FIELD(SEGMENTO.COBRAR,'-',2)

        SEGMENTACION.CC.COBRAR = FIELD(STRING.RESPUESTA,'|',4)
        CC.COBRAR.COMISON = FIELD(SEGMENTACION.CC.COBRAR,'-',2)


        SEGMENTACION.RENTECION = FIELD(STRING.RESPUESTA,'|',3)
        CC.RETENCION = FIELD(SEGMENTACION.RENTECION,'-',2)


        CARGO.TOTAL = DROUND(FIELD(STRING.RESPUESTA,'|',5),2)

        ;*R.NEW(CHG.DEBIT.ACCOUNT) = CC.COBRAR.COMISON
        CALL GET.LOC.REF ('AC.CHARGE.REQUEST', 'LF.ACC.DESC', POS)
        R.NEW(CHG.LOCAL.REF)<1,POS> = CC.RETENCION
        IF OFS$OPERATION EQ 'VALIDATE' THEN
        R.NEW(CHG.CHARGE.AMOUNT) = CARGO.TOTAL
        END

        RETURN
    END
    RETURN
ESCRIBIR.ARCHIVO:
    DIR.NAME= 'MHLogs'
    R.ID   = 'OFS$OPERATION':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN

CRT_ERROR:
    ETEXT = STRERR
    AS = 1
    CALL STORE.END.ERROR
    RETURN
    END
