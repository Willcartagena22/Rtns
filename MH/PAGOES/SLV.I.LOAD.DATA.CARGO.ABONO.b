*-----------------------------------------------------------------------------
* <Rating>-48</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.I.LOAD.DATA.CARGO.ABONO
*-----------------------------------------------------------------------------
* Descripcion: RTN de carga de data info del colector para realizar la Nota de Cargo
*              del colector en el FUNDS.TRANSFER
*
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.ACCOUNT
    $INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------
    IF OFS$OPERATION EQ 'VALIDATE' THEN
        GOSUB INICIALIZAR
        GOSUB OPENFILE
        GOSUB PROCESS
    END

INICIALIZAR:
    FN.FUNDS.TRANFERS = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANFERS = ''

    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    RETURN

OPENFILE:
    CALL OPF(FN.FUNDS.TRANFERS,F.FUNDS.TRANFERS)
    RETURN
PROCESS:
    COLECTOR = R.NEW(FT.PAYMENT.DETAILS)
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

        SEGMENTACION.CC.COBRAR = FIELD(STRING.RESPUESTA,'|',6)
        CC.RECAUDADORA = FIELD(SEGMENTACION.CC.COBRAR,'-',2)

        GOSUB SALDO.CUENTA

        ;*R.NEW(FT.DEBIT.ACCT.NO) = CC.RECAUDADORA

        R.NEW(FT.DEBIT.AMOUNT) = WORKING.BALANCE

        RETURN
    END
*    ELSE
*    STRERR = STRING.RESPUESTA
*    GOSUB CRT_ERROR
*    END
    RETURN

SALDO.CUENTA:
    CALL F.READ(FN.ACC, CC.RECAUDADORA,RECORD.ACC,F.ACC,ERR.ACC)
    WORKING.BALANCE = RECORD.ACC<AC.WORKING.BALANCE>
    RETURN

*CRT_ERROR:
*    ETEXT = STRERR
*    AS = 1
*    CALL STORE.END.ERROR
*    RETURN
    END
