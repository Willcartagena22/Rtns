*-----------------------------------------------------------------------------
* <Rating>-64</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.CONT.TELLER.SIMP
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $INSERT I_F.EB.SLV.CTA.SIMPLIFICADA.TXN
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
    $INSERT I_F.AA.ACCOUNT
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.EB.SLV.CUENTA.SIMP.TRX.DIA
*-----------------------------------------------------------------------------

    GOSUB INICIAR
    GOSUB ABRIR
    GOSUB VALIDAR.ACUMULADO.DIA
    GOSUB CAT.SIMP
    GOSUB PROCESS


INICIAR:
    FN.TELLER = 'F.TELLER'
    F.TELLER  = ''
    FN.ACC 	= 'F.ACCOUNT'
    F.ACC  	= ''
    DTL.APP.FN= 'F.EB.SLV.CTA.SIMPLIFICADA.TXN'
    DTL.APP.F= ''
    FN.TABLE.PA = 'F.EB.SLV.GLOBAL.PARAM'
    F.TABLE.PA = ''
    FN.TRX.DIA= 'F.EB.SLV.CUENTA.SIMP.TRX.DIA'
    F.TRX.DIA = ''
    CATEGORY.SIMP=''

    RETURN

ABRIR:
    CALL OPF(FN.TELLER, F.TELLER)
    CALL OPF(FN.ACC, F.ACC)
    CALL OPF(DTL.APP.FN,DTL.APP.F)
    CALL OPF(FN.TABLE.PA, F.TABLE.PA)
    CALL OPF(FN.TRX.DIA,F.TRX.DIA)
    RETURN



PROCESS:
    ERROR='NO'
    V_NAME_FIELD=TFS.AMOUNT
    CUENTA1 = R.NEW(TT.TE.ACCOUNT.1)
    CUENTA2 = R.NEW(TT.TE.ACCOUNT.2)
    
    MONTO	= R.NEW(TT.TE.NET.AMOUNT)
    YEAR_MONTH         = TODAY[1,4]:TODAY[5,2]
*    CUENTA1='10000000824297'
*    MONTO=300

;*-------------------------VALIDAR SI EL MONTO ES MAYOR AL L�MITE DIARIO.----------------------------
    LIMITE.DIARIO=''
    PARAM.DIA = 'CTA.SIMPLIFICADA.MONTO.TRX'
    CALL F.READ(FN.TABLE.PA, PARAM.DIA, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    LIMITE.DIARIO=R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    CUENTA=CUENTA1
    GOSUB VALIDAR.SIMP
    CATEGORY=''
    CUENTA=CUENTA2
    GOSUB VALIDAR.SIMP

    RETURN

VALIDAR.SIMP:

    CALL F.READ(FN.ACC, CUENTA, R.ACCOUNT, F.ACC, Y.ERR.AC)
    CATEGORY=R.ACCOUNT<AC.CATEGORY>
    CAT.S=CATEGORY.SIMP
    IF CATEGORY EQ CAT.S THEN
        IF MONTO GT LIMITE.DIARIO THEN
            STRERR ='EB-SIMP001'
            GOSUB CRT_ERROR
        END
        ACC.SIMP.DIA=CUENTA
        GOSUB VALIDAR.MONTO.DIA
        GOSUB VALIDAR.SALDO
        GOSUB VALIDAR.ACUMULADO
        IF ERROR EQ 'NO' THEN
            COUNT.ADD      = R.APP<EB.SLV25.CONTADOR.TRX>+1
            NEW.TRX.DETAIL = FIELDS(DETAIL.FOUND,'~',1):'~':ACUMULADO.ACTUAL
            R.APP<EB.SLV25.CONTADOR.TRX>      = COUNT.ADD
            R.APP<EB.SLV25.TRX.DETALLE,V.VAL> = NEW.TRX.DETAIL
            TEXTO.ARCHIVO='WRITE R.APP :':R.APP
            DETALLE=R.APP<EB.SLV25.TRX.DETALLE>

            IF DETALLE THEN
                IF DETALLE EQ '~' THEN
                    DETALLE=YEAR_MONTH:'~':MONTO
                    R.APP<EB.SLV25.TRX.DETALLE>=DETALLE
                END

                CALL F.WRITE(DTL.APP.FN,CUENTA, R.APP)
                CALL JOURNAL.UPDATE(DTL.APP.FN)
            END

            ELSE
            DETALLE=YEAR_MONTH:'~':MONTO
            R.APP<EB.SLV25.TRX.DETALLE>=DETALLE

            CALL F.WRITE(DTL.APP.FN,CUENTA, R.APP)
            CALL JOURNAL.UPDATE(DTL.APP.FN)
        END

    END
    END
    RETURN


VALIDAR.ACUMULADO:
    IF ERROR EQ 'NO' THEN

        PARAM.DIA = 'CTA.SIMPLIFICADA.MONTO.MONTH'
        CALL F.READ(FN.TABLE.PA, PARAM.DIA, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
        ACUMULADO.TOPE=R.TABLE.PA<EB.SLV39.VALOR.PARAM>
        CALL F.READ(DTL.APP.FN, CUENTA, R.APP, DTL.APP.F, F.ERR.APP)
        TRX.ARR = R.APP<EB.SLV25.TRX.DETALLE>
        FINDSTR YEAR_MONTH IN TRX.ARR SETTING V.FLD, V.VAL THEN
            DETAIL.FOUND   = TRX.ARR<1,V.VAL>
            ACUMULADO.ACTUAL = FIELDS(DETAIL.FOUND,'~',2)
            ACUMULADO.ACTUAL=ACUMULADO.ACTUAL+MONTO
        END
        IF ACUMULADO.ACTUAL GE ACUMULADO.TOPE THEN
            STRERR ='EB-SIMP002'
            GOSUB CRT_ERROR

        END
    END

    RETURN

VALIDAR.ACUMULADO.DIA:

        PARAM.DIA = 'CTA.SIMPLIFICADA.MONTO.DIA'
        CALL F.READ(FN.TABLE.PA, PARAM.DIA, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
        ACUMULADO.DIA=R.TABLE.PA<EB.SLV39.VALOR.PARAM>


    RETURN

VALIDAR.SALDO:
    IF ERROR EQ 'NO' THEN
        PARAM.DIA = 'CTA.SIMPLIFICADA.SALDO.VALIDO'
        CALL F.READ(FN.TABLE.PA, PARAM.DIA, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
        SALDO.VALIDO=R.TABLE.PA<EB.SLV39.VALOR.PARAM>

        CALL F.READ(FN.ACC, CUENTA, REC.ACC.CURR, F.ACC, ERR.ACC.CURR)
        SALDO.CUENTA= REC.ACC.CURR<AC.ONLINE.ACTUAL.BAL>
        SALDO.CUENTA=SALDO.CUENTA+MONTO

        IF SALDO.CUENTA GT SALDO.VALIDO THEN
            STRERR ='EB-SIMP003'
            GOSUB CRT_ERROR

        END
    END

    RETURN

VALIDAR.MONTO.DIA:

    IDCUENTA=ACC.SIMP.DIA:'~':TODAY
    CALL F.READ(FN.TRX.DIA,IDCUENTA,TRX.DIA.R,F.TRX.DIA,TRX.DIA.ERR)
	
    IF TRX.DIA.ERR EQ '' TRX.DIA.ERR THEN
    CRT TRX.DIA.ERR
        CONT=TRX.DIA.R<EB.SLV96.CONTADOR>
        TRX.DIA.R<EB.SLV96.CONTADOR>=CONT+1
        MONTODIA=TRX.DIA.R<EB.SLV96.RESERVADO1>
        MONTONUEVO=MONTO + MONTODIA
        IF MONTONUEVO GT ACUMULADO.DIA THEN
            STRERR ='EB-SIMP005'
            GOSUB CRT_ERROR
        END
        ELSE
        TRX.DIA.R<EB.SLV96.RESERVADO1>=MONTONUEVO
        CALL F.WRITE(FN.TRX.DIA,IDCUENTA,TRX.DIA.R)
*        CALL JOURNAL.UPDATE(FN.TRX.DIA)
        END

    END

    ELSE
	TRX.DIA.R<EB.SLV96.CONTADOR>=1
	TRX.DIA.R<EB.SLV96.RESERVADO1>=MONTO
	CALL F.WRITE(FN.TRX.DIA,IDCUENTA,TRX.DIA.R)
*	CALL JOURNAL.UPDATE(FN.TRX.DIA)
    END


    RETURN

CRT_ERROR:
    AF  = V_NAME_FIELD
    AV 	= 1
    ETEXT = STRERR
    CALL STORE.END.ERROR
    ERROR='SI'
    RETURN
CAT.SIMP:

    FN.AA.PRODUCT.DESIGNER = 'F.AA.PRD.DES.ACCOUNT'
    F.AA.PRODUCT.DESIGNER = ''
    CALL OPF(FN.AA.PRODUCT.DESIGNER,F.AA.PRODUCT.DESIGNER)
    Y.PRODUCTO='CUENTA.AHORRO.SIM'
    SELECT.PROD.DES = "SELECT " : FN.AA.PRODUCT.DESIGNER : " WITH @ID LIKE '" : Y.PRODUCTO : "...'"
    CALL EB.READLIST(SELECT.PROD.DES, PROD.DES,'',NO.REC.PROD.DES, ERR.REC.PROD.DES)
    IF NO.REC.PROD.DES NE 0 THEN
        CALL F.READ(FN.AA.PRODUCT.DESIGNER,PROD.DES, PROD.DES.REC, F.AA.PRODUCT.DESIGNER, ERR.REC1)
        IF PROD.DES.REC THEN


            CALL F.READ(FN.AA.PRODUCT.DESIGNER,PROD.DES, PROD.DES.REC, F.AA.PRODUCT.DESIGNER, ERR.REC1)
            IF PROD.DES.REC THEN
                CATEGORY.SIMP = PROD.DES.REC<AA.AC.CATEGORY> ;*Obtener el parent para luego ir a traer sus propiedades
            END

        END

    END
    RETURN


ESCRIBIR.ARCHIVO:
    DIR.NAME= 'SIMPLIFICADA'
    R.ID   = 'TT':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN


    END
