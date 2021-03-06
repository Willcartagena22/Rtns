*-----------------------------------------------------------------------------
* <Rating>-59</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.R.TRX.CTA.SIM
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.CTA.SIMPLIFICADA.TXN
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.ACCOUNT
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPEN.FILE
    GOSUB GET.VALUE
    GOSUB PROCESS.DEBIT
    GOSUB PROCESS
    RETURN

INIT:
    FT.FN       = 'F.FUNDS.TRANSFER'
    FT.F        = ''
    DTL.APP.FN  = 'F.EB.SLV.CTA.SIMPLIFICADA.TXN'
    DTL.APP.F   = ''
    ACC.FN      = 'F.ACCOUNT'
    ACC.F       = ''
    FT.FN$HIS   = 'F.FUNDS.TRANSFER$HIS'
    FUNDS.T$HIS = ''
    RETURN

OPEN.FILE:
    CALL OPF(FT.FN,FT.F)
    CALL OPF(DTL.APP.FN,DTL.APP.F)
    CALL OPF(ACC.FN,ACC.F)
    CALL OPF(FT.FN$HIS,FUNDS.T$HIS)
    RETURN

GET.VALUE:
    CALL F.READ(FT.FN,ID.NEW,FT.R,FT.F,FT.ERR)

    IF FT.ERR NE '' THEN
        CALL EB.READ.HISTORY.REC(FT.FN$HIS,ID.NEW,FT.R,FT.ERR)
    END

    ACC.SIMPLIFICADA   = FT.R<FT.CREDIT.ACCT.NO>
    ACC.SIMPLIFICADA.D = FT.R<FT.DEBIT.ACCT.NO>
    FT.AMOUNT          = FT.R<FT.DEBIT.AMOUNT>
    YEAR_MONTH         = TODAY[1,4]:TODAY[5,2]
    NEW_TRX.DETAIL     = YEAR_MONTH:'~':FT.AMOUNT
    RETURN

PROCESS.DEBIT:
    CALL F.READ(ACC.FN,ACC.SIMPLIFICADA.D,ACC.R,ACC.F,ACC.ERR)

    IF ACC.R<AC.CATEGORY> EQ '6016' THEN

        ;*VALIDAMOS SI LA CUENTA A TRANSACCIONAR YA POSEE REGISTRO EN LA APLICACION EB.SLV.CTA.SIMPLIFICADA.TXN
        CALL F.READ(DTL.APP.FN,ACC.SIMPLIFICADA.D,DTL.R.D,DTL.APP.F,DTL.ERR)

        IF DTL.ERR NE '' THEN
            ;*DTL.R<EB.SLV25.CONTADOR.TRX> = 1
            ;*DTL.R<EB.SLV25.TRX.DETALLE>  = NEW_TRX.DETAIL
            ;*CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA, DTL.R)
        END
        ELSE
        TRX.ARR = DTL.R.D<EB.SLV25.TRX.DETALLE>
        FINDSTR YEAR_MONTH IN TRX.ARR SETTING V.FLD, V.VAL THEN
            COUNT.ADD      = DTL.R.D<EB.SLV25.CONTADOR.TRX>-1
            DETAIL.FOUND   = TRX.ARR<1,V.VAL>
            CURRENT.AMOUNT = FIELDS(DETAIL.FOUND,'~',2)
            NEW.AMOUNT     = CURRENT.AMOUNT - FT.AMOUNT
            NEW.TRX.DETAIL = FIELDS(DETAIL.FOUND,'~',1):'~':NEW.AMOUNT
            DTL.R.D<EB.SLV25.CONTADOR.TRX>      = COUNT.ADD
            DTL.R.D<EB.SLV25.TRX.DETALLE,V.VAL> = NEW.TRX.DETAIL
            CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA.D, DTL.R.D)
        END

    END
    END
    RETURN

PROCESS:
;*VALIDAMOS QUE LA CUENTA DE CREDITO SEA UNA CUENTA SIMPLIFICADA
    CALL F.READ(ACC.FN,ACC.SIMPLIFICADA,ACC.R,ACC.F,ACC.ERR)

    IF ACC.R<AC.CATEGORY> EQ '6016' OR ACC.R<AC.CATEGORY> EQ '6017' THEN

        ;*VALIDAMOS SI LA CUENTA A TRANSACCIONAR YA POSEE REGISTRO EN LA APLICACION EB.SLV.CTA.SIMPLIFICADA.TXN
        CALL F.READ(DTL.APP.FN,ACC.SIMPLIFICADA,DTL.R,DTL.APP.F,DTL.ERR)

        IF DTL.ERR NE '' THEN
            ;*DTL.R<EB.SLV25.CONTADOR.TRX> = 1
            ;*DTL.R<EB.SLV25.TRX.DETALLE>  = NEW_TRX.DETAIL
            ;*CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA, DTL.R)
        END
        ELSE
        TRX.ARR = DTL.R<EB.SLV25.TRX.DETALLE>
        FINDSTR YEAR_MONTH IN TRX.ARR SETTING V.FLD, V.VAL THEN
            COUNT.ADD      = DTL.R<EB.SLV25.CONTADOR.TRX>-1
            DETAIL.FOUND   = TRX.ARR<1,V.VAL>
            CURRENT.AMOUNT = FIELDS(DETAIL.FOUND,'~',2)
            NEW.AMOUNT     = CURRENT.AMOUNT - FT.AMOUNT
            NEW.TRX.DETAIL = FIELDS(DETAIL.FOUND,'~',1):'~':NEW.AMOUNT
            DTL.R<EB.SLV25.CONTADOR.TRX>      = COUNT.ADD
            DTL.R<EB.SLV25.TRX.DETALLE,V.VAL> = NEW.TRX.DETAIL
            CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA, DTL.R)
        END

    END
    END
    RETURN

    END
