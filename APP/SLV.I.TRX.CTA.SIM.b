*-----------------------------------------------------------------------------
* <Rating>117</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.I.TRX.CTA.SIM
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
    $INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------
    IF OFS$OPERATION EQ 'PROCESS' OR V$FUNCTION EQ 'I' OR V$FUNCTION EQ 'A' THEN
        GOSUB INIT
        GOSUB OPEN.FILE
        GOSUB VAR
        GOSUB PROCESS.DEBIT
        GOSUB PROCESS
    END ;*END IF OFS$OPERATION EQ 'PROCESS' AND V$FUNCTION EQ 'I' THEN
    ELSE IF OFS$OPERATION EQ 'PROCESS' AND V$FUNCTION EQ 'R' THEN
    CALL SLV.R.TRX.CTA.SIM
    END ;*END ELSE OFS$OPERATION EQ 'PROCESS' AND V$FUNCTION EQ 'I' THEN
    RETURN

INIT:
    DTL.APP.FN        = 'F.EB.SLV.CTA.SIMPLIFICADA.TXN'
    DTL.APP.F         = ''
    ACC.FN            = 'F.ACCOUNT'
    ACC.F             = ''
    RETURN

OPEN.FILE:
    CALL OPF(DTL.APP.FN,DTL.APP.F)
    CALL OPF(ACC.FN,ACC.F)
    RETURN

VAR:
    FT.ID              = ID.NEW
    ACC.SIMPLIFICADA   = R.NEW(FT.CREDIT.ACCT.NO)
    ACC.SIMPLIFICADA.D = R.NEW(FT.DEBIT.ACCT.NO)
    FT.AMOUNT          = R.NEW(FT.DEBIT.AMOUNT)
    FLAG.CREDIT        = 'C'
    FLAG.DEBIT         = 'D'
    YEAR_MONTH         = TODAY[1,4]:TODAY[5,2]
    NEW_TRX.DETAIL     = YEAR_MONTH:'~':FT.AMOUNT
    NEW_TRX.DETAIL.D   = YEAR_MONTH:'~':FT.AMOUNT
    RETURN


PROCESS.DEBIT:
;*VALIDAMOS QUE LA CUENTA DE CREDITO SEA UNA CUENTA SIMPLIFICADA
    CALL F.READ(ACC.FN,ACC.SIMPLIFICADA.D,ACC.R.D,ACC.F,ACC.ERR)

    IF ACC.R.D<AC.CATEGORY> EQ '6016' THEN

        ;*VALIDAMOS SI LA CUENTA A TRANSACCIONAR YA POSEE REGISTRO EN LA APLICACION EB.SLV.CTA.SIMPLIFICADA.TXN
        CALL F.READ(DTL.APP.FN,ACC.SIMPLIFICADA.D,DTL.R.D,DTL.APP.F,DTL.ERR)

        IF DTL.ERR NE '' THEN
            DTL.R.D<EB.SLV25.CONTADOR.TRX> = 1
            DTL.R.D<EB.SLV25.TRX.DETALLE>  = NEW_TRX.DETAIL.D
            CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA.D, DTL.R.D)
        END ;*END IF DTL.ERR NE ''
        ELSE
            TRX.ARR = DTL.R.D<EB.SLV25.TRX.DETALLE>
            FINDSTR YEAR_MONTH IN TRX.ARR SETTING V.FLD, V.VAL THEN
                    COUNT.ADD      = DTL.R.D<EB.SLV25.CONTADOR.TRX>+1
                    DETAIL.FOUND   = TRX.ARR<1,V.VAL>
                    CURRENT.AMOUNT = FIELDS(DETAIL.FOUND,'~',2)
                    NEW.AMOUNT     = CURRENT.AMOUNT + FT.AMOUNT
                    NEW.TRX.DETAIL = FIELDS(DETAIL.FOUND,'~',1):'~':NEW.AMOUNT
                    DTL.R.D<EB.SLV25.CONTADOR.TRX>      = COUNT.ADD
                    DTL.R.D<EB.SLV25.TRX.DETALLE,V.VAL> = NEW.TRX.DETAIL
                    CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA.D, DTL.R.D)
            END ;*END FINDSTR YEAR_MONTH
            ELSE
                    COUNT.ADD                      = DTL.R.D<EB.SLV25.CONTADOR.TRX>+1
                    UPDATE.DETAIL                  = DTL.R.D<EB.SLV25.TRX.DETALLE>:VM:NEW_TRX.DETAIL
                    DTL.R.D<EB.SLV25.CONTADOR.TRX> = COUNT.ADD
                    DTL.R.D<EB.SLV25.TRX.DETALLE>  = UPDATE.DETAIL
                    CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA.D, DTL.R.D)
            END ;*END ELSE FINDSTR
        END ;*END ELSE DTL.ERR NE ''
    END ;*END IF ACC.R.D<AC.CATEGORY> EQ '6016'
    ELSE
        IF ACC.R.D<AC.CATEGORY> EQ '6017' THEN
              ;*VALIDAMOS SI LA CUENTA A TRANSACCIONAR YA POSEE REGISTRO EN LA APLICACION EB.SLV.CTA.SIMPLIFICADA.TXN
              CALL F.READ(DTL.APP.FN,ACC.SIMPLIFICADA.D,DTL.R.D,DTL.APP.F,DTL.ERR)

              IF DTL.ERR NE '' THEN
                    DTL.R.D<EB.SLV25.CONTADOR.TRX> = 1
                    DTL.R.D<EB.SLV25.TRX.DETALLE>  = NEW_TRX.DETAIL.D
                    CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA.D, DTL.R.D)
              END ;*END IF DTL.ERR NE ''
              ELSE
                    TRX.ARR = DTL.R.D<EB.SLV25.TRX.DETALLE>
                    FINDSTR YEAR_MONTH IN TRX.ARR SETTING V.FLD, V.VAL THEN
                                COUNT.ADD      = DTL.R.D<EB.SLV25.CONTADOR.TRX>+1
                                DETAIL.FOUND   = TRX.ARR<1,V.VAL>
                                CURRENT.AMOUNT = FIELDS(DETAIL.FOUND,'~',2)
                                NEW.AMOUNT     = CURRENT.AMOUNT - FT.AMOUNT
                                NEW.TRX.DETAIL = FIELDS(DETAIL.FOUND,'~',1):'~':NEW.AMOUNT
                                DTL.R.D<EB.SLV25.CONTADOR.TRX>      = COUNT.ADD
                                DTL.R.D<EB.SLV25.TRX.DETALLE,V.VAL> = NEW.TRX.DETAIL
                                CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA.D, DTL.R.D)
                    END ;*END FINDSTR YEAR_MONTH
                    ELSE
                                COUNT.ADD                      = DTL.R.D<EB.SLV25.CONTADOR.TRX>+1
                                UPDATE.DETAIL                  = DTL.R.D<EB.SLV25.TRX.DETALLE>:VM:NEW_TRX.DETAIL
                                DTL.R.D<EB.SLV25.CONTADOR.TRX> = COUNT.ADD
                                DTL.R.D<EB.SLV25.TRX.DETALLE>  = UPDATE.DETAIL
                                CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA.D, DTL.R.D)
                    END ;*END ELSE FINDSTR
              END ;*END ELSE DTL.ERR NE ''
              
              CALL SLV.CARGO.RETIRO.ANT.CTA.PROP
        END ;*END IF ACC.R.D<AC.CATEGORY> EQ '6017'
         
    END ;*END ELSE ACC.R.D<AC.CATEGORY> EQ '6016'
    
    
    RETURN

PROCESS:
;*VALIDAMOS QUE LA CUENTA DE CREDITO SEA UNA CUENTA SIMPLIFICADA O UNA CUENTA DE AHORRO CON PROPOSITO
    CALL F.READ(ACC.FN,ACC.SIMPLIFICADA,ACC.R,ACC.F,ACC.ERR)

    IF ACC.R<AC.CATEGORY> EQ '6016' OR ACC.R<AC.CATEGORY> EQ '6017'  THEN

        ;*VALIDAMOS SI LA CUENTA A TRANSACCIONAR YA POSEE REGISTRO EN LA APLICACION EB.SLV.CTA.SIMPLIFICADA.TXN
        CALL F.READ(DTL.APP.FN,ACC.SIMPLIFICADA,DTL.R,DTL.APP.F,DTL.ERR)

        IF DTL.ERR NE '' THEN
            DTL.R<EB.SLV25.CONTADOR.TRX> = 1
            DTL.R<EB.SLV25.TRX.DETALLE>  = NEW_TRX.DETAIL
            CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA, DTL.R)
        END ;*END IF DTL.ERR NE ''
        ELSE
            TRX.ARR = DTL.R<EB.SLV25.TRX.DETALLE>
            FINDSTR YEAR_MONTH IN TRX.ARR SETTING V.FLD, V.VAL THEN
                    COUNT.ADD      = DTL.R<EB.SLV25.CONTADOR.TRX>+1
                    DETAIL.FOUND   = TRX.ARR<1,V.VAL>
                    CURRENT.AMOUNT = FIELDS(DETAIL.FOUND,'~',2)
                    NEW.AMOUNT     = CURRENT.AMOUNT + FT.AMOUNT
                    NEW.TRX.DETAIL = FIELDS(DETAIL.FOUND,'~',1):'~':NEW.AMOUNT
                    DTL.R<EB.SLV25.CONTADOR.TRX>      = COUNT.ADD
                    DTL.R<EB.SLV25.TRX.DETALLE,V.VAL> = NEW.TRX.DETAIL
                    CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA, DTL.R)
            END ;*END FINDSTR
            ELSE
                    COUNT.ADD                    = DTL.R<EB.SLV25.CONTADOR.TRX>+1
                    UPDATE.DETAIL                = DTL.R<EB.SLV25.TRX.DETALLE>:VM:NEW_TRX.DETAIL
                    DTL.R<EB.SLV25.CONTADOR.TRX> = COUNT.ADD
                    DTL.R<EB.SLV25.TRX.DETALLE>  = UPDATE.DETAIL
                    CALL F.WRITE (DTL.APP.FN, ACC.SIMPLIFICADA, DTL.R)
            END ;*END ELSE FINDSTR
         END ;*END ELSE DTL.ERR NE ''
    END ;*END IF ACC.R<AC.CATEGORY> EQ '6016'
    RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'FATCA'
    R.ID   = 'SLV.I.TRX.CTA.SIM_':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN

    END
