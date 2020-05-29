*-----------------------------------------------------------------------------
* <Rating>-56</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.AA.GET.WOR.BAL.AMT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)

*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_AA.APP.COMMON
    $INSERT I_AA.ACTION.CONTEXT
    $INSERT I_F.EB.SLV.CUS.EARLY.RELEASE

*-----------------------------------------------------------------------------

    GOSUB INI
    GOSUB OPENFILE
    GOSUB PROCESS

INI:
    FN.EARLY.RELEASE = 'F.EB.SLV.CUS.EARLY.RELEASE'
    F.EARLY.RELEASE = ''

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    RETURN

OPENFILE:
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    CALL OPF(FN.EARLY.RELEASE,F.EARLY.RELEASE)
    RETURN

PROCESS:
    Y.NUMERO.CUENTA = AA$R.ARRANGEMENT<9>
    Y.AMT.TXN = AA$R.ARRANGEMENT.ACTIVITY<19>
    Y.STR.INPUTER = AA$R.ARRANGEMENT.ACTIVITY<58>
    Y.ID.TXN = FIELD(Y.STR.INPUTER,'_',9)
    Y.APP = SUBSTRINGS(Y.ID.TXN,1,3)

    CALL F.READ(FN.ACCOUNT,Y.NUMERO.CUENTA,RECORD.ACCOUNT,F.ACCOUNT,ERROR.ACC)
    Y.CUSTOMER = RECORD.ACCOUNT<AC.CUSTOMER>
    Y.AMT.ACC = RECORD.ACCOUNT<AC.WORKING.BALANCE>

    IF Y.APP NE 'CHG' THEN
        GOSUB CALC.LIBERACION
    END
    ELSE
    CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
    END.VALUE = Y.AMT.ACC - Y.AMT.TXN
    END
    RETURN

CALC.LIBERACION:

;*CALCULANDO EL DISPONIBLE EN LA CUENTA
;*--------------------------------------
;*NOTA: TOMAR EN CUENTA QUE EL WORKING BALANCE NO DEVUELVE EL SALDO ACTUAL, DEVUELVE COMO QUEDA DESPUES DE LA TRANSACCION
;*------------------------------------------------------------------------------------------------------------------------
    IF Y.AMT.ACC LT '0' THEN
        Y.MONTO.DISPO.CUENTA = Y.AMT.TXN - ABS(Y.AMT.ACC)
    END
    ELSE
    Y.MONTO.DISPO.CUENTA = Y.AMT.ACC
    END

    IF RECORD.ACCOUNT GT '0' THEN

        ;*VALIDANDO SI TIENE ER ACTIVA
        ;*----------------------------
        CALL SLV.V.ER.ACTIVA(Y.NUMERO.CUENTA,Y.POSEE.LIBERACION)

        IF Y.POSEE.LIBERACION EQ 'Y' THEN

            ;*POSEE FONDOS SUFICIENTES EN LA CUENTA
            ;*-------------------------------------
            IF Y.MONTO.DISPO.CUENTA LT Y.AMT.TXN THEN
                ;*REALIZANDO LIBERACION DE FONDOS
                ;*-------------------------------
                CALL SLV.I.EARLY.RELEASE.START(Y.NUMERO.CUENTA,Y.AMT.TXN,Y.CUSTOMER,Y.OUT,Y.ID.TXN,Y.MONTO.DISPO.CUENTA)

                CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
                END.VALUE = Y.OUT
            END
            ELSE
            CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
        END

    END
    ELSE
    CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
    END
    END
    END
    ELSE
    CALL AA.GET.WORKING.BALANCE.AMOUNT(PROPERTY.ID, START.DATE, END.DATE, CURRENT.DATE, BALANCE.TYPE, ACTIVITY.IDS,CURRENT.VALUE, START.VALUE, END.VALUE)
    END
    RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'LogsLiberacionFondos'
    R.ID   = 'LogsL':TODAY:'.txt'
;*hacer que escriba un archivo
    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN

    END
