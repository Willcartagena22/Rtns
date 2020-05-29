*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.S.VALIDATE.DUI(DUI.NUMBER, RESULT, ERROR.CODE)
*-----------------------------------------------------------------------------
*Parameters
*
*IN:
*     DUI.NUMBER
*OUT:
*     RESULT (Result: Y = The DUI is valid, N = incorrect DUI)
*     Error.code (Error code in case RESULT = N)
*
*-----------------------------------------------------------------------------
* Modification History :
*
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*-----------------------------------------------------------------------------
    RESULT     = ''
    ERROR.CODE = ''

    IF DUI.NUMBER EQ '' THEN
        RETURN
    END
    CHANGE '-' TO '' IN DUI.NUMBER

    Y.DUI.LEN = LEN(DUI.NUMBER)

    IF Y.DUI.LEN GT 9 OR Y.DUI.LEN EQ 0 THEN
        RESULT     = 'N'
        ERROR.CODE = 'ST-SLV.GEN.LENGHT.INCORRECT' :FM: 'DUI'
        RETURN
    END

    IF Y.DUI.LEN LT 9 THEN
        DUI.NUMBER = FMT(DUI.NUMBER, "R%9")
    END

    IF NUM(DUI.NUMBER) ELSE
        RESULT     = 'N'
        ERROR.CODE = 'ST-SLV.GEN.INCORRECT.DATATYPE' :FM: 'DUI'
        RETURN
    END
    Y.I       = 1
    Y.DUI.LEN = 9

    LOOP
    WHILE Y.I LT Y.DUI.LEN
        Y.NUM = DUI.NUMBER[Y.DUI.LEN - Y.I,1]
        Y.I++
        Y.SUM += Y.NUM * Y.I
    REPEAT

    IF Y.SUM GT 10 THEN
        Y.REM      = MOD(Y.SUM, 10)
        Y.FIN.RES  = 10 - Y.REM
    END ELSE
        Y.FIN.RES = ''
    END

    IF Y.FIN.RES EQ 10 THEN
        Y.FIN.RES = '0'
    END

    Y.CHK.DIGI = DUI.NUMBER[9,1]

    IF Y.FIN.RES EQ Y.CHK.DIGI THEN
        RESULT     = 'Y'
        ERROR.CODE = ''
    END ELSE
        RESULT     = 'N'
        ERROR.CODE = 'ST-SLV.GEN.INCORRECT.VERIF.DIGIT' :FM: 'DUI'
    END
    RETURN
    END