*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.S.VALIDATE.NIT(NIT.NUMBER, RESULT, ERROR.CODE)
*-----------------------------------------------------------------------------
*Parameters
*
*IN:
*     NIT.NUMBER
*OUT:
*     RESULT (Result: Y = The NIT is valid, N = incorrect NIT)
*     Error.code (Error code in case RESULT = N)
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*-----------------------------------------------------------------------------

    RESULT     = ''
    ERROR.CODE = ''

    IF NIT.NUMBER EQ '' THEN
        RETURN
    END

    CHANGE '-' TO '' IN NIT.NUMBER

    Y.NIT.LEN = LEN(NIT.NUMBER)

    IF Y.NIT.LEN NE 14 THEN
        RESULT     = 'N'
        ERROR.CODE = 'ST-SLV.GEN.LENGHT.INCORRECT' :FM: 'NIT'
        RETURN
    END

    IF NUM(NIT.NUMBER) ELSE
        RESULT     = 'N'
        ERROR.CODE = 'ST-SLV.GEN.INCORRECT.DATATYPE' :FM: 'NIT'
        RETURN
    END
    IF NIT.NUMBER EQ '00000000000000' THEN 
    	 RESULT ='N'
    	 ERROR.CODE = 'ST-SLV.GEN.INVALID.DATA' :FM: 'NIT'
        RETURN
    	
    END 

    Y.SCH.POS = NIT.NUMBER[11,1]

    IF Y.SCH.POS EQ 0 THEN
        Y.I       = 1
        Y.NIT.LEN = 14

        LOOP
        WHILE Y.I LT Y.NIT.LEN
            Y.NUM = NIT.NUMBER[Y.NIT.LEN - Y.I,1]
            Y.I++
            Y.SUM += Y.NUM * Y.I
        REPEAT
        Y.NEAR.MUL = Y.SUM/11
        Y.NEAR.MUL = FIELD(Y.NEAR.MUL,'.',1)
        Y.NEAR.MUL = Y.NEAR.MUL * 11
        Y.DIFF     = Y.SUM - Y.NEAR.MUL
        IF Y.DIFF EQ 10 THEN
            Y.DIFF = 0
        END
    END ELSE
        Y.I       = 1
        Y.NIT.LEN = 14
        Y.CHK.STR = 2765432765432
        LOOP
        WHILE Y.I LT Y.NIT.LEN
            Y.CHK.POS = Y.NIT.LEN - Y.I
            Y.NUM     = NIT.NUMBER[Y.CHK.POS,1]
            Y.DIG     = Y.CHK.STR[Y.CHK.POS,1]
            Y.SUM    += Y.NUM * Y.DIG
            Y.I++
        REPEAT
        Y.NEAR.MUL = Y.SUM/11
        Y.NEAR.MUL = FIELD(Y.NEAR.MUL, '.', 1)
        Y.NEAR.MUL = Y.NEAR.MUL * 11
        Y.DIFF     = Y.SUM - Y.NEAR.MUL
        Y.DIFF     = 11 - Y.DIFF
        IF Y.DIFF EQ 10 THEN
            Y.DIFF = 0
        END
    END
    Y.CHK.DIGI = NIT.NUMBER[14,1]

    IF Y.DIFF EQ Y.CHK.DIGI THEN
        RESULT     = 'Y'
        ERROR.CODE = ''
    END ELSE
        RESULT     = 'N'
        ERROR.CODE = 'ST-SLV.GEN.INCORRECT.VERIF.DIGIT' :FM: 'NIT'
    END
    RETURN
    END
