*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.CUS.VALIDATE.NIT
*-----------------------------------------------------------------------------
* LF.NIT field validation routine, it calls the generic routine SLV.S.VALIDATE.NIT
* checks if the entered NIT number is already used for a customer by reading the 
* application SLV.NIT.CUSTOMER.CNT
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SLV.NIT.CUSTOMER.CNT
*-----------------------------------------------------------------------------

    Y.NIT.NUMBER = COMI

    IF Y.NIT.NUMBER EQ '' THEN
        RETURN
    END
    
    CALL SLV.S.VALIDATE.NIT(Y.NIT.NUMBER, Y.RESULT, Y.ERROR.CODE)

    IF Y.ERROR.CODE THEN
        ETEXT = Y.ERROR.CODE
        CALL STORE.END.ERROR
        RETURN
    END

    CHANGE '-' TO '' IN Y.NIT.NUMBER

    FN.SLV.NIT.CUSTOMER.CNT = 'F.SLV.NIT.CUSTOMER.CNT'
    F.SLV.NIT.CUSTOMER.CNT  = ''
    R.SLV.NIT.CUSTOMER.CNT  = ''
    SLV.NIT.CUSTOMER.CNT.ER = ''

    CALL OPF(FN.SLV.NIT.CUSTOMER.CNT, F.SLV.NIT.CUSTOMER.CNT)

    CALL F.READ(FN.SLV.NIT.CUSTOMER.CNT, Y.NIT.NUMBER, R.SLV.NIT.CUSTOMER.CNT, F.SLV.NIT.CUSTOMER.CNT, SLV.NIT.CUSTOMER.CNT.ER)

    IF R.SLV.NIT.CUSTOMER.CNT THEN
        Y.CUST.CODE = R.SLV.NIT.CUSTOMER.CNT<SLV.NIT.CUSTOMER.CODE>
        IF ID.NEW NE Y.CUST.CODE THEN
            ETEXT = 'ST-SLV.GEN.CODE.ALREADY.REG' :FM: Y.NIT.NUMBER :VM: Y.CUST.CODE
            CALL STORE.END.ERROR
        END
    END
    RETURN
    END
