*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.COUNTER.RESET(Y.COUNTER.ID)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_SLV.COUNTER.RESET.COMMON
*-----------------------------------------------------------------------------

    IF Y.COUNTER.ID EQ '' THEN

        MESSAGE.INFO = ''         ;* Handling Fatal error to halt the process
        MESSAGE.INFO<1> = ''
        MESSAGE.INFO<2> = Y.COUNTER.ID
        MESSAGE.INFO<3> = ''
        MESSAGE.INFO<4> = "Record not found "
        MESSAGE.INFO<5> = 'YES'
        TEXT = ''
        CALL FATAL.ERROR(MESSAGE.INFO) 

    END ELSE
        CALL F.DELETE(FN.COUNTER,Y.COUNTER.ID)
    END

    RETURN

    END

