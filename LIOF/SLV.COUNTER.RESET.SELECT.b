*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.COUNTER.RESET.SELECT
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_SLV.COUNTER.RESET.COMMON
*-----------------------------------------------------------------------------

    SEL.COUNTER = "SELECT " : FN.COUNTER
    CALL EB.READLIST(SEL.COUNTER,SEL.LIST.COUNTER,'',NO.OF.REC,SEL.ERR)
    IF SEL.LIST.COUNTER THEN
        CALL BATCH.BUILD.LIST('',SEL.LIST.COUNTER)
    END   
    RETURN 

    END
