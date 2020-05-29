*-----------------------------------------------------------------------------
* <Rating>-40</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.CUST.SIMP.CNT
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.SLV.DUI.CUSTOMER.CNT
*-----------------------------------------------------------------------------


    GOSUB INIT
    GOSUB ABRIR
    GOSUB PROCESAR
    RETURN

INIT:
    FN.SLV.DUI.CUSTOMER.CNT = 'F.SLV.DUI.CUSTOMER.CNT'
    F.SLV.DUI.CUSTOMER.CNT  = ''
    RETURN

ABRIR:
    CALL OPF(FN.SLV.DUI.CUSTOMER.CNT, F.SLV.DUI.CUSTOMER.CNT)
    RETURN


PROCESAR:

    GOSUB VAR_VALUE_DOC
    
    
    R.SLV.DUI.CUSTOMER.CNT<SLV.DUI.CUSTOMER.CODE> = ID.NEW

    IF DUI NE '' THEN
        CALL F.WRITE(FN.SLV.DUI.CUSTOMER.CNT, DUI, R.SLV.DUI.CUSTOMER.CNT)
    END

    RETURN



VAR_VALUE_DOC:

    NO_MVNIT	=	DCOUNT(R.NEW(EB.CUS.LEGAL.ID),VM)
    FOR Y=1 TO NO_MVNIT
        BEGIN CASE
            CASE R.NEW(EB.CUS.LEGAL.DOC.NAME)<1,Y> EQ 'DOCTO.UNICO.IDENT'
                DUI = R.NEW(EB.CUS.LEGAL.ID)<1,Y>
        END CASE
    NEXT Y


    RETURN



    END
