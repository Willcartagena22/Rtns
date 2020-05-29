*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.CUS.VALIDATE.DUI
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
		Y.TIPO.DOC=FIELD(R.NEW(EB.CUS.LEGAL.DOC.NAME),VM,2)
		CALL GET.LOC.REF('CUSTOMER','LF.QT.DP.MTH.CT',LOCPOS.LF.QT.DP.MTH.CT)
		VAR.TIPO.DOC=R.NEW(EB.CUS.LOCAL.REF)<1,LOCPOS.LF.QT.DP.MTH.CT>
		;*Y.TIPO.DOC=R.NEW(EB.CUS.LEGAL.DOC.NAME)<1,1>
        Y.DUI.NUMBER = COMI
        Y.VERSION = APPLICATION:PGM.VERSION
        BEGIN CASE
        CASE VAR.TIPO.DOC NE 'DUI' AND (Y.VERSION EQ 'CUSTOMER,SLV.AML.PN' OR Y.VERSION EQ 'CUSTOMER,SLV.EDIT.DOCS')
        	RETURN
        END CASE

        IF Y.DUI.NUMBER EQ '' THEN
            RETURN
        END
        CALL SLV.S.VALIDATE.DUI(Y.DUI.NUMBER, Y.RESULT, Y.ERROR.CODE)
		
		COMI = Y.DUI.NUMBER
        
        IF Y.ERROR.CODE THEN
            ETEXT = Y.ERROR.CODE
            CALL STORE.END.ERROR
            RETURN
        END

        CHANGE '-' TO '' IN Y.DUI.NUMBER

        FN.SLV.DUI.CUSTOMER.CNT = 'F.SLV.DUI.CUSTOMER.CNT'
        F.SLV.DUI.CUSTOMER.CNT  = ''
        R.SLV.DUI.CUSTOMER.CNT  = ''
        SLV.DUI.CUSTOMER.CNT.ER = ''

        CALL OPF(FN.SLV.DUI.CUSTOMER.CNT, F.SLV.DUI.CUSTOMER.CNT)

        CALL F.READ(FN.SLV.DUI.CUSTOMER.CNT, Y.DUI.NUMBER, R.SLV.DUI.CUSTOMER.CNT, F.SLV.DUI.CUSTOMER.CNT, SLV.DUI.CUSTOMER.CNT.ER)

        IF R.SLV.DUI.CUSTOMER.CNT THEN
            Y.CUST.CODE = R.SLV.DUI.CUSTOMER.CNT<SLV.DUI.CUSTOMER.CODE>
            IF ID.NEW NE Y.CUST.CODE THEN
                ETEXT = 'ST-SLV.GEN.CODE.ALREADY.REG' :FM: Y.DUI.NUMBER :VM: Y.CUST.CODE
                CALL STORE.END.ERROR
            END
        END
        RETURN
    END
