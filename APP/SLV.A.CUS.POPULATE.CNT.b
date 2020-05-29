*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.A.CUS.POPULATE.CNT
*-----------------------------------------------------------------------------
*
*------------------------------------------------

* Company Name:			BANCO AZUL     

* Developed By:			Shanmuga Sundaram J - Capgemini    

* Date  :				2014/07/14

*------------------------------------------------

* Subroutine Type:		VERSION

* Attached to:			CUSTOMER,SLV.INPUT, CUSTOMER,SLV.JURIDICO, CUSTOMER,SLV.PATRIMONIO, CUSTOMER,SLV.CONGLOMERADO

* Attached as:			AUTH.ROUTINE

* Primary Purpose:		To update the concat table SLV.DUI.CUSTOMER.CNT, SLV.NIT.CUSTOMER.CNT & SLV.CUSTOMER.ENT.CNT

*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.SLV.DUI.CUSTOMER.CNT
    $INSERT I_F.SLV.NIT.CUSTOMER.CNT
    $INSERT I_F.SLV.CUSTOMER.ENR.CNT

*-----------------------------------------------------------------------------

    FN.SLV.DUI.CUSTOMER.CNT = 'F.SLV.DUI.CUSTOMER.CNT'
    F.SLV.DUI.CUSTOMER.CNT  = ''

    CALL OPF(FN.SLV.DUI.CUSTOMER.CNT, F.SLV.DUI.CUSTOMER.CNT)

    FN.SLV.NIT.CUSTOMER.CNT = 'F.SLV.NIT.CUSTOMER.CNT'
    F.SLV.NIT.CUSTOMER.CNT = ''

    CALL OPF(FN.SLV.NIT.CUSTOMER.CNT, F.SLV.NIT.CUSTOMER.CNT)

    FN.SLV.CUSTOMER.ENR.CNT = 'F.SLV.CUSTOMER.ENR.CNT'
    F.SLV.CUSTOMER.ENR.CNT  = ''

    CALL OPF(FN.SLV.CUSTOMER.ENR.CNT, F.SLV.CUSTOMER.ENR.CNT)

    Y.APP = 'CUSTOMER'
    Y.FLD = 'LF.DUI' :VM: 'LF.NIT' :VM: 'LF.NIT.ANTER' :VM: 'SEGMENT'
    Y.POS = ''

    CALL MULTI.GET.LOC.REF(Y.APP,Y.FLD,Y.POS)

    Y.DUI.POS 	  = Y.POS<1,1>
    Y.NIT.POS 	  = Y.POS<1,2>
    Y.NIT.ANT.POS = Y.POS<1,3>
    Y.SEG.POS	  = Y.POS<1,4>
    Y.CUS.LFS     = R.NEW(EB.CUS.LOCAL.REF)
    Y.DUI.VAL     = Y.CUS.LFS<1,Y.DUI.POS>
    Y.NIT.VAL     = Y.CUS.LFS<1,Y.NIT.POS>
    Y.NIT.ANT.VAL = Y.CUS.LFS<1,Y.NIT.ANT.POS>
    Y.SEG		  = Y.CUS.LFS<1,Y.SEG.POS>

    Y.OLD.CUS.LFS     = R.OLD(EB.CUS.LOCAL.REF)
    Y.OLD.DUI.VAL     = Y.OLD.CUS.LFS<1,Y.DUI.POS>
    Y.OLD.NIT.VAL     = Y.OLD.CUS.LFS<1,Y.NIT.POS>
    Y.OLD.NIT.ANT.VAL = Y.OLD.CUS.LFS<1,Y.NIT.ANT.POS>
    Y.CUS.ID	 	  = ID.NEW

    IF Y.DUI.VAL NE Y.OLD.DUI.VAL THEN
        IF Y.OLD.DUI.VAL NE '' THEN
            CALL F.DELETE(FN.SLV.DUI.CUSTOMER.CNT, Y.OLD.DUI.VAL)
            CALL F.DELETE(FN.SLV.CUSTOMER.ENR.CNT, Y.CUS.ID)
        END

        R.SLV.DUI.CUSTOMER.CNT<SLV.DUI.CUSTOMER.CODE> = ID.NEW

        IF Y.DUI.VAL NE '' THEN
            CALL F.WRITE(FN.SLV.DUI.CUSTOMER.CNT, Y.DUI.VAL, R.SLV.DUI.CUSTOMER.CNT)
        END
    END

    IF Y.SEG EQ 1 THEN
        IF Y.DUI.VAL THEN
            Y.NAME		 = R.NEW(EB.CUS.NAME.1)<1,1>
            Y.SHORT.NAME = R.NEW(EB.CUS.SHORT.NAME)<1,1>
            
            R.ENR<SLV.ENR.ENRICHMENT.DUI> = Y.NAME : ' ' : Y.SHORT.NAME : '. DUI ' : Y.DUI.VAL

            CALL F.WRITE(FN.SLV.CUSTOMER.ENR.CNT, Y.CUS.ID, R.ENR)
        END ELSE
        	Y.NAME		 = R.NEW(EB.CUS.NAME.1)<1,1>
            Y.SHORT.NAME = R.NEW(EB.CUS.SHORT.NAME)<1,1>
            
            R.ENR<SLV.ENR.ENRICHMENT.DUI> = Y.NAME : ' ' : Y.SHORT.NAME
            
            CALL F.WRITE(FN.SLV.CUSTOMER.ENR.CNT, Y.CUS.ID, R.ENR)
        END
    END ELSE
        Y.SHORT.NAME = R.NEW(EB.CUS.SHORT.NAME)<1,1>
        R.ENR        = Y.SHORT.NAME

        CALL F.WRITE(FN.SLV.CUSTOMER.ENR.CNT, Y.CUS.ID, R.ENR)
    END

    IF Y.NIT.VAL NE Y.OLD.NIT.VAL THEN
        IF Y.OLD.NIT.VAL NE '' THEN
            CALL F.DELETE(FN.SLV.NIT.CUSTOMER.CNT, Y.OLD.NIT.VAL)
        END
        R.SLV.NIT.CUSTOMER.CNT<SLV.NIT.CUSTOMER.CODE> = ID.NEW
        IF Y.NIT.VAL NE '' THEN
            CALL F.WRITE(FN.SLV.NIT.CUSTOMER.CNT, Y.NIT.VAL, R.SLV.NIT.CUSTOMER.CNT)
        END
    END

    IF Y.NIT.ANT.VAL NE Y.OLD.NIT.ANT.VAL THEN
        IF Y.OLD.NIT.ANT.VAL NE '' THEN
            CALL F.DELETE(FN.SLV.NIT.CUSTOMER.CNT, Y.OLD.NIT.ANT.VAL)
        END
        R.SLV.NIT.ANT.CUSTOMER.CNT<SLV.NIT.CUSTOMER.CODE> = ID.NEW
        IF Y.NIT.ANT.VAL NE '' THEN
            CALL F.WRITE(FN.SLV.NIT.CUSTOMER.CNT, Y.NIT.ANT.VAL, R.SLV.NIT.ANT.CUSTOMER.CNT)
        END
    END
    RETURN
    END
