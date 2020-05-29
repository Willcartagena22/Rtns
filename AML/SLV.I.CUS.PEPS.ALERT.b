*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.I.CUS.PEPS.ALERT
*-----------------------------------------------------------------------------
*** Company Name:    BANCO AZUL
* Developed By:    Muthamil - Capgemini
* Date  :          2014/09/06
*------------------------------------------------
* Subroutine Type:  Input Routine
* Attached to:      Version.Control
* Attached as:     Customer
* Primary Purpose: 	Input Routine to generate Alert
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

    IF V$FUNCTION EQ 'I' THEN
        Y.ALERT.ID = 'PEPS'
        Y.CUS.ID = ID.NEW
        TXN.CODE = ''
        Y.ACC.NO = ''
        Y.AMT.LCY = ''
        ADDIT.INFO = ''
        IF APPLICATION EQ 'CUSTOMER' THEN
* Boo
* ADDIT.INFO will have the current APPLICATION's OVERRIDE field No.
* It helps to raise the override properly
            ADDIT.INFO = EB.CUS.OVERRIDE
        END
        ADDIT.MSG = ''

        CALL SLV.R35.GEN.RTN(Y.ALERT.ID,TXN.CODE,Y.ACC.NO,Y.CUS.ID,Y.AMT.LCY,ADDIT.INFO,ADDIT.MSG)

    END
    RETURN
    END
