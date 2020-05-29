* Version 1 13/04/00  GLOBUS Release No. 200508 30/06/05
*-----------------------------------------------------------------------------
* <Rating>-15</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.B.AML.AL.UIF.SELECT
*-----------------------------------------------------------------------------
* Select routine to setup the common area for the multi-threaded Close of Business
* job <<PREFIX>>.EOD
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_F.SLV.AML.MOVS.HIST
    $INSERT I_SLV.B.AML.AL.UIF.COMMON
*-----------------------------------------------------------------------------
* Setup the parameters for BATCH.BUILD.LIST
* LIST.PARAMETERS<1> = blank, this is the list file name, NEVER enter a value here
* LIST.PARAMETERS<2> = the filename to be selected, e.g. F.ACCOUNT, BATCH.BUILD.LIST will open it
* LIST.PARAMETERS<3> = selection criteria for file, e.g. CURRENCY EQ "GBP", this first WITH is not required
*                      and will be added by BATCH.BUILD.LIST
* ID.LIST = predefined list, for example from a CONCAT file record.
*           ID.LIST will take precedence over LIST.PARAMETERS
* CONTROL.LIST = common list used by BATCH.JOB.CONTROL
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*    Date              Author               Reference             Comments
* 30-Mar-2015       C.Yepez							   			Exlude null values on the list
*-----------------------------------------------------------------------------

    SEL.CMD = "SELECT ":FN.SLV.AML.MOVS.HIST:" WITH TXNS.DATE GE ":Y.START.DATE:" AND TXNS.DATE LE ":Y.END.DATE
    SEL.LIST = '' ; NO.OF.RECS = '' ; RET.ERR = ''
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.RECS,RET.ERR)
    LOOP
        REMOVE Y.HIST.ID FROM SEL.LIST SETTING HIS.POS
    WHILE Y.HIST.ID : HIS.POS
        Y.CUST.ID = FIELD(Y.HIST.ID,'-',1)
        * 2015/03/30 CYepez
        IF Y.CUST.ID NE '' THEN
            IF Y.HIST.ID.LIST EQ '' THEN
                Y.HIST.ID.LIST =Y.CUST.ID
            END ELSE
                GOSUB CHECK.DUP ; *Check the duplicacy of the customer
            END
        END
    REPEAT
    CALL BATCH.BUILD.LIST('',Y.HIST.ID.LIST)

    RETURN

*-----------------------------------------------------------------------------

*** <region name= CHECK.DUP>
CHECK.DUP:
*** <desc>Check the duplicacy of the customer </desc>
    LOCATE Y.CUST.ID IN Y.HIST.ID.LIST SETTING CUS.POS ELSE
    Y.HIST.ID.LIST:=FM:Y.CUST.ID
    END
    RETURN
*** </region>

    END


