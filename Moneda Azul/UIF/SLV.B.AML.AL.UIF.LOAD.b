*-----------------------------------------------------------------------------
* <Rating>-15</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.B.AML.AL.UIF.LOAD
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
**Company Name:  BANCO AZUL
* Developed By:  Aravindhan - Capgemini
* Date  :        2014/11/24
*------------------------------------------------
* Subroutine Type: BATCH
* Attached to:
* Attached as:     Load routine of Multi threaded batch routine>BNK/SLV.B.AML.AL.UIF
* Primary Purpose: Multi Thread routine to raise an alert
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*    Date              Author               Reference             Comments
* 10-Apr-2015          C.Yepez				PACS00451448 		  Consider debit and credit side analysis for txn code that applies both sides
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_SLV.B.AML.AL.UIF.COMMON
    $INSERT I_F.SLV.AML.PROFILE.PARAMETER
    GOSUB INIT ; *Initialise the new variables
    RETURN
*-----------------------------------------------------------------------------

*** <region name= INIT>
INIT:
*** <desc>Initialise the new variables </desc>
    
    FN.SLV.AML.MOVS.HIST = 'F.SLV.AML.MOVS.HIST'
    F.SLV.AML.MOVS.HIST = ''
    CALL OPF(FN.SLV.AML.MOVS.HIST,F.SLV.AML.MOVS.HIST)

    FN.SLV.AML.ALERT.LOG = 'F.SLV.AML.ALERT.LOG'
    F.SLV.AML.ALERT.LOG = ''
    CALL OPF(FN.SLV.AML.ALERT.LOG,F.SLV.AML.ALERT.LOG)

    *Cyepez 2015/04/09 start PACS00451448
    FN.TELLER = 'F.TELLER'
    F.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)
    
    FN.TELLER$NAU='F.TELLER$NAU'
    F.TELLER$NAU=''
    CALL OPF(FN.TELLER$NAU,F.TELLER$NAU)

	FN.TELLER$HIS='F.TELLER$HIS'
    F.TELLER$HIS=''
    CALL OPF(FN.TELLER$HIS,F.TELLER$HIS)
    
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
	*Cyepez 2015/04/09 end

    Y.START.DATE = BATCH.DETAILS<3,1,1>
    Y.END.DATE = BATCH.DETAILS<3,1,2>
    FN.SLV.AML.PROFILE.PARAMETER = 'F.SLV.AML.PROFILE.PARAMETER'
    R.SLV.AML.PROFILE.PARAMETER = '' ; SLV.AML.PROFILE.PARAMETER.ERR = ''
    CALL CACHE.READ(FN.SLV.AML.PROFILE.PARAMETER,'SYSTEM',R.SLV.AML.PROFILE.PARAMETER,SLV.AML.PROFILE.PARAMETER.ERR)
    IF R.SLV.AML.PROFILE.PARAMETER THEN
        Y.MAX.CASH = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.MAX.UIF.AMOUNT>
        Y.MAX.NONCASH = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.MAX.UIF.AMOUNT.OTR>
    END
    RETURN

*** </region>

    END

