*-----------------------------------------------------------------------------
* <Rating>-42</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.S.AML.EVAL.DECL.JURADA(GROUP.TXN,ACCT.NO,CUSTOMER.NO,AMOUNT,ADDIT.INFO,RESULT,ADDIT.MSG)
*-----------------------------------------------------------------------------
*----------------------------------------------------------------------------------------------------------------
**Company Name:  BANCO AZUL
* Developed By:  Aravindhan - Capgemini
* Date  :        2015/01/14
*------------------------------------------------
* Subroutine Type: EVALUTION.RTN.
* Attached to:     SLV.AML.ALERT.PROFILE>
* Attached as:     N/A
* Primary Purpose: Routine responsible for assess whether Account in a transaction individually
*                  exceeded the threshold amount,
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*    Date              Author               Reference             Comments
*  2015/03/04			CYepez				PACS00444199 		  Consider more than one local field in limit calculation
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SLV.AML.PROFILE.PARAMETER
    $INSERT I_F.SLV.AML.MOVS.TODAY
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT

    GOSUB INIT
    GOSUB PROCESS

    RETURN
*-----------------------------------------------------------------------------
INIT:
*****
**Initialises the variables

    FN.SLV.AML.PROFILE.PARAMETER = 'F.SLV.AML.PROFILE.PARAMETER'
    F.SLV.AML.PROFILE.PARAMETER = ''
    CALL OPF(FN.SLV.AML.PROFILE.PARAMETER,F.SLV.AML.PROFILE.PARAMETER)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    Y.POS = ''
    Y.TOT.AMT = 0

    RETURN
*-----------------------------------------------------------------------------
PROCESS:
*******

    CALL CACHE.READ(FN.SLV.AML.PROFILE.PARAMETER,'SYSTEM',R.SLV.AML.PROFILE.PARAMETER,ERR.SLV.AML.PROFILE.PARAMETER)
    IF R.SLV.AML.PROFILE.PARAMETER THEN
        Y.LF.CUSTOMER = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.LF.CUSTOMER>
        Y.PRODUCT = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.PRODUCT>
        Y.TOLERANCE = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.TOLERANCE>
    END
    Y.PRODUCT.CNT = DCOUNT(Y.PRODUCT,VM)
    Y.LF.CNT = 1
    LOOP
    WHILE Y.LF.CNT LE Y.PRODUCT.CNT
        Y.PRODUCT.ID = Y.PRODUCT<1,Y.LF.CNT>
        Y.GRP.TXN.ID = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.TXN.GROUP.ID,Y.LF.CNT>
        FINDSTR GROUP.TXN IN Y.GRP.TXN.ID SETTING Y.FIELD, Y.VALUE,Y.POS THEN
            *CRYC 2015/03/04
            *Y.LF.CUSTOMER = Y.LF.CUSTOMER<Y.FIELD, Y.VALUE,Y.POS>
            Y.LF.CUSTOMER = Y.LF.CUSTOMER<1, Y.LF.CNT,Y.POS>
            GOSUB GET.LOC.REF.FIELDS
        END
        Y.LF.CNT++
    REPEAT

    GOSUB READ.ACCOUNT
    RETURN
*-------------------------------------------------------------------------------
GET.LOC.REF.FIELDS:
********************
*2015/03/04 CRYC
    APPL.NAME   = 'ACCOUNT'
    FIELD.POS   = ''
    TWO.FIELD.FLAG=''
    CHECK.TWO.FIELDS=FIELD(Y.LF.CUSTOMER,",",2)

    IF CHECK.TWO.FIELDS THEN
        CHANGE "," TO VM IN Y.LF.CUSTOMER
        FIELD.NAME  = Y.LF.CUSTOMER
        TWO.FIELD.FLAG='1'
    END ELSE
        FIELD.NAME  = Y.LF.CUSTOMER
    END

    CALL MULTI.GET.LOC.REF(APPL.NAME,FIELD.NAME,FIELD.POS)

    IF TWO.FIELD.FLAG EQ '' THEN
        Y.LF.POS =FIELD.POS<1,1>
    END ELSE
        FLD1.POS=FIELD.POS<1,1>
        FLD2.POS=FIELD.POS<1,2>
    END

    RETURN

READ.ACCOUNT:
*************

    R.ACCOUNT = '' ; ACCOUNT.ERR = ''
    CALL F.READ(FN.ACCOUNT,ACCT.NO,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
*2015/03/04 CRYC
    IF TWO.FIELD.FLAG EQ '' THEN
        Y.MAX.LIMIT = R.ACCOUNT<AC.LOCAL.REF,Y.LF.POS>
    END  ELSE
        Y.MAX.LIMIT = R.ACCOUNT<AC.LOCAL.REF,FLD1.POS> + R.ACCOUNT<AC.LOCAL.REF,FLD2.POS>
    END
    Y.AFFIDAVIT= Y.MAX.LIMIT
    Y.THRES.AMOUNT = Y.AFFIDAVIT + (Y.AFFIDAVIT*Y.TOLERANCE/100)
    AMOUNT = ABS(AMOUNT)
    IF AMOUNT GT Y.THRES.AMOUNT THEN
        RESULT = 'S'
    END
    RETURN

    END
