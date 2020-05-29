*-----------------------------------------------------------------------------
* <Rating>-8</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.S.AML.EVAL.CLI.EXEMPT(GROUP.TXN,ACCT.NO,CUSTOMER.NO,AMOUNT,ADDIT.INFO,RESULT,ADDIT.MSG)
*--------------------------------------------------------------------------------------------------------------
* Company Name:  BANCO AZUL
* Developed By:  Praveen - Capgemini
* Date  :        2014/07/22
*------------------------------------------------
* Subroutine Type:  Evaluation Routine
* Attached to:      SLV.AML.ALERT.PROFILE>EXENTOS
* Attached as:      N/A
* Primary Purpose:  Routine responsible for assessing if the client is marked as free and surpassing of journaling in a day or 30 days of deposits.
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*    Date              Author               Reference             Comments
* 07-Nov-2014       Aravindhan B			CR-028		   Logic to get the list of transactions assocaited with alert and pass ids in ADDIT.MSG<1>
* 06-Jan-2014       Aravindhan B			CR-033		   Logic to raise the alert based on account
* 04-Mar-2015		C.Yepez					PACS00444199   Consider more than one local field in the limit
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SLV.AML.CUS.EXENTOUIF
    $INSERT I_F.SLV.AML.PROFILE.PARAMETER
    $INSERT I_F.SLV.AML.MOVS.TODAY
*$INSERT I_F.SLV.AML.MOVS.CONSOL
    $INSERT I_F.SLV.AML.MOVS.CONSOL.ACC
    $INSERT I_F.SLV.AML.MOVS.HIST.ACC
    $INSERT I_F.SLV.AML.MOVS.HIST.CONCAT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT

    GOSUB INIT
    GOSUB PROCESS

    RETURN
*-----------------------------------------------------------------------------
INIT:
*****
**Initialises the variables

    FN.SLV.AML.CUS.EXENTOUIF = 'F.SLV.AML.CUS.EXENTOUIF'
    F.SLV.AML.CUS.EXENTOUIF = ''
    CALL OPF(FN.SLV.AML.CUS.EXENTOUIF,F.SLV.AML.CUS.EXENTOUIF)


    FN.SLV.AML.MOVS.TODAY = 'F.SLV.AML.MOVS.TODAY'
    F.SLV.AML.MOVS.TODAY = ''
    CALL OPF(FN.SLV.AML.MOVS.TODAY,F.SLV.AML.MOVS.TODAY)

    FN.SLV.AML.PROFILE.PARAMETER = 'F.SLV.AML.PROFILE.PARAMETER'
    F.SLV.AML.PROFILE.PARAMETER = ''
    CALL OPF(FN.SLV.AML.PROFILE.PARAMETER,F.SLV.AML.PROFILE.PARAMETER)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
*** CR -33 Starts ***
*    FN.SLV.AML.MOVS.CONSOL = 'F.SLV.AML.MOVS.CONSOL'
*    F.SLV.AML.MOVS.CONSOL = ''
*    CALL OPF(FN.SLV.AML.MOVS.CONSOL,F.SLV.AML.MOVS.CONSOL)

    FN.SLV.AML.MOVS.CONSOL.ACC = 'F.SLV.AML.MOVS.CONSOL.ACC'
    F.SLV.AML.MOVS.CONSOL.ACC = ''
    CALL OPF(FN.SLV.AML.MOVS.CONSOL.ACC,F.SLV.AML.MOVS.CONSOL.ACC)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.SLV.AML.MOVS.HIST.ACC = 'F.SLV.AML.MOVS.HIST.ACC'
    F.SLV.AML.MOVS.HIST.ACC = ''
    CALL OPF(FN.SLV.AML.MOVS.HIST.ACC,F.SLV.AML.MOVS.HIST.ACC)

    FN.SLV.AML.MOVS.HIST.CONCAT = 'F.SLV.AML.MOVS.HIST.CONCAT'
    F.SLV.AML.MOVS.HIST.CONCAT = ''
    CALL OPF(FN.SLV.AML.MOVS.HIST.CONCAT,F.SLV.AML.MOVS.HIST.CONCAT)
*** CR -33 Ends ***

    Y.TRANS.ID.TEMP.ARR = ''
    Y.TOT.AMT = 0
    Y.POS = ''
    RETURN
*------------------------------------------------------------------------------------
PROCESS:
*******
*Gets the EXEMPT value from SLV.AML.PROFILE.PARAMETER

    CALL F.READ(FN.SLV.AML.CUS.EXENTOUIF,CUSTOMER.NO,R.SLV.AML.CUS.EXENTOUIF,F.SLV.AML.CUS.EXENTOUIF,ERR.SLV.AML.CUS.EXENTOUIF)
    IF ERR.SLV.AML.CUS.EXENTOUIF EQ '' THEN
        CALL F.READ(FN.SLV.AML.PROFILE.PARAMETER,'SYSTEM',R.SLV.AML.PROFILE.PARAMETER,F.SLV.AML.PROFILE.PARAMETER,ERR.SLV.AML.PROFILE.PARAMETER)
        IF R.SLV.AML.PROFILE.PARAMETER THEN
            Y.LF.CUSTOMER = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.LF.CUSTOMER>
            Y.PRODUCT = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.PRODUCT>
        END
        Y.PRODUCT.CNT = DCOUNT(Y.PRODUCT,VM)
        Y.LF.CNT = 1
        LOOP
        WHILE Y.LF.CNT LE Y.PRODUCT.CNT
            Y.PRODUCT.ID = Y.PRODUCT<1,Y.LF.CNT>
*** CR -33 Starts ***
            *Y.LF.CUSTOMER.ID = Y.LF.CUSTOMER<1,Y.LF.CNT>
*** CR -33 Ends ***
            Y.GRP.TXN.ID = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.TXN.GROUP.ID,Y.LF.CNT>
            Y.EXEMPT.ID = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.AL.CLI.EXENTO,Y.LF.CNT>
*** CR -33 Starts ***
            *        FINDSTR GROUP.TXN IN Y.GRP.TXN.ID SETTING Y.FIELD, Y.VALUE THEN
            FINDSTR GROUP.TXN IN Y.GRP.TXN.ID SETTING Y.FIELD, Y.VALUE, Y.POS THEN
                *Y.LF.CUSTOMER = Y.LF.CUSTOMER.ID
                *CRYC 2015/03/04
                *Y.LF.CUSTOMER = Y.LF.CUSTOMER<Y.FIELD, Y.VALUE, Y.POS>
                Y.LF.CUSTOMER = Y.LF.CUSTOMER<1, Y.LF.CNT,Y.POS>
*** CR -33 Ends ***
                Y.GROUP.ID = Y.GRP.TXN.ID
                Y.EXEMPT = Y.EXEMPT.ID
                GOSUB GET.LOC.REF.FIELDS
            END
            Y.LF.CNT++
        REPEAT
    END

    IF Y.EXEMPT EQ 'SI' THEN
*** CR -33 Starts ***
        *GOSUB READ.CUSTOMER
        GOSUB READ.ACCOUNT
*** CR -33 Ends ***
    END

    RETURN
*----------------------------------------------------------------------------------------
GET.LOC.REF.FIELDS:
********************
*Local Reference field position is obtained

*CRYC 2015.03.04
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
*Out argument RESULT is passed out based on the amount value.
*** CR-33 Starts ***
**READ.CUSTOMER:
************
*
*    CALL F.READ(FN.CUSTOMER,CUSTOMER.NO,R.CUSTOMER,F.CUSTOMER,ERR.CUSTOMER)
*    IF R.CUSTOMER THEN
*        Y.MAX.LIMIT = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.LF.POS>
*    END
READ.ACCOUNT:
*************
    R.ACCOUNT = '' ; ACCOUNT.ERR = ''
    CALL F.READ(FN.ACCOUNT,ACCT.NO,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)

    IF R.ACCOUNT THEN
        *CRYC 2015.03.04
        IF TWO.FIELD.FLAG EQ '' THEN
            Y.MAX.LIMIT = R.ACCOUNT<AC.LOCAL.REF,Y.LF.POS>
        END  ELSE
            Y.MAX.LIMIT = R.ACCOUNT<AC.LOCAL.REF,FLD1.POS> + R.ACCOUNT<AC.LOCAL.REF,FLD2.POS>
        END
    END
*** CR-33 Ends ***
    AMOUNT = ABS(AMOUNT)
    IF AMOUNT GT Y.MAX.LIMIT THEN
        RESULT = 'S'
    END ELSE
        Y.TOT.AMT = AMOUNT

        Y.VAR1 = CUSTOMER.NO:'-':GROUP.TXN
        SEL.CMD = "SELECT ":FN.SLV.AML.MOVS.TODAY :" WITH @ID LIKE ...":Y.VAR1:"..."
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.SEL)
        IF SEL.LIST NE '' THEN
            GOSUB SELECT.MOVS
        END ELSE
            GOSUB MOVS.CONSOL
        END

    END
    RETURN
*---------------------------------------------------------------------------------------------------
SELECT.MOVS:
************

    LOOP
        REMOVE Y.SLV.ID FROM SEL.LIST SETTING POS1
    WHILE Y.SLV.ID:POS1
*** CR-33 Starts ***
        GOSUB READ.MOVS.TODAY ; *Read the table SLV.AML.MOVS.TODAY and get the account ref
        IF Y.ACCOUNT.REF EQ ACCT.NO THEN
            GOSUB MOVS.TODAY.PROCS ; *
        END
*** CR-33 Ends ***
    REPEAT
    IF RESULT EQ '' THEN
        GOSUB MOVS.CONSOL
    END
    RETURN
*-----------------------------------------------------------------------------------------
MOVS.CONSOL:
***********

*Out argument RESULT is passed out based on the amount value
*** CR-33 Starts ***
*    SEL.CMD1 = "SELECT ":FN.SLV.AML.MOVS.CONSOL :" WITH @ID LIKE ...":CUSTOMER.NO:"..."
*    SEL.CMD1 = "SELECT ":FN.SLV.AML.MOVS.CONSOL.ACC :" WITH @ID LIKE ...":ACCT.NO:"..."
    SEL.CMD1 = "SELECT ":FN.SLV.AML.MOVS.CONSOL.ACC :" WITH @ID EQ ":ACCT.NO
*** CR-33 Ends ***
    CALL EB.READLIST(SEL.CMD1,SEL.LIST1,'',NO.OF.REC,ERR.SEL)
    LOOP
        REMOVE Y.CONSOL.ID FROM SEL.LIST1 SETTING POS1
    WHILE Y.CONSOL.ID:POS1
*** CR-33 Starts ***
        *        CALL F.READ(FN.SLV.AML.MOVS.CONSOL,Y.CONSOL.ID,R.SLV.AML.MOVS.CONSOL,F.SLV.AML.MOVS.CONSOL,ERR.SLV.AML.MOVS.CONSOL)
        *        IF R.SLV.AML.MOVS.CONSOL THEN
        *            Y.GROUP.CONSOL = R.SLV.AML.MOVS.CONSOL<SLV.CONSOL.GROUP.TXN.ID>
        *            Y.AMOUNT.CONSOL = R.SLV.AML.MOVS.CONSOL<SLV.CONSOL.TOTAL.AMOUNT>
        *        END
*** CR-33 Ends ***
        CALL F.READ(FN.SLV.AML.MOVS.CONSOL.ACC,Y.CONSOL.ID,R.SLV.AML.MOVS.CONSOL.ACC,F.SLV.AML.MOVS.CONSOL.ACC,ERR.SLV.AML.MOVS.CONSOL.ACC)
        IF R.SLV.AML.MOVS.CONSOL.ACC THEN
            Y.GROUP.CONSOL = R.SLV.AML.MOVS.CONSOL.ACC<SLV.CONSOL.ACC.GROUP.TXN.ID>
            Y.AMOUNT.CONSOL = R.SLV.AML.MOVS.CONSOL.ACC<SLV.CONSOL.ACC.TOTAL.AMOUNT>
        END
        LOCATE GROUP.TXN IN Y.GROUP.CONSOL<1,1> SETTING POS2 THEN
        Y.AMT1 = Y.AMOUNT.CONSOL<1,POS2>
        Y.TOT.AMT1 = Y.TOT.AMT1 + Y.AMT1
        IF Y.TOT.AMT1 GT Y.MAX.LIMIT THEN
            RESULT = 'S'
*** CR-28 Starts ***
            GOSUB GET.TXN.ID.HIST.TODAY ; *Get the contract ids to pass in the argument TRANS.ID
            CHANGE FM TO VM IN Y.TRANS.ID.TEMP.ARR
            ADDIT.MSG<1> = Y.TRANS.ID.TEMP.ARR
*** CR-28 Ends ***
        END ELSE
            RESULT = ''
        END
    END
    REPEAT
    RETURN
*-----------------------------------------------------------------------------
*** CR-28 Starts ***
*** <region name= GET.TXN.ID.HIST.TODAY>
GET.TXN.ID.HIST.TODAY:
*** <desc>Get the contract ids to pass in the argument TRANS.ID </desc>

    R.SLV.AML.MOVS.HIST.CONCAT = '' ; SLV.AML.MOVS.HIST.CONCAT.ERR = ''
*** CR-33 Starts ***
*CALL F.READ(FN.SLV.AML.MOVS.HIST.CONCAT,CUSTOMER.NO,R.SLV.AML.MOVS.HIST.CONCAT,F.SLV.AML.MOVS.HIST.CONCAT,SLV.AML.MOVS.HIST.CONCAT.ERR)
    CALL F.READ(FN.SLV.AML.MOVS.HIST.CONCAT,ACCT.NO,R.SLV.AML.MOVS.HIST.CONCAT,F.SLV.AML.MOVS.HIST.CONCAT,SLV.AML.MOVS.HIST.CONCAT.ERR)
*** CR-33 Ends ***
    IF R.SLV.AML.MOVS.HIST.CONCAT THEN
        LOOP
            REMOVE Y.MOVS.HIST.ID FROM R.SLV.AML.MOVS.HIST.CONCAT SETTING HIS.POS
        WHILE Y.MOVS.HIST.ID : HIS.POS
*** CR-33 Starts ***
            *            CALL F.READ(FN.SLV.AML.MOVS.HIST,Y.HIST.ID,R.SLV.AML.MOVS.HIST,F.SLV.AML.MOVS.HIST,ERR.SLV.AML.MOVS.HIST)
            *            IF R.SLV.AML.MOVS.HIST THEN
            *                Y.GRP.TXN.ID = R.SLV.AML.MOVS.HIST<SLV.HIST.GROUP.TXN.ID>
            CALL F.READ(FN.SLV.AML.MOVS.HIST.ACC,Y.MOVS.HIST.ID,R.SLV.AML.MOVS.HIST.ACC,F.SLV.AML.MOVS.HIST.ACC,ERR.SLV.AML.MOVS.HIST.ACC)
            IF R.SLV.AML.MOVS.HIST.ACC THEN
                Y.GRP.TXN.ID = R.SLV.AML.MOVS.HIST.ACC<SLV.HIST.ACC.GROUP.TXN.ID>
*** CR-33 Ends ***
                LOCATE GROUP.TXN IN Y.GRP.TXN.ID<1,1> SETTING Y.POS1 THEN
*** CR-33 Starts ***
                *                Y.TRANS.ID.TEMP = R.SLV.AML.MOVS.HIST<SLV.HIST.CONTRACT.ID,Y.POS1>
                Y.TRANS.ID.TEMP = R.SLV.AML.MOVS.HIST.ACC<SLV.HIST.ACC.CONTRACT.ID,Y.POS1>
*** CR-33 Ends ***
                Y.TRANS.ID.TEMP =GROUP.TXN:'*':Y.TRANS.ID.TEMP
            END
            CHANGE SM TO FM IN Y.TRANS.ID.TEMP
            IF Y.TRANS.ID.TEMP.ARR EQ '' THEN
                Y.TRANS.ID.TEMP.ARR = Y.TRANS.ID.TEMP
            END ELSE
                Y.TRANS.ID.TEMP.ARR:=FM:Y.TRANS.ID.TEMP
            END
        END
    REPEAT
    END
    RETURN
*** CR-28 Ends ***

*-----------------------------------------------------------------------------
*** CR-33 Starts ***
*** <region name= READ.MOVS.TODAY>
READ.MOVS.TODAY:
*** <desc>Read the table SLV.AML.MOVS.TODAY and get the account ref </desc>
    R.SLV.AML.MOVS.TODAY = '' ; SLV.AML.MOVS.TODAY.ERR = ''
    CALL F.READ(FN.SLV.AML.MOVS.TODAY,Y.SLV.ID,R.SLV.AML.MOVS.TODAY,F.SLV.AML.MOVS.TODAY,SLV.AML.MOVS.TODAY.ERR)
    IF R.SLV.AML.MOVS.TODAY THEN
        Y.ACCOUNT.REF = R.SLV.AML.MOVS.TODAY<SLV.MOVS.ACCOUNT.REF>
    END
    RETURN
*** </region>
*** CR-33 Ends ***
*-----------------------------------------------------------------------------
*** </region>

*-----------------------------------------------------------------------------

*** <region name= MOVS.TODAY.PROCS>
MOVS.TODAY.PROCS:
*** <desc> </desc>
    Y.AMT = FIELD(Y.SLV.ID,'-',4)
*** CR-28 Starts ***
    Y.TRANS.ID = FIELD(Y.SLV.ID,'-',2,2)
    CHANGE '-' TO '*' IN Y.TRANS.ID
    IF Y.TRANS.ID.TEMP.ARR EQ '' THEN
        Y.TRANS.ID.TEMP.ARR = Y.TRANS.ID
    END ELSE
        Y.TRANS.ID.TEMP.ARR:=FM:Y.TRANS.ID
    END
*** CR-28 Ends ***
    Y.TOT.AMT = Y.TOT.AMT + Y.AMT
    IF Y.TOT.AMT GT Y.MAX.LIMIT THEN
        RESULT = 'S'
*** CR-28 Starts ***
        CHANGE FM TO VM IN Y.TRANS.ID.TEMP.ARR
        ADDIT.MSG<1> = Y.TRANS.ID.TEMP.ARR
*** CR-28 Ends ***
    END ELSE
        RESULT = ''
    END
    RETURN
*** </region>

    END

