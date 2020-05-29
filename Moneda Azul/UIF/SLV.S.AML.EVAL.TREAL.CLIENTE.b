*-----------------------------------------------------------------------------
* <Rating>-101</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.S.AML.EVAL.TREAL.CLIENTE(GROUP.TXN,ACCT.NO,CUSTOMER.NO,AMOUNT,ADDIT.INFO,RESULT,ADDIT.MSG)
*----------------------------------------------------------------------------------------------------------------
**Company Name:  BANCO AZUL
* Developed By:  Praveen - Capgemini
* Date  :        2014/07/21
*------------------------------------------------
* Subroutine Type: EVALUTION.RTN.
* Attached to:     SLV.AML.ALERT.PROFILE TIEMPO.REAL.CLIENTE
* Attached as:     N/A
* Primary Purpose: Routine responsible for assess whether customer in a transaction individually
*                  or accumulated in a day has exceeded the amount the customer established in KYC limit.
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*    Date              Author               Reference             Comments
* 07-Nov-2014       Aravindhan B			CR-028		   Logic to get the list of transactions assocaited with alert and pass ids in ADDIT.MSG<1>
* 06-Jan-2014       Aravindhan B			CR-033		   Logic to raise the alert based on account
* 04-Mar-2015		CYepez					PACS00444199   Consider more than one local field in limit calculation
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
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
*** CR -33 Ends ***
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
*** CR -33 Starts ***
        *FINDSTR GROUP.TXN IN Y.GRP.TXN.ID SETTING Y.FIELD, Y.VALUE THEN
        FINDSTR GROUP.TXN IN Y.GRP.TXN.ID SETTING Y.FIELD, Y.VALUE,Y.POS THEN
            *Y.LF.CUSTOMER = Y.LF.CUSTOMER.ID
            *CRYC 2015/03/04
            *Y.LF.CUSTOMER = Y.LF.CUSTOMER<Y.FIELD, Y.VALUE,Y.POS>
            Y.LF.CUSTOMER = Y.LF.CUSTOMER<1, Y.LF.CNT,Y.POS>
*** CR -33 Ends ***
            GOSUB GET.LOC.REF.FIELDS
        END
        Y.LF.CNT++
    REPEAT

*** CR -33 Starts ***
*GOSUB READ.CUSTOMER
    GOSUB READ.ACCOUNT
*** CR -33 Ends ***
    RETURN
*-------------------------------------------------------------------------------
GET.LOC.REF.FIELDS:
********************
**Local Reference field position is fetched
*CRYC 2015/03/04
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
*-----------------------------------------------------------------------------
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
        *CRYC 2015/03/04
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
        *    END ELSE
        *SANTHOSH 6th Feb
        Y.TOT.AMT = AMOUNT
*-----------------------------------------------------------------------------
;*eurias logica para acumular alerta tiempo real cliente, si es me; realizar like en select para tomar en cuenta me
GRUPO.TRX = GROUP.TXN
		IF ((GRUPO.TRX EQ 'ABONO.CTA.AHO.ME') OR (GRUPO.TRX EQ 'ABONO.CTA.AH.CO')) THEN
			GRUPO.TRX = 'ABONO.CTA.AH'
		END
		IF ((GRUPO.TRX EQ 'ABONO.CTA.CORR.ME') OR (GRUPO.TRX EQ 'ABONO.CTA.CORRIENTE')) THEN
			GRUPO.TRX = 'ABONO.CTA.CORR'
		END
		
		IF ((GRUPO.TRX EQ 'RETIRO.CTA.AHO.ME') OR (GRUPO.TRX EQ 'RETIRO.CTA.AH.CO')) THEN
			GRUPO.TRX = 'RETIRO.CTA.AH'
		END
		IF ((GRUPO.TRX EQ 'RETIRO.CTA.CORR.ME') OR (GRUPO.TRX EQ 'RETIRO.CTA.CORRIENTE')) THEN
			GRUPO.TRX = 'RETIRO.CTA.CORR'
		END
*-----------------------------------------------------------------------------

       ;*Y.VAR1 = CUSTOMER.NO:'-':GROUP.TXN
       Y.VAR1 = CUSTOMER.NO:'-':GRUPO.TRX
        SEL.CMD = "SELECT ":FN.SLV.AML.MOVS.TODAY :" WITH @ID LIKE ...":Y.VAR1:"..."
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.SEL)
        LOOP
            REMOVE Y.SLV.ID FROM SEL.LIST SETTING POS1
        WHILE Y.SLV.ID:POS1
*** CR-33 Starts ***
            GOSUB READ.MOVS.TODAY ; *Read the table SLV.AML.MOVS.TODAY and get the account ref
            IF Y.ACCOUNT.REF EQ ACCT.NO THEN
                GOSUB MAIN.PROCESS ; *
            END
*** CR-33 Ends ***
        REPEAT
    END ELSE
        Y.TOT.AMT = AMOUNT
*-----------------------------------------------------------------------------
		;*eurias logica para acumular alerta tiempo real cliente, si es me; realizar like en select para tomar en cuenta me 
		GRUPO.TRX = GROUP.TXN
		IF ((GRUPO.TRX EQ 'ABONO.CTA.AHO.ME') OR (GRUPO.TRX EQ 'ABONO.CTA.AH.CO')) THEN
			GRUPO.TRX = 'ABONO.CTA.AH'
		END
		IF ((GRUPO.TRX EQ 'ABONO.CTA.CORR.ME') OR (GRUPO.TRX EQ 'ABONO.CTA.CORRIENTE')) THEN
			GRUPO.TRX = 'ABONO.CTA.CORR'
		END
		
		IF ((GRUPO.TRX EQ 'RETIRO.CTA.AHO.ME') OR (GRUPO.TRX EQ 'RETIRO.CTA.AH.CO')) THEN
			GRUPO.TRX = 'RETIRO.CTA.AH'
		END
		IF ((GRUPO.TRX EQ 'RETIRO.CTA.CORR.ME') OR (GRUPO.TRX EQ 'RETIRO.CTA.CORRIENTE')) THEN
			GRUPO.TRX = 'RETIRO.CTA.CORR'
		END
*-----------------------------------------------------------------------------
        ;*Y.VAR1 = CUSTOMER.NO:'-':GROUP.TXN
        Y.VAR1 = CUSTOMER.NO:'-':GRUPO.TRX
        SEL.CMD = "SELECT ":FN.SLV.AML.MOVS.TODAY :" WITH @ID LIKE ...":Y.VAR1:"..."
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.SEL)
        LOOP
            REMOVE Y.SLV.ID FROM SEL.LIST SETTING POS1
        WHILE Y.SLV.ID:POS1
*** CR-33 Starts ***
            GOSUB READ.MOVS.TODAY ; *Read the table SLV.AML.MOVS.TODAY and get the account ref
            IF Y.ACCOUNT.REF EQ ACCT.NO THEN
                GOSUB MAIN.PROCESS ; *
            END
*** CR-33 Ends ***
        REPEAT

    END
    RETURN

*-----------------------------------------------------------------------------

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


*-----------------------------------------------------------------------------

*** <region name= MAIN.PROCESS>
MAIN.PROCESS:
*** <desc> </desc>

    Y.AMT = FIELD(Y.SLV.ID,'-',4)
*** CR-28 Starts ***
    Y.TRANS.ID = FIELD(Y.SLV.ID,'-',2,2)
    CHANGE '-' TO '*' IN Y.TRANS.ID
    IF TRANS.ID.ARR EQ '' THEN
        TRANS.ID.ARR = Y.TRANS.ID
    END ELSE
        TRANS.ID.ARR:=FM:Y.TRANS.ID
    END
*** CR-28 Ends ***
    Y.TOT.AMT = Y.TOT.AMT + Y.AMT
    IF Y.TOT.AMT GT Y.MAX.LIMIT THEN
        RESULT = 'S'
*** CR-28 Starts ***
        CHANGE FM TO VM IN TRANS.ID.ARR
        ADDIT.MSG<1> = TRANS.ID.ARR
*** CR-28 Ends ***

    END ELSE
        RESULT = ''
    END
    RETURN
*** </region>

    END


