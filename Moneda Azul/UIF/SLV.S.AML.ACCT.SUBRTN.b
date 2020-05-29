*-----------------------------------------------------------------------------
* <Rating>252</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.S.AML.ACCT.SUBRTN(APPL.TYPE,OPERATION.TYPE,STMT.ENT.ID,STMT.RECORD)
*-----------------------------------------------------------------------------
**Company Name:  BANCO AZUL
* Developed By:  Praveen - Capgemini.
* Date  :        2014/07/21
*------------------------------------------------
* Subroutine Type:
* Attached to:     ACCOUNTING.SUBRTN associate ACCOUNT.PARAMETER.
* Attached as:     N/A
* Primary Purpose: Routine responsible for assigned at the entrance of STMT.ENTRY and STMT.ENTRY.DETAIL the transaction group to which this movement
*                    associate (field local LF.AML.GRP.TXN) and on the other hand to register the movement of this group in the table of daily movements.
*-----------------------------------------------------------------------------
* Modification History :
* CYepez	  20141031	Analysis if category is allowed. Change Request 1 - AML
* Aravindhan  20141121  Cash or Non-cash flag has been included in ID format of SLV.AML.MOVS.TODAY
* CYepez	  20150312	Consideration for inter branches
* CYepez      20150317	PACS00446517   Exclude internal account for population of SLV.AML.MOVS.HIST.ACC
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.ACCOUNT
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.SLV.AML.TRANSACTION.GROUP
    $INSERT I_F.SLV.AML.ALERT.PROFILE
    $INSERT I_F.SLV.AML.MOVS.TODAY
    $INSERT I_F.SLV.AML.STMT.MOVS.CNT


    GOSUB INIT
    GOSUB GET.LOC.REF.FIELDS
    GOSUB PROCESS

    RETURN
*----------------------------------------------------------------------------
INIT:
****
* Initialises the variables

    FN.TT = 'F.TELLER$NAU'
    F.TT = ''
    CALL OPF(FN.TT,F.TT)

    FN.FT = 'F.FUNDS.TRANSFER$NAU'
    F.FT = ''
    CALL OPF(FN.FT,F.FT)

    FN.ALERT= 'F.SLV.AML.ALERT.PROFILE'
    F.ALERT = ''
    CALL OPF(FN.ALERT,F.ALERT)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.STMT.ENTRY = 'F.STMT.ENTRY'
    F.STMT.ENTRY = ''
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)

    FN.SLV.AML.TRANSACTION.GROUP = 'F.SLV.AML.TRANSACTION.GROUP'
    F.SLV.AML.TRANSACTION.GROUP = ''
    CALL OPF(FN.SLV.AML.TRANSACTION.GROUP,F.SLV.AML.TRANSACTION.GROUP)


    FN.SLV.AML.MOVS.TODAY = 'F.SLV.AML.MOVS.TODAY'
    F.SLV.AML.MOVS.TODAY = ''
    CALL OPF(FN.SLV.AML.MOVS.TODAY,F.SLV.AML.MOVS.TODAY)

    FN.SLV.AML.STMT.MOVS.CNT = 'F.SLV.AML.STMT.MOVS.CNT'
    F.SLV.AML.STMT.MOVS.CNT = ''
    CALL OPF(FN.SLV.AML.STMT.MOVS.CNT,F.SLV.AML.STMT.MOVS.CNT)

*CYepez 2015/03/17 Start
    R.SLV.AML.TXN.GROUP = ''
    ERR.SLV.AML.TXN.GROUP = ''
    TXN.GRP.POS = ''
    Y.APPLY.INT.ACCT = ''
    Y.NARR.INT = ''
*CYepez 2015/03/17 End

    RETURN
*-----------------------------------------------------------------------------
GET.LOC.REF.FIELDS:
******************
**Local Reference field position is retrived.

    APPL.NAME   = 'STMT.ENTRY'
    FIELD.NAME  = 'LF.AML.GRP.TXN'
    FIELD.POS   = ''

    CALL MULTI.GET.LOC.REF(APPL.NAME,FIELD.NAME,FIELD.POS)
    Y.LF.TXN.GROUP.POS = FIELD.POS<1,1>

    RETURN
*-----------------------------------------------------------------------------
PROCESS:
********

    Y.ACC.NO = STMT.RECORD<AC.STE.ACCOUNT.NUMBER>
    CALL F.READ(FN.ACCOUNT,Y.ACC.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    IF R.ACCOUNT THEN
        Y.CUS = R.ACCOUNT<AC.CUSTOMER>
        Y.CAT = R.ACCOUNT<AC.CATEGORY>
    END


*CYepez 2015/03/17 Start
    IF Y.CUS EQ "" AND Y.CAT NE "" THEN
        CALL CACHE.READ(FN.SLV.AML.TRANSACTION.GROUP,"ABONO.CTA.INTERNA",R.SLV.AML.TXN.GROUP,ERR.SLV.AML.TXN.GROUP)
        IF R.SLV.AML.TXN.GROUP THEN
            LOCATE Y.CAT IN R.SLV.AML.TXN.GROUP<SLV.TRANS.CATEGORY,1> SETTING TXN.GRP.POS THEN
            Y.APPLY.INT.ACCT = "1"
        END
    END
    END
*   IF Y.CUS THEN
*CYepez 2015/03/17 End


    IF Y.CUS OR Y.APPLY.INT.ACCT THEN
        Y.TXN.ID = STMT.RECORD<AC.STE.TRANS.REFERENCE>

        *CYepez 20150312 start
        Y.TXN.ID.COMP = ''
        Y.TXN.ID.COMP = DCOUNT(Y.TXN.ID,'\') ;* To get transaction id if other branch
        IF Y.TXN.ID.COMP GT 1 THEN
            Y.TXN.ID = FIELD(Y.TXN.ID,'\',1)
        END
        *CYepez 20150312 end

        APPLICATION.ID = STMT.RECORD<AC.STE.SYSTEM.ID>
        Y.AMT.LCY = STMT.RECORD<AC.STE.AMOUNT.LCY>
        IF Y.AMT.LCY LT 0 THEN
            DEB.CRED = 'DEBITO'
        END ELSE
            DEB.CRED = 'CREDITO'
        END
        Y.AMT.LCY = ABS(Y.AMT.LCY)

        *CYepez 2015/03/17 Start
        IF Y.APPLY.INT.ACCT THEN
            IF APPLICATION.ID EQ 'TT' THEN
                Y.NARR.INT = R.NEW(TT.TE.NARRATIVE.1)
                Y.CUS = FIELD(Y.NARR.INT,'-',2)
            END
        END
        *CYepez 2015/03/17 Start

        BEGIN CASE
            CASE APPLICATION.ID EQ 'FT'

                TXN.CODE = R.NEW(FT.TRANSACTION.TYPE)
                Y.REC.STATUS = R.NEW(FT.RECORD.STATUS)
                Y.STMT.NO = R.NEW(FT.STMT.NOS)
                Y.STMT.ID = FIELD(Y.STMT.NO,'.',1)

            CASE APPLICATION.ID EQ 'TT'

                TXN.CODE = R.NEW(TT.TE.TRANSACTION.CODE)
                Y.REC.STATUS = R.NEW(TT.TE.RECORD.STATUS)
                Y.STMT.NO = R.NEW(TT.TE.STMT.NO)
                Y.STMT.ID = FIELD(Y.STMT.NO,'.',1)

            CASE APPLICATION.ID NE 'FT' AND APPLICATION.ID NE 'TT'

                TXN.CODE = STMT.RECORD<AC.STE.TRANSACTION.CODE>

        END CASE
        CALL SLV.S.AML.GET.TXNGROUP(TXN.CODE,APPLICATION.ID,DEB.CRED,GROUP.CODE)
        IF GROUP.CODE NE '' THEN
            GOSUB STMT.WRITE
        END
    END
    RETURN
*-------------------------------------------------------------------------------
STMT.WRITE:
**********
**Writes the GROUP.CODE into the local reference field

    LOOP
        REMOVE Y.GROUP.CODE FROM GROUP.CODE SETTING POS4
    WHILE Y.GROUP.CODE:POS4

        *CY 20141031
        CALL F.READ(FN.SLV.AML.TRANSACTION.GROUP,Y.GROUP.CODE,R.SLV.AML.TRANSACTION.GROUP,F.SLV.AML.TRANSACTION.GROUP,ERR.SLV.AML.TRANSACTION.GROUP)
        Y.CATEGORIES = ''
        Y.FLAG.CAT = ''
        Y.CATEGORIES = R.SLV.AML.TRANSACTION.GROUP<SLV.TRANS.CATEGORY>

        GOSUB CHECK.CAT

        IF Y.FLAG.CAT EQ '' THEN

            *CYepez 2015/03/17
            *IF APPL.TYPE EQ "STMT" THEN
            IF APPL.TYPE EQ "STMT" AND Y.APPLY.INT.ACCT EQ '' THEN
                Y.LF.TXN.GROUP = STMT.RECORD<AC.STE.LOCAL.REF,Y.LF.TXN.GROUP.POS>
                IF Y.LF.TXN.GROUP EQ '' THEN
                    STMT.RECORD<AC.STE.LOCAL.REF,Y.LF.TXN.GROUP.POS> = Y.GROUP.CODE
                    CALL F.WRITE(FN.STMT.ENTRY,STMT.ENT.ID,STMT.RECORD)
                END ELSE
                    STMT.RECORD<AC.STE.LOCAL.REF,Y.LF.TXN.GROUP.POS> :=SM: Y.GROUP.CODE
                    CALL F.WRITE(FN.STMT.ENTRY,STMT.ENT.ID,STMT.RECORD)
                END
            END
            Y.DATE = TIME()
            Y.TIME = OCONV(Y.DATE,'MT')
*** CR028 Starts ***

            Y.CASH.FLAG = '' ; Y.TXN.ID.CASH.CHK = ''
            Y.TXN.ID.CASH.CHK = Y.TXN.ID
            Y.OUT.UIF.TXN = ''
            Y.READ = ''

            CALL SLV.S.AML.CHK.CASH.OTR(Y.TXN.ID.CASH.CHK,Y.READ,Y.OUT.UIF.TXN,Y.AMOUNT)
            IF Y.TXN.ID.CASH.CHK EQ 'OTR' THEN
                Y.CASH.FLAG = 'O'
            END
            IF Y.TXN.ID.CASH.CHK EQ 'CASH' THEN
                Y.CASH.FLAG = 'C'
            END

            Y.IN.CONTRACT = Y.TXN.ID
            Y.IN.TYPE = Y.OUT.UIF.TXN
            Y.IN.ACCT = Y.ACC.NO
            Y.IN.READ = ''
            Y.OUT.APPLY = ''
            Y.OUT.TYPE = ''
            Y.DEB.CRE.IND = ''
            CALL SLV.AML.APPLY.UIF(Y.IN.TYPE,Y.IN.ACCT,Y.IN.CONTRACT,Y.IN.READ,Y.OUT.APPLY,Y.OUT.TYPE)
            IF Y.OUT.APPLY EQ 'Y' THEN
                Y.DEB.CRE.IND = Y.OUT.TYPE
            END
            *Y.SLV.AML.MOVS.ID = Y.CUS:'-':Y.GROUP.CODE:'-':Y.TXN.ID:'-':Y.AMT.LCY:'-':Y.TIME
            Y.SLV.AML.MOVS.ID = Y.CUS:'-':Y.GROUP.CODE:'-':Y.TXN.ID:'-':Y.AMT.LCY:'-':Y.TIME:'-':Y.CASH.FLAG:'-':Y.DEB.CRE.IND
*** CR028 Ends ***
            R.SLV.AML.MOVS.TODAY<SLV.MOVS.STMT.ENTRY.REF> = STMT.ENT.ID
            R.SLV.AML.MOVS.TODAY<SLV.MOVS.ACCOUNT.REF> = Y.ACC.NO

            IF V$FUNCTION EQ 'R' OR Y.REC.STATUS EQ 'RNAU' THEN

                SEL.CMD = "SELECT ":FN.SLV.AML.MOVS.TODAY:" WITH @ID LIKE ...":Y.TXN.ID:"..."
                CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.SEL)
                GOSUB DELETE.REC
            END ELSE
                CALL F.WRITE(FN.SLV.AML.MOVS.TODAY,Y.SLV.AML.MOVS.ID,R.SLV.AML.MOVS.TODAY)
                GOSUB ENTRIES.CNT
            END
        END
    REPEAT
    RETURN
*---------------------------------------------------------------------------
DELETE.REC:
**********
    LOOP
        REMOVE Y.MOVS.ID FROM SEL.LIST SETTING Y.POS
    WHILE Y.MOVS.ID:Y.POS
        CALL F.DELETE(FN.SLV.AML.MOVS.TODAY,Y.MOVS.ID)
    REPEAT
    GOSUB DELETE.CNT
    RETURN
*----------------------------------------------------------------------------
ENTRIES.CNT:
***********
**Writes into SLV.AML.STMT.MOVS.CNT

    CALL F.READ(FN.SLV.AML.STMT.MOVS.CNT,STMT.ENT.ID,R.SLV.AML.STMT.MOVS.CNT,F.SLV.AML.STMT.MOVS.CNT,ERR.SLV.AML.STMT.MOVS.CNT)
    IF R.SLV.AML.STMT.MOVS.CNT THEN
        Y.TODAY.REF = R.SLV.AML.STMT.MOVS.CNT<SLV.STMT.MOVS.TODAY.REF>
        LOCATE Y.SLV.AML.MOVS.ID IN Y.TODAY.REF<1,1> SETTING Y.POS ELSE
        GOSUB WRITE.NEW.TXN
    END
    END ELSE

    GOSUB WRITE.TXN
    END
    RETURN
*---------------------------------------------------------------------------
WRITE.NEW.TXN:
*************
    IF V$FUNCTION NE 'R' THEN
        R.SLV.AML.STMT.MOVS.CNT<SLV.STMT.MOVS.TODAY.REF> :=FM: Y.SLV.AML.MOVS.ID
        CALL F.WRITE(FN.SLV.AML.STMT.MOVS.CNT,STMT.ENT.ID,R.SLV.AML.STMT.MOVS.CNT)
    END ELSE
        GOSUB DELETE.CNT
    END
    RETURN
*-------------------------------------------------------------------------
WRITE.TXN:
*********
    IF V$FUNCTION NE 'R' THEN
        R.SLV.AML.STMT.MOVS.CNT<SLV.STMT.MOVS.TODAY.REF> = Y.SLV.AML.MOVS.ID
        CALL F.WRITE(FN.SLV.AML.STMT.MOVS.CNT,STMT.ENT.ID,R.SLV.AML.STMT.MOVS.CNT)
    END ELSE
        GOSUB DELETE.CNT
    END
    RETURN
*----------------------------------------------------------------------------
DELETE.CNT:
**********
    SEL.CMD = "SELECT ":FN.SLV.AML.STMT.MOVS.CNT:" WITH MOVS.TODAY.REF LIKE ...":Y.TXN.ID:"..."
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.SEL)
    LOOP
        REMOVE Y.CNT.ID FROM SEL.LIST SETTING Y.POS
    WHILE Y.CNT.ID:Y.POS
        CALL F.DELETE(FN.SLV.AML.STMT.MOVS.CNT,Y.CNT.ID)
    REPEAT
    RETURN

* CY 20141031
*-----------------------------------------------------------------------------------
CHECK.CAT:
*********
*Checks if it is restricted for a specific category

    IF Y.CATEGORIES EQ '' THEN
        Y.FLAG.CAT = ''
        RETURN
    END

    LOCATE Y.CAT IN R.SLV.AML.TRANSACTION.GROUP<SLV.TRANS.CATEGORY,1> SETTING POS THEN
    Y.FLAG.CAT = ''
    END ELSE
    Y.FLAG.CAT = 'S'
    END

    RETURN

    END
