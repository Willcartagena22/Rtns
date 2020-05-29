*-----------------------------------------------------------------------------
* <Rating>-75</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.AML.LOG.WRITE(Y.ARR,Y.LOG.ID)
*-----------------------------------------------------------------------------
** Company Name:  BANCO AZUL
* Developed By:   Praveen - Capgemini 
* Date  :         2014/07/21
*------------------------------------------------
* Subroutine Type: API
* Attached to:
* Attached as:     Call routine used by other subroutines
* Primary Purpose: To write the records in SLV.AML.ALERT.LOG
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Aravindhan 20141107	 Call to logic for updating concat file SLV.AML.TRANS.IDS
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SLV.AML.ALERT.LOG 
    $INSERT I_F.SLV.AML.TRANS.IDS  
    $INSERT I_F.LOCKING

    GOSUB INIT
    GOSUB PROCESS

    RETURN
*-----------------------------------------------------------------------------
INIT:
*****
**Initialises the variables

    FN.SLV.AML.ALERT.LOG = 'F.SLV.AML.ALERT.LOG'
    F.SLV.AML.ALERT.LOG = ''
    CALL OPF(FN.SLV.AML.ALERT.LOG,F.SLV.AML.ALERT.LOG)

    FN.LOCKING = 'F.LOCKING'
    F.LOCKING = ''
    CALL OPF(FN.LOCKING,F.LOCKING)

    LOCKING.KEY = ''
*** CR-28 Starts ***
    FN.SLV.AML.TRANS.IDS = 'F.SLV.AML.TRANS.IDS'
    F.SLV.AML.TRANS.IDS = ''
    CALL OPF(FN.SLV.AML.TRANS.IDS,F.SLV.AML.TRANS.IDS)
*** CR-28 Ends ***
    RETURN
*------------------------------------------------------------------------------------
PROCESS:
********
**Writes into the SLV.AML.ALERT.LOG table.


    Y.DATE = FIELD(Y.ARR,'-',1)
    Y.TIME = FIELD(Y.ARR,'-',2)
    Y.BRANCH = FIELD(Y.ARR,'-',3)
    Y.ALR.ID = FIELD(Y.ARR,'-',4)
    Y.ALR.USER = FIELD(Y.ARR,'-',5)
    Y.CONTRACT.ID = FIELD(Y.ARR,'-',6)
    Y.CUSTOMER = FIELD(Y.ARR,'-',7)
    Y.VERSION = FIELD(Y.ARR,'-',8)

    Y.DOC.TYPE = FIELD(Y.ARR,'-',9)
    Y.DOC.NUMBER = FIELD(Y.ARR,'-',10)
*** CR-28 Starts ***
    Y.GROUP.CODE = FIELD(Y.ARR,'-',11)
    Y.ACC.NO = FIELD(Y.ARR,'-',12)
    Y.TRANS.ID = FIELD(Y.ARR,'-',13)
    Y.ALR.DAILY.HIS.IND = FIELD(Y.ARR,'-',14)
    Y.ALR.DEB.CRE.IND = FIELD(Y.ARR,'-',15)
*** CR-28 Ends ***
    Y.JUL.DATE = ICONV(Y.DATE,'D')

*    IF LOCAL8 EQ '' THEN
*        LOCAL8 = '00001'
*        OUT.ARG = FMT(LOCAL8,'R%5')
*    END ELSE
*           LOCAL8 = LOCAL8 + 1
*        OUT.ARG = FMT(LOCAL8,'R%5')


    GOSUB UPD.LOCKING
    RETURN
*-------------------------------------------------------------------------------
READ.LOCKING:
************
    READU R.LOCKING FROM F.LOCKING,LOCKING.KEY THEN
    END
    RETURN
*----------------------------------------------------------------------------------------
UPD.LOCKING:
*********
    LOCKING.KEY='SLV.AML.ALERT.LOG'
    GOSUB READ.LOCKING
    LOCK.CONTENT = R.LOCKING<EB.LOK.CONTENT>
    IF LOCK.CONTENT EQ '' THEN
        R.LOCKING<EB.LOK.CONTENT>="1"
        R.LOCKING<EB.LOK.CONTENT>=FMT(R.LOCKING<EB.LOK.CONTENT>,"5'0'R")
        GOSUB WRITE.LOCK
        Y.ID='AL':Y.JUL.DATE:'00001'
        GOSUB WRITE.LOG
        RETURN
    END
    Y.SEQ.NO= R.LOCKING<EB.LOK.CONTENT>
    Y.SEQ.NO++
    Y.SEQ.NO=FMT(Y.SEQ.NO,"5'0'R")
    R.LOCKING<EB.LOK.CONTENT>=Y.SEQ.NO
    GOSUB WRITE.LOCK
    Y.ID='AL':Y.JUL.DATE:Y.SEQ.NO
    GOSUB WRITE.LOG
    RETURN
*----------------------------------------------------------------------------------------------------
WRITE.LOCK:
**********
    WRITE R.LOCKING TO F.LOCKING,LOCKING.KEY

        RETURN
        *-------------------------------------------------------------------------------------------------------------
WRITE.LOG:
*********
        *        Y.ID = 'AL':Y.JUL.DATE:OUT.ARG

        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.DATE> = Y.DATE
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.TIME> = Y.TIME
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.BRANCH> = Y.BRANCH
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.ID> = Y.ALR.ID
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.USR> = Y.ALR.USER
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.CONTRACT> = Y.CONTRACT.ID
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.CUSTOMER> = Y.CUSTOMER
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.VERSION> = Y.VERSION
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.DOC.TYPE> = Y.DOC.TYPE
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.DOC.NUMBER> = Y.DOC.NUMBER
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.GEN.INFO> = ''
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.TXN.GROUP> = Y.GROUP.CODE
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.ACCOUNT> = Y.ACC.NO
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.DAILY.HIS.IND> = Y.ALR.DAILY.HIS.IND
        R.SLV.AML.ALERT.LOG<SLV.ALERT.ALR.DEB.CRE.IND> = Y.ALR.DEB.CRE.IND
        CALL F.WRITE(FN.SLV.AML.ALERT.LOG,Y.ID,R.SLV.AML.ALERT.LOG)
        Y.LOG.ID = Y.ID
*** CR-28 Starts ***

        IF Y.TRANS.ID THEN
            CHANGE VM TO FM IN Y.TRANS.ID
            CALL F.WRITE(FN.SLV.AML.TRANS.IDS,Y.LOG.ID,Y.TRANS.ID)
        END
*** CR-28 Ends ***
        RETURN

    END
