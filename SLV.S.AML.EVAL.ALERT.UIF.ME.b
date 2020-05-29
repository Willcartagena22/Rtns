*-----------------------------------------------------------------------------
* <Rating>1</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.S.AML.EVAL.ALERT.UIF.ME(GROUP.TXN,ACCT.NO,CUSTOMER.NO,AMOUNT,ADDIT.INFO,RESULT,ADDIT.MSG)
**-----------------------------------------------------------------------------
* Company Name:  BANCO AZUL
* Developed By:  eurias
* Date  :        2016/03/29
*------------------------------------------------
* Subroutine Type: EVALUTION.RTN.
* Attached to:     SLV.AML.ALERT.PROFILE>UIF.MEDIOS.ELECTRO
* Attached as:     N/A.
* Primary Purpose: Routine responsible for whether in a single or cumulative transaction in one day parameterized for UIF value has been exceeded on electronics channels.
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SLV.AML.CUS.EXENTOUIF
    $INSERT I_F.SLV.AML.PROFILE.PARAMETER
    $INSERT I_F.SLV.AML.MOVS.TODAY
    $INSERT I_F.CUSTOMER

    GOSUB INIT

    RETURN
*-----------------------------------------------------------------------------
INIT:
*****
*Initialises the variables

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

    Y.TOT.AMT = AMOUNT
    Y.POS = ''

    Y.TRANS.ID.CASH.CHK = ID.NEW
    Y.READ = ''
    Y.OUT.UIF.TXN = ''
    Y.AMOUNT = ''
    
    CALL SLV.S.AML.CHK.CASH.OTR(Y.TRANS.ID.CASH.CHK,Y.READ,Y.OUT.UIF.TXN,Y.AMOUNT)
    IF Y.TRANS.ID.CASH.CHK EQ 'OTR' THEN
        Y.TRANS.ID.CASH.CHK = ''
        Y.IN.CONTRACT = ID.NEW
        Y.IN.TYPE = Y.OUT.UIF.TXN
        Y.IN.ACCT = ACCT.NO
        Y.IN.READ = ''
        Y.OUT.APPLY = ''
        Y.OUT.TYPE = ''
        Y.DEB.CRE.IND = ''
        CALL SLV.AML.APPLY.UIF(Y.IN.TYPE,Y.IN.ACCT,Y.IN.CONTRACT,Y.IN.READ,Y.OUT.APPLY,Y.OUT.TYPE)
        IF Y.OUT.TYPE AND Y.OUT.APPLY EQ 'Y' THEN
            Y.DEB.CRE.IND = Y.OUT.TYPE
            ADDIT.MSG<2>= 'D'
            ADDIT.MSG<3>= Y.DEB.CRE.IND
            GOSUB PROCESS
        END
    END
    RETURN
*-----------------------------------------------------------------------------
PROCESS:
*******
    CALL F.READ(FN.SLV.AML.CUS.EXENTOUIF,CUSTOMER.NO,R.SLV.AML.CUS.EXENTOUIF,F.SLV.AML.CUS.EXENTOUIF,ERR.SLV.AML.CUS.EXENTOUIF)
    IF ERR.SLV.AML.CUS.EXENTOUIF NE '' THEN
        CALL CACHE.READ(FN.SLV.AML.PROFILE.PARAMETER,'SYSTEM',R.SLV.AML.PROFILE.PARAMETER,ERR.SLV.AML.PROFILE.PARAMETER)
        IF R.SLV.AML.PROFILE.PARAMETER THEN
        ;*eurias obtener el monto max para uif me
		CALL GET.LOC.REF('SLV.AML.PROFILE.PARAMETER', 'LF.AML.MAX.ME', POS.LF.MX)        
        	Y.MAX.AMT = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.LOCAL.REF,POS.LF.MX>
            ;*Y.MAX.AMT = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.MAX.UIF.AMOUNT.OTR>
            Y.PRODUCT = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.PRODUCT>
        END
        AMOUNT = ABS(AMOUNT)
        IF AMOUNT GT Y.MAX.AMT THEN
            RESULT = 'S'
        END ELSE
*            GOSUB GROUPTXN.CHECK
*            GOSUB READ.CUSTOMER
        END
*santhosh 5th Feb
 GOSUB READ.CUSTOMER
    END

    RETURN
*-----------------------------------------------------------------------------
GROUPTXN.CHECK:
**************
    Y.PRODUCT.CNT = DCOUNT(Y.PRODUCT,VM)
    Y.LF.CNT = 1
    LOOP
    WHILE Y.LF.CNT LE Y.PRODUCT.CNT
        Y.PRODUCT.ID = Y.PRODUCT<1,Y.LF.CNT>
        Y.GRP.TXN.ID = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.TXN.GROUP.ID,Y.LF.CNT>

        FINDSTR GROUP.TXN IN Y.GRP.TXN.ID SETTING Y.FIELD, Y.VALUE THEN

            Y.GROUP.ID = Y.GRP.TXN.ID
        END
        Y.LF.CNT++
    REPEAT
    RETURN
*-------------------------------------------------------------------------------------
READ.CUSTOMER:
*************
	Y.TOT.AMT = ABS(Y.TOT.AMT)
    Y.VAR1 = CUSTOMER.NO ; Y.TRANS.ID = '' ;  TRANS.ID.ARR = ''
    SEL.CMD = "SELECT ":FN.SLV.AML.MOVS.TODAY :" WITH @ID LIKE ...":Y.VAR1:"..."
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.SEL)
    LOOP
        REMOVE Y.SLV.ID FROM SEL.LIST SETTING POS1
    WHILE Y.SLV.ID:POS1
        Y.CUST = FIELD(Y.SLV.ID,'-',1)
        Y.AMT = FIELD(Y.SLV.ID,'-',4)
        Y.TRANS.ID.CASH.FLAG = ''
        Y.TRANS.ID = FIELD(Y.SLV.ID,'-',2,2)
        Y.TRANS.ID.CASH.FLAG = FIELD(Y.SLV.ID,'-',6)
        Y.TXN.TYPE = FIELD(Y.SLV.ID,'-',7)
        ;*eurias
        Y.ALERT = FIELD(Y.SLV.ID,'-',2)
        
        ;*EURIAS solo acumular las trx me validando por la alerta
        Y.ALERT.NAME = Y.ALERT
        Y.VAL.ME.CANT.CHAR = DCOUNT(Y.ALERT.NAME,'.')
		Y.VAL.ME = FIELD(Y.ALERT.NAME,'.',Y.VAL.ME.CANT.CHAR)
             
        IF Y.CUST EQ CUSTOMER.NO AND Y.TRANS.ID.CASH.FLAG EQ 'O' AND Y.TXN.TYPE EQ Y.DEB.CRE.IND AND Y.VAL.ME EQ 'ME' THEN
            CHANGE '-' TO '*' IN Y.TRANS.ID
            IF TRANS.ID.ARR EQ '' THEN
                TRANS.ID.ARR = Y.TRANS.ID
            END ELSE
         ;*eurias acumular solo trx de medios electronicos
                IF (Y.ALERT EQ 'ABONO.CTA.AHO.ME') OR (Y.ALERT EQ 'ABONO.CTA.CORR.ME') OR (Y.ALERT EQ 'RETIRO.CTA.AHO.ME') OR (Y.ALERT EQ 'RETIRO.CTA.CORR.ME') OR (Y.ALERT EQ 'PAGO.PRESTAMOS.ME') THEN
	                TRANS.ID.ARR:=FM:Y.TRANS.ID
	            END
            END
			
            Y.TOT.AMT = Y.TOT.AMT + Y.AMT
            IF Y.TOT.AMT GT Y.MAX.AMT THEN
                RESULT = 'S'
                CHANGE FM TO VM IN TRANS.ID.ARR
                ADDIT.MSG<1> = TRANS.ID.ARR
            END ELSE
                RESULT = ''
            END
        END
    REPEAT
    RETURN
    END
