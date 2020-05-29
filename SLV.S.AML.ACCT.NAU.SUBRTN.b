*-----------------------------------------------------------------------------
* <Rating>-81</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.S.AML.ACCT.NAU.SUBRTN(YFILE,YTYPE,YENTRYID,YENTRY,YOVER) 
*-----------------------------------------------------------------------------
**Company Name:  BANCO AZUL
* Developed By:  Praveen - Capgemini.
* Date  :        2014/07/21.
*------------------------------------------------
* Subroutine Type:
* Attached to:     ACCT.NAU.SUBRTN associate ACCOUNT.PARAMETER.
* Attached as:      N/A
* Primary Purpose: This routine is responsible for identifying the transaction associated with the movement, the Group of transactions to which it belongs, and identify alerts that apply on that group.
*                  Check the alerts and generate them when conditions are met..
*-----------------------------------------------------------------------------
* Modification History :
* CYepez		20141031	Analysis if category is allowed. Change Request 1 - AML
* hpasquel  	20141031    Call to logic for sending mails
* Aravindhan 	20141107	CR-028		   Included one more outgoing argmument TRANS.ID, and logic to get the list of transactions assocaited with alert.
* Aravindhan    20150107    CR-033		   Logic to inlude the Account number in Override.
* CYepez		20150131	PACS00430440   Consideration of internal account for case workaround of DAP
* CYepez		20150204	PACS00438049   Variable ADDIT.MSG need to be assigned to null before calling eval rtn.
* CYepez		20150306    PACS00443729   Customer Name defined according bank requirement
* CYepez		20150319    PACS00446974   Display account number properly for case of workaroung of lending and deposits
* Cyepez		20150601				   Change amount format from R2,#10 to R2,#16
* Cyepez		20150630    PACS00467484   Natural and corporate customer are identified according SEGMENT field	
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.ACCOUNT
    $INSERT I_F.SLV.AML.TRANSACTION.GROUP
    $INSERT I_F.SLV.AML.ALERT.PROFILE
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AA.ACCOUNT
    $INSERT I_F.INDUSTRY
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.AA.PRODUCT
    $INSERT I_F.TELLER.TRANSACTION
    $INSERT I_F.TRANSACTION
* 
    $INCLUDE I_SLV.EMAIL.COMMON 

    IF V$FUNCTION EQ 'I' THEN
        GOSUB INIT
        GOSUB PROCESS
        YOVER = Y.OVERRIDES
    END
    RETURN

*----------------------------------------------------------------------------
INIT:
****
*Initialises the variables

    FN.ALERT= 'F.SLV.AML.ALERT.PROFILE'
    F.ALERT = ''
    CALL OPF(FN.ALERT,F.ALERT)

    FN.SLV.AML.TRANSACTION.GROUP = 'F.SLV.AML.TRANSACTION.GROUP'
    F.SLV.AML.TRANSACTION.GROUP = ''
    CALL OPF(FN.SLV.AML.TRANSACTION.GROUP,F.SLV.AML.TRANSACTION.GROUP)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
*
    FN.SLV.MAIL.MESSAGE = 'F.SLV.MAIL.MESSAGE'
    F.SLV.MAIL.MESSAGE = ''
    CALL OPF(FN.SLV.MAIL.MESSAGE,F.SLV.MAIL.MESSAGE)
*
    FN.AA.ARRANGEMENT='F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT=''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.AA.PRODUCT='F.AA.PRODUCT'
    F.AA.PRODUCT=''
    CALL OPF(FN.AA.PRODUCT,F.AA.PRODUCT)

    FN.FT.TXN.TYPE.CONDITION='F.FT.TXN.TYPE.CONDITION'
    F.FT.TXN.TYPE.CONDITION=''
    CALL OPF(FN.FT.TXN.TYPE.CONDITION,F.FT.TXN.TYPE.CONDITION)

    FN.TELLER.TRANSACTION='F.TELLER.TRANSACTION'
    F.TELLER.TRANSACTION=''
    CALL OPF(FN.TELLER.TRANSACTION,F.TELLER.TRANSACTION)

    FN.TRANSACTION='F.TRANSACTION'
    F.TRANSACTION=''
    CALL OPF(FN.TRANSACTION,F.TRANSACTION)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.INDUSTRY='F.INDUSTRY'
    F.INDUSTRY=''
    CALL OPF(FN.INDUSTRY,F.INDUSTRY)

*
    Y.CUS = ''
    R.SLV.AML.TXN.GROUP = ''
    ERR.SLV.AML.TXN.GROUP = ''
    TXN.GRP.POS = ''
    Y.APPLY.INT.ACCT = ''
    Y.NARR.INT = ''

*PACS00443729 Start
*APPL.NAME   = 'AA.PRD.DES.ACCOUNT'
*FIELD.NAME  = 'LF.AML.DEP.PROY':VM:'LF.AML.RET.PROY'
*FIELD.POS   = ''
    APPL.NAME   = 'AA.PRD.DES.ACCOUNT':FM:'CUSTOMER'
    FIELD.NAME  = 'LF.AML.DEP.PROY':VM:'LF.AML.RET.PROY':FM:'LF.RAZON.SOCIAL':VM:'SEGMENT'
    FIELD.POS   = ''
*PACS00443729 End

    CALL MULTI.GET.LOC.REF(APPL.NAME,FIELD.NAME,FIELD.POS)
    DEP.POS = FIELD.POS<1,1>
    RET.POS=FIELD.POS<1,2>
    LOC.CUS.POS= FIELD.POS<2,1>
    Y.SEG.POS = FIELD.POS<2,2>

    RETURN
*-----------------------------------------------------------------------------
PROCESS:
********
    AA.ID = ''
    Y.ACC.NO = YENTRY<AC.STE.ACCOUNT.NUMBER>
    Y.ACC.MAIL = Y.ACC.NO

    CALL F.READ(FN.ACCOUNT,Y.ACC.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    IF R.ACCOUNT THEN
        Y.CUS = R.ACCOUNT<AC.CUSTOMER>
        * CY
        Y.CAT = R.ACCOUNT<AC.CATEGORY>
    END


	
*PACS00446974 Start
    CALL INT.ACC(Y.ACC.NO,Y.INT.FLG)
    IF Y.INT.FLG THEN
        IF APPLICATION EQ 'TELLER' THEN
            Y.NARR.INT = R.NEW(TT.TE.NARRATIVE.1)
            AA.ID = FIELD(Y.NARR.INT,'-',1)
            Y.ACC.MAIL = AA.ID
        END
    END
*18th Feb - santhosh
*   AA.ID= R.ACCOUNT<AC.ARRANGEMENT.ID> 
*PACS00446974 End

    IF AA.ID EQ '' THEN
        AA.ID= R.ACCOUNT<AC.ARRANGEMENT.ID>
    END

    ArrangementID = AA.ID
    idPropertyClass='ACCOUNT'
    idProperty=''
    effectiveDate=TODAY

    CALL AA.GET.ARRANGEMENT.CONDITIONS(ArrangementID, idPropertyClass, idProperty, effectiveDate, returnIds, returnConditions, returnError)
    ReturnCondition=RAISE(returnConditions)
    DEP.AMT=ReturnCondition<AA.AC.LOCAL.REF><1,DEP.POS>
    RET.AMT=ReturnCondition<AA.AC.LOCAL.REF><1,RET.POS>

    CALL F.READ(FN.AA.ARRANGEMENT,ArrangementID,R.AA.ARRANGEMENT,F.AA.ARRANGEMENT,ARR.ERR)
    PRD.NAME=R.AA.ARRANGEMENT<AA.ARR.PRODUCT>
    PRD.LINE=R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE>

    CALL F.READ(FN.AA.PRODUCT,PRD.NAME,R.AA.PRD,F.AA.PRODUCT,PRD.ERR)
    PRD.DES= R.AA.PRD<AA.PDT.DESCRIPTION>

*CY 20150131 Start
    IF Y.CUS EQ "" AND Y.CAT NE "" THEN
        *CALL F.READ(FN.SLV.AML.TRANSACTION.GROUP,"ABONO.CTA.INTERNA",R.SLV.AML.TXN.GROUP,F.SLV.AML.TRANSACTION.GROUP,ERR.SLV.AML.TXN.GROUP)
        CALL CACHE.READ(FN.SLV.AML.TRANSACTION.GROUP,"ABONO.CTA.INTERNA",R.SLV.AML.TXN.GROUP,ERR.SLV.AML.TXN.GROUP)
        IF R.SLV.AML.TXN.GROUP THEN
            LOCATE Y.CAT IN R.SLV.AML.TXN.GROUP<SLV.TRANS.CATEGORY,1> SETTING TXN.GRP.POS THEN
            Y.APPLY.INT.ACCT = "1"
        END
    END
    END
* IF Y.CUS THEN
*CY 20150131 End

    IF Y.CUS OR Y.APPLY.INT.ACCT THEN
        Y.TXN.ID = YENTRY<AC.STE.TRANS.REFERENCE>
        APPLICATION.ID = YENTRY<AC.STE.SYSTEM.ID>
        Y.AMT.LCY = YENTRY<AC.STE.AMOUNT.LCY>


        IF Y.AMT.LCY LT 0 THEN
            DEB.CRED = 'DEBITO'
        END ELSE
            DEB.CRED = 'CREDITO'
        END

        *CY 20150131 Start
        IF Y.APPLY.INT.ACCT THEN
            IF APPLICATION.ID EQ 'TT' THEN
                Y.NARR.INT = R.NEW(TT.TE.NARRATIVE.1)
                Y.CUS = FIELD(Y.NARR.INT,'-',2)
            END
        END
        *CY 20150131 End

        GOSUB APPL.CHK

        CALL SLV.S.AML.GET.TXNGROUP(TXN.CODE,APPLICATION.ID,DEB.CRED,GROUP.CODE)

        GOSUB TXN.GRP

    END
    RETURN
*---------------------------------------------------------------------------------------------
APPL.CHK:
********
*Application is checked and the TRANSACTION.CODE is obtained

    BEGIN CASE
        CASE APPLICATION.ID EQ 'FT'

            TXN.CODE = R.NEW(FT.TRANSACTION.TYPE)
            *CALL F.READ(FN.FT.TXN.TYPE.CONDITION,TXN.CODE,R.FTTC,F.FT.TXN.TYPE.CONDITION,FTTC.ERR)
            CALL CACHE.READ(FN.FT.TXN.TYPE.CONDITION,TXN.CODE,R.FTTC,FTTC.ERR)
            IF R.FTTC THEN
                FT.TT.DESCRIPTION=R.FTTC<FT6.DESCRIPTION>
            END
        CASE APPLICATION.ID EQ 'TT'

            TXN.CODE = R.NEW(TT.TE.TRANSACTION.CODE)
            *CALL F.READ(FN.TELLER.TRANSACTION,TXN.CODE,R.TT,F.TELLER.TRANSACTION,TT.ERR)
            CALL CACHE.READ(FN.TELLER.TRANSACTION,TXN.CODE,R.TT,TT.ERR)

            FT.TT.DESCRIPTION=R.TT<TT.TR.DESC>

        CASE APPLICATION.ID NE 'FT' AND APPLICATION.ID NE 'TT'
            TXN.CODE = Y.ENTRY<AC.STE.TRANSACTION.CODE>
            *CALL F.READ(FN.TRANSACTION,TXN.CODE,R.TRANS,F.TRANSACTION,TRANS.ERR)
            CALL CACHE.READ(FN.TRANSACTION,TXN.CODE,R.TRANS,TRANS.ERR)
            FT.TT.DESCRIPTION=R.TRANS<AC.TRA.NARRATIVE>

    END CASE
    RETURN
*-------------------------------------------------------------------------------
TXN.GRP:
******
*Reads the SLV.AML.TRANSACTION.GROUP Table.

    LOOP
        REMOVE Y.GROUP.CODE FROM GROUP.CODE SETTING POS3
    WHILE Y.GROUP.CODE:POS3
        CALL F.READ(FN.SLV.AML.TRANSACTION.GROUP,Y.GROUP.CODE,R.SLV.AML.TRANSACTION.GROUP,F.SLV.AML.TRANSACTION.GROUP,ERR.SLV.AML.TRANSACTION.GROUP)

        * CY
        Y.CATEGORIES = ''
        Y.FLAG.CAT = ''

        IF R.SLV.AML.TRANSACTION.GROUP THEN
            * CY
            Y.CATEGORIES = R.SLV.AML.TRANSACTION.GROUP<SLV.TRANS.CATEGORY>
            GOSUB CHECK.CAT
            IF Y.FLAG.CAT EQ '' THEN

                Y.ALERTS.ID = R.SLV.AML.TRANSACTION.GROUP<SLV.TRANS.ALERT>
                Y.ALERT.CNT = DCOUNT(Y.ALERTS.ID,VM)
                Y.CNT = 1
                LOOP
                WHILE Y.CNT LE Y.ALERT.CNT
                    Y.ALERT = Y.ALERTS.ID<1,Y.CNT>
                    GOSUB ALERT.PROFILE
                    Y.CNT++
                REPEAT
            END
        END
    REPEAT
    RETURN
* ----------------------------------------------------------------------------
ALERT.PROFILE:
*************
*Reads the SLV.AML.ALERT.PROFILE Table.
    R.ALERT = ""
    CALL F.READ(FN.ALERT,Y.ALERT,R.ALERT,F.ALERT,ERR.ALERT)
    IF R.ALERT THEN
        Y.ONLINE.FLAG = R.ALERT<SLV.ALERT.ONLINE.FLAG>
        Y.ACTIVE = R.ALERT<SLV.ALERT.ACTIVE>
        Y.DEST.EMAIL = R.ALERT<SLV.ALERT.DEST.EMAIL>
        Y.OVERRIDE.ID = R.ALERT<SLV.ALERT.OVERRIDE.ID>
        Y.LOG.FLAG = R.ALERT<SLV.ALERT.LOG.FLAG>
    END
    
    ;*eurias no lanzar alertas por liof para alertas banca en linea	
	IF(Y.ALERT EQ 'TCIB.NUM.BENEF' OR Y.ALERT EQ 'TCIB.NUM.REMIT' OR Y.ALERT EQ 'TCIB.NUM.FTTEN' OR Y.ALERT EQ 'TCIB.NUM.FTTSA')THEN
	Y.TRANSACTION = YENTRY<AC.STE.TRANSACTION.CODE>
    TRANSACCIONES.LIOF = '259':VM:'550':VM:'551':VM:'552':VM:'553':VM:'554':VM:'555':VM:'558'
   		FIND Y.TRANSACTION IN TRANSACCIONES.LIOF SETTING Ap, Vp THEN
			RETURN;* si es nueva alerta y liof no evaluar movimiento
		END
	END
*** CR 033 -Starts ***
    IF Y.ALERT EQ 'TIEMPO.REAL.CLIENTE' OR Y.ALERT EQ 'EXENTOS' OR Y.ALERT EQ 'CRED.PAG.ANTIC' OR Y.ALERT EQ 'DECLARACION.JURADA' THEN
        Y.OVERRIDE.ID = Y.OVERRIDE.ID:VM:Y.ACC.NO
    END
*** CR 033 -Ends ***
    IF Y.ONLINE.FLAG EQ 'SI' AND Y.ACTIVE EQ 'SI' THEN
        Y.EVAL.RTN = R.ALERT<SLV.ALERT.EVALUATION.RTN>
        Y.EVAL.RTN = FIELD(Y.EVAL.RTN,'@',2)
        GOSUB EVAL.CHK
    END

    RETURN
*---------------------------------------------------------------------------------------------
EVAL.CHK:
********

		
        
*Checks the value of the field EVALUATION.RTN
    IF Y.EVAL.RTN THEN
		
		Y.TRANSACTION = YENTRY<AC.STE.TRANSACTION.CODE>
        TRANSACCIONES.LIOF = '259':VM:'550':VM:'551':VM:'552':VM:'553':VM:'554':VM:'555':VM:'558'
        
        *CYepez	20150204
        ADDIT.MSG = ""
		TAX.FLAG =''
	
	
	FIND Y.TRANSACTION IN TRANSACCIONES.LIOF SETTING Ap, Vp THEN
		montoTrx = ''
		auxLioF = Y.AMT.LCY;* guardar temporal
    	TAX.FLAG ='1'
		IF(APPLICATION.ID EQ 'FT')THEN
		montoTrx = R.NEW(FT.CREDIT.AMOUNT)
		IF montoTrx EQ '' THEN
			montoTrx = R.NEW(FT.DEBIT.AMOUNT)
		END 
		END
		IF(APPLICATION.ID EQ 'TT')THEN
			montoTrx = R.NEW(TT.TE.AMOUNT.LOCAL.1)
			IF montoTrx EQ '' THEN
				montoTrx = R.NEW(TT.TE.AMOUNT.LOCAL.2)
			END 
		END
		Y.AMT.LCY = ABS(Y.AMT.LCY) + montoTrx
	
	 END 
	 
       CALL @Y.EVAL.RTN(Y.GROUP.CODE,Y.ACC.NO,Y.CUS,Y.AMT.LCY,ADDIT.INFO,RESULT,ADDIT.MSG)
   
   FIND Y.TRANSACTION IN TRANSACCIONES.LIOF SETTING Ap, Vp THEN      
		Y.AMT.LCY = auxLioF
		auxLioF =''
   END        
   
*** CR 028 -Starts ***
*-----------------------------------------------------------------------------
;*eurias bloque de tratamiento para liof dejar marca para leer en log alertas si es el monto de cargo liof
*        IF ADDIT.MSG<1> THEN
*            Y.TRANS.IDS = Y.GROUP.CODE:'*':Y.TXN.ID:VM:ADDIT.MSG<1>
*        END ELSE
*            Y.TRANS.IDS = Y.GROUP.CODE:'*':Y.TXN.ID
*        END
;*eurias tratamiento para mostrar monto liof en detalle y crear linea detaller para mostrar monto de operacion
    	
    	MONTO.TAX = YENTRY<AC.STE.AMOUNT.LCY>
       	MONTO.TAX = ABS(MONTO.TAX)
		;*:'|':MONTO.TAX
        IF ADDIT.MSG<1> THEN
    		FIND Y.TRANSACTION IN TRANSACCIONES.LIOF SETTING Ap, Vp THEN ;*si es stmt liof cambiar monto () y generar nueva linea de detalle para log alerta
		      	Y.TRANS.IDS = Y.GROUP.CODE:'*':Y.TXN.ID:VM:Y.GROUP.CODE:'*':Y.TXN.ID:VM:ADDIT.MSG<1>;*se agrega monto liof y linea detalle monto trx  
		    END ELSE   
            	Y.TRANS.IDS = Y.GROUP.CODE:'*':Y.TXN.ID:VM:ADDIT.MSG<1>
            END
        END ELSE
    		FIND Y.TRANSACTION IN TRANSACCIONES.LIOF SETTING Ap, Vp THEN ;*si es stmt liof cambiar monto () y generar nueva linea de detalle para log alerta
            	Y.TRANS.IDS = Y.GROUP.CODE:'*':Y.TXN.ID:VM:Y.GROUP.CODE:'*':Y.TXN.ID;*se agrega monto liof y linea detalle monto trx
            END ELSE 
            	Y.TRANS.IDS = Y.GROUP.CODE:'*':Y.TXN.ID
            END
        END
        CALL SLV.I.CHNL.TXN(Y.LOG.ID,TAX.FLAG)
;*end eurias
        Y.ALR.DAILY.HIS.IND = ADDIT.MSG<2>
        Y.ALR.DEB.CRE.IND = ADDIT.MSG<3>
*** CR 028 -Ends ***
        IF RESULT EQ 'S' THEN
            GOSUB ALERT.CHK
            RESULT = ''
        END
    END
    RETURN
*-----------------------------------------------------------------------------------
ALERT.CHK:
*********
*Populates the required overrides and Writes into SLV.AML.ALERT.LOG

    Y.TODAY = TODAY
    Y.DATE = TIME()
    Y.TIME = OCONV(Y.DATE,'MT')
    Y.COMP = ID.COMPANY
    Y.VERSION = APPLICATION:PGM.VERSION
    IF Y.DEST.EMAIL NE '' THEN
        *Y.AMT.LCY = YENTRY<AC.STE.AMOUNT.LCY>
        *   AML - Y.ALERT - Y.CUS
        R.EMAIL = ""
        R.EMAIL<E_MAIL.ID>         = Y.TXN.ID : "-" : APPLICATION.ID
        R.EMAIL<E_MAIL.TO>         = Y.DEST.EMAIL
        R.EMAIL<E_MAIL.TYPE>       = "html"
        R.EMAIL<E_MAIL.SUBJECT>    = "AML-" :  Y.ALERT : "-CLIENTE:" : Y.CUS
        GOSUB GET.CUST.NAME
        *
        Y.LINE = ""
        Y.SEP = "@vm"
        Y.LINE<-1> = "Fecha:"    : Y.SEP : Y.TODAY
        Y.LINE<-1> = "Hora:"     : Y.SEP : Y.TIME
        Y.LINE<-1> = "Sucursal:" : Y.SEP : Y.COMP
        Y.LINE<-1> = "Alerta: "  : Y.SEP : Y.ALERT
        Y.LINE<-1> = "Usuario:"  : Y.SEP : OPERATOR
        Y.LINE<-1> = "Referencia:" : Y.SEP : Y.TXN.ID
        Y.LINE<-1> = "Cliente:" : Y.SEP : Y.CUS : "-" : Y.CUS.NAME
        Y.LINE<-1> = "Cuenta: " : Y.SEP : Y.ACC.MAIL
        Y.LINE<-1> = "Version:" : Y.SEP : CHANGE(Y.VERSION,",",">")


        Y.LINE<-1> = "Tipo de Producto:" : Y.SEP :PRD.DES

        Y.LINE<-1>="Monto de la transacción :":Y.SEP:FMT(Y.AMT.LCY,"R2,#16")
        Y.LINE<-1>="Nombre de la Transacción:":Y.SEP:FT.TT.DESCRIPTION

        *

        Y.LINE<-1>="Monto Declaración Jurada: "
        IF PRD.LINE NE 'LENDING' THEN
            Y.LINE<-1>="Depósitos": Y.SEP :FMT(DEP.AMT,"R2,#16")
            Y.LINE<-1>="Retiros": Y.SEP :FMT(RET.AMT,"R2,#16")
        END ELSE
            Y.LINE<-1>="Cuota del Crédito": Y.SEP :FMT(DEP.AMT,"R2,#16")
            Y.LINE<-1>="Pago Adicional": Y.SEP :FMT(RET.AMT,"R2,#16")

        END
        Y.LINE<-1>="Actividad Económica:": Y.SEP:INDUSTRY.DES

        Y.ARR = "<table>"
        LOOP
            REMOVE yBodyLine FROM Y.LINE SETTING yPos
        WHILE yBodyLine
            Y.ARR<-1> = "<tr><td>" : CHANGE(yBodyLine, Y.SEP,"</td><td>") : "</td></tr>"
        REPEAT
        Y.ARR<-1> = "</table>"
        Y.ARR = CHANGE(Y.ARR, FM, "")
        *
        R.EMAIL<E_MAIL.BODY>       = Y.ARR
        *
        Y.MAIL.ID = ""
        CALL ALLOCATE.UNIQUE.TIME(Y.MAIL.ID)
        CALL F.WRITE(FN.SLV.MAIL.MESSAGE, Y.MAIL.ID, R.EMAIL)
        *
    END
    IF Y.LOG.FLAG EQ 'SI' THEN
***CR-028 STARTS***
        *Y.ARR = Y.TODAY:'-':Y.TIME:'-':Y.COMP:'-':Y.ALERT:'-':OPERATOR:'-':Y.TXN.ID:'-':Y.CUS:'-':Y.VERSION
        Y.ARR = Y.TODAY:'-':Y.TIME:'-':Y.COMP:'-':Y.ALERT:'-':OPERATOR:'-':Y.TXN.ID:'-':Y.CUS:'-':Y.VERSION:'-':'':'-':'':'-':Y.GROUP.CODE:'-':Y.ACC.NO:'-':Y.TRANS.IDS:'-':Y.ALR.DAILY.HIS.IND:'-':Y.ALR.DEB.CRE.IND
***CR-028 ENDS***

        CALL SLV.AML.LOG.WRITE(Y.ARR,Y.LOG.ID)
        
        
    END
    IF Y.OVERRIDE.ID NE '' THEN
        Y.OVERRIDES<-1> = Y.OVERRIDE.ID
    END
    RETURN

* CY
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
*-----------------------------------------------------------------------------------
GET.CUST.NAME:
*-----------------------------------------------------------------------------------
    R.CUS = ""
    CALL F.READ(FN.CUSTOMER, Y.CUS, R.CUS, F.CUSTOMER, Y.ERR)

*PACS00443729 Start
*Y.CUS.NAME = R.CUS<EB.CUS.NAME.1,1> : " " : R.CUS<EB.CUS.SHORT.NAME,1>
    Y.FIRST.NAME.1 = ""
    Y.LAST.NAME.1 = ""
    Y.FIRST.NAME.2 = ""
    Y.FIRST.NAME.3 = ""
    Y.LAST.NAME.2 = ""
    Y.LAST.NAME.3 = ""
    Y.CORP.NAME = ""
    Y.SEGMENT = ""

    Y.FIRST.NAME.1 = R.CUS<EB.CUS.NAME.1,1>
    Y.LAST.NAME.1 = R.CUS<EB.CUS.TEXT,1>
    Y.FIRST.NAME.2 = R.CUS<EB.CUS.NAME.2,1>
    Y.FIRST.NAME.3 = R.CUS<EB.CUS.GIVEN.NAMES,1>
    Y.LAST.NAME.2 = R.CUS<EB.CUS.FAMILY.NAME,1>
    Y.LAST.NAME.3 = R.CUS<EB.CUS.PREVIOUS.NAME,1>
    Y.CORP.NAME = R.CUS<EB.CUS.LOCAL.REF,LOC.CUS.POS>
    Y.SEGMENT = R.CUS<EB.CUS.LOCAL.REF,Y.SEG.POS>

*PACS00467484
*    IF Y.FIRST.NAME.1 NE "" AND Y.LAST.NAME.1 NE "" THEN
    IF Y.SEGMENT EQ '1' THEN
        * Natural Customer
        Y.CUS.NAME = Y.FIRST.NAME.1
        IF Y.FIRST.NAME.2 NE "" THEN
            Y.CUS.NAME = Y.CUS.NAME : " " : Y.FIRST.NAME.2
        END
        IF Y.FIRST.NAME.3 NE "" THEN
            Y.CUS.NAME = Y.CUS.NAME : " " : Y.FIRST.NAME.3
        END
        Y.CUS.NAME = Y.CUS.NAME : " " : Y.LAST.NAME.1
        IF Y.LAST.NAME.2 NE "" THEN
            Y.CUS.NAME = Y.CUS.NAME : " " : Y.LAST.NAME.2
        END
        IF Y.LAST.NAME.3 NE "" THEN
            Y.CUS.NAME = Y.CUS.NAME : " " :Y.LAST.NAME.3
        END
    END ELSE
        * Corporate Customer
        Y.CUS.NAME = TRIM(Y.CORP.NAME,"", "D")
    END
*PACS00443729 End

    INDUSTRY.NAME=R.CUS<EB.CUS.INDUSTRY>
*CALL F.READ(FN.INDUSTRY,INDUSTRY.NAME,R.INDUSTRY,F.INDUSTRY,INDUS.ERR)
    CALL CACHE.READ(FN.INDUSTRY,INDUSTRY.NAME,R.INDUSTRY,INDUS.ERR)
    IF R.INDUSTRY THEN
        INDUSTRY.DES=R.INDUSTRY<EB.IND.DESCRIPTION><1,1>
    END

    RETURN
*-----------------------------------------------------------------------------------
    END
