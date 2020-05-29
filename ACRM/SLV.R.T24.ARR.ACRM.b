*----------------------------------------------------------------------------------------------------
* <Rating>8969</Rating>
*----------------------------------------------------------------------------------------------------
* Nombre: SLV.R.T24.ARR.ACRM.b
* Descripcion: Genera archivo csv con informacion ARRANGEMENT T24.
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		Gerson		24.11.15	Version inicial
* 1.1       Gerson      24.11.17    se modifica property class para evalar plazo
* 1.2       RFlamenco   15.01.18    Integracion de Modulo AFD (se agregan campos LF.BENEFICIARIO y Depto)
* 1.3       GAMARTINEZ  28.05.19	Se modifica para envio de jms a colas OSB
*----------------------------------------------------------------------------------------------------

    SUBROUTINE SLV.R.T24.ARR.ACRM
*----------------------------------------------------------------------------------------------------
*
*----------------------------------------------------------------------------------------------------
* Modification History
*----------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
	$INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_AA.LOCAL.COMMON    
    $INSERT I_F.ACCOUNT
	$INSERT I_AA.APP.COMMON
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.ACCOUNT
    $INSERT I_F.AA.INTEREST    
    $INSERT I_F.AA.SETTLEMENT
    $INSERT I_F.AA.PRODUCT
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.AA.CHANGE.PRODUCT    
    $INSERT I_F.AA.ACCOUNT.DETAILS
	$INSERT I_F.AA.PAYMENT.SCHEDULE  
	$INSERT I_F.CUSTOMER
	$INSERT I_F.AA.CUSTOMER  

*-----------------------------------------------------------------------------
* Modulos
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

RETURN

*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
 	FN.ARRANGEMENT		= 'FBNK.AA.ARRANGEMENT'
    F.ARRANGEMENT 		= ''
    	
	FN.ARR.INTEREST 	= 'FBNK.AA.ARR.INTEREST'
  	F.ARR.INTEREST 		= '' 
	
	FN.ARR.TERM.AMO 	= 'FBNK.AA.ARR.TERM.AMOUNT'
    F.ARR.TERM.AMO  	= ''
    
	FN.ACC.DET 			= 'FBNK.AA.ACCOUNT.DETAILS'
    F.ACC.DET  			= ''
    
    FN.ACC 				= 'FBNK.ACCOUNT'
    F.ACC  				= ''
    
    FN.AA.ACCOUNT		= 'FBNK.AA.ACCOUNT'
    F.AA.ACCOUNT		= ''
    
    FN.CUSTOMER			= 'FBNK.CUSTOMER'
    F.CUSTOMER			= ''
    
	FN.PAYMENT.SCHEDULE = 'FBNK.AA.PAYMENT.SCHEDULE'
 	F.PAYMENT.SCHEDULE  = ''
 	
 	FN.ARR.ACT.HIS		= 'F.AA.ACTIVITY.HISTORY'
    F.ARR.ACT.HIS		= ''
 	
	;*Definicion de variables
    DIR.NAME	= 'MON_AML'
    NAME.FILE	= '_T24Arrangement.csv'
    Y.DATETIME	= ''
    
	;*Product Line
	Y.ACCOUNTS 	= 'ACCOUNTS'
	Y.DEPOSITS	= 'DEPOSITS'
	Y.LENDING 	= 'LENDING'
	
	;*Variables para arreglo de datos
	STR.ARR   = ''
	STR.BLANK = ''
	V.ARRANGEMENT.ID = ''

	;*Separador de linea
    EndLine = '|'	
    
    EQU localZone TO 'America/El_Salvador'  ;*Para setear Zona Horaria   

		
	;*fecha y hora	
	FECHAYHORA		= TIMEDATE()
	HORA 			= FECHAYHORA[1,8]:'.000'
	D_DATENOW		=	OCONV(DATE(),'D')
	D_DATENOW 		= 	OCONV(ICONV(D_DATENOW,"DMDY"),"D-E[2,A3,2]")
	UsLocGetDate	=	D_DATENOW[7,4]:D_DATENOW[4,2]:D_DATENOW[1,2]:'T':HORA
	
	UsLocPropertyId     = ''

	;*Variables de informacion a generar
	  Arrangement 		= ""
      ArrNumReference 	= ""
      CustomerId 		= ""
      ArrStatus 		= ""
      ArrUserInput 		= ""
      ArrStartDate 		= ""
      ArrBranchId 		= ""
      Product 			= ""
      ProductGroup 		= ""
      ProductLine 		= ""
      ProEfectDate 		= ""
      ProductStatus 	= ""
      CrdAmount 		= ""
      CrdInterestRate 	= ""
      CrdEffectiveRate 	= ""
      CrdReferenceRate	= ""
      CrdDisburstDate 	= ""
      CrdDueDate 		= ""
      CrdTermLending 	= ""
      CrdBalance 		= ""
      CrdPayAmount 		= ""
      CrdPaymentMethod 	= ""
      CrdPaymentDate 	= ""
      CrdType 			= ""
      CrdStatus 		= ""
      CrdJudicial 		= ""
      CrdAmountMoraCap 	= ""
      CrdAmountMoraInt 	= ""
      CrdCapMorDays 	= ""
      CrdIntMorDays 	= ""
      CrdCurrInteres 	= ""
      CrdOverInteres 	= ""
      CrdCurrTax 		= ""
      CrdOverTax 		= ""
      CrdCurrCap 		= ""
      CrdOverCap 		= ""
      CrdCancelDate 	= ""
      CrdDestinySSF 	= ""
      CrdCreditCondition 	= ""
      CrdPaymentAmountDJ 	= ""
      CrdAdvancedAmountDJ 	= ""
      CrdSourceFundsDJ 		= ""
      CrdSellingOfficer 	= ""
      CrdExpenseId 			= ""
      CrdExpenseType 		= ""
      CrdExpenseAmount 		= ""
      CrdNameAccount 		= ""
      CrdInsuranceFee 		= ""
      PaymentFreq 			= ""
      PrimaryOfficer 		= ""
      CrdDepGuarantee 		= ""
      CrdAssetGuarantee 	= ""
      CrdPayOutAccount 		= ""
      ArrInputter 			= ""
      ArrDateTime 			= ""
      ArrAuthorizer 		= ""
      ArrCompany 			= ""
      ArrCurrNum 			= ""
      ArrOverride 			= ""
      DapAmount 			= ""
      DapDueDate 			= ""
      DapTermDeposit 		= ""
      DapInterestRate 		= ""
      DapCapitalizationPeriod = ""
      DapOpeningDate 		= ""
      DapPaymentMethod 		= ""
      DapPaymentAccount 	= ""
      DapCapitalizationDate = ""
      DapAccruedInt			= ""
      DapDailyInt 			= ""
      DapCategory 			= ""
      DapFormNo 			= ""
      DapEffectiveRate 		= ""
      DapMarginRate 		= ""
      DapPeriodicIndex 		= ""
      DapStatus 			= ""
      DapCancelDate 		= ""
      DapProjectAmountDJ 	= ""
      DapSourceFundsDJ 		= ""
      AccWorkBalance 		= ""
      AccBlockBalance 		= ""
      AccOpeningDate 		= ""
      AccInterestRate 		= ""
      AccRestrictBalance 	= ""
      AccStatus 			= ""
      AccCancelDate 		= ""
      AccInactiveDate 		= ""
      AccRenewalDate 		= ""
      AccRestrictPeriod 	= ""
      AccProjectDepDJ 		= ""
      AccProjectWithdrwDJ 	= ""
      AccSourceFundsDJ 		= ""      
      CrCantTxMensual	    = ""
	  DbCantTxMensual	    = ""
	  AccBeneficiario       = ""
      AccDepCode            = ""
	   
	  ;*Definición de campos locales
	  CALL GET.LOC.REF('ACCOUNT','LF.DEP.STATUS', SDAP.POS)
	  CALL GET.LOC.REF('AA.ARR.INTEREST','LF.REF.INT.RATE', LOCPOSTasaReferencia)
	  CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.MONTO.APER', AMT.POS)
	  CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.NO.FORM', NFORM.POS)
	  CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.AML.DEP.PROY', DPRO.POS)
	  CALL GET.LOC.REF('AA.ARR.ACCOUNT',"LF.AML.PROC.FON", PFON.POS)
	  CALL GET.LOC.REF('AA.ARR.ACCOUNT',"LF.AML.RET.PROY", RPRO.POS)
	  CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.QT.CR.MTH.TX', LOCPOSCrMontTx)
	  CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.QT.DB.MTH.TX', LOCPOSDbMontTx)
	  CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.BENEFICIARIO', BENEF.POS)
	  
	  
	  
RETURN

DELETE_AND_OPEN:
		;* Eliminando archivo existente
		;*------------------------------
		;*GAMARTINEZ (1.3)
*	    DELETESEQ DIR.NAME,NAME.FILE THEN
*	    END
	
		;* Abriendo archivo para escritura
		;*---------------------------------
		;*GAMARTINEZ (1.3)
*	    OPENSEQ DIR.NAME,NAME.FILE TO SEQ.PTR THEN
*	        WEOFSEQ NAME.FILE
*	    END
RETURN

*-----------------------------------------------------------------------------
OPENFILE:
*-----------------------------------------------------------------------------
    CALL OPF(FN.ACC.DET, F.ACC.DET)
    CALL OPF(FN.ACC, F.ACC)
RETURN

PROCESS:
	IF c_aalocActivityStatus EQ 'AUTH' THEN
* -----------------------------------------------------------------------------------------------------------------------------
		V.ARRANGEMENT.ID = c_aalocArrId
		;* Abrir Archivo a Escribir
		;*--------------------------
		
		;*GAMARTINEZ (1.3)
*		NAME.FILE	= '_T24Arrangement.': c_aalocArrId :'.csv'
*		GOSUB DELETE_AND_OPEN
		
		;*Obtener informacion de arrangement y property
		GOSUB GET.ARR.PROPERTY	
		
		;*AUDITORIA ARRANGEMENT
*------------------------------------------------------------------------------
		IF AA$R.ARRANGEMENT.ACTIVITY<AA.ARR.ACT.INPUTTER> EQ '' THEN
			ArrInputter   = '-'
		END ELSE
			ArrInputter   = FIELD(AA$R.ARRANGEMENT.ACTIVITY<AA.ARR.ACT.INPUTTER>,"_", 2)
		END
		
		;* Personalizar DateTime Field
		;*-----------------------------------------------------------------
		Y.DATETIME = R.AA.ACC.NEW<AA.AC.DATE.TIME>
		GOSUB PARSE_DATETIME
		ArrDateTime   	  = UsLocGetDate
		;*-----------------------------------------------------------------		

		
		IF AA$R.ARRANGEMENT.ACTIVITY<AA.ARR.ACT.AUTHORISER> EQ '' THEN
			ArrAuthorizer = '-'
		END ELSE
			ArrAuthorizer = FIELD(AA$R.ARRANGEMENT.ACTIVITY<AA.ARR.ACT.AUTHORISER>,"_", 2)
		END
		
		IF AA$R.ARRANGEMENT<AA.ARR.CO.CODE> EQ '' THEN
			ArrCompany 	  = '-'
		END ELSE
			ArrCompany 	  = AA$R.ARRANGEMENT<AA.ARR.CO.CODE>
		END
		
		IF AA$R.ARRANGEMENT.ACTIVITY<AA.ARR.ACT.CURR.NO> EQ '' THEN
			ArrCurrNum	  = '0'
		END ELSE
			ArrCurrNum    = AA$R.ARRANGEMENT.ACTIVITY<AA.ARR.ACT.CURR.NO>
		END
		;*Obtener ultimo valor de campo OVERRIDE
		OVER 		  = DCOUNT(AA$R.ARRANGEMENT.ACTIVITY<AA.ARR.ACT.OVERRIDE>, SM)
		IF OVER EQ 0 THEN
			ArrOverride 		= '-' ;*No hay valor, se le asigna '-' 
		END ELSE
			ArrOverride 		= AA$R.ARRANGEMENT.ACTIVITY<AA.ARR.ACT.OVERRIDE><1, OVER>	;*Ultimo override
		END

		;*ARRANGEMENT
*------------------------------------------------------------------------------
		Arrangement 	= c_aalocArrId
		ArrNumReference = AA$LINKED.ACCOUNT 
		ArrCustomerId 	= AA$R.ARRANGEMENT<AA.ARR.CUSTOMER> ;*R.NEW(AC.CUSTOMER)
		ArrStatus 		= AA$R.ARRANGEMENT<AA.ARR.ARR.STATUS>
		ArrUserInput 	= ArrInputter 
		ArrStartDate 	= c_aalocActivityEffDate
		ArrBranchId 	= ArrCompany 
		Product 		= AA$ARR.PRODUCT.ID
		Product 		= AA$R.ARRANGEMENT<AA.ARR.PRODUCT>
		ProductGroup 	= AA$R.ARRANGEMENT<AA.ARR.PRODUCT.GROUP> 
		ProductLine 	= AA$R.ARRANGEMENT<AA.ARR.PRODUCT.LINE>
		ProEfectDate 	= AA$R.ARRANGEMENT<AA.ARR.PROD.EFF.DATE>
		ProductStatus 	= AA$R.ARRANGEMENT<AA.ARR.PRODUCT.STATUS> 
		aalocPropertyId = c_aalocPropertyId
			
	    UsLocPropertyId	= aalocPropertyId	
*------------------------------------------------------------------------------
		;*CREDITOS
*------------------------------------------------------------------------------	
		IF ProductLine EQ Y.LENDING THEN
	 	GOSUB GET.AA.LOA.PROPERTY
			;*Definicion de campos locales para creditos
			CALL GET.LOC.REF('AA.ARR.ACCOUNT', 'LF.DESTINO.SSF',DEST.POS)
			PROPERTY.CLASS = 'TERM.AMOUNT'
			PROPERTY = 'COMMITMENT'
			RETURN.VALUES = ''
			RETURN.IDS = ''
*	        		CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.LIST<I>,'TERM.AMOUNT','COMMITMENT',TODAY,RETURN.ID2, REC.AMT, ERROR2)   
			CALL AA.GET.ARRANGEMENT.CONDITIONS(V.ARRANGEMENT.ID, PROPERTY.CLASS, PROPERTY, TODAY, RETURN.IDS, RETURN.VALUES, RETURN.ER)
			R.TERM.NEW = RAISE(RETURN.VALUES)
	
			PROPERTY.CLASS = 'INTEREST'
			PROPERTY = ''
			RETURN.VALUES = ''
			RETURN.IDS = ''
			CALL AA.GET.ARRANGEMENT.CONDITIONS(V.ARRANGEMENT.ID, PROPERTY.CLASS, PROPERTY, TODAY, RETURN.IDS, RETURN.VALUES, RETURN.ER)
			R.INT.NEW = RAISE(RETURN.VALUES)
			
			CrdAmount = R.TERM.AMT.NEW<AA.AMT.AMOUNT>
			CrdInterestRate = R.INT.NEW<AA.INT.FIXED.RATE>
			CrdEffectiveRate = R.INT.NEW<AA.INT.EFFECTIVE.RATE>
			CrdReferenceRate = FIELD(R.INT.NEW<1,AA.INT.LOCAL.REF>,SM,LOCPOSTasaReferencia) ;*Tasa de Referencia
			CrdDisburstDate = AA$R.ARRANGEMENT<AA.ARR.START.DATE>				
			CrdDueDate = ''
			CrdTermLending = R.TERM.NEW<AA.AMT.TERM> 
			CrdBalance = ''
			CrdPayAmount = R.TERM.NEW<AA.AMT.AMOUNT> 
			CrdPaymentMethod = '' 
			CrdPaymentDate = ''
			CrdType = AA$R.ARRANGEMENT<AA.ARR.ARRANGEMENT.TYPE> 
			CrdStatus = AA$R.ARRANGEMENT<AA.ARR.ARR.STATUS>
			CrdJudicial = ''
			CrdAmountMoraCap = ''
			CrdAmountMoraInt = ''
			CrdCapMorDays = ''
			CrdIntMorDays = ''
			CrdCurrInteres = ''
			CrdOverInteres = ''
			CrdCurrTax = ''
			CrdOverTax = ''
			CrdCurrCap = ''
			CrdOverCap = ''
			CrdCancelDate = ''
			CrdDestinySSF = R.TERM.AMT.NEW<AA.AMT.LOCAL.REF><1, DEST.POS>
			CrdCreditCondition = ''
			CrdPaymentAmountDJ = R.CTA.NEW<AA.AC.LOCAL.REF><1,DPRO.POS>;*1
			CrdAdvancedAmountDJ = R.CTA.NEW<AA.AC.LOCAL.REF><1,RPRO.POS>
			CrdSourceFundsDJ = ''
			CrdSellingOfficer = ''
			CrdExpenseId = ''
			CrdExpenseType = ''
			CrdExpenseAmount = ''
			CrdNameAccount = ''
			CrdInsuranceFee = ''
			PaymentFreq = ''
			PrimaryOfficer = ''
			CrdDepGuarantee = ''
			CrdAssetGuarantee = ''
			CrdPayOutAccount = ''
		END
		
		;*DEPOSITOS
*------------------------------------------------------------------------------
		IF ProductLine EQ Y.DEPOSITS THEN
			;*CHANGE.PRODUCT PROPERTY
			PROPERTY.CLASS = 'INTEREST'
			PROPERTY = 'DEPOSITINT'
			RETURN.VALUES = ''
			RETURN.IDS = ''
			CALL AA.GET.ARRANGEMENT.CONDITIONS(V.ARRANGEMENT.ID, PROPERTY.CLASS, PROPERTY, TODAY, RETURN.IDS, RETURN.VALUES, RETURN.ER)
			R.DAPIN.NEW = RAISE(RETURN.VALUES)		
	
	

			;*AA.ARR.TERM.AMOUNT PROPERTY
			PROPERTY.CLASS = 'TERM.AMOUNT'
			PROPERTY = 'COMMITMENT'
			RETURN.VALUES = ''
			RETURN.IDS = ''
			CALL AA.GET.ARRANGEMENT.CONDITIONS(V.ARRANGEMENT.ID, PROPERTY.CLASS, PROPERTY, TODAY, RETURN.IDS, RETURN.VALUES, RETURN.ER)
			R.TERM.AMT.NEW = RAISE(RETURN.VALUES)
			
			;*CHANGE.PRODUCT PROPERTY
			PROPERTY.CLASS = 'CHANGE.PRODUCT'
			PROPERTY = 'RENEWAL'
			RETURN.VALUES = ''
			RETURN.IDS = ''
			CALL AA.GET.ARRANGEMENT.CONDITIONS(V.ARRANGEMENT.ID, PROPERTY.CLASS, PROPERTY, TODAY, RETURN.IDS, RETURN.VALUES, RETURN.ER)
			R.CHG.PRD.NEW = RAISE(RETURN.VALUES)
	
			;*SETTLEMENT PROPERTY
			PROPERTY.CLASS = 'SETTLEMENT'
			PROPERTY = 'SETTLEMENT'
			RETURN.VALUES = ''
			RETURN.IDS = ''
			CALL AA.GET.ARRANGEMENT.CONDITIONS(V.ARRANGEMENT.ID, PROPERTY.CLASS, PROPERTY, TODAY, RETURN.IDS, RETURN.VALUES, RETURN.ER)
			R.SETTL.NEW = RAISE(RETURN.VALUES)
		
*		;*ACCOUNT PROPERTY	
			DapAmount 				= R.TERM.AMT.NEW<AA.AMT.AMOUNT>
			DapDueDate 				= R.ACC.DET.NEW<AA.AD.RENEWAL.DATE>
			DapTermDeposit 			= R.CHG.PRD.NEW<AA.CP.CHANGE.PERIOD>
			DapInterestRate 		= R.DAPIN.NEW<AA.INT.EFFECTIVE.RATE>
			DapCapitalizationPeriod = R.PAY.SCHE.NEW<AA.PS.PAYMENT.FREQ>
			DapOpeningDate 			= R.ACC.DET.NEW<AA.AD.CONTRACT.DATE>
			DapPaymentMethod 		= R.ACC.DET.NEW<AA.AD.PAY.METHOD>
			DapPaymentAccount 		= R.SETTL.NEW<AA.SET.PAYOUT.ACCOUNT>
			DapCapitalizationDate 	= R.ACC.DET.NEW<AA.AD.PAYMENT.START.DATE>
			DapAccruedInt 			= ''
			DapDailyInt 			= ''
			DapCategory 			= R.AA.ACC.NEW<AA.AC.CATEGORY>
			DapFormNo 				= R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,NFORM.POS>
			DapEffectiveRate 		= R.DAPIN.NEW<AA.INT.EFFECTIVE.RATE>
			DapMarginRate 			= R.DAPIN.NEW<AA.INT.MARGIN.RATE>
			DapPeriodicIndex 		= R.DAPIN.NEW<AA.INT.PERIODIC.INDEX>
			DapStatus 				= R.ACC.DET.NEW<AA.AD.ARR.AGE.STATUS>
			DapCancelDate 			= R.ACC.DET.NEW<AA.AD.REPORT.END.DATE>
			DapProjectAmountDJ 		= R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,DPRO.POS>
			DapSourceFundsDJ 		= R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,PFON.POS>
			;*Nuevos campos
			CrCantTxMensual	    	= R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,LOCPOSCrMontTx>
	  		DbCantTxMensual	   	 	= R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,LOCPOSDbMontTx>
	  		
			
		END	
	
		;*CUENTAS
*------------------------------------------------------------------------------
		IF ProductLine EQ Y.ACCOUNTS THEN
			;*Obtener informacion de cuentas
			GOSUB GET.AA.ACC.PROPERTY
			
			CALL GET.LOC.REF('ACCOUNT',"LF.ESTADO.CTA",SACC.POS)
		
			PROPERTY.CLASS 	= 'CINTEREST'
			PROPERTY 		= ''
			RETURN.VALUES 	= ''
			RETURN.IDS 		= ''
			
			CALL AA.GET.ARRANGEMENT.CONDITIONS(V.ARRANGEMENT.ID, PROPERTY.CLASS, PROPERTY, TODAY, RETURN.IDS, RETURN.VALUES, RETURN.ER)
			R.INT.NEW = RAISE(RETURN.VALUES)
			
			AccWorkBalance 		= R.CTA.NEW<AC.WORKING.BALANCE>
			AccBlockBalance 	= R.CTA.NEW<AC.LOCKED.AMOUNT>
			AccOpeningDate 		= R.ACC.DET.NEW<AA.AD.CONTRACT.DATE> ;*R.ACC.NEW<AC.OPENING.DATE>
			AccInterestRate 	= R.INT.NEW<AA.INT.FIXED.RATE>
			AccRestrictBalance  = R.CTA.NEW<AC.LOCKED.AMOUNT>
			AccStatus 			= R.CTA.NEW<AC.LOCAL.REF><1,SACC.POS>
			AccCancelDate 		= R.CTA.NEW<AC.CLOSURE.DATE>
			AccInactiveDate 	= ''
			AccRenewalDate 		= R.ACC.DET.NEW<AA.AD.RENEWAL.DATE>
			AccRestrictPeriod 	= ''
			AccProjectDepDJ 	= R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,DPRO.POS>
			AccProjectWithdrwDJ = R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,RPRO.POS>
			;*Nuevos campos
			CrCantTxMensual	    = R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,LOCPOSCrMontTx>
	  		DbCantTxMensual	    = R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,LOCPOSDbMontTx>
	  		;*MONTO
	  		CrdAmount			= R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,AMT.POS>
	  		;* nuevos campos para AFD	
	  		AccBeneficiario     = R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,BENEF.POS>
	  		AccDepCode          = R.CTA.NEW<AC.DEPT.CODE>	
	  		
			
			IF(R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,PFON.POS> EQ '') THEN
				AccSourceFundsDJ	= '-' ;*No hay valor, se le asigna '-' para que actue como fin de linea		
			END ELSE
				AccSourceFundsDJ 	= R.AA.ACC.NEW<AA.AC.LOCAL.REF><1,PFON.POS>
			END
		END

		;*Construcccion de arreglos para generar archivo
		
		STR.ARR = "{"
		
		STR.ARR  := '"Arrangement":"': Arrangement			   : '",'	;*01
	    STR.ARR := '"ArrNumReference":"': ArrNumReference	       : '",'	;*02
	    STR.ARR := '"CustomerId":"': ArrCustomerId		   : '",'	;*03
	    STR.ARR := '"ArrStatus":"': ArrStatus               : '",'	;*04
	    STR.ARR := '"ArrUserInput":"': ArrUserInput		       : '",'	;*05
		IF ArrStartDate EQ '' THEN 
			ArrStartDate = '""'
		END ELSE
			ArrStartDate ='"':ArrStartDate[1,4]:'-':ArrStartDate[5,2]:'-':ArrStartDate[7,2]:'"'
		END	    
	    STR.ARR := '"ArrStartDate":': ArrStartDate : ','	;*06
	    STR.ARR := '"ArrBranchId":"': ArrBranchId             : '",'	;*07
	    STR.ARR := '"Product":"': Product                 : '",'	;*08
	    STR.ARR := '"ProductGroup":"': ProductGroup		       : '",'	;*09
	    STR.ARR := '"ProductLine":"': ProductLine             : '",'	;*10
		IF ProEfectDate EQ '' THEN 
			ProEfectDate = '""'
		END ELSE
			ProEfectDate ='"':ProEfectDate[1,4]:'-':ProEfectDate[5,2]:'-':ProEfectDate[7,2]:'"'
		END		    
	    STR.ARR := '"ProEfectDate":':  ProEfectDate            : ','	;*11
	    STR.ARR := '"ProductStatus":"': ProductStatus           : '",'	;*12
	    IF CrdAmount EQ '' THEN CrdAmount = '0'
	    STR.ARR := '"CrdAmount":': CrdAmount               : ','	;*13
	    IF CrdInterestRate EQ '' THEN CrdInterestRate = '0'
	    STR.ARR := '"CrdInterestRate":': CrdInterestRate         : ','	;*14
	    IF CrdEffectiveRate EQ '' THEN CrdEffectiveRate = '0'
	    STR.ARR := '"CrdEffectiveRate":': CrdEffectiveRate        : ','	;*15
	    IF CrdReferenceRate EQ '' THEN CrdReferenceRate = '0'
	    STR.ARR := '"CrdReferenceRate":': CrdReferenceRate		   : ','    ;*16
		IF CrdDisburstDate EQ '' THEN 
			CrdDisburstDate = '""'
		END ELSE
			CrdDisburstDate ='"':CrdDisburstDate[1,4]:'-':CrdDisburstDate[5,2]:'-':CrdDisburstDate[7,2]:'"'
		END		    
	    STR.ARR := '"CrdDisburstDate":':   CrdDisburstDate   : ','	;*17
		IF CrdDueDate EQ '' THEN 
			CrdDueDate = '""'
		END ELSE
			CrdDueDate ='"':CrdDueDate[1,4]:'-':CrdDueDate[5,2]:'-':CrdDueDate[7,2]:'"'
		END		    
	    STR.ARR := '"CrdDueDate":':  CrdDueDate : ','	;*18
	    STR.ARR := '"CrdTermLending":"': CrdTermLending          : '",'	;*19
	    IF CrdBalance EQ '' THEN CrdBalance = '0'
	    STR.ARR := '"CrdBalance":': CrdBalance              : ','	;*20
	    IF CrdPayAmount EQ '' THEN CrdPayAmount = '0'
	    STR.ARR := '"CrdPayAmount":': CrdPayAmount            : ','	;*21
	    STR.ARR := '"CrdPaymentMethod":"': CrdPaymentMethod        : '",'	;*22
		IF CrdPaymentDate EQ '' THEN 
			CrdPaymentDate = '""'
		END ELSE
			CrdPaymentDate ='"':CrdPaymentDate[1,4]:'-':CrdPaymentDate[5,2]:'-':CrdPaymentDate[7,2]:'"'
		END		    
	    STR.ARR := '"CrdPaymentDate":':  CrdPaymentDate          : ','	;*23
	    STR.ARR := '"CrdType":"': CrdType                 : '",'	;*24
	    STR.ARR := '"CrdStatus":"': CrdStatus               : '",'	;*25
	    IF CrdJudicial EQ '' THEN CrdJudicial = '0'
	    STR.ARR := '"CrdJudicial":': CrdJudicial             : ','	;*26
	    IF CrdAmountMoraCap EQ '' THEN CrdAmountMoraCap = '0'
	    STR.ARR := '"CrdAmountMoraCap":': CrdAmountMoraCap        : ','	;*27
	    IF CrdAmountMoraInt EQ '' THEN CrdAmountMoraInt = '0'
	    STR.ARR := '"CrdAmountMoraInt":': CrdAmountMoraInt        : ','	;*28
	    IF CrdCapMorDays EQ '' THEN CrdCapMorDays = '0'
	    STR.ARR := '"CrdCapMorDays":': CrdCapMorDays           : ','	;*29
	    IF CrdIntMorDays EQ '' THEN CrdIntMorDays = '0'
	    STR.ARR := '"CrdIntMorDays":': CrdIntMorDays           : ','	;*30
	    IF CrdCurrInteres EQ '' THEN CrdCurrInteres = '0'
	    STR.ARR := '"CrdCurrInteres":': CrdCurrInteres          : ','	;*31
	    IF CrdOverInteres EQ '' THEN CrdOverInteres = '0'
	    STR.ARR := '"CrdOverInteres":': CrdOverInteres          : ','	;*32
	    IF CrdCurrTax EQ '' THEN CrdCurrTax = '0'
	    STR.ARR := '"CrdCurrTax":': CrdCurrTax              : ','	;*33
	    IF CrdOverTax EQ '' THEN CrdOverTax = '0'
	    STR.ARR := '"CrdOverTax":': CrdOverTax			   : ','	;*34
	    IF CrdCurrCap EQ '' THEN CrdCurrCap = '0'
	    STR.ARR := '"CrdCurrCap":': CrdCurrCap              : ','	;*35
	    IF CrdOverCap EQ '' THEN CrdOverCap = '0'
	    STR.ARR := '"CrdOverCap":': CrdOverCap              : ','	;*36
		IF CrdCancelDate EQ '' THEN 
			CrdCancelDate = '""'
		END ELSE
			CrdCancelDate ='"':CrdCancelDate[1,4]:'-':CrdCancelDate[5,2]:'-':CrdCancelDate[7,2]:'"'
		END		    
	    STR.ARR := '"CrdCancelDate":':  CrdCancelDate          : ','	;*37
	    STR.ARR := '"CrdDestinySSF":"': CrdDestinySSF           : '",'	;*38
	    STR.ARR := '"CrdCreditCondition":"': CrdCreditCondition      : '",'	;*39
	    IF CrdPaymentAmountDJ EQ '' THEN CrdPaymentAmountDJ = '0'
	    STR.ARR := '"CrdPaymentAmountDJ":': CrdPaymentAmountDJ      : ','	;*40
	    IF CrdAdvancedAmountDJ EQ '' THEN CrdAdvancedAmountDJ = '0'
	    STR.ARR := '"CrdAdvancedAmountDJ":': CrdAdvancedAmountDJ     : ','	;*41
	    STR.ARR := '"CrdSourceFundsDJ":"': CrdSourceFundsDJ        : '",'	;*42
	    STR.ARR := '"CrdSellingOfficer":"': CrdSellingOfficer       : '",'	;*43
	    STR.ARR := '"CrdExpenseId":"': CrdExpenseId            : '",'	;*44
	    STR.ARR := '"CrdExpenseType":"': CrdExpenseType          : '",'	;*45
	    IF CrdExpenseAmount EQ '' THEN CrdExpenseAmount = '0'
	    STR.ARR := '"CrdExpenseAmount":': CrdExpenseAmount        : ','	;*46
	    STR.ARR := '"CrdNameAccount":"': CrdNameAccount          : '",'	;*47
	    IF CrdInsuranceFee EQ '' THEN CrdInsuranceFee = '0'
	    STR.ARR := '"CrdInsuranceFee":': CrdInsuranceFee         : ','	;*48
	    STR.ARR := '"PaymentFreq":"': PaymentFreq             : '",'	;*49
	    STR.ARR := '"PrimaryOfficer":"': PrimaryOfficer          : '",'	;*50
	    STR.ARR := '"CrdDepGuarantee":"': CrdDepGuarantee         : '",'	;*51
	    STR.ARR := '"CrdAssetGuarantee":"': CrdAssetGuarantee       : '",'	;*52
	    STR.ARR := '"CrdPayOutAccount":"': CrdPayOutAccount        : '",'	;*53
	    STR.ARR := '"ArrInputter":"': ArrInputter             : '",'	;*54
	    STR.ARR := '"ArrDateTime":"':  ArrDateTime[1,4]:'-':ArrDateTime[5,2]:'-':ArrDateTime[7,2]: ArrDateTime[9,LEN(ArrDateTime)] : '-06:00",'	;*55
	    STR.ARR := '"ArrAuthorizer":"': ArrAuthorizer           : '",'	;*56
	    STR.ARR := '"ArrCompany":"': ArrCompany              : '",'	;*57
	    IF ArrCurrNum EQ '' THEN ArrCurrNum = '0'
	    STR.ARR := '"ArrCurrNum":': ArrCurrNum              : ','	;*58
	    STR.ARR := '"ArrOverride":"': ArrOverride             : '",'	;*59
	    IF DapAmount EQ '' THEN DapAmount = '0'
	    STR.ARR := '"DapAmount":': DapAmount               : ','	;*60
		IF DapDueDate EQ '' THEN 
			DapDueDate = '""'
		END ELSE
			DapDueDate ='"':DapDueDate[1,4]:'-':DapDueDate[5,2]:'-':DapDueDate[7,2]:'"'
		END		    
	    STR.ARR := '"DapDueDate":':  DapDueDate             : ','	;*61
	    STR.ARR := '"DapTermDeposit":"': DapTermDeposit          : '",'	;*62
	    IF DapInterestRate EQ '' THEN DapInterestRate = '0'
	    STR.ARR := '"DapInterestRate":': DapInterestRate         : ','	;*63
	    STR.ARR := '"DapCapitalizationPeriod":"': DapCapitalizationPeriod : '",'	;*64
		IF DapOpeningDate EQ '' THEN 
			DapOpeningDate = '""'
		END ELSE
			DapOpeningDate ='"':DapOpeningDate[1,4]:'-':DapOpeningDate[5,2]:'-':DapOpeningDate[7,2]:'"'
		END		    
	    STR.ARR := '"DapOpeningDate":':  DapOpeningDate    	   : ','	;*65
	    STR.ARR := '"DapPaymentMethod":"': DapPaymentMethod		   : '",'	;*66
	    STR.ARR := '"DapPaymentAccount":"': DapPaymentAccount       : '",'	;*67
	    STR.ARR := '"DapCapitalizationDate":"': DapCapitalizationDate   : '",'	;*68
	    IF DapAccruedInt EQ '' THEN DapAccruedInt = '0'
	    STR.ARR := '"DapAccruedInt":': DapAccruedInt           : ','	;*69
	    IF DapDailyInt EQ '' THEN DapDailyInt = '0'
	    STR.ARR := '"DapDailyInt":': DapDailyInt             : ','	;*70
	    STR.ARR := '"DapCategory":"': DapCategory             : '",'	;*71
	    STR.ARR := '"DapFormNo":"': DapFormNo               : '",'	;*72
	    IF DapEffectiveRate EQ '' THEN DapEffectiveRate = '0'
	    STR.ARR := '"DapEffectiveRate":': DapEffectiveRate        : ','	;*73
	    IF DapMarginRate EQ '' THEN DapMarginRate = '0'
	    STR.ARR := '"DapMarginRate":': DapMarginRate           : ','	;*74
	    STR.ARR := '"DapPeriodicIndex":"': DapPeriodicIndex        : '",'	;*75
	    STR.ARR := '"DapStatus":"': DapStatus               : '",'	;*76
		IF DapCancelDate EQ '' THEN 
			DapCancelDate = '""'
		END ELSE
			DapCancelDate ='"':DapCancelDate[1,4]:'-':DapCancelDate[5,2]:'-':DapCancelDate[7,2]:'"'
		END		    
	    STR.ARR := '"DapCancelDate":':  DapCancelDate           : ','	;*77
	    IF DapProjectAmountDJ EQ '' THEN DapProjectAmountDJ = '0'
	    STR.ARR := '"DapProjectAmountDJ":': DapProjectAmountDJ      : ','	;*78
	    STR.ARR := '"DapSourceFundsDJ":"': DapSourceFundsDJ        : '",'	;*79
	    IF AccWorkBalance EQ '' THEN AccWorkBalance = '0'
	    STR.ARR := '"AccWorkBalance":': AccWorkBalance          : ','	;*80
	    IF AccBlockBalance EQ '' THEN AccBlockBalance = '0'
	    STR.ARR := '"AccBlockBalance":': AccBlockBalance         : ','	;*81
		IF AccOpeningDate EQ '' THEN 
			AccOpeningDate = '""'
		END ELSE
			AccOpeningDate ='"':AccOpeningDate[1,4]:'-':AccOpeningDate[5,2]:'-':AccOpeningDate[7,2]:'"'
		END		    
	    STR.ARR := '"AccOpeningDate":':  AccOpeningDate          : ','	;*82
	    IF AccInterestRate EQ '' THEN AccInterestRate = '0'
	    STR.ARR := '"AccInterestRate":': AccInterestRate         : ','	;*83
	    IF AccRestrictBalance EQ '' THEN AccRestrictBalance = '0'
	    STR.ARR := '"AccRestrictBalance":': AccRestrictBalance      : ','	;*84
	    STR.ARR := '"AccStatus":"': AccStatus               : '",'	;*85
		IF AccCancelDate EQ '' THEN 
			AccCancelDate = '""'
		END ELSE
			AccCancelDate ='"':AccCancelDate[1,4]:'-':AccCancelDate[5,2]:'-':AccCancelDate[7,2]:'"'
		END		    
	    STR.ARR := '"AccCancelDate":':  AccCancelDate  : ','	;*86
		IF AccInactiveDate EQ '' THEN 
			AccInactiveDate = '""'
		END ELSE
			AccInactiveDate ='"':AccInactiveDate[1,4]:'-':AccInactiveDate[5,2]:'-':AccInactiveDate[7,2]:'"'
		END		    
	    STR.ARR := '"AccInactiveDate":':   AccInactiveDate     : ','	;*87
		IF AccRenewalDate EQ '' THEN 
			AccRenewalDate = '""'
		END ELSE
			AccRenewalDate ='"':AccRenewalDate[1,4]:'-':AccRenewalDate[5,2]:'-':AccRenewalDate[7,2]:'"'
		END		    
	    STR.ARR := '"AccRenewalDate":':  AccRenewalDate      : ','	;*88
	    IF AccRestrictPeriod EQ '' THEN AccRestrictPeriod = '0'
	    STR.ARR := '"AccRestrictPeriod":': AccRestrictPeriod       : ','	;*89
	    IF AccProjectDepDJ EQ '' THEN AccProjectDepDJ = '0'
	    STR.ARR := '"AccProjectDepDJ":': AccProjectDepDJ         : ','	;*90
	    IF AccProjectWithdrwDJ EQ '' THEN AccProjectWithdrwDJ = '0'
	    STR.ARR := '"AccProjectWithdrwDJ":': AccProjectWithdrwDJ     : ','	;*91
	    STR.ARR := '"AccSourceFundsDJ":"': AccSourceFundsDJ        : '",'	;*92
	    STR.ARR := '"CrCantTxMensual":"': CrCantTxMensual         : '",'   	;*93
	    STR.ARR := '"DbCantTxMensual":"': DbCantTxMensual         : '",'   	;*94
*       *** AFD  ***
	    STR.ARR := '"LfBeneficiario":"':  AccBeneficiario        : '",'    ;*95      
		STR.ARR := '"AccDepCode":"':  AccDepCode   :'"'                   ;*96 
		
		STR.ARR := "}"	
	
		;* Generacion de csv array para escritura de en csv
		;*-----------------------------------------------------------------------------
	    ;*Agregar arreglo al archivo
		;*-------------------------------------
		;*GAMARTINEZ (1.3)
*	    WRITEBLK STR.ARR ON SEQ.PTR THEN ;*sin retorno de carro
*	    END	
		;*----------------------------------------------------------------------------------------------------
	   	;*GAMARTINEZ (1.3)
*	   	CLOSESEQ SEQ.PTR

		;* GAMARTINEZ (1.3)
		TEXTO.ARCHIVO = STR.ARR :'c_aalocActivityEffDate:': c_aalocActivityEffDate
		GOSUB ESCRIBIR.ARCHIVO 
		JMS.MSJ = STR.ARR
		GOSUB SEND.JMS	

   END
RETURN

*------------------------------------------------------------------------------	
GET.ARR.PROPERTY:
*------------------------------------------------------------------------------
	;*ACCOUNT PROPERTY	
	R.AA.ACC.NEW = RAISE(AA$PROPERTY.NEW)

	;*INTEREST PROPERTY
	R.ACR.INT.NEW = RAISE(AA$ACCR.DETS)
	
	;*CHANGE.PRODUCT PROPERTY
	PROPERTY.CLASS = 'AA.ACCOUNT.DETAILS'
	PROPERTY = ''
	RETURN.VALUES = ''
	RETURN.IDS = ''
	CALL AA.GET.ARRANGEMENT.CONDITIONS(V.ARRANGEMENT.ID, PROPERTY.CLASS, PROPERTY, TODAY, RETURN.IDS, RETURN.VALUES, RETURN.ER)
	R.ACC.DET.NEW = RAISE(RETURN.VALUES)
	
	CALL F.READ(FN.AAC.DET, V.ARRANGEMENT.ID, R.ACC.DET.NEW , F.ACC.DET, ERR.ACCDET)
	
	;*PAYMENT.SCHEDULE PROPERTY
	PROPERTY.CLASS = 'PAYMENT.SCHEDULE'
	PROPERTY = 'SCHEDULE'
	RETURN.VALUES = ''
	RETURN.IDS = ''
	CALL AA.GET.ARRANGEMENT.CONDITIONS(V.ARRANGEMENT.ID, PROPERTY.CLASS, PROPERTY, TODAY, RETURN.IDS, RETURN.VALUES, RETURN.ER)
	R.PAY.SCHE.NEW = RAISE(RETURN.VALUES)
	
RETURN


*------------------------------------------------------------------------------	
GET.AA.ACC.PROPERTY:
*------------------------------------------------------------------------------
	;*CUSTOMER PROPERTY
	PROPERTY.CLASS = 'ACCOUNT'
	PROPERTY = ''
	RETURN.VALUES = ''
	RETURN.IDS = ''

	CALL AA.GET.ARRANGEMENT.CONDITIONS(V.ARRANGEMENT.ID, PROPERTY.CLASS, PROPERTY, TODAY, RETURN.IDS, RETURN.VALUES, RETURN.ER)
	R.CTA.NEW = RAISE(RETURN.VALUES)
	
	Y.REF.ID  = R.CTA.NEW<AA.AC.ACCOUNT.REFERENCE>
	CALL F.READ(FN.ACC, Y.REF.ID, R.CTA.NEW , F.ACC, ERR.ACC)
	
	
RETURN
    
*------------------------------------------------------------------------------	
GET.AA.LOA.PROPERTY:
*------------------------------------------------------------------------------
	;*CUSTOMER PROPERTY
	PROPERTY.CLASS = 'ACCOUNT'
	PROPERTY = ''
	RETURN.VALUES = ''
	RETURN.IDS = ''

	CALL AA.GET.ARRANGEMENT.CONDITIONS(V.ARRANGEMENT.ID, PROPERTY.CLASS, PROPERTY, TODAY, RETURN.IDS, RETURN.VALUES, RETURN.ER)
	R.CTA.NEW = RAISE(RETURN.VALUES)
	
*	Y.REF.ID  = R.CTA.NEW<AA.AC.ACCOUNT.REFERENCE>
*	CALL F.READ(FN.ACC, Y.REF.ID, R.CTA.NEW , F.ACC, ERR.ACC)	
RETURN
     
PARSE_DATETIME:
		utcDateTime =  Y.DATETIME
		UTC.FLAG = ''
		;*Evaluar UTC Time or Standard Time		
		FINDSTR "." IN utcDateTime SETTING Ap, Vp THEN
			UTC.FLAG = '1'
		END
		
		IF UTC.FLAG EQ '1' THEN
			localZoneDate1 = OCONV(LOCALDATE(utcDateTime,localZone),'D4/E')
			localZoneTime1= OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
			Y.VOU.FEC = localZoneDate1:' ':localZoneTime1
			Y.DATETIME = Y.VOU.FEC[7,4]:Y.VOU.FEC[4,2]:Y.VOU.FEC[1,2]: ' ' : localZoneTime1			 
		END
		ELSE
			localZoneTime1=OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
			Y.DATETIME = utcDateTime:' ':localZoneTime1	
		END

RETURN

;* GAMARTINEZ (1.3)
SEND.JMS:

	JMS.RESPONSE=''
	JMS.ERROR=''
	JMS.APP = 'ACRM_ARRANGEMENT'
	JMS.METHOD = 'send'
	
	CALL SLV.B.SEND.JMS(JMS.METHOD,JMS.APP:"##":JMS.MSJ,JMS.RESPONSE,JMS.ERROR)
	
	CRT JMS.RESPONSE
	CRT JMS.ERROR
	TEXTO.ARCHIVO = JMS.RESPONSE:' - ': JMS.ERROR
	GOSUB ESCRIBIR.ARCHIVO

RETURN

ESCRIBIR.ARCHIVO:
DIR.NAME= 'MON_AML'
R.ID   = 'DATE':TODAY:'.txt'
OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
END
CLOSESEQ SEQ.PTR
RETURN

END