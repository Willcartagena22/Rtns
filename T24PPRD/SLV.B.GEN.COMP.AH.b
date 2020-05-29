*-----------------------------------------------------------------------------
* <Rating>105</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.GEN.COMP.AH
*-----------------------------------------------------------------------------
*eurias comprobantes abono salario
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.FT.BULK.MASTER
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

GOSUB PROCESS
RETURN

	PROCESS: 
		FN.FUNDS.TRANSFER 		= 'F.FUNDS.TRANSFER'
	   	F.FUNDS.TRANSFER		= ''
	   	FN.FT.BULK.MASTER 		= 'F.FT.BULK.MASTER'
	   	F.FT.BULK.MASTER		= ''
	   	F.CUSTOMER 			= 'F.CUSTOMER'
	   	FN.CUSTOMER			= ''
	    LIST.ARR				= ''
		
		CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)
		CALL OPF(FN.FT.BULK.MASTER,F.FT.BULK.MASTER)
		CALL OPF(F.CUSTOMER,FN.CUSTOMER)
		 
		;*SELECT.CMD = "SELECT ":FN.FUNDS.TRANSFER:" WITH INWARD.PAY.TYPE LK FTBULK-BKM... AND TRANSACTION.TYPE EQ ACRP ACSE"
		SELECT.CMD = "SELECT ":FN.FUNDS.TRANSFER:" WITH TRANSACTION.TYPE EQ ACSE ACPS ACPH"
	   			
		;* Extrayendo IDs de FUNDS.TRANSFER
	    CALL EB.READLIST(SELECT.CMD, LIST.ARR, '', NO.OF.RECS, Y.ERR.FT)
	    
	    FOR I = 1 TO NO.OF.RECS
	    	CALL F.READ(FN.FUNDS.TRANSFER,LIST.ARR<I>, R.FT,F.FUNDS.TRANSFER,ERR.FUNDS.TRANSFER)
	     	inward.pay = R.FT<FT.INWARD.PAY.TYPE>
	     	customerId = R.FT<FT.CREDIT.CUSTOMER>
	     	
	     	CALL F.READ(F.CUSTOMER,customerId, R.CUSTOMER,FN.CUSTOMER,F.CUSTOMER.ERR)
		    primerNombre   = R.CUSTOMER<EB.CUS.NAME.1>
		    primerApellido = R.CUSTOMER<EB.CUS.TEXT>
		    ftBulkId = FIELD(FIELD(inward.pay,'.',1),'-',2)
	     	
	     	IF (inward.pay[1,10] EQ 'FTBULK-BKM') THEN
	     		CALL F.READ(FN.FT.BULK.MASTER,ftBulkId, R.FT.BULK.MASTER,F.FT.BULK.MASTER,ERR.FT.BULK.MASTER)
	       		fileName = R.FT.BULK.MASTER<FT.BLK.MAS.UPLOAD.REFERENCE>
	       		cuentaCredito = R.FT<FT.CREDIT.ACCT.NO>
	     		Agencia = R.FT<FT.CO.CODE>
	     		Usuario = FIELD(R.FT<FT.INPUTTER>,'_',2)
	     	 
	     		Y.DATETIME = R.FT<FT.DATE.TIME>
	     		;*GOSUB PARSE_DATETIME
				CALL SLV.R.CNV.FT.TZ.DATE(Y.DATETIME)
				Y.FECHA   = Y.DATETIME[0,8]
	     		FechaHora = Y.FECHA[7,2] : '/' : Y.FECHA[5,2] : '/' : Y.FECHA[0,4] : Y.DATETIME[9,LEN(Y.DATETIME)]
	     		tipoCuenta = cuentaCredito
	     	
	     		CALL SLV.UTIL.GET.TIPO.CTA(tipoCuenta)
	     		IF(tipoCuenta EQ 0)THEN
	     			TipoCuenta = 'CUENTA DE AHORROS' 
	     		END 
	     		IF(tipoCuenta EQ 1)THEN
	     			TipoCuenta = 'CUENTA CORRIENTE' 
	     		END
	     		NumReferencia = LIST.ARR<I>
	     		NumReferenciaFile = fileName:'-':primerNombre:'_':primerApellido:'_':Y.DATE.FILE.NAME
	     		NumCuenta = cuentaCredito
	     		Cliente = R.FT<FT.CREDIT.CUSTOMER>
	     	
	     		CALL SLV.UTIL.GET.NOM.CLIENTE(Cliente)
	     		Efectivo = '0.00'
	     		ChqAjeno = '0.00'
	     		ChqProp = '0.00'
	     		CargoCta = R.FT<FT.CREDIT.AMOUNT>
	     		IF NOT(CargoCta)THEN
	     			CargoCta = R.FT<FT.DEBIT.AMOUNT>
	     		END
	     		
	     		CALL SLV.UTIL.FORMATO.MONEDA(CargoCta)    	
	     		Total  = R.FT<FT.CREDIT.AMOUNT>
	     		IF NOT(Total)THEN
	     			Total = R.FT<FT.DEBIT.AMOUNT>
	     		END
	     	
	     		CALL SLV.UTIL.FORMATO.MONEDA(Total)
	     		TotalLetras = R.FT<FT.CREDIT.AMOUNT>
	     		IF NOT(TotalLetras)THEN
	     			TotalLetras = R.FT<FT.DEBIT.AMOUNT>
	     		END
	     	
	     		CALL SLV.UTIL.IMP.GET.NUMLETRAS(TotalLetras)
	     		Concepto = 'Abono Salario'
	     		IF R.FT<FT.TRANSACTION.TYPE> EQ 'ACSE' THEN
	     			Concepto = 'Abono Programa de Ahorro'
	     		END
	     	
		     	stringData = ''
		     	stringData := 'Agencia=':Agencia:';'
		     	stringData := 'Usuario=':Usuario:';'
		     	stringData := 'FechaHora=':FechaHora:';'
		     	stringData := 'TipoCuenta=':TipoCuenta:';'
		     	stringData := 'NumReferencia=':NumReferencia:';'
		     	stringData := 'NumCuenta=':NumCuenta:';'
		     	stringData := 'Cliente=':Cliente:';'
		     	stringData := 'Efectivo=':Efectivo:';'
		     	stringData := 'ChqAjeno=':ChqAjeno:';'
		     	stringData := 'ChqProp=':ChqProp:';'
		     	stringData := 'CargoCta=':CargoCta:';'
		     	stringData := 'Total=':Total:';'
		     	stringData := 'TotalLetras=':TotalLetras:';'
		     	stringData := 'Concepto=':Concepto:';'
		     	stringData := 'inward.pay=':inward.pay:';';*asociar el bulkPayment
		     	stringData := 'fileName=':fileName:';'
	     	
	     		idArchivo = 'ReciboSalario-':CHANGE(NumReferenciaFile," ",""):'.txt'
	     		;*direcArchivo ='C:\Users\jurias\Documents\JARS\TESTBULK\'
	     		direcArchivo = 'SPOOL.FILES'
		     	OPENSEQ direcArchivo, idArchivo TO SEQ.PTR ELSE
			    CREATE SEQ.PTR ELSE
					ETEXT = 'No se puede crear el archivo.'
		    		CALL STORE.END.ERROR
			    END
			END
		
			;* Escribiendo los datos en el archivo
			WRITESEQ stringData ON SEQ.PTR ELSE 
				ETEXT = 'No se pueden escribir los datos en el archivo.' 
				CALL STORE.END.ERROR
			END
		
			;* Cerrando el archivo
			CLOSESEQ SEQ.PTR
		  END
	    NEXT I
	RETURN
END