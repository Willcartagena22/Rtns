*----------------------------------------------------------------------------------------------------
* <Rating>1947</Rating>
*----------------------------------------------------------------------------------------------------
* Nombre: SLV.R.T24.TRANSACTIONS.FDATA.b
* Descripcion: Genera archivo csv con informacion de Transacciones T24.
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		O.Cornejo	26.11.15	Version inicial
* 1.1		ITurcios	18.04.16	Se modifica para que baje datos de customer debitado y acreditado
* 1.2		ITurcios	06.05.16	Se agrega para que baje la IP del cliente y se agrega parseo de campo DateTime debido a la data erronea que viene de los ambientes
*----------------------------------------------------------------------------------------------------

SUBROUTINE SLV.R.T24.TRANSACTION.FDATA
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*----------------------------------------------------------------------------- 
$INSERT I_COMMON    
$INSERT I_EQUATE 
$INSERT I_F.TELLER 
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.TELLER.TRANSACTION
$INSERT I_F.FT.TXN.TYPE.CONDITION 
$INSERT I_TSS.COMMON 
$INSERT I_F.ACCOUNT
$INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------

GOSUB INIT 
GOSUB OPENFILE
GOSUB PROCESS
RETURN

	INIT:
		FN.TT 		= 'F.TELLER'
	    F.TT  		= ''
	    FN.TT.TXN 	= 'F.TELLER.TRANSACTION'
	    F.TT.TXN	= ''
	    FN.FT 		= 'F.FUNDS.TRANSFER'
	    F.FT  		= ''
	    FN.FT.TXN	= 'F.FT.TXN.TYPE.CONDITION'
	    F.FT.TXN	= ''
	    FN.ACC		= 'F.ACCOUNT'
	    F.ACC		= ''
	
	
		;* Tipos de aplicaciones
		;*----------------------
	    TT_Aplication = 'TT'
	    FT_Aplication = 'FT' 
	    EndLine = '|'	;*Separador de linea
		
		EQU localZone TO 'America/El_Salvador'
		
		;*Definicion de variables
		;*-----------------------
	    DIR.NAME		= 'SIC'
	    NAME.FILE		= '' 
	    Y.DATETIME 		= ''
	    ARR.CHARGE		= ''
	    ARR.IDCHARGES	= ''
	    STR.DATACHARGE	= ''
	    CONT			= 1
	RETURN
	
	OPENFILE:
		;* Apertura de Archivos
		;*----------------------
		CALL OPF(FN.TT, F.TT)
		CALL OPF(FN.TT.TXN, F.TT.TXN)
		CALL OPF(FN.FT, F.FT)
		CALL OPF(FN.FT.TXN, F.FT.TXN)
		CALL OPF(FN.ACC, F.ACC)
		RETURN
	
	DELETE_AND_OPEN:
		;* Eliminando archivo existente
		;*------------------------------
	    DELETESEQ DIR.NAME,NAME.FILE THEN
	    END
	
		;* Abriendo archivo para escritura
		;*---------------------------------
	    OPENSEQ DIR.NAME,NAME.FILE TO SEQ.PTR THEN
	        WEOFSEQ NAME.FILE
	    END
	RETURN
	
	PROCESS:	
	IF OFS$OPERATION EQ 'PROCESS' THEN
		TransactionId 	= ID.NEW 											;*Id de la Transaccion
		
		;* Abrir Archivo a Escribir
		;*--------------------------
		NAME.FILE		= '_T24Transaction.' : TransactionId : '.csv'
		GOSUB DELETE_AND_OPEN
		
		;* Si es Transaccion por TELLER Entrar
		;*------------------------------------- 
		IF LEFT(TransactionId, 2) EQ TT_Aplication THEN
			TellerId      	= R.NEW(TT.TE.TELLER.ID.1) 						;*Id del Cajero
			TranDateTime  	= R.NEW(TT.TE.VALUE.DATE.1) 					;*Fecha y Hora de la Transaccion (AAMMDD HH:MM)
			Amount 			= R.NEW(TT.TE.AMOUNT.LOCAL.1) 					;*Monto de Transaccion
			CheckNumber 	= R.NEW(TT.TE.CHEQUE.NUMBER) 					;*Numero de Cheque
			CheckType 		= R.NEW(TT.TE.CHEQ.TYPE) 						;*Tipo de Cheque
			;*Charges,Commissions y Taxes	
					
			CANT_CHRG_TYPE = DCOUNT(R.NEW(TT.TE.CHARGE.CODE), VM)
			FOR K = 1 TO CANT_CHRG_TYPE
				 STR.DATACHARGE		 = TransactionId		 											 								: ";"		;*TransactionID
				 STR.DATACHARGE 	:= ''																								: ";"		;*Codigo de Cargo
				 STR.DATACHARGE 	:= R.NEW(TT.TE.CHARGE.CODE)<1,K>	 									   							: ";"		;*Tipo de Cargo
			 	 STR.DATACHARGE 	:= R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,K>																	: ";"		;*Monto de Cargo
			 	 STR.DATACHARGE 	:= ''																								: ";"		;*Tipo de Tax
			 	 STR.DATACHARGE 	:= ''																								: ";"		;*Monto de Tax
			 	 STR.DATACHARGE		:= 'CHARGE-TT'																									;*Tipo de Cargo
			 	 ;*Arreglo con registro de cargos y comisiones			 	 			 	 
			 	 ARR.CHARGE<CONT>    = STR.DATACHARGE
			 	 ;*Arreglo con registro de id de cargos/comisiones para nombre de archivo
			 	 ARR.IDCHARGES<CONT> = R.NEW(TT.TE.CHARGE.CODE)<1,K>
			 	 				 	 
			 	 STR.DATACHARGE 	= ''	
			 	 CONT 		    	= CONT + 1 	
 	 		NEXT K
 	 					
			TransactionType = R.NEW(TT.TE.TRANSACTION.CODE)		 			;*Tipo de Transaccion FT.TXN.TYPE.CONDITION y TELLER.TRANSACTION
			IVA 			= SUBSTRINGS(R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,2>,4,LEN(R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,2>))				;*Monto Total de IVA			
			ISR 			= '' 											;*Monto de ISR
			StatusTransaction = R.NEW(TT.TE.RECORD.STATUS) 					;*Estatus de la Transaccion
			TotalCharged	= ''											;*Total Cargos
			TotalDebited 	= ''	 										;*Total Debitado
			TotalCredited 	= ''	 										;*Total Acreditado
			TfsReference 	= R.NEW(TT.TE.TFS.REFERENCE) 					;*Numero de Referencia de TFS
			StockNumber 	= R.NEW(TT.TE.STOCK.NUMBER) 					;*Stock de Cheques Asignados a una Agencia
			StockRegister 	= R.NEW(TT.TE.STOCK.REGISTER) 					;*Segmento de Cheques
			SeriesId 		= R.NEW(TT.TE.SERIES.ID) 						;*Serie
			
			CANT_STMT		= DCOUNT(R.NEW(TT.TE.STMT.NO), SM)
			StmtNo 			= R.NEW(TT.TE.STMT.NO)<1,CANT_STMT>				;*Statement Entry
			
			PayeeName 		= R.NEW(TT.TE.PAYEE.NAME) 						;*Beneficiario del Cheque
			IdActivity 		= '' 											;*Id Actividad cuando Afecta Arrangement		
			Inputter 		= FIELD(R.NEW(TT.TE.INPUTTER), "_", 2) 			;*Digitador
			SourceID		= R.NEW(TT.TE.INPUTTER)							;*Inputter completo para leer el canal 
			Ip				= TSS$CLIENTIP 									;*Ip del cliente
			
			;*Leyendo valores de cuenta destino y origen (depende de que si es un CREDIT o DEBIT)
			IF(R.NEW(TT.TE.DR.CR.MARKER) EQ 'CREDIT') THEN
				OriginAccount 		= R.NEW(TT.TE.ACCOUNT.2) 						;*Cuenta Origen (cuenta a quien se le debita)				
				IF(SUBSTRINGS(OriginAccount,1,3) EQ 'USD') THEN		;*Si es cuenta Interna se le asigna id Banco '999994'
					DebitCustomer	= '999994'
				END ELSE
					DebitCustomer	= R.NEW(TT.TE.CUSTOMER.2)						;*Id del cliente a quien se le debita
				END
				DestinyAccount 		= R.NEW(TT.TE.ACCOUNT.1) 						;*Cuenta Destino (cuenta interna del banco)
				IF(SUBSTRINGS(DestinyAccount,1,3) EQ 'USD') THEN	;*Si es cuenta Interna se le asigna id Banco '999994'
					CreditCustomer	= '999994'
				END ELSE
					CreditCustomer	= R.NEW(TT.TE.CUSTOMER.1)						;*Id Cliente a quien se le acredita
				END												
			END ELSE ;*si es DEBIT
				OriginAccount 		= R.NEW(TT.TE.ACCOUNT.1) 						;*Cuenta Origen
				IF(SUBSTRINGS(OriginAccount,1,3) EQ 'USD') THEN		;*Si es cuenta Interna se le asigna id Banco '999994'
					DebitCustomer	= '999994'
				END ELSE
					DebitCustomer	= R.NEW(TT.TE.CUSTOMER.1)						;*Id Cliente a quien se le debita
				END				
				DestinyAccount 		= R.NEW(TT.TE.ACCOUNT.2) 						;*Cuenta Destino
				IF(SUBSTRINGS(DestinyAccount,1,3) EQ 'USD') THEN
					CreditCustomer  = '999994'						;*Si es cuenta Interna se le asigna id Banco '999994'
				END ELSE									
					CreditCustomer	= R.NEW(TT.TE.CUSTOMER.2)						;*Id del cliente a quien se le acredita
				END	
			END			
			;* Personalizar DateTime Field
			;*-----------------------------------------------------------------
			Y.DATETIME		= R.NEW(TT.TE.DATE.TIME)
			GOSUB PARSE.DATETIME
			;*-----------------------------------------------------------------							
			DateTime 		= Y.DATETIME									;*Fecha y Hora del Registro
			Authorizer 		= FIELD(R.NEW(TT.TE.AUTHORISER),"_",2)			;*Autorizador
			Company 		= R.NEW(TT.TE.CO.CODE) 							;*Agencia
			CurrNum 		= R.NEW(TT.TE.CURR.NO) 							;*Numero Actual del Registro
			
			;* Obtener ultimo valor de campo OVERRIDE
			;*----------------------------------------
			OVER = DCOUNT(R.NEW(TT.TE.OVERRIDE), SM)
			Override 		= R.NEW(TT.TE.OVERRIDE)<1, OVER>					;*Ultimo override
		
			Y.REF.ID  = OriginAccount
			CALL F.READ(FN.ACC, Y.REF.ID, R.CTA, F.ACC, ERR.ACC)
			WorkBalanceOrig = R.CTA<AC.WORKING.BALANCE>					 ;*Saldo de la cuenta Origen
		END
		;* Si es Transaccion por FUNDS.TRANSFER Entrar Aca
		;*------------------------------------------------
		ELSE IF LEFT(TransactionId, 2) EQ FT_Aplication THEN
				CALL GET.LOC.REF("FUNDS.TRANSFER","LF.PERCEN.COM",PERCOM.POS)  ;*Porcentaje de Cargo de Comisión
				CALL GET.LOC.REF("FUNDS.TRANSFER","LF.DIF.AMT.COM",DIFAMT.POS) ;*Cargo de Comisión (Diferido - por desembolso)
				CALL GET.LOC.REF("FUNDS.TRANSFER","LF.FIX.AMT.COM",FIXAMT.POS) ;*Costo de Comisión (a ingreso - por desembolso)
		
				TellerId      	= '' 											;*Id del Cajero				
				TranDateTime  	= R.NEW(FT.PROCESSING.DATE)	 					;*Fecha y Hora de la Transaccion (AAMMDD HH:MM)
				Amount 			= R.NEW(FT.LOC.AMT.DEBITED)	 					;*Monto de Transaccion
				OriginAccount 	= R.NEW(FT.DEBIT.ACCT.NO) 				 		;*Cuenta Origen
				DestinyAccount 	= R.NEW(FT.CREDIT.ACCT.NO) 						;*Cuenta Destino
				CheckNumber 	= R.NEW(FT.CHEQUE.NUMBER)	 					;*Numero de Cheque
				CheckType 		= R.NEW(FT.CHEQ.TYPE)	 						;*Tipo de Cheque
				;*Commissions y Taxes
				CANT_COM_TYPE  	= DCOUNT(R.NEW(FT.COMMISSION.TYPE), VM)
				FOR J = 1 TO CANT_COM_TYPE
					 STR.DATACHARGE 	= TransactionId		 			 													: ";"	;*Id de Transacción
					 STR.DATACHARGE 	:= R.NEW(FT.COMMISSION.CODE)														: ";"   ;*Codigo de Comision
					 STR.DATACHARGE 	:= R.NEW(FT.COMMISSION.TYPE)<1,J> 												    : ";"	;*Tipo de Comision
				 	 STR.DATACHARGE 	:= SUBSTRINGS(R.NEW(FT.COMMISSION.AMT)<1,J> ,4, LEN(R.NEW(FT.COMMISSION.AMT)<1,J>)) : ";"	;*Monto de Comision
				 	 STR.DATACHARGE 	:= R.NEW(FT.TAX.TYPE)<1,J> 		 									 	 			: ";"	;*Tipo de Tax
				 	 STR.DATACHARGE 	:= SUBSTRINGS(R.NEW(FT.TAX.AMT)<1,J> ,4, LEN(R.NEW(FT.TAX.AMT)<1,J>))				: ";"	;*Monto de Tax
				 	 STR.DATACHARGE 	:= 'COMMISSION-FT'																			;*Tipo de Cargo
				 	 
				 	 ;*Arreglo con registro de comision y taxes		 	 
				 	 ARR.CHARGE<CONT>   = STR.DATACHARGE
				 	 ;*Arreglo con registro de id de comision para nombre de archivo
				 	 ARR.IDCHARGES<CONT>= R.NEW(FT.COMMISSION.TYPE)<1,J>
				 	 				 	 
				 	 STR.DATACHARGE 	= ''	
				 	 CONT 		    	= CONT + 1 			 	 	
				NEXT J
				
				;*Charges
				CANT_CHRG_TYPE = DCOUNT(R.NEW(FT.CHARGE.TYPE), VM)
				FOR K = 1 TO CANT_CHRG_TYPE
					 STR.DATACHARGE		= TransactionId		 												 				: ";"		;*Id de Transacción
					 STR.DATACHARGE 	:= R.NEW(FT.CHARGE.CODE) 															: ";"		;*Codigo de Cargo
					 STR.DATACHARGE 	:= R.NEW(FT.CHARGE.TYPE)<1,K> 		 									   			: ";"		;*Tipo de Cargo
				 	 STR.DATACHARGE 	:= SUBSTRINGS(R.NEW(FT.CHARGE.AMT)<1,K> ,4, LEN(R.NEW(FT.CHARGE.AMT)<1,K>))			: ";"		;*Monto de Cargo				 	 
				 	 STR.DATACHARGE 	:= ''																				: ";"		;*Tipo de Tax
				 	 STR.DATACHARGE 	:= ''																				: ";"		;*Monto de Tax
				 	 STR.DATACHARGE 	:= 'CHARGE-FT'																					;*Tipo de Cargo

					 ;*Arreglo con registro de cargo 				 	 			 	 
				 	 ARR.CHARGE<CONT>   = STR.DATACHARGE
				 	 ;*Arreglo con registro de id de cargo para nombre de archivo
				 	 ARR.IDCHARGES<CONT>= R.NEW(FT.CHARGE.TYPE)<1,K>
				 	 				 	 
				 	 STR.DATACHARGE 	= ''	
				 	 CONT 		    	= CONT + 1 	
 	 			NEXT K				
				
				TransactionType = R.NEW(FT.TRANSACTION.TYPE) 												;*Tipo de Transaccion						
				IVA 			= SUBSTRINGS(R.NEW(FT.TOTAL.TAX.AMOUNT),4,LEN(R.NEW(FT.TOTAL.TAX.AMOUNT)))	;*Monto total de IVA
				ISR 			= '' 											;*Monto de ISR
				StatusTransaction = R.NEW(FT.RECORD.STATUS) 					;*Estatus de la Transaccion
				TotalCharged	= SUBSTRINGS(R.NEW(FT.TOTAL.CHARGE.AMOUNT) ,4, LEN(R.NEW(FT.TOTAL.CHARGE.AMOUNT)));*Total de Cargos (Tax + Commisions)
				TotalDebited 	= SUBSTRINGS(R.NEW(FT.AMOUNT.DEBITED), 4, LEN(R.NEW(FT.AMOUNT.DEBITED)))	;*Total Debitado				
				TotalCredited 	= SUBSTRINGS(R.NEW(FT.AMOUNT.CREDITED), 4, LEN(R.NEW(FT.AMOUNT.CREDITED)))	;*Total Acreditado	
				
				IF(SUBSTRINGS(OriginAccount,1,3) EQ 'USD') THEN
					DebitCustomer	= '999994'
				END ELSE
					DebitCustomer	= R.NEW(FT.DEBIT.CUSTOMER)						;*Id Cliente a quien se le debita
				END				
				IF(SUBSTRINGS(DestinyAccount,1,3) EQ 'USD') THEN
					CreditCustomer	= '999994'
				END ELSE
					CreditCustomer	= R.NEW(FT.CREDIT.CUSTOMER)						;*Id del cliente a quien se le acredita
				END							
														
				TfsReference 	= R.NEW(FT.TFS.REFERENCE)	 					;*Numero de Referencia de TFS
				StockNumber 	= R.NEW(FT.STOCK.NUMBER)	 					;*Stock de Cheques Asignados a una Agencia
				StockRegister 	= R.NEW(FT.STOCK.REGISTER)	 					;*Segmento de Cheques
				SeriesId 		= R.NEW(FT.SERIES.ID)	 						;*Serie
				
				CANT_STMT		= DCOUNT(R.NEW(TT.TE.STMT.NO), SM)				
				StmtNo 			= R.NEW(FT.STMT.NOS)<1,CANT_STMT>				;*Statement Entry
				
				PayeeName 		= R.NEW(FT.PAYEE.NAME)	 						;*Beneficiario del Cheque
				IdActivity 		= '' 											;*Id Actividad cuando Afecta Arrangement		
				Inputter 		= FIELD(R.NEW(FT.INPUTTER), "_", 2) 			;*Digitador
				SourceID		= R.NEW(FT.INPUTTER)							;*Inputter completo para leer el canal 
				Ip				= TSS$CLIENTIP									;*Ip del cliente
				;* Personalizar DateTime Field
				;*-----------------------------------------------------------------
				Y.DATETIME		= R.NEW(FT.DATE.TIME)
				GOSUB PARSE.DATETIME
				;*-----------------------------------------------------------------						
				DateTime 		= Y.DATETIME									;*Fecha y Hora del Registro
				Authorizer 		= FIELD(R.NEW(FT.AUTHORISER),"_",2)				;*Autorizador
				Company 		= R.NEW(FT.CO.CODE) 							;*Agencia
				CurrNum 		= R.NEW(FT.CURR.NO) 							;*Numero Actual del Registro			
				;* Obtener ultimo valor de campo OVERRIDE
				;*----------------------------------------
				OVER 			= DCOUNT(R.NEW(FT.OVERRIDE), SM)
				Override 		= R.NEW(FT.OVERRIDE)<1, OVER>					;*Ultimo override
								
				Y.REF.ID  = OriginAccount
				CALL F.READ(FN.ACC, Y.REF.ID, R.CTA, F.ACC, ERR.ACC) 
				WorkBalanceOrig = R.CTA<AC.WORKING.BALANCE>
		END		
		;*Generar archivo de FT o TT
		GOSUB PREPARE_AND_SEND
		;*Gnerar archivos de cargos, comisiones y taxes
		GOSUB SEND_CHARGES
	END	
	RETURN
	
	;* Preparar y Enviar Arreglo a Archivo CSV
	;*-----------------------------------------
	PREPARE_AND_SEND:
		STR.ARR := TransactionId 	: ";" ;*1 
		STR.ARR := TellerId 		: ";" ;*2
		STR.ARR := TranDateTime     : ";" ;*3	
		STR.ARR := Amount           : ";" ;*4
		STR.ARR := OriginAccount 	: ";" ;*5
		STR.ARR := DestinyAccount 	: ";" ;*6
		STR.ARR := CheckNumber 		: ";" ;*7
		STR.ARR := CheckType 		: ";" ;*8
		STR.ARR := TransactionType 	: ";" ;*9
		STR.ARR := IVA 				: ";" ;*10	
		STR.ARR := ISR 				: ";" ;*11
		STR.ARR := StatusTransaction : ";";*12
		STR.ARR := TotalCharged		:";"  ;*13
		STR.ARR := TotalDebited 	: ";" ;*14
		STR.ARR := TotalCredited 	: ";" ;*15 
		STR.ARR	:= DebitCustomer 	: ";" ;*16
		STR.ARR := CreditCustomer 	: ";" ;*17
		STR.ARR := TfsReference 	: ";" ;*18
		STR.ARR := StockNumber 		: ";" ;*19
		STR.ARR := StockRegister	: ";" ;*20
		STR.ARR := SeriesId 		: ";" ;*21
		STR.ARR := StmtNo 			: ";" ;*22
		STR.ARR := PayeeName 		: ";" ;*23
		STR.ARR := IdActivity 		: ";" ;*24		
		STR.ARR := Inputter			: ";" ;*25 
		STR.ARR := SourceID 		: ";" ;*26
		STR.ARR	:= Ip				: ";" ;*27
		STR.ARR := DateTime 		: ";" ;*28
		STR.ARR := Authorizer 		: ";" ;*29
		STR.ARR := Company 			: ";" ;*30
		STR.ARR := CurrNum 			: ";" ;*31
		STR.ARR := Override			: ";" ;*32
		STR.ARR := WorkBalanceOrig 		  ;*33
		STR.ARR := EndLine				  ;*34	
 
		;*Generacion de csv array para escritura en csv
		
	    ;*Agregar arreglo al archivo
		;*-------------------------------------
	    WRITEBLK STR.ARR ON SEQ.PTR THEN ;*sin retorno de carro
	    END	
		;*--------------------------------------------------
		
	    CLOSESEQ SEQ.PTR
	RETURN
	
	SEND_CHARGES:
		FOR LM = 1 TO CONT - 1 
			;*File Name
			NAME.FILE = '_T24TransactionCharges.' : ID.NEW : '-' : ARR.IDCHARGES<LM> :'.csv'
			
			GOSUB DELETE_AND_OPEN

		    ;*Escribiendo
		  	WRITEBLK ARR.CHARGE<LM> ON SEQ.PTR THEN 
		  	;*sin retorno de carro
		  	END 
		  	
			CLOSESEQ SEQ.PTR
			
			NAME.FILE = ' '
		NEXT LM	
	RETURN
	
	;*Respuesta de Errores
	;*--------------------
	CRT_ERROR:
	    ETEXT = STRERR
    	CALL STORE.END.ERROR  
	RETURN
	
	PARSE.DATETIME:
			utcDateTime =  Y.DATETIME
			UTC.FLAG = ''
			;*Evaluar UTC Time or Standard Time
			
			FINDSTR "." IN utcDateTime SETTING Ap, Vp THEN
				UTC.FLAG = '1' 
			END
			
			IF UTC.FLAG EQ '1' THEN
				localZoneDate1 = OCONV(LOCALDATE(utcDateTime,localZone),'D4/E')
				localZoneTime1= OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
				;*16/07/2016 12:17:01 --dia/mes/año devuelve se modifica para enviar (AÑOMESDIA 12:17:00)
				Y.DATETIME = localZoneDate1[7,4]:localZoneDate1[4,2]:localZoneDate1[1,2]:' ':localZoneTime1			
			END
			ELSE
				Y.DAY.BC = utcDateTime[3,2]
				Y.MONTH.BC = utcDateTime[5,2]
				Y.YEAR.BC = utcDateTime[1,2]
				Y.DATE.BC = OCONV(ICONV(utcDateTime[1,6],'D'),'D4/E')
				;*07/16/2016 12:17:01 --mes/dia/año devuelve se modifica para enviar (AÑOMESDIA 12:17:00)						
				Y.DATE.BC2 = Y.DATE.BC[7,4]:Y.DATE.BC[1,2]:Y.DATE.BC[4,2] ;*AÑOMESDIA
				Y.TIME.BC = utcDateTime[7,2]:':':utcDateTime[9,2]:':':'00'
				Y.DATETIME = Y.DATE.BC2: ' ': Y.TIME.BC
			END
	RETURN
END
