*----------------------------------------------------------------------------------------------------
* <Rating>7141</Rating>
*----------------------------------------------------------------------------------------------------
* Nombre: SLV.R.T24.TRANSACTION.ACRM.b
* Descripcion: Genera archivo csv con informacion de Transacciones T24.
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		Gerson	 26.11.15	 Version inicial
* 1.1       rortiz   25.01.17    Se agrega campo con la nacionalidad del cliente en las transacciones que realizara.
* 1.2       dmontes  27.07.17    Se agrega campo con la nacionalidad del cliente en las transacciones que realizara.
* 1.3       rortiz   30.11.17    Se elimina el caracter ";" para el último campo y se valida si la variable DepartmentCode viene vacía
* 1.4       rflamenco 18.12.17   Se agrega validación del OFS.SOURCE para las transacciones de BULK PAYMENT
* 1.5       RFlamenco  28.09.17    Se agregan campos para Monitor AFD
* 1.6       VBurgos  19.07.18    Integrando desarrollo de Remesas de JIraheta
*----------------------------------------------------------------------------------------------------

SUBROUTINE SLV.R.T24.TRANSACTION.ACRM
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.TELLER
$INSERT I_F.USER
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.TELLER.TRANSACTION
$INSERT I_F.FT.TXN.TYPE.CONDITION
$INSERT I_F.ACCOUNT
$INSERT I_GTS.COMMON
$INSERT I_F.CUSTOMER
$INSERT I_F.USER
$INSERT I_F.EB.SLV.COLECTOR
$INSERT I_F.AC.CHARGE.REQUEST
$INSERT I_F.CATEGORY
$INSERT I_F.FT.COMMISSION.TYPE
$INSERT I_F.AC.LOCKED.EVENTS
$INSERT I_F.FT.BULK.MASTER
$INSERT I_F.ATM.TRANSACTION
$INSERT I_F.EB.SLV.PAGO.REMESA
;*$INSERT I_F.TELLER.FINANCIAL.SERVICES

*Control de ejecucion
IF OFS$OPERATION EQ 'PROCESS' THEN 
     GOSUB INIT
     GOSUB OPENFILE
     GOSUB PROCESS
END
    RETURN 

*-----------------------------------------------------------------------------

	INIT:
		FN.TT 		= 'F.TELLER'
	    F.TT  		= ''
	    FN.TT.TXN 	= 'F.TELLER.TRANSACTION'
	    F.TT.TXN	= ''
	    FN.FT 		= 'F.FUNDS.TRANSFER'
	    F.FT  		= ''
	    FN.FT.TXN	= 'F.FT.TXN.TYPE.CONDITION'
	    F.FT.TXN	= ''
		FN_ACC		= 'F.ACCOUNT'
		F_ACC		= '' 
		FN_CUS      = 'F.CUSTOMER'
		F_CUS       = ''
		FN.USER     = 'F.USER'
	    F.USER      = ''
	    FN.SLV.COL = 'F.EB.SLV.COLECTOR'
	    F.SLV.COL  = '' 
		FN.CATE     = 'F.CATEGORY'
		F.CATE      = ''
		FN.COMMI    = 'F.FT.COMMISSION.TYPE'
		F.COMMI     = ''
		FN.LCK 		= 'F.AC.LOCKED.EVENTS'
		F.LCK 		= '' 
		FN.BULK.MASTER = 'F.FT.BULK.MASTER'
		F.BULK.MASTER  = ''
		FN.ATM.TRX 	= 'F.ATM.TRANSACTION'
		F.ATM.TRX 	= '' 
		;*Informacion de remesa 
		FN.PAG.REME	= 'F.EB.SLV.PAGO.REMESA'
		F.PAG.REME 	= '' 
		;*FN.TFS 		= 'F.TELLER.FINANCIAL.SERVICES'
		;*F.TFS 		= ''
		
		
		  ;*fecha y hora
		    FECHAYHORA		=   TIMEDATE()
			HORA 			=   FECHAYHORA[1,8]:'.000'
*			D_DATENOW		=	OCONV(DATE(),'D')
*			D_DATENOW 		= 	OCONV(ICONV(D_DATENOW,"DMDY"),"D-E[2,A3,2]")
*			UsLocGetDate	=	D_DATENOW[7,4]:D_DATENOW[4,2]:D_DATENOW[1,2]:' ':HORA
            
            D_DATENOW       =   OCONV(DATE(),"D/")
            UsLocGetDate	=	D_DATENOW[7,4]:D_DATENOW[1,2]:D_DATENOW[4,2]:' ':HORA
		  
		  ;* ult variable de Arreglo
		  TelefonoOrdenanteBenef              = "-"  
		
		;* Tipos de aplicaciones
		;*----------------------
	    TT_Aplication = 'TT'
	    FT_Aplication = 'FT'
	    CHG_Aplication 	= 'CHG' 
	    ComissionAmount  = ''
	    EndLine = '|'	;*Separador de linea
	
		;*Definicion de variables
		;*-----------------------
	    DIR.NAME		= 'MON_AML'
	    NAME.FILE		= '' 
	    
	    DIR_CH.NAME		= 'MON_AML'
	    NAME_CH.FILE		= ''
	    
	    Y.DATETIME = ''
	    ACCOUNT_WORK=''
	    
	    EQU TXN.REME  TO '104'
	    EQU Y.ID.REME TO 'TransNetwork'
	    
	    ;*@Author: Ronald Ortiz
	    ;*EQU OFS.SOURCE.BANCA.LINEA TO 'INPUTTER__WS___OFS_TCIBCORP'
	RETURN
	
	OPENFILE:
		;* Apertura de Archivos
		;*----------------------
*		CALL OPF(FN.TT, F.TT)
		CALL OPF(FN.TT.TXN, F.TT.TXN)
*		CALL OPF(FN.FT, F.FT)
		CALL OPF(FN.FT.TXN, F.FT.TXN)
		CALL OPF(FN_ACC,F_ACC)
		CALL OPF(FN_CUS,F_CUS)
		CALL OPF(FN.SLV.COL,F.SLV.COL)
		CALL OPF(FN.CATE,F.CATE)
		CALL OPF(FN.COMMI,F.COMMI)
		CALL OPF(FN.BULK.MASTER,F.BULK.MASTER)
		CALL OPF(FN.ATM.TRX, F.ATM.TXN)
		CALL OPF(FN.PAG.REME, F.PAG.REME)	
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

		TransactionId 	= ID.NEW  ;*Id de la Transaccion
		V.FLAG = '0'
		;* Abrir Archivo a Escribir
		;*--------------------------
		NAME.FILE		= '_T24Transaction.' : TransactionId : '.csv'
		GOSUB DELETE_AND_OPEN
		
;* Si es Transaccion por TELLER Entrar
		;*-------------------------------------
			;*-----------------------------------
			FN.USER = 'F.USER'
			F.USER =''
			V.USER= OPERATOR
			CALL OPF(FN.USER,F.USER)
			CALL F.READ(FN.USER,V.USER,RR.USER,F.USER,Y.ERR)
			;*-----------------------------------
			
		IF LEFT(TransactionId, 2) EQ TT_Aplication THEN
			TellerId      	= R.NEW(TT.TE.TELLER.ID.1) 						;*Id del Cajero
			TranDateTime  	= R.NEW(TT.TE.VALUE.DATE.1) 					;*Fecha y Hora de la Transaccion (AAMMDD HH:MM)
			Amount 			= R.NEW(TT.TE.AMOUNT.LOCAL.1) 					;*Monto de Transaccion
			CheckNumber 	= R.NEW(TT.TE.CHEQUE.NUMBER) 					;*Numero de Cheque
			CheckType 		= R.NEW(TT.TE.CHEQ.TYPE) 						;*Tipo de Cheque
			TypeComission 	= '' 											;*Tipo de Comision
			ComissionAmount = '' 											;*Monto de Comisiones
			TypeCharge 		= R.NEW(TT.TE.CHARGE.CODE) 						;*Tipo de Cargo
			ChargeAmount 	= R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,1>				;*Monto de Cargos
			TransactionType = R.NEW(TT.TE.TRANSACTION.CODE)		 			;*Tipo de Transaccion FT.TXN.TYPE.CONDITION y TELLER.TRANSACTION
			IVA 			= SUBSTRINGS(R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,2>,4,LEN(R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,2>))				;*Monto de IVA
			ISR 			= '' 											;*Monto de ISR
			StatusTransaction = R.NEW(TT.TE.RECORD.STATUS) 					;*Estatus de la Transaccion
			TotalDebited 	= ''	 										;*Total Debitado
			TotalCredited 	= ''	 										;*Total Acreditado
			TfsReference 	= R.NEW(TT.TE.TFS.REFERENCE) 					;*Numero de Referencia de TFS
			StockNumber 	= R.NEW(TT.TE.STOCK.NUMBER) 					;*Stock de Cheques Asignados a una Agencia
			StockRegister 	= R.NEW(TT.TE.STOCK.REGISTER) 					;*Segmento de Cheques
			SeriesId 		= R.NEW(TT.TE.SERIES.ID) 						;*Serie
			StmtNo 			= R.NEW(TT.TE.STMT.NO) 							;*Statement Entry
			PayeeName 		= R.NEW(TT.TE.PAYEE.NAME) 						;*Beneficiario del Cheque
			IdActivity 		= '' 											;*Id Actividad cuando Afecta Arrangement		
			Inputter 		= FIELD(R.NEW(TT.TE.INPUTTER), "_", 2) 			;*Digitador
			SourceID		= R.NEW(TT.TE.INPUTTER)							;*Inputter completo para leer el canal 
			Ip				= TSS$CLIENTIP 									;*Ip del cliente
			Narrative2		= R.NEW(TT.TE.NARRATIVE.2)						;*Descripcion
			DepartmentCode	= RR.USER<EB.USE.DEPARTMENT.CODE>				;*DepartmentCode
			 
			IF TfsReference NE '' THEN
				ClieteExterno=''
			END ELSE
				CALL GET.LOC.REF("TELLER","LF.DOC.CLIEN.EX",DOC.NOCLIENT.POS)
				ClieteExterno	= R.NEW(TT.TE.LOCAL.REF)<1, DOC.NOCLIENT.POS> ;*CLIENTE EXTERNO
			END
			
			;*Leyendo valores de cuenta destino y origen (depende de que si es un CREDIT o DEBIT)
			IF(R.NEW(TT.TE.DR.CR.MARKER) EQ 'CREDIT') THEN
				OriginAccount 	= R.NEW(TT.TE.ACCOUNT.2) 						;*Cuenta Origen (cuenta a quien se le debita)
				DestinyAccount 	= R.NEW(TT.TE.ACCOUNT.1) 						;*Cuenta Destino (cuenta interna del banco)
				DebitCustomer	= R.NEW(TT.TE.CUSTOMER.2)						;*Id Cliente a quien se le debita					
				CreditCustomer	= R.NEW(TT.TE.CUSTOMER.1)						;*Id del cliente a quien se le acredita	
			END ELSE ;*si es DEBIT
				OriginAccount 	= R.NEW(TT.TE.ACCOUNT.1) 						;*Cuenta Origen
				DestinyAccount 	= R.NEW(TT.TE.ACCOUNT.2) 						;*Cuenta Destino
				DebitCustomer	= R.NEW(TT.TE.CUSTOMER.1)						;*Id Cliente a quien se le debita					
				CreditCustomer	= R.NEW(TT.TE.CUSTOMER.2)						;*Id del cliente a quien se le acredita	
			END			
			;* Personalizar DateTime Field
			;*-----------------------------------------------------------------
		    IF(DCOUNT(R.NEW(TT.TE.DATE.TIME),".") > 0 ) THEN
	    		Y.DATETIME = R.NEW(TT.TE.DATE.TIME)
	    		GOSUB PARSE_DATETIME ;*Se Parsea dado que la fecha viene mala por ambiente malo
			END ELSE				
			 	D_DATENOW  = OCONV(ICONV(R.NEW(TT.TE.DATE.TIME),"DMDY"),"D-E[2,A3,2]")
		    	Y.DATETIME = D_DATENOW[7,4]:D_DATENOW[4,2]:D_DATENOW[1,2] 			
    		END    	
			;*-----------------------------------------------------------------								
			DateTime 		= UsLocGetDate									;*Fecha y Hora del Registro
			;*Authorizer 		= FIELD(R.NEW(TT.TE.AUTHORISER),"_",2)			;*Autorizador
			
			CAJERO = OPERATOR
		    CALL F.READ(FN.USER,CAJERO,USR.DATA,F.USER,USR.ERROR)
			
			;*Authorizer  = FIELD(R.NEW(TFS.AUTHORISER),"_",2)
			UsuarioAutorizador  = USR.DATA<EB.USE.SIGN.ON.NAME>
				
			IF UsuarioAutorizador NE '' THEN
			   Authorizer = UsuarioAutorizador
			END
			ELSE
			   Authorizer = FIELD(R.NEW(TFS.AUTHORISER),"_",2)
			END
			
			
			Company 		= R.NEW(TT.TE.CO.CODE) 							;*Agencia
			CurrNum 		= R.NEW(TT.TE.CURR.NO) 							;*Numero Actual del Registro
			
			;* Obtener ultimo valor de campo OVERRIDE
			;*----------------------------------------
			OVER = DCOUNT(R.NEW(TT.TE.OVERRIDE), SM)
			IF OVER EQ 0 THEN
				OVER = 1
			END
			Override 		= R.TT<TT.TE.OVERRIDE><1, OVER>					;*Ultimo override
			
					;*Charges,Commissions y Taxes	
					
			CANT_CHRG_TYPE = DCOUNT(R.NEW(TT.TE.CHARGE.CODE), VM)
			FOR K = 1 TO CANT_CHRG_TYPE
				IF R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,K> NE 0.00 THEN
					 
					 ;*STR.DATACHARGE 	:= R.NEW(TT.TE.CHARGE.CODE)<1,K>	 									   						;*Tipo de Cargo
				 	 STR.MONTO 	= R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,K>																		;*Monto de Cargo
				 	 STR.TRANSACTION		= 'CHGTT'																						;*Tipo de Cargo
				 	 ;*Arreglo con registro de cargos y comisiones			 	 			 	 
				 	 ;*----ARR.CHARGE<CONT>    = STR.DATACHARGE
				 	 ;*Arreglo con registro de id de cargos/comisiones para nombre de archivo
				 	 ARR.IDCHARGES = R.NEW(TT.TE.CHARGE.CODE)<1,K>
				 	 
				 	 IF ARR.IDCHARGES EQ '11' THEN
				 		STR.TRANSACTION 	 = 'TAXTT'																			;*Tipo de Cargo
				     END
				 	 
				 	 GOSUB SEND_CHARGES				 	 
				 	 ;*----STR.DATACHARGE 	= ''	
				 	 CONT 		    	= CONT + 1 	
			 	 END
 	 		NEXT K
 	 		
 	 		;*Obtener informacion de remesa familiar
			IF TransactionType EQ TXN.REME THEN
				GOSUB GET.INFO.REMESA
			END
			
		END
		;* Si es Transaccion por FUNDS.TRANSFER Entrar Aca
		;*------------------------------------------------
		ELSE IF LEFT(TransactionId, 2) EQ FT_Aplication THEN
				TellerId      	= '' 											;*Id del Cajero				
				TranDateTime  	= R.NEW(FT.PROCESSING.DATE)	 					;*Fecha y Hora de la Transaccion (AAMMDD HH:MM)
				Amount 			= R.NEW(FT.LOC.AMT.DEBITED)	 					;*Monto de Transaccion
				OriginAccount 	= R.NEW(FT.DEBIT.ACCT.NO) 						;*Cuenta Origen
				DestinyAccount 	= R.NEW(FT.CREDIT.ACCT.NO) 						;*Cuenta Destino
				CheckNumber 	= R.NEW(FT.CHEQUE.NUMBER)	 					;*Numero de Cheque
				CheckType 		= R.NEW(FT.CHEQ.TYPE)	 						;*Tipo de Cheque
				TypeComission 	= R.NEW(FT.COMMISSION.CODE)						;*Tipo de Comision
				;*ComissionAmount = SUBSTRINGS(R.NEW(FT.COMMISSION.AMT),5,LEN(R.NEW(FT.COMMISSION.AMT)))		;*Monto de Comisiones
				ComissionAmount  = '' ;*SUBSTRINGS(R.NEW(FT.COMMISSION.AMT),5,LEN(R.NEW(FT.COMMISSION.AMT)))		;*Monto de Comisiones
				TypeCharge 		= R.NEW(FT.CHARGE.CODE) 						;*Tipo de Cargo
				ChargeAmount 	= R.NEW(FT.CHARGE.AMT)							;*Monto de Cargos
				TransactionType = R.NEW(FT.TRANSACTION.TYPE) 					;*Tipo de Transaccion						
				IVA 			= SUBSTRINGS(R.NEW(FT.TOTAL.TAX.AMOUNT),4,LEN(R.NEW(FT.TOTAL.TAX.AMOUNT)))	;*Monto de IVA
				ISR 			= '' 											;*Monto de ISR
				StatusTransaction = R.NEW(FT.RECORD.STATUS) 					;*Estatus de la Transaccion
				TotalDebited 	= R.NEW(FT.AMOUNT.DEBITED)						;*Total Debitado				
				TotalCredited 	= R.NEW(FT.AMOUNT.CREDITED)						;*Total Acreditado	
				DebitCustomer	= R.NEW(FT.DEBIT.CUSTOMER)						;*Id Cliente a quien se le debita					
				CreditCustomer	= R.NEW(FT.CREDIT.CUSTOMER)						;*Id del cliente a quien se le acredita
				
				;*Se hace SubString porque el Formato que Viene es Ej: USD ###.## el USD no interesa
				;*----------------------------------------------------------------------------------
				TotalDebited	= SUBSTRINGS(TotalDebited,5,LEN(TotalDebited))
				TotalCredited	= SUBSTRINGS(TotalCredited,5,LEN(TotalCredited))
							
				TfsReference 	= R.NEW(FT.TFS.REFERENCE)	 					;*Numero de Referencia de TFS
				StockNumber 	= R.NEW(FT.STOCK.NUMBER)	 					;*Stock de Cheques Asignados a una Agencia
				StockRegister 	= R.NEW(FT.STOCK.REGISTER)	 					;*Segmento de Cheques
				SeriesId 		= R.NEW(FT.SERIES.ID)	 						;*Serie
				StmtNo 			= R.NEW(FT.STMT.NOS) 							;*Statement Entry
				PayeeName 		= R.NEW(FT.PAYEE.NAME)	 						;*Beneficiario del Cheque
				IdActivity 		= '' 											;*Id Actividad cuando Afecta Arrangement		
				Inputter 		= FIELD(R.NEW(FT.INPUTTER), "_", 2) 			;*Digitador
				SourceID		= R.NEW(FT.INPUTTER)							;*;*Inputter completo para leer el canal 
				Ip				= TSS$CLIENTIP 									;*Ip del cliente
				Narrative2		= R.NEW(FT.ORDERING.CUST)
				DepartmentCode	= RR.USER<EB.USE.DEPARTMENT.CODE>				;*DepartmentCode
				
				;*_________________Para Moneda azul MAPE___________________
				CALL GET.LOC.REF('FUNDS.TRANSFER','LF.DOC.CLIEN.EX', DOC.CLIEN.EX)
				FT.DOC.CLIEN.EX	= R.NEW(FT.LOCAL.REF)<1,DOC.CLIEN.EX>
				CALL GET.LOC.REF("FUNDS.TRANSFER","LF.COLECTOR.COD", COLECTOR.COD)
				LOCAL.COLECTOR.COD =  R.NEW(FT.LOCAL.REF)<1,COLECTOR.COD>
				IF LEFT(LOCAL.COLECTOR.COD,2) EQ 'SV' THEN
					SourceID = LOCAL.COLECTOR.COD
				END
				
				IF FT.DOC.CLIEN.EX EQ '' THEN
					FT.DOC.CLIEN.EX = '-'
				END
				
				;*_________________________________________________________
				
			   ;* para Colectores	
			   ;*-------------------------------------------------------------------
			   CALL GET.LOC.REF ("FUNDS.TRANSFER", "LF.COD.CL",CODL.POS)	
			    LfCodCl  = R.NEW(FT.LOCAL.REF)<1,CODL.POS>	;*codigo de colector			
						
			  	CALL F.READ(FN.SLV.COL, LfCodCl, R.COL,F.SLV.COL,COL.ERRERR) 		
					
			  	Categoria = R.COL<EB.CL.CATEGORIA.COLECTOR>		
			  	Colector  = R.COL<EB.CL.NOMBRE.COLECTOR>
			  	
			  	;* obtener IdTerminal para trx ATM y POS
			  	CALL GET.LOC.REF("FUNDS.TRANSFER","AT.UNIQUE.ID", ATM.ID.TRA)
			  	;*Y.ID.ATM.TRA = R.FT<FT.LOCAL.REF, ATM.ID.TRA>
			  	Y.ID.ATM.TRA = R.NEW(FT.LOCAL.REF)<1,ATM.ID.TRA>
			  	
				CALL F.READ(FN.ATM.TRX, Y.ID.ATM.TRA, R.ATMTRX,F.ATM.TRX,AT.ERR) 		
					
			  	CarAccId   = R.ATMTRX<AT.REV.CARD.ACC.ID>		
			  	CarAccName = R.ATMTRX<AT.REV.CARD.ACC.NAME.LOC>
			  	AtmPos     = R.ATMTRX<AT.REV.ATM.OR.POS>
				;*AtmNarr    = R.ATMTRX<AT.REV.NARRATIVE>
				AtmStatus  = R.ATMTRX<AT.REV.TRANS.STATUS>
			  	
				;*Agregar caracter para que ultimo campo de arreglo no lleve vacio
				IF AtmStatus EQ '' OR AtmStatus EQ NULL THEN
					AtmStatus = "-"
				END			
			;* --------------------------------------------------------------------	
								
				;*@Author:Ronald Ortiz
				;*@Date: 20171109
				;*@Description: Verify OFS SOURCE Banca en línea
				;*--------------------------------------------------------------
				GOSUB VERIFY.SOURCEID.BULKPAYMENT
				
				;* Personalizar DateTime Field
				;*--------------------------------------------------------------
			    IF(DCOUNT(R.NEW(FT.DATE.TIME),".") > 0 ) THEN
		    		Y.DATETIME = R.NEW(FT.DATE.TIME)
		    		GOSUB PARSE_DATETIME ;*Se Parsea dado que la fecha viene mala por ambiente malo
				END ELSE				
				 	D_DATENOW  = OCONV(ICONV(R.NEW(FT.DATE.TIME),"DMDY"),"D-E[2,A3,2]")
			    	Y.DATETIME = D_DATENOW[7,4]:D_DATENOW[4,2]:D_DATENOW[1,2] 				
	    		END    	
			;*------------------------------------------------------------------						
				DateTime 		= UsLocGetDate									;*Fecha y Hora del Registro
				;*Authorizer 		= FIELD(R.NEW(FT.AUTHORISER),"_",2)				;*Autorizador
				
				CAJERO = OPERATOR
				CALL F.READ(FN.USER,CAJERO,USR.DATA,F.USER,USR.ERROR)
			
				;*Authorizer  = FIELD(R.NEW(TFS.AUTHORISER),"_",2)
				UsuarioAutorizador  = USR.DATA<EB.USE.SIGN.ON.NAME>
				
				IF UsuarioAutorizador NE '' THEN
				   Authorizer = UsuarioAutorizador
				END
				ELSE
				   Authorizer = FIELD(R.NEW(TFS.AUTHORISER),"_",2)
				END
				
				Company 		= R.NEW(FT.CO.CODE) 							;*Agencia
				CurrNum 		= R.NEW(FT.CURR.NO) 							;*Numero Actual del Registro
				
				;* Obtener ultimo valor de campo OVERRIDE
				;*----------------------------------------
				OVER = DCOUNT(R.NEW(FT.OVERRIDE), SM)
				IF OVER EQ 0 THEN
					OVER = 1
				END
				Override 		= R.FT<FT.OVERRIDE><1, OVER> 					;*Ultimo override
								
				;*Commissions y Taxes
				CANT_COM_TYPE  	= DCOUNT(R.NEW(FT.COMMISSION.TYPE), VM)
				FOR J = 1 TO CANT_COM_TYPE
					IF SUBSTRINGS(R.NEW(FT.COMMISSION.AMT)<1,J> ,4, LEN(R.NEW(FT.COMMISSION.AMT)<1,J>)) NE 0.00 THEN
						;*STR.DATACHARGE 	:= R.NEW(FT.COMMISSION.CODE)														: ";"   ;*Codigo de Comision
						;*STR.DATACHARGE 	:= R.NEW(FT.COMMISSION.TYPE)<1,J> 												    : ";"	;*Tipo de Comision
					 	STR.MONTO 			 = SUBSTRINGS(R.NEW(FT.COMMISSION.AMT)<1,J> ,4, LEN(R.NEW(FT.COMMISSION.AMT)<1,J>)) ;*: ";"	;*Monto de Comision
					 	
					 	;*STR.DATACHARGE 	:= R.NEW(FT.TAX.TYPE)<1,J> 		 									 	 			: ";"	;*Tipo de Tax
					 	;*STR.DATACHARGE 	:= SUBSTRINGS(R.NEW(FT.TAX.AMT)<1,J> ,4, LEN(R.NEW(FT.TAX.AMT)<1,J>))				: ";"	;*Monto de Tax
					 	STR.TRANSACTION 	 = 'COMFT'																			;*Tipo de Cargo
					 	
					 	;*Arreglo con registro de comision y taxes		 	 
					 	;*ARR.CHARGE<CONT>   = STR.DATACHARGE
					 	;*Arreglo con registro de id de comision para nombre de archivo
					 	ARR.IDCHARGES = R.NEW(FT.COMMISSION.TYPE)<1,J>
					 	
					 	GOSUB SEND_CHARGES				 	 
					 	;*STR.DATACHARGE 	= ' '	
					 	CONT 		   	= CONT + 1 
				 	END			 	 	 
				NEXT J
			
				;*Charges
				CANT_CHRG_TYPE = DCOUNT(R.NEW(FT.CHARGE.TYPE), VM)
				FOR K = 1 TO CANT_CHRG_TYPE
					IF SUBSTRINGS(R.NEW(FT.CHARGE.AMT)<1,K> ,4, LEN(R.NEW(FT.CHARGE.AMT)<1,K>)) NE 0.00 THEN
						;*STR.DATACHARGE	= TransactionId		 												 				: ";"		;*Id de Transacción
						;*STR.DATACHARGE 	= R.NEW(FT.CHARGE.CODE) 															: ";"		;*Codigo de Cargo
						;*STR.DATACHARGE 	= R.NEW(FT.CHARGE.TYPE)<1,K> 		 									   			: ";"		;*Tipo de Cargo
					 	STR.MONTO 	        = SUBSTRINGS(R.NEW(FT.CHARGE.AMT)<1,K> ,4, LEN(R.NEW(FT.CHARGE.AMT)<1,K>))			  		    ;*Monto de Cargo				 	 
					 	STR.TRANSACTION 	= 'CHGFT'																					;*Tipo de Cargo
			        
						;*Arreglo con registro de cargo 				 	 			 	 
					 	;*ARR.CHARGE<CONT>   = STR.DATACHARGE
					 	;*Arreglo con registro de id de cargo para nombre de archivo
					 	ARR.IDCHARGES   = R.NEW(FT.CHARGE.TYPE)<1,K>
					 	GOSUB SEND_CHARGES
					 					 	 
					 	;*STR.DATACHARGE 	= ''	
						CONT 		    	= CONT + 1 	
					END
		 	 	NEXT K	
		 	 	
		 	 	;*Taxes
				CANT_TAX_TYPE = DCOUNT(R.NEW(FT.TAX.TYPE), VM)
				FOR I = 1 TO CANT_TAX_TYPE
					IF SUBSTRINGS(R.NEW(FT.TAX.AMT)<1,I> ,4, LEN(R.NEW(FT.TAX.AMT)<1,I>)) NE 0.00 THEN
						;*STR.DATACHARGE	= TransactionId		 												 				: ";"		;*Id de Transacción
						;*STR.DATACHARGE 	= R.NEW(FT.CHARGE.CODE) 															: ";"		;*Codigo de Cargo
						;*STR.DATACHARGE 	= R.NEW(FT.CHARGE.TYPE)<1,K> 		 									   			: ";"		;*Tipo de Cargo
					 	STR.MONTO 	        = SUBSTRINGS(R.NEW(FT.TAX.AMT)<1,I> ,4, LEN(R.NEW(FT.TAX.AMT)<1,I>))					        ;*Monto de Cargo				 	 
					 	STR.TRANSACTION 	= 'TAXFT'																					    ;*Tipo de Cargo
			        
						;*Arreglo con registro de cargo 				 	 			 	 
					 	;*ARR.CHARGE<CONT>   = STR.DATACHARGE
					 	;*Arreglo con registro de id de cargo para nombre de archivo
					 	ARR.IDCHARGES   = R.NEW(FT.TAX.TYPE)<1,I>
					 	GOSUB SEND_CHARGES
					 					 	 
					 	;*STR.DATACHARGE 	= ''	
						CONT 		    	= CONT + 1 	
					END
		 	 	NEXT K		
				IF Amount LT 0 THEN
					RETURN
				END
				
				;*valores para LBTR(implementacion temporar debara sustituirse por proyecto LBTR)
				 lbtrReason =''
				 ReassonDb	=''
				 ReassonCr	=''
							  
				IF R.NEW(FT.CREDIT.THEIR.REF) NE '' THEN
					ReassonCr=R.NEW(FT.CREDIT.THEIR.REF)
				END
					
				IF R.NEW(FT.DEBIT.THEIR.REF) NE '' THEN
					ReassonDb=R.NEW(FT.DEBIT.THEIR.REF)
				END
					lbtrReason=ReassonCr:ReassonDb
				;*temporalmente se utilizara AtmNarr, en la implementacion del proyecto LBTR se debara sustituir
					AtmNarr=lbtrReason
							
			END
			
			
			;* Si es Transaccion por FUNDS.TRANSFER Entrar Aca
			;*------------------------------------------------
			ELSE IF LEFT(TransactionId, 3) EQ CHG_Aplication THEN	 
			
				COMMI.TRANS = 	R.NEW(CHG.CHARGE.CODE)
				CALL F.READ(FN.COMMI,COMMI.TRANS,R.COMMI,F.COMMI,Y.ERR)				
				COMMI.CATE = R.COMMI<FT4.CATEGORY.ACCOUNT>
				CALL F.READ(FN.CATE,COMMI.CATE,R.CATE,F.CATE,Y.ERR)
				DESTINY.ACCOUNT = R.CATE<EB.CAT.SYSTEM.IND>:R.COMMI<FT4.CATEGORY.ACCOUNT>
												 
				TellerId      	= '' 												;*Id del Cajero				
				TranDateTime  	= R.NEW(CHG.CHARGE.DATE)	 						;*Fecha y Hora de la Transaccion (AAMMDD HH:MM)
				Amount 			= R.NEW(CHG.CHARGE.AMOUNT)	 						;*Monto de Transaccion
				OriginAccount 	= R.NEW(CHG.DEBIT.ACCOUNT)							;*Cuenta Origen
				DestinyAccount 	= DESTINY.ACCOUNT		 							;*Cuenta Destino
				CheckNumber 	= '' 					 							;*Numero de Cheque
				CheckType 		= ''  					 							;*Tipo de Cheque
				TypeComission 	= ''     					 						;*Tipo de Comision
				ComissionAmount = R.NEW(CHG.CHARGE.AMOUNT)		                	;*Monto de Comisiones
				TypeCharge 		= ''                    							;*Tipo de Cargo
				ChargeAmount 	= R.NEW(CHG.CHARGE.AMOUNT)							;*Monto de Cargos
				TransactionType = 'ACCHG'				 						    ;*Tipo de Transaccion						
				IVA 			= ''	 		 									;*Monto de IVA
				ISR 			= '' 												;*Monto de ISR
				StatusTransaction = R.NEW(CHG.RECORD.STATUS)	 					;*Estatus de la Transaccion
				TotalDebited 	  = R.NEW(CHG.TOTAL.CHG.AMT)						;*Total Debitado				
				TotalCredited 	  = ''												;*Total Acreditado	
				DebitCustomer	  = R.NEW(CHG.CUSTOMER.NO)							;*Id Cliente a quien se le debita					
				CreditCustomer	  = ''												;*Id del cliente a quien se le acredita
				
				;*Se hace SubString porque el Formato que Viene es Ej: USD ###.## el USD no interesa
				;*----------------------------------------------------------------------------------
				TotalDebited	= SUBSTRINGS(TotalDebited,5,LEN(TotalDebited))
				TotalCredited	= SUBSTRINGS(TotalCredited,5,LEN(TotalCredited))
							
				TfsReference 	= ''						 					;*Numero de Referencia de TFS
				StockNumber 	= '' 						 					;*Stock de Cheques Asignados a una Agencia
				StockRegister 	= ''						 					;*Segmento de Cheques
				SeriesId 		= ''					 						;*Serie
				CANT_STMT		= DCOUNT(R.NEW(CHG.STMT.NOS), VM)				
				StmtNo 			= R.NEW(CHG.STMT.NOS)<1,CANT_STMT>				;*Statement Entry
				PayeeName 		= ''					 						;*Beneficiario del Cheque
				IdActivity 		= '' 											;*Id Actividad cuando Afecta Arrangement		
				Inputter 		= FIELD(R.NEW(CHG.INPUTTER), "_", 2) 			;*Digitador
				SourceID		= R.NEW(CHG.INPUTTER)							;*;*Inputter completo para leer el canal 
				Ip				= TSS$CLIENTIP 									;*Ip del cliente
				Narrative2		= ''											;*Descripcion
				DepartmentCode	= ''											;*DepartmentCode
				
				;* Personalizar DateTime Field
				;*--------------------------------------------------------------
				IF(DCOUNT(R.NEW(CHG.DATE.TIME),".") > 0 ) THEN
					Y.DATETIME = R.NEW(CHG.DATE.TIME)
					GOSUB PARSE_DATETIME ;*Se Parsea dado que la fecha viene mala por ambiente malo
				END ELSE				
				 	D_DATENOW  = OCONV(ICONV(R.NEW(CHG.DATE.TIME),"DMDY"),"D-E[2,A3,2]")
					Y.DATETIME = D_DATENOW[7,4]:D_DATENOW[4,2]:D_DATENOW[1,2] 				
				END    	
				;*----------------------------------------------------------------						
				DateTime 		= Y.DATETIME									;*Fecha y Hora del Registro
				Authorizer 		= FIELD(R.NEW(CHG.AUTHORISER),"_",2)			;*Autorizador
				Company 		= R.NEW(CHG.CO.CODE)  							;*Agencia
				CurrNum 		= R.NEW(CHG.CURR.NO)  							;*Numero Actual del Registro
				
				;* Obtener ultimo valor de campo OVERRIDE
				;*----------------------------------------
				OVER = DCOUNT(R.NEW(CHG.OVERRIDE), SM)
				IF OVER EQ 0 THEN
					OVER = 1
				END
				Override 		= R.NEW<CHG.OVERRIDE><1, OVER>					;*Ultimo override
				
				;*Taxes
				CANT_TAX_TYPE = DCOUNT(R.NEW(CHG.TAX.CODE), VM)
				FOR I = 1 TO CANT_TAX_TYPE
					IF R.NEW(CHG.TAX.AMT)<1,I> NE 0.00 THEN
						;*STR.DATACHARGE	= TransactionId		 												 				: ";"		;*Id de Transacción
						;*STR.DATACHARGE 	= R.NEW(FT.CHARGE.CODE) 															: ";"		;*Codigo de Cargo
						;*STR.DATACHARGE 	= R.NEW(FT.CHARGE.TYPE)<1,K> 		 									   			: ";"		;*Tipo de Cargo
					 	;*STR.MONTO 	        = SUBSTRINGS(R.NEW(CHG.TAX.AMT)<1,I> ,4, LEN(R.NEW(CHG.TAX.AMT)<1,I>))					        ;*Monto de Cargo
					 	STR.MONTO 	        = R.NEW(CHG.TAX.AMT)<1,I>					        				 	 
					 	STR.TRANSACTION 	= 'TAXCH'																					    ;*Tipo de Cargo
			        
						;*Arreglo con registro de cargo 				 	 			 	 
					 	;*ARR.CHARGE<CONT>   = STR.DATACHARGE
					 	;*Arreglo con registro de id de cargo para nombre de archivo
					 	ARR.IDCHARGES   = R.NEW(CHG.TAX.CODE)<1,I>
					 	GOSUB SEND_CHARGES
					 					 	 
					 	;*STR.DATACHARGE 	= ''	
						CONT 		    	= CONT + 1 	
					END
		 	 	NEXT K		
			END	
			
	
			
			IF OriginAccount[1,3] EQ 'USD' THEN  
				ACCOUNT_WORK=DestinyAccount
			END ELSE  
				ACCOUNT_WORK=OriginAccount
			END	
		
			;* Obteniendo saldos de cuentas de la transaccion
			CALL F.READ(FN_ACC, ACCOUNT_WORK, R_ACC,F_ACC,ACC_ERR) 
			TMP.AC.WORKING.BALANCE = R_ACC<AC.WORKING.BALANCE>
			GOSUB CALCULATE_BALANCE
			WorkingBalance = Y.WORKING.AMOUNT
			;* Obteniendo saldos de cuentas  Origen		
			CALL F.READ(FN_ACC, OriginAccount, R_ACC,F_ACC,ACC_ERR) 
			TMP.AC.WORKING.BALANCE= R_ACC<AC.WORKING.BALANCE>
			GOSUB CALCULATE_BALANCE
			WorkingBalanceO = Y.WORKING.AMOUNT
			;* Obteniendo saldos de cuentas Destino
			CALL F.READ(FN_ACC, DestinyAccount, R_ACC,F_ACC,ACC_ERR) 			
			TMP.AC.WORKING.BALANCE =R_ACC<AC.WORKING.BALANCE>
			GOSUB CALCULATE_BALANCE
			WorkingBalanceD = Y.WORKING.AMOUNT

			Currency='USD'
			
			;* fecha ultima actualizacion de cuenta
			AccDateLastUpd = R_ACC<AC.DATE.LAST.UPDATE>
			
			IF OriginAccount[1,3] NE 'USD' THEN
				IF LEFT(TransactionId, 2) EQ TT_Aplication THEN
				 	IF WorkingBalance LT 0 THEN
						RETURN				 		
				 	END
				END
			END

			GOSUB PREPARE_AND_SEND 
		
			
	RETURN
	
	CALCULATE_BALANCE:
	
			;* Calculando el Monto Bloqueado en la Cuenta
			SMT = "SELECT " : FN.LCK : " WITH ACCOUNT.NUMBER EQ '" : TMP.ACCOUNT : "'"
			CALL EB.READLIST(SMT, LIST.LCK, NAME.LCK, SELECTED.LCK, ERR.LCK)
			Y.WORKING.AMOUNT=0
			Y.SUM.LCK = 0
			FOR LCK = 1 TO SELECTED.LCK
			                CALL F.READ(FN.LCK, LIST.LCK<LCK>, RECORD.LCK, F.LCK, ERROR.LCK)
			                Y.SUM.LCK = Y.SUM.LCK + RECORD.LCK<AC.LCK.LOCKED.AMOUNT>
			NEXT LCK

			;* Saldo Disponible de la Cuenta (Menos Saldo Bloqueado) - AC.WORKING.BALANCE
			Y.WORKING.AMOUNT = TMP.AC.WORKING.BALANCE - Y.SUM.LCK
	RETURN
	
	;*-----------------------------------------
	GET.INFO.REMESA:
	;* Obtener informacion de remesa
	;*-----------------------------------------
	    Y.APPLI = "CUSTOMER"
	    Y.FIELD = "LF.DEPTO.3":VM:"LF.MUNICIPIO.3":VM:"LF.REME.ACTIVA":VM:"LF.REME.MOTIVO":VM:"LF.REME.MONTO"
	    Y.POS.FIELD = ""
	    
	    ;*Obtener posicion de campos locales
	    CALL MULTI.GET.LOC.REF(Y.APPLI, Y.FIELD, Y.POS.FIELD)
	
	    Y.POS.DEPT = Y.POS.FIELD<1,1>
	    Y.POS.MUNI = Y.POS.FIELD<1,2>
	    Y.POS.RACT = Y.POS.FIELD<1,3>
	    Y.POS.MOTI = Y.POS.FIELD<1,4>
	    Y.POS.MONT = Y.POS.FIELD<1,5>

		;*Registro de remesa
		Y.REM.ID.REG = R.NEW(TT.TE.OUR.REFERENCE)

		CALL F.READ(FN.PAG.REME, Y.REM.ID.REG, R.REMESA, F.PAG.REME, ERR.PAG.REME)
		Y.REM.ID.CUS = R.REMESA<EB.SLV.PRF.REM.ID.CUS>
		IF R.REMESA THEN
			IF Y.REM.ID.CUS THEN
				CALL CACHE.READ(FN_CUS,Y.REM.ID.CUS,R.REM.CUS,F_CUS,ERR.REM.CUS)
				;*Extraer ultimas 2 posiciones del departemento : 08.05.2018
				;*----------------------------------------------------------------
				DepartamentoBenef	   = RIGHT(R.REM.CUS<EB.CUS.LOCAL.REF><1,Y.POS.DEPT>, 2) ;*52
				;*Extraer ultimas 4 posiciones del municipio : OCORNEJO 08.05.2018
				;*----------------------------------------------------------------
				MunicipioBenef		   = RIGHT(R.REM.CUS<EB.CUS.LOCAL.REF><1,Y.POS.MUNI>, 4) ;*53 
				;*----------------------------------------------------------------
				ConceptoRemesa		   = R.REM.CUS<EB.CUS.LOCAL.REF><1,Y.POS.MOTI> ;*55
				RecibeRemesa		   = R.REM.CUS<EB.CUS.LOCAL.REF><1,Y.POS.RACT> ;*62
				MontoMensualRem		   = R.REM.CUS<EB.CUS.LOCAL.REF><1,Y.POS.MONT> ;*63
				NacionalidadBenef	   = R.REM.CUS<EB.CUS.NATIONALITY> 			   ;*51			
				TelefonoOrdenanteBenef = R.REM.CUS<EB.CUS.PHONE.1><1,1>			   ;*64
				;*Agregar caracter para que ultimo campo de arreglo no lleve vacio
				IF TelefonoOrdenanteBenef EQ '' OR TelefonoOrdenanteBenef EQ NULL THEN
					TelefonoOrdenanteBenef = "-"
				END

			END
			Y.REMIT.NAME  = TRIM(R.REMESA<EB.SLV.PRF.REMIT.FIRST.NAME>) :" ": TRIM(R.REMESA<EB.SLV.PRF.REMIT.SECOND.NAME>):" "
			Y.REMIT.NAME := TRIM(R.REMESA<EB.SLV.PRF.REMIT.LAST.NAME>)  :" ": TRIM(R.REMESA<EB.SLV.PRF.REMIT.SE.LAST.NAME>)
			Y.REMIT.NAME = TRIM(Y.REMIT.NAME)

			TipoDocBenef		   = R.REMESA<EB.SLV.PRF.BENEF.DOC.TYPE> 	;*49
			NumDocBenef			   = R.REMESA<EB.SLV.PRF.BENEF.ID.DOCUMENT> ;*50
			VinculacionBenefOrd	   = R.REMESA<EB.SLV.PRF.REM.ID.VINCULO> 	;*54
			NumRefRemesa		   = R.REMESA<EB.SLV.PRF.REMITTANCE.NUMBER> ;*56
			Remesadora			   = Y.ID.REME 								;*57
			FechaEnvioRemesa	   = R.REMESA<EB.SLV.PRF.REM.SALE.DATE> 	;*58
			NombreOrdenante		   = Y.REMIT.NAME 							;*59
			PaisOrigenRem		   = R.REMESA<EB.SLV.PRF.REM.ORIG.COUNTRY> 	;*60
			ClienteUsuario		   = R.REMESA<EB.SLV.PRF.REM.ID.CUS> 		;*61
		END
	RETURN
		
	;* Preparar y Enviar Arreglo a Archivo CSV
	;*-----------------------------------------
	PREPARE_AND_SEND:
	    STR.ARR = ""
		STR.ARR := TransactionId : ";"	;*1
		STR.ARR := TellerId : ";"		;*2
		STR.ARR := TranDateTime : ";"	;*3	
		STR.ARR := Amount : ";"			;*4
		STR.ARR := OriginAccount : ";"	;*5
		STR.ARR := DestinyAccount : ";" ;*6
		STR.ARR := CheckNumber : ";"	;*7
		STR.ARR := CheckType : ";"		;*8
		STR.ARR := TypeComission : ";"	;*9
		STR.ARR := ComissionAmount : ";";*10 
		STR.ARR := TypeCharge : ";"		;*11
		STR.ARR := ChargeAmount : ";"	;*12	
		STR.ARR := TransactionType : ";";*13
		STR.ARR := IVA : ";"			;*14	
		STR.ARR := ISR : ";"			;*15
		STR.ARR := LEFT(TRIM(StatusTransaction, ";","A"), 9) : ";"	;*16
		STR.ARR := TotalDebited : ";"								;*17
		STR.ARR := TotalCredited : ";"								;*18
		STR.ARR	:= DebitCustomer : ";"								;*19
		STR.ARR := CreditCustomer : ";"								;*20
		STR.ARR := TfsReference : ";"								;*21
		STR.ARR := LEFT(TRIM(StockNumber, ";","A"), 29): ";"		;*22
		STR.ARR := LEFT(TRIM(StockRegister, ";","A"),29 ): ";"		;*23
		STR.ARR := LEFT(TRIM(SeriesId, ";","A"),29) : ";"			;*24
		STR.ARR := LEFT(TRIM(StmtNo, ";","A"),249) : ";"			;*25
		STR.ARR := LEFT(TRIM(PayeeName, ";","A"),59) : ";"			;*26
		STR.ARR := IdActivity : ";"									;*27
		STR.ARR := LEFT(TRIM(Inputter, ";","A"),59) : ";"			;*28
		STR.ARR := TRIM(SourceID, ";","A") : ";"			;*29
		STR.ARR	:= Ip		: ";"									;*30
		STR.ARR := DateTime : ";"									;*31
		STR.ARR := LEFT(TRIM(Authorizer, ";","A"),14) : ";"			;*32
		STR.ARR := Company : ";"									;*33
		STR.ARR := CurrNum : ";"									;*34
		STR.ARR := LEFT(TRIM(Override, ";","A"),349)	:";"		;*35	
		STR.ARR := WorkingBalanceO:";"  							;*36
		STR.ARR := WorkingBalanceD:";"  							;*37
		STR.ARR := WorkingBalance:";"   							;*38		
		STR.ARR := Currency:";"         							;*39
		STR.ARR := LEFT(TRIM(ClieteExterno, ";","A"),49):";"    	;*40
		STR.ARR := LEFT(TRIM(Narrative2, ";","A"),249):";"       	;*41
		STR.ARR := LEFT(TRIM(DepartmentCode, ";","A"),19)  	 :";" 	;*42
		STR.ARR := AccDateLastUpd	 						 :";" 	;*43 -- para AFD --
		STR.ARR := LEFT(TRIM(LfCodC, ";","A"),6)			 :";"	;*44  
		STR.ARR := LEFT(TRIM(Categoria, ";","A"),19)		 :";"	;*45 
		STR.ARR := LEFT(TRIM(Colector, ";","A"),39)			 :";"	;*46
		STR.ARR := LEFT(TRIM(CarAccId, ";","A"),19)			 :";"   ;*47
		STR.ARR := LEFT(TRIM(CarAccName, ";","A"),99) 		 :";"   ;*48 --   **   --
		STR.ARR := LEFT(TRIM(AtmPos, ";","A"),10)            :";"   ;*49  --  **  --
		STR.ARR := LEFT(TRIM(AtmNarr, ";","A"),199)          :";"   ;*50 
		STR.ARR := LEFT(TRIM(AtmStatus, ";","A"),49)         :";"   ;*51  --   **   -- 
		;*Campos de remesa familiares
		STR.ARR := TipoDocBenef		      :";" ;*52
		STR.ARR := NumDocBenef			  :";" ;*53
		STR.ARR := NacionalidadBenef	  :";" ;*54
		STR.ARR := DepartamentoBenef   	  :";" ;*55
		STR.ARR := MunicipioBenef		  :";" ;*56
		STR.ARR := VinculacionBenefOrd	  :";" ;*57
		STR.ARR := ConceptoRemesa		  :";" ;*58
		STR.ARR := NumRefRemesa		   	  :";" ;*59
		STR.ARR := Remesadora			  :";" ;*60
		STR.ARR := FechaEnvioRemesa	   	  :";" ;*61
		STR.ARR := LEFT(TRIM(NombreOrdenante, ";","A"),149) :";" ;*62
		STR.ARR := PaisOrigenRem		  :";" ;*63
		STR.ARR := ClienteUsuario		  :";" ;*64
		STR.ARR := RecibeRemesa		   	  :";" ;*65
		STR.ARR := MontoMensualRem 		  :";" ;*66
		STR.ARR := TelefonoOrdenanteBenef :";" ;*67
		;*Agregado para moneda Azul MAPE
		STR.ARR := FT.DOC.CLIEN.EX
		

		;* Generacion de csv array para escritura de en csv
		;*--------------------------------------------------
		    ;*Agregar arreglo al archivo
		    ;*--------------------------
*		    WRITESEQ STR.ARR APPEND TO SEQ.PTR THEN ;*con retorno de carro
*    		END
    		WRITEBLK STR.ARR ON SEQ.PTR THEN ;*sin retorno de carro
    		END
		;*--------------------------------------------------
		
	    CLOSESEQ SEQ.PTR
	RETURN
	
	PARSE_DATETIME:
		utcDateTime =  Y.DATETIME
		localZoneDate1 = OCONV(LOCALDATE(utcDateTime,localZone),'D4/C') ;*08/22/1970 mes dia año
		localZoneDate2 = localZoneDate1[7,4]:localZoneDate1[1,2]:localZoneDate1[4,2]
		localZoneTime1=OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
		Y.DATETIME = localZoneDate2:' ':localZoneTime1		
	RETURN
	
	
	DELETE_AND_OPEN_CH:
		;* Eliminando archivo existente
		;*------------------------------
	    DELETESEQ DIR_CH.NAME,NAME_CH.FILE THEN
	    END
	
		;* Abriendo archivo para escritura
		;*---------------------------------
	    OPENSEQ DIR_CH.NAME,NAME_CH.FILE TO SEQ_CH.PTR THEN
	        WEOFSEQ NAME_CH.FILE
	    END
	RETURN
	
	SEND_CHARGES:
			;*File Name
			
			IF OriginAccount[1,3] EQ 'USD' THEN  
				ACCOUNT_WORK=DestinyAccount
			END ELSE  
				ACCOUNT_WORK=OriginAccount
			END	
		
			;* Obteniendo saldos de cuentas de la transaccion
			CALL F.READ(FN_ACC, ACCOUNT_WORK, R_ACC,F_ACC,ACC_ERR) 
			TMP.AC.WORKING.BALANCE = R_ACC<AC.WORKING.BALANCE>
			GOSUB CALCULATE_BALANCE
			WorkingBalance = Y.WORKING.AMOUNT
			;* Obteniendo saldos de cuentas  Origen		
			CALL F.READ(FN_ACC, OriginAccount, R_ACC,F_ACC,ACC_ERR) 
			TMP.AC.WORKING.BALANCE= R_ACC<AC.WORKING.BALANCE>
			GOSUB CALCULATE_BALANCE
			WorkingBalanceO = Y.WORKING.AMOUNT
			;* Obteniendo saldos de cuentas Destino
			CALL F.READ(FN_ACC, DestinyAccount, R_ACC,F_ACC,ACC_ERR) 			
			TMP.AC.WORKING.BALANCE =R_ACC<AC.WORKING.BALANCE>
			GOSUB CALCULATE_BALANCE
			WorkingBalanceD = Y.WORKING.AMOUNT

			Currency='USD'
			
			IF OriginAccount[1,3] NE 'USD' THEN
				IF LEFT(TransactionId, 2) EQ TT_Aplication THEN
				 	IF WorkingBalance LT 0 THEN
						RETURN				 		
				 	END
				END
			END
			NAME_CH.FILE = '_T24Transaction.' : ID.NEW : '-' : ARR.IDCHARGES :'.csv'
			
			GOSUB DELETE_AND_OPEN_CH
			
			STR.CHR = ""
			STR.CHR := TransactionId : ";"	;*1
			STR.CHR := TellerId : ";"		;*2
			STR.CHR := TranDateTime : ";"	;*3	
			STR.CHR := STR.MONTO : ";"		;*4
			STR.CHR := OriginAccount : ";"	;*5
			STR.CHR := DestinyAccount : ";" ;*6
			STR.CHR := CheckNumber : ";"	;*7
			STR.CHR := CheckType : ";"		;*8
			STR.CHR := TypeComission : ";"	;*9
			STR.CHR := ComissionAmount : ";";*10 
			STR.CHR := TypeCharge : ";"		;*11
			STR.CHR := ChargeAmount : ";"	;*12	
			STR.CHR := STR.TRANSACTION : ";";*13
			STR.CHR := IVA : ";"			;*14	
			STR.CHR := ISR : ";"			;*15
			STR.CHR := LEFT(TRIM(StatusTransaction, ";","A"), 9) : ";"	;*16
			;*STR.CHR := TotalDebited : ";"	;*17
			STR.CHR :=  "0;"	;*17
			;*STR.CHR := TotalCredited : ";"	;*18
			STR.CHR :=  "0;"	;*18
			STR.CHR	:= DebitCustomer : ";"	;*19
			STR.CHR := CreditCustomer : ";"	;*20
			STR.CHR := TfsReference : ";"	;*21
			STR.CHR := LEFT(TRIM(StockNumber, ";","A"), 29): ";"		;*22
			STR.CHR := LEFT(TRIM(StockRegister, ";","A"),29 ): ";"		;*23
			STR.CHR := LEFT(TRIM(SeriesId, ";","A"),29) : ";"			;*24
			STR.CHR := LEFT(TRIM(StmtNo, ";","A"),249) : ";"			;*25
			STR.CHR := LEFT(TRIM(PayeeName, ";","A"),59) : ";"			;*26
			STR.CHR := IdActivity : ";"		;*27
			STR.CHR := LEFT(TRIM(Inputter, ";","A"),59) : ";"			;*28
			STR.CHR := TRIM(SourceID, ";","A") : ";"			;*29
			STR.CHR	:= Ip		: ";"		;*30
			STR.CHR := DateTime : ";"		;*31
			STR.CHR := LEFT(TRIM(Authorizer, ";","A"),14) : ";"			;*32
			STR.CHR := Company : ";"		;*33
			STR.CHR := CurrNum : ";"		;*34
			STR.CHR := LEFT(TRIM(Override, ";","A"),349)	:";"		;*35	
			STR.CHR := WorkingBalanceO:";"	;*36
			STR.CHR := WorkingBalanceD:";"  ;*37
			STR.CHR := WorkingBalance:";"   ;*38		
			STR.CHR := Currency:";"			;*39	
			STR.CHR := LEFT(TRIM(ClieteExterno, ";","A"),49):";"    	;*40
			STR.CHR := LEFT(TRIM(Narrative2, ";","A"),249):";"       	;*41
			STR.CHR := LEFT(TRIM(DepartmentCode, ";","A"),19)  :";" 	;*42	
			STR.CHR := AccDateLastUpd	 :";" 							;*43 -- para AFD --
			STR.CHR := LEFT(TRIM(LfCodC, ";","A"),6)			 :";"	;*44  
			STR.CHR := LEFT(TRIM(Categoria, ";","A"),19)		 :";"	;*45 
			STR.CHR := LEFT(TRIM(Colector, ";","A"),39)			 :";"	;*46
			STR.CHR := LEFT(TRIM(CarAccId, ";","A"),19)			 :";"   ;*47
			STR.CHR := LEFT(TRIM(CarAccName, ";","A"),99) 		 :";"   ;*48 --   **   -- 
			STR.CHR := LEFT(TRIM(AtmPos, ";","A"),10)            :";"   ;*49  --  **  --
			STR.CHR := LEFT(TRIM(AtmNarr, ";","A"),199)           :";"  ;*50  
			STR.CHR := LEFT(TRIM(AtmStatus, ";","A"),49)          :";"  ;*51  --   **   -- 	
			STR.CHR := TipoDocBenef		      :";" ;*52
			STR.CHR := NumDocBenef			  :";" ;*53
			STR.CHR := NacionalidadBenef	  :";" ;*54
			STR.CHR := DepartamentoBenef	  :";" ;*55
			STR.CHR := MunicipioBenef		  :";" ;*56
			STR.CHR := VinculacionBenefOrd	  :";" ;*57
			STR.CHR := ConceptoRemesa		  :";" ;*58
			STR.CHR := NumRefRemesa		   	  :";" ;*59
			STR.CHR := Remesadora			  :";" ;*60
			STR.CHR := FechaEnvioRemesa	   	  :";" ;*61
			STR.CHR := LEFT(TRIM(NombreOrdenante, ";","A"),149)	:";" ;*62
			STR.CHR := PaisOrigenRem		  :";" ;*63
			STR.CHR := ClienteUsuario		  :";" ;*64
			STR.CHR := RecibeRemesa		   	  :";" ;*65
			STR.CHR := MontoMensualRem  	  :";" ;*66
			STR.CHR := TelefonoOrdenanteBenef :";" ;*67
			;*Agregado para moneda Azul MAPE 
			STR.CHR := FT.DOC.CLIEN.EX		  

			
				
			;*V$FUNCTION  
			;*OFS$OPERATION
		    ;*Escribiendo
		  	WRITEBLK STR.CHR ON SEQ_CH.PTR THEN 
		  	;*sin retorno de carro
		  	END 
		  	
			CLOSESEQ SEQ_CH.PTR
			
			NAME_CH.FILE = ' '

	RETURN
	
	;*@Author:Ronald Ortiz
;*@Date: thursday 09th November, 2017 
;*@Description: SubProcess for verify the SourceID to send to Monitor Database from TCIB/TCE/TELLER Bulk Payment
VERIFY.SOURCEID.BULKPAYMENT:

FINDSTR "FT.BULK.PROCESS" IN SourceID SETTING Ap, Vp THEN 

IF TransactionType EQ 'ACAT' OR TransactionType EQ 'ACPT' THEN
    ;*SourceID = OFS.SOURCE.BANCA.LINEA
    SourceID = SourceID:'.TCIB'
END
ELSE
    
    InwardPayType = R.NEW(FT.INWARD.PAY.TYPE)
    IdBulkMasterft  = FIELD(InwardPayType,'-',2)
    
    FINDSTR "." IN IdBulkMasterft SETTING Ap1, Vp1 THEN
       IdBulkMaster = FIELD(IdBulkMasterft,'.',1)
    END
    ELSE
       IdBulkMaster = IdBulkMasterft
    END
    
    CALL F.READ(FN.BULK.MASTER,IdBulkMaster,ResponseBlkMaster,F.BULK.MASTER,ERR.BULKMASTER)
    
    CALL GET.LOC.REF('FT.BULK.MASTER','LF.FECHA.BP',PosBulkPaymentFT)
    TrxBulkPaymentDateFt = ResponseBlkMaster<FT.BLK.MAS.LOCAL.REF,PosBulkPaymentFT>
    
    IF TrxBulkPaymentDateFt NE '' THEN
    ;*SourceID = OFS.SOURCE.BANCA.LINEA
    SourceID = SourceID:'.TCIB'
    END
    
END

END ;*END FINDSTR

RETURN

END

