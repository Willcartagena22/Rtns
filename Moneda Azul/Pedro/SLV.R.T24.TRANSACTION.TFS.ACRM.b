*----------------------------------------------------------------------------------------------------
* <Rating>1221</Rating>
*----------------------------------------------------------------------------------------------------
* Nombre: SLV.R.T24.TRANSACTION.TFS.ACRM.b
* Descripcion: Genera archivo csv con informacion de Transacciones TFS T24.
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		galfaro	26.11.15	Version inicial
* 1.1       Galfaro 24.13.17        Revision para envio de reversas	
* 1.2		RRAMOS	29.10.18		Agregado campo MotivoAzul con dato motivo de la transferencia de Moneda Azul 
*----------------------------------------------------------------------------------------------------

SUBROUTINE SLV.R.T24.TRANSACTION.TFS.ACRM
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_GTS.COMMON
$INSERT I_EQUATE
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.USER
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.AC.LOCKED.EVENTS
*-----------------------------------------------------------------------------

*Control de ejecucion - TFS generadas al momento del validate.
IF OFS$OPERATION EQ 'PROCESS' THEN
	GOSUB INIT
	GOSUB OPENFILE
	GOSUB PROCESS
END
     
RETURN

	INIT:
		FN.TFS		= 'F.TELLER.FINANCIAL.SERVICES'
	    F.TFS		= ''
	    
	    FN.USER     = 'F.USER'
	    F.USER      = ''
	    
	   	FN.AA     = 'F.AA.ARRANGEMENT'
	    F.AA      = ''
	    
	    FN.LCK.EVT 		= 'F.AC.LOCKED.EVENTS$HIS'
    	F.LCK.EVT 		= ''
	    
		;* Tipos de aplicaciones
		;*----------------------
	    TFS_Aplication = 'TFS'	    
	    EndLine = '|'	;*Separador de linea
	    
	    EQU localZone TO 'America/Chicago' ;*Para setear Zona Horaria
	
		;*Definicion de variables
		;*-----------------------
	    DIR.NAME		= 'MON_AML'
	    NAME.FILE		= '' 
	    Y.DATETIME 		= ''
	    STR.DATA 		= ''
	    
	    
	    
	    ;*Iniciando variables
		TransactionIdTFS 	= ''    ;*1
		TransactionId   	= '' 	;*2
		TellerId			= '' 	;*3
		TranDateTime 		= '' 	;*4
		Amount 				= '' 	;*5
		OriginAccount 		= '' 	;*6
		DestinyAccount 		= '' 	;*7
		CheckNumber 		= '' 	;*8
		CheckType 			= '' 	;*9
		TypeComission 		= '' 	;*10
		ComissionAmount 	= '' 	;*11
		TypeCharge 			= '' 	;*12
		ChargeAmount 		= '' 	;*13
		TransactionType 	= '' 	;*14		
		IVA					= '' 	;*15
		ISR 				= '' 	;*16
		StatusTransaction 	= ''	;*17
		TotalDebited 		= '' 	;*18
		TotalCredited 		= '' 	;*19
		TfsReference 		= '' 	;*20
		StockNumber 		= ''	;*21
		StockRegister		= '' 	;*22
		SeriesId 			= ''	;*23
		StmtNo 				= ''	;*24
		PayeeName 			= ''	;*25
		IdActivity 			= ''	;*26
		Ip 					= ''	;*27
		Inputter 			= ''	;*28
		DateTime 			= ''	;*29
		Authorizer 			= ''	;*30
		Company 			= ''	;*31
		CurrNum 			= ''	;*32
		Override			= ''	;*33
		PrimayAcclf			= ''	;*34
		DepartmentCode		= ''    ;*35
		ClieteExterno		= ''	;*36
	RETURN
	
	OPENFILE:
		;* Apertura de Archivos
		;*----------------------
		CALL OPF(FN.TFS, F.TFS)
		CALL OPF(FN.USER,F.USER)
		CALL OPF(FN.AA,F.AA)
		CALL OPF(FN.LCK.EVT,F.LCK.EVT)
	RETURN
	
	
	PROCESS:
		;* Valor para final de archivo
		MotivoAzul = "-"	
					
		C_REG = DCOUNT(R.NEW(TFS.TRANSACTION), VM)
		
		;*Extraer Campos Locales
		;*----------------------
		APPL.NAME = 'TELLER.FINANCIAL.SERVICES'
	    FLD.NAME  = 'LF.DOC.CLIEN.EX':VM:'LF.NOM.PER':VM:'LF.TFS.LOAN.REF':VM:'LF.NUM.REF'
	    FLD.POS   = ''
	    
	    CALL MULTI.GET.LOC.REF(APPL.NAME,FLD.NAME,FLD.POS)
	
	    Y.LF.DOC = FLD.POS<1,1>
	    Y.LF.NOM.PER = FLD.POS<1,2>
	    Y.LF.TFS.LOAN.REF = FLD.POS<1,3>
	    Y.LF.NUM.REF = FLD.POS<1,4>
	    ;*----------------------

		;*================================
		;*Obteniendo campo local motivo transferencia para Moneda Azul
		
		CALL GET.LOC.REF ('AC.LOCKED.EVENTS','LF.MOTIVO.AZUL',MOTIVO.MONEDA.AZUL)
			
		;*================================

				;*-----------------------------------
			    FN.USER = 'F.USER'
			    F.USER =''
				V.USER= OPERATOR
				CALL OPF(FN.USER,F.USER)
				CALL F.READ(FN.USER,V.USER,RR.USER,F.USER,Y.ERR)
				;*-----------------------------------
		
		FOR I = 1 TO C_REG	
			 TransactionIdTFS = ID.NEW:'-':I 		;*Id de la Transaccion						
			;* Si es Transaccion por TELLER.FINANCIAL.SERVICE Entrar Aca
			;*-----------------------------------------------------------
			IF LEFT(TransactionIdTFS, 3) EQ TFS_Aplication THEN	
			
			CALL GET.LOC.REF ('TELLER.FINANCIAL.SERVICES','LF.TFS.LOAN.REF',LOCPOSLfPrimAc)
			    PrimayAcclf		=R.NEW(TFS.LOCAL.REF)<1,LOCPOSLfPrimAc>				
				TransactionId	= R.NEW(TFS.UNDERLYING)<1,I>					;*Id de la transacci�n TT o FT
				TellerId      	= R.NEW(TFS.DEPT.CODE)							;*Id del Cajero		
				

						
				TranDateTime  	= R.NEW(TFS.BOOKING.DATE)	 					;*Fecha y Hora de la Transaccion (AAMMDD HH:MM)		
											
				FOR J = 1 TO C_REG
					Amount 			= Amount + R.NEW(TFS.AMOUNT)<1,J>				;*Monto de Transaccion
					TotalDebited 	= TotalDebited + R.NEW(TFS.AMOUNT.DR)<1,J>		;*Total Debitado				
					TotalCredited 	= TotalCredited + R.NEW(TFS.AMOUNT.CR)<1,J>		;*Total Acreditado
				NEXT J
				
				OriginAccount 	= R.NEW(TFS.ACCOUNT.DR)<1,I> 					;*Cuenta Origen
				DestinyAccount 	= R.NEW(TFS.ACCOUNT.CR)<1,I>	 				;*Cuenta Destino
				CheckNumber 	= R.NEW(TFS.CHEQUE.NUMBER)<1,I> 				;*Numero de Cheque
				CheckType 		= R.NEW(TFS.CHQ.TYPE)<1,I> 						;*Tipo de Cheque
				TypeComission 	= ''											;*Tipo de Comision
				ComissionAmount = ''											;*Monto de Comisiones
				TypeCharge 		= R.NEW(TFS.CHG.CODE)<1,I>						;*Tipo de Cargo
				ChargeAmount 	= R.NEW(TFS.CHG.AMT)<1,I>						;*Monto de Cargos				
				TransactionType = R.NEW(TFS.TRANSACTION)<1,I>					;*Tipo de Transaccion						
				IVA 			= ''											;*Monto de IVA
				ISR 			= '' 											;*Monto de ISR
				StatusTransaction = R.NEW(TFS.RECORD.STATUS) 					;*Estatus de la Transaccion
				TfsReference 	= TransactionIdTFS							    ;*Numero de Referencia de TFS
				StockNumber 	= ''	 										;*Stock de Cheques Asignados a una Agencia
				StockRegister 	= ''	 										;*Segmento de Cheques
				SeriesId 		= ''	 										;*Serie
				StmtNo 			= R.NEW(TFS.R.UL.STMT.NO)<1,I>					;*Statement Entry
				
				PayeeName 		= R.NEW(TFS.LOCAL.REF)<1,Y.LF.NOM.PER>			;*Beneficiario del Cheque
				IdActivity 		= '' 											;*Id Actividad cuando Afecta Arrangement
				Ip				= TSS$CLIENTIP 									;*Ip del cliente														
				Inputter 		= FIELD(R.NEW(TFS.INPUTTER), "_", 2) 			;*Digitador
				DepartmentCode	= RR.USER<EB.USE.DEPARTMENT.CODE>				;*DepartmentCode
				
			;*Agregar caracter para que ultimo campo de arreglo no lleve vacio
				IF DepartmentCode EQ '' THEN
					DepartmentCode = "-"
				END			
				
                ;* Personalizar DateTime Field
                ;*--------------------------------------------------------------
                ;*IF(DCOUNT(R.NEW(TFS.DATE.TIME),".") > 0 ) THEN
 
                    Y.DATETIME = R.NEW(TFS.DATE.TIME)
                ;*  GOSUB PARSE_DATETIME ;*Se Parsea dado que la fecha viene mala por ambiente malo
                ;*END ELSE              
                ;*  D_DATENOW  = OCONV(ICONV(R.NEW(TFS.DATE.TIME),"DMDY"),"D-E[2,A3,2]")
                    D_DATENOW       =   OCONV(DATE(),"D/")
                    ;*Y.DATETIME = D_DATENOW[7,4]:D_DATENOW[4,2]:D_DATENOW[1,2]
                    Y.DATETIME = D_DATENOW[7,4]:D_DATENOW[1,2]:D_DATENOW[4,2]                   
                ;*END       
                ;*------------------------------------------------------------------   		
				DateTime 		= Y.DATETIME									;*Fecha y Hora del Registro
				;*Authorizer 		= FIELD(R.NEW(TFS.AUTHORISER),"_",2)			;*Autorizador
				
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
				
				Company 		= R.NEW(TFS.CO.CODE) 							;*Agencia
				CurrNum 		= R.NEW(TFS.CURR.NO) 							;*Numero Actual del Registro
				
				
				;* Obtener ultimo valor de campo OVERRIDE
				;*----------------------------------------
				OVER = DCOUNT(R.NEW(TFS.OVERRIDE), SM)
				IF OVER EQ 0 THEN
					OVER = 1
				END
				ClieteExterno	= ''
				ClieteExterno	= R.NEW(TFS.LOCAL.REF)<1, Y.LF.DOC>			;*CLIENTE EXTERNO
				Override 		= R.TFS<TFS.OVERRIDE><1, OVER>					;*Ultimo override
				;*Override        = R.NEW(TFS.OVERRIDE)
				ReferenciaAA = R.NEW(TFS.LOCAL.REF)<1, Y.LF.TFS.LOAN.REF> 
				 
				;*================================
				;*Obteniendo campo local motivo transferencia para Moneda Azul 
				ID.LCK.EVT = R.NEW(TFS.LOCAL.REF)<1,Y.LF.NUM.REF>				
				
				CALL F.READ(FN.LCK.EVT,ID.LCK.EVT,R.LCK.EVT,F.LCK.EVT,E.LCK.EVT)
				MotivoAzul = R.LCK.EVT<AC.LCK.LOCAL.REF,MOTIVO.MONEDA.AZUL>
				
				;*================================
				
				IF MotivoAzul EQ '' THEN
					MotivoAzul = '-'
				END
					
				;*Generar arreglo con la informaci�n					
				STR.ARR := TransactionIdTFS : ";"   ;*1
				STR.ARR := TransactionId    : ";"	;*2
				STR.ARR := TellerId :		  ";"	;*3
				STR.ARR := TranDateTime : 	  ";"	;*4
				STR.ARR := Amount : 		  ";"	;*5
				STR.ARR := OriginAccount :    ";"	;*6
				STR.ARR := DestinyAccount :   ";"	;*7
				STR.ARR := CheckNumber :      ";"	;*8
				STR.ARR := CheckType : 		  ";"	;*9
				STR.ARR := TypeComission :    ";"	;*10
				STR.ARR := ComissionAmount :  ";"	;*11
				STR.ARR := TypeCharge : 	  ";"	;*12
				STR.ARR := ChargeAmount :     ";"	;*13
				STR.ARR := TransactionType :                         ";"	;*14		
				STR.ARR := IVA : 			                         ";"	;*15
				STR.ARR := ISR :                                     ";"	;*16
				STR.ARR := LEFT(TRIM(StatusTransaction, ";","A"),9): ";"  	;*17
				STR.ARR := TotalDebited :                            ";"	;*18
				STR.ARR := TotalCredited :                           ";"	;*19
				STR.ARR := TfsReference :                            ";"	;*20
				STR.ARR := LEFT(TRIM(StockNumber, ";","A"), 249):    ";"	;*21
				STR.ARR := LEFT(TRIM(StockRegister, ";","A"), 249):  ";"	;*22
				STR.ARR := LEFT(TRIM(SeriesId, ";","A"),249) :       ";"	;*23
				STR.ARR := LEFT(TRIM(StmtNo, ";","A"), 249) :        ";"	;*24
				STR.ARR := LEFT(TRIM(PayeeName, ";","A"), 249):      ";"	;*25
				STR.ARR := IdActivity :                              ";"	;*26
				STR.ARR := Ip 		:	                             ";"	;*27
				STR.ARR := LEFT(TRIM(Inputter, ";","A"),29) :        ";"	;*28
				STR.ARR := DateTime :                                ";"	;*29
				STR.ARR := LEFT(TRIM(Authorizer, ";","A"),29) :      ";"    ;*30
				STR.ARR := Company :                                 ";"	;*31
				STR.ARR := CurrNum:                                  ";"	;*32
				STR.ARR := PrimayAcclf:                              ";"	;*33
				STR.ARR := LEFT(TRIM(Override, ";","A"),350):        ";"	;*34	
				STR.ARR := LEFT(TRIM(ClieteExterno, ";","A"), 49):   ";" 	;*35
				STR.ARR := LEFT(TRIM(DepartmentCode, ";","A"), 19):	 ";"	;*36
				STR.ARR := LEFT(TRIM(MotivoAzul, ";","A"), 149) 										;*37
				STR.DATA<I> =  STR.ARR
				STR.ARR = ''    
			END						
		NEXT I
		
		FOR Z=1 TO C_REG
			NAME.FILE	= '_T24TransactionTFS.' : ID.NEW : '-' : Z :'.csv'
			;* Eliminando archivo existente
			;*------------------------------
		    DELETESEQ DIR.NAME,NAME.FILE THEN
		    END
		
			;* Abriendo archivo para escritura
			;*---------------------------------
		    OPENSEQ DIR.NAME,NAME.FILE TO SEQ.PTR THEN
		        WEOFSEQ NAME.FILE
		    END
		
			;* Generacion de csv array para escritura de en csv
			;*--------------------------------------------------			
		    WRITEBLK STR.DATA<Z> ON SEQ.PTR THEN ;*sin retorno de carro
		    END	
			;*--------------------------------------------------			
		    CLOSESEQ SEQ.PTR
		NEXT Z
		

	RETURN		
	
*	PARSE_DATETIME:
*		utcDateTime =  Y.DATETIME
*		localZoneDate1 = OCONV(LOCALDATE(utcDateTime,localZone),'D4/C');*08/22/1970 mes dia a�o
*		localZoneDate2 = localZoneDate1[7,4]:localZoneDate1[1,2]:localZoneDate1[4,2]
*		localZoneTime1=OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
*		Y.DATETIME = localZoneDate2:' ':localZoneTime1		
*	RETURN


END
