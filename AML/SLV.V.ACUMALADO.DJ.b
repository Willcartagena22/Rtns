*-----------------------------------------------------------------------------
* <Rating>2436</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.ACUMALADO.DJ
*-----------------------------------------------------------------------------
*
* Nombre: SLV.V.ACUMALADO.DJ
* Descripción: Rutina para validar si el cliente ya sobrepaso limite para declaracion jurada acumulado
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor			Fecha		Comentario
*-----------------------------------------------------------------------------
* jhenriquez	5.07.2019	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_EQUATE
$INSERT I_COMMON
$INSERT I_F.TELLER
$INSERT I_F.ACCOUNT
$INSERT I_F.CUSTOMER
$INSERT I_F.AA.ACCOUNT
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.EB.SLV.KEYS.PARAMS
$INSERT I_F.EB.SLV.GLOBAL.PARAM
$INSERT I_F.EB.SLV.CONTADOR.MAZUL
$INSERT I_F.SLV.AML.PROFILE.PARAMETER 
*-----------------------------------------------------------------------------

GOSUB INI
GOSUB OPENFILE
GOSUB REVERSA
IF V$FUNCTION NE 'R' THEN
	IF V$FUNCTION NE 'D' THEN
		GOSUB PROCESS
	END
END 
RETURN

INI:
	FN.CONTA 	= 'F.EB.SLV.CONTADOR.MAZUL'
	F.CONTA		= ''
	
	FN.KP 		= 'F.EB.SLV.KEYS.PARAMS'
	F.KP 		= ''
	
	FN.ACC 		= 'F.ACCOUNT'
	F.ACC 		= ''
	
	FN.FT 		= 'F.FUNDS.TRANSFER'
	F.FT 		= ''
	
	FN.TT		= 'F.TELLER'
	F.TT		= ''
	 
	FN.CUS		= 'F.CUSTOMER'
	F.CUS		= ''
	
	FN.AML.PROFILE = 'F.SLV.AML.PROFILE.PARAMETER'
	F.AML.PROFILE = ''
	
	FN.GBL = 'F.EB.SLV.GLOBAL.PARAM'
	F.GBL  = ''
	CALL OPF(FN.GBL, F.GBL)
	
	CALL F.READ(FN.GBL, 'LOG.DJA', R.GBL, F.GBL, E.GBL)
	DIR.NAME   = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',1)	;*Ruta de Salida del Archivo
	R.ID       = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',3) : '.' : TODAY : '.log'	;*Nombre de Archivo Log
	LOG.ACTIVO = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',2)	;*Bandera de Log Activo/Inactivo
	
	V.AMT.LIMIT.RET 		= ''
	V.AMT.LIMIT.DEP			= ''
	V.AMT.TOLERANCIA.DEP 	= ''
	V.AMT.LIMIT.DEP			= ''
	V.AMT.LIMIT.RET			= ''
	V.AMT.TOLERANCIA.RET	= ''
	V.AMT.LIMIT.RET			= ''	
	EQU LIMIT.DJA TO 'SLV.LIMIT.MES.DCA'
RETURN

OPENFILE:
	CALL OPF(FN.CONTA, F.CONTA)
	CALL OPF(FN.KP, F.KP)
	CALL OPF(FN.ACC, F.ACC)
	CALL OPF(FN.FT, F.FT)
	CALL OPF(FN.TT, F.TT)
	CALL OPF(FN.CUS, F.CUS)
	CALL OPF(FN.AML.PROFILE, F.AML.PROFILE)
RETURN

REVERSA:
	IF V$FUNCTION EQ 'R' OR V$FUNCTION EQ 'D' THEN
    	Y.APP    = APPLICATION
		Y.TXN.ID = ID.NEW
   		Y.TXN.AMT = ''
   		
   		BEGIN CASE
    		CASE Y.APP EQ 'FUNDS.TRANSFER'
    			Y.TXN.AMT     = R.NEW(FT.DEBIT.AMOUNT)
    			IF Y.TXN.AMT EQ 0 OR Y.TXN.AMT EQ '' THEN
	   				Y.TXN.AMT = R.NEW(FT.CREDIT.AMOUNT)
   	 			END
   	 			Y.DEB.CUS   = R.NEW(FT.DEBIT.CUSTOMER)
				Y.CRE.CUS 	= R.NEW(FT.CREDIT.CUSTOMER)
				
				IF 	Y.DEB.CUS EQ Y.CRE.CUS THEN
					CALL SLV.I.COUNT.WRT.AMT.DJA(Y.DEB.CUS, Y.TXN.AMT)
				END
				ELSE
					CALL SLV.I.COUNT.WRT.AMT.DJA(Y.DEB.CUS, Y.TXN.AMT)
					CALL SLV.I.COUNT.WRT.AMT.DJA(Y.CRE.CUS, Y.TXN.AMT)
				END
    		CASE Y.APP EQ 'TELLER'
    				Y.TXN.AMT	= R.NEW(TT.TE.AMOUNT.LOCAL.1)
					V.CUS.1 	= R.NEW(TT.TE.CUSTOMER.1)
				    V.CUS.2 	= R.NEW(TT.TE.CUSTOMER.2)
				    
				    V.ACC.DEP 	= R.NEW(TT.TE.ACCOUNT.2)
				    CALL F.READ(FN.ACC, V.ACC.DEP, RECORD.ACC.DEP, F.ACC, ERRO.ACC)
					IF NOT(V.CUS.2) THEN
						V.CUS.2 = RECORD.ACC.DEP<AC.CUSTOMER>	
					END
					
				    V.ACC.RET 	= R.NEW(TT.TE.ACCOUNT.1)
					CALL F.READ(FN.ACC, V.ACC.RET, RECORD.ACC.RET, F.ACC, ERRO.ACC)
				    IF NOT(RECORD.ACC.RET) THEN
				    	V.CUS.1 = RECORD.ACC.RET<AC.CUSTOMER>
				    END
				    
				    IF V.CUS.1 EQ V.CUS.2 THEN 
				    	CALL SLV.I.COUNT.WRT.AMT.DJA(V.CUS.1, Y.TXN.AMT)
				    END
				    ELSE
				    	CALL SLV.I.COUNT.WRT.AMT.DJA(V.CUS.2, Y.TXN.AMT)
				    	CALL SLV.I.COUNT.WRT.AMT.DJA(V.CUS.1, Y.TXN.AMT)
				    END
    	END CASE
    END 
RETURN

PROCESS:
	Y.APP    = APPLICATION
	Y.TXN.ID = ID.NEW
    Y.TXN.AMT = ''
;*Obteniendo la Tolerancia definida
;*=================================
	CALL F.READ(FN.AML.PROFILE, 'SYSTEM', RECORD.AML.PROFILE, FN.AML.PROFILE, ERROR.AML.PROFILE)
	V.TOLERANCE	=	RECORD.AML.PROFILE<SLV.PROF.TOLERANCE>
	
	;*::::Log's:::::
	TEXTO.ARCHIVO = 'V.TOLERANCE = ':V.TOLERANCE
	GOSUB LOG.WRITE
	    
    BEGIN CASE
    	CASE Y.APP EQ 'FUNDS.TRANSFER'
    	GOSUB V.DJA.FT
    	CASE Y.APP EQ 'TELLER'
    	GOSUB V.DJA.TT
    END CASE
RETURN

V.DJA.FT:

	Y.TXN.AMT     = R.NEW(FT.DEBIT.AMOUNT)
;*Se valida para verificar que el campo debit tiene valor
;*=======================================================
    IF Y.TXN.AMT EQ 0 OR Y.TXN.AMT EQ '' THEN
	   	Y.TXN.AMT = R.NEW(FT.CREDIT.AMOUNT)
    END
    
	Y.FET.DATE  = R.NEW(FT.DEBIT.VALUE.DATE)
	Y.DEB.CUS   = R.NEW(FT.DEBIT.CUSTOMER)
	Y.CRE.CUS 	= R.NEW(FT.CREDIT.CUSTOMER)
	
	;*Se verifica que el la variable cliente tenga datos
	;*=================================================
	IF 	NOT(Y.DEB.CUS) THEN
		IF 	NOT(Y.CRE.CUS) THEN
			RETURN
		END
	END
	;*::::Log's:::::
	TEXTO.ARCHIVO = 'INFO DE LA TXN = ':Y.TXN.AMT:' // ':Y.FET.DATE:' // ':Y.DEB.CUS:' // ':Y.CRE.CUS
	GOSUB LOG.WRITE
	
	Y.DEBIT.ACC = R.NEW(FT.DEBIT.ACCT.NO)
	CALL F.READ(FN.ACC, Y.DEBIT.ACC, REDORD.ACC.DEBIT, F.ACC, ERRO.ACC)
	V.ARR.DEB = REDORD.ACC.DEBIT<AC.ARRANGEMENT.ID>
	
	;*::::Log's:::::
	TEXTO.ARCHIVO = 'INFO. CUENTA DEBITO = ':Y.DEBIT.ACC:' // ':V.ARR.DEB
	GOSUB LOG.WRITE
	
	Y.CREDI.ACC = R.NEW(FT.CREDIT.ACCT.NO)
	CALL F.READ(FN.ACC, Y.CREDI.ACC, REDORD.ACC.DEBIT, F.ACC, ERRO.ACC)
	V.ARR.CRE = REDORD.ACC.DEBIT<AC.ARRANGEMENT.ID>
	
	;*::::Log's:::::
	TEXTO.ARCHIVO = 'INFO. CUENTA CREDITO = ':Y.CREDI.ACC:' // ':V.ARR.CRE
	GOSUB LOG.WRITE
	
;*Se verifica si la transaccion es del mismo cliente, si lo es solo se tomara debito o credito
;*=======================================================
	IF 	Y.DEB.CUS EQ Y.CRE.CUS THEN
		;*Cliente Debito
		;*---------------------------
		;*Obteniendo el saldo acumulado que se tiene.
		;*---------------------------		
		CALL F.READ(FN.CONTA, Y.DEB.CUS:'-DJA', RECORD.DJA.DEB, F.CONTA, ERROR.CONTA.DEB)
		V.AMT.DJA.DEB = 0
		IF RECORD.DJA.DEB THEN
			V.AMT.DJA.DEB = RECORD.DJA.DEB<EB.SLV.TMA.ACUM.MES.MONTO>
			V.AMT.DJA.DEB = V.AMT.DJA.DEB + Y.TXN.AMT
	   	END
	   	
	   	;*::::Log's:::::
		TEXTO.ARCHIVO = 'TXN PROPIA = ':Y.DEB.CUS:'-DJA':' // ':RECORD.DJA.DEB:' // ':V.AMT.DJA.DEB
		GOSUB LOG.WRITE
	   
		;*Obteniendo el monto limite declarado para retiros
		;*---------------------------
		V.ARR = ''
		V.ARR = V.ARR.DEB
		V.TYPE = 'RET'
		GOSUB AMT.DECLARADO.CLIENTE
		
		;*Ejemplo de override con varios &
		;*TEXT = "INVALID FILE CLASS & FOR &":FM:FILE.CLASS:VM:APPLICATION
        ;*CALL FATAL.ERROR("UNAUTH.RECORD.WRITE") 
		
		;*Realizando validacion para el cliente Debito
		;*==============================================
		IF V.AMT.DJA.DEB GT V.AMT.LIMIT.RET THEN
			TEXT  = 'SLV.AML.DECL.JUR.ACUM':FM:Y.DEB.CUS:VM:Y.DEBIT.ACC
			CURRNO = DCOUNT(R.NEW(FT.OVERRIDE),VM) + 1
			CALL STORE.OVERRIDE(CURRNO)
		END
		
		;*Obteniendo el monto limite declarado para depositos
		;*---------------------------
		V.ARR = ''
		V.ARR = V.ARR.CRE
		V.TYPE = 'DEP'
		GOSUB AMT.DECLARADO.CLIENTE
		
		;*Realizando validacion para el cliente Credito
		;*===========================================	
		IF V.AMT.DJA.DEB GT V.AMT.LIMIT.DEP THEN
			;*TEXT  = 'SLV.AML.DECL.JUR':FM:Y.CREDI.ACC:' - Cliente ':Y.CRE.CUS
			TEXT  = 'SLV.AML.DECL.JUR.ACUM':FM:Y.DEB.CUS:VM:Y.CREDI.ACC
			CURRNO = DCOUNT(R.NEW(FT.OVERRIDE),VM) + 1
			CALL STORE.OVERRIDE(CURRNO)
		END			
	END
	ELSE
;***************************************Cliente Debito***********************************************	
		V.NOSTRO 	= ''
		V.ACC.USD 	= ''
		CALL F.READ(FN.ACC, Y.DEBIT.ACC, R.ACC.DEB, F.ACC, ERR.ACC.DEB)
		V.NOSTRO 	= R.ACC.DEB<AC.LIMIT.REF>
		V.ACC.USD 	= SUBSTRINGS(Y.DEBIT.ACC, 1, 3) 
		
		IF Y.DEB.CUS THEN	
			IF V.NOSTRO NE 'NOSTRO' THEN
				IF V.ACC.USD NE 'USD' THEN
					CALL F.READ(FN.CONTA, Y.DEB.CUS:'-DJA', RECORD.DJA.DEB, F.CONTA, ERROR.CONTA.DEB)
					V.AMT.DJA.DEB = 0
					IF RECORD.DJA.DEB THEN
						V.AMT.DJA.DEB = RECORD.DJA.DEB<EB.SLV.TMA.ACUM.MES.MONTO>
						V.AMT.DJA.DEB = V.AMT.DJA.DEB + Y.TXN.AMT
					END
								
					;*Obteniendo el monto limite declarado
					;*---------------------------
					V.ARR = ''
					V.ARR = V.ARR.DEB
					V.TYPE = 'RET'
					GOSUB AMT.DECLARADO.CLIENTE
					
					;*::::Log's:::::
					TEXTO.ARCHIVO = 'VALIDACION ACC DEBITO = ':V.AMT.DJA.DEB:' // ':V.AMT.LIMIT.RET
					GOSUB LOG.WRITE
					
					;*Realizando validacion para el cliente Debito
					;*===========================================
					IF V.AMT.DJA.DEB GT V.AMT.LIMIT.RET THEN
						TEXT  = 'SLV.AML.DECL.JUR.ACUM':FM:Y.DEB.CUS:VM:Y.DEBIT.ACC
						CURRNO = DCOUNT(R.NEW(FT.OVERRIDE),VM) + 1
						CALL STORE.OVERRIDE(CURRNO)
					END	
				END
			END
		END	
;******************************************Cliente Credito********************************************
		V.NOSTRO 	= ''
		V.ACC.USD 	= ''
		CALL F.READ(FN.ACC, Y.CREDI.ACC, R.ACC.DEB, F.ACC, ERR.ACC.DEB)
		V.NOSTRO 	= R.ACC.DEB<AC.LIMIT.REF>
		V.ACC.USD 	= SUBSTRINGS(Y.DEB.ACC, 1, 3) 
		
		IF Y.CRE.CUS THEN
			IF V.NOSTRO NE 'NOSTRO' THEN
				IF V.ACC.USD NE 'USD' THEN
					CALL F.READ(FN.CONTA, Y.CRE.CUS:'-DJA', RECORD.DJA.CRE, F.CONTA, ERROR.CONTA.CRE)
					V.AMT.DJA.CRE = 0
					IF RECORD.DJA.CRE THEN
						V.AMT.DJA.CRE = RECORD.DJA.CRE<EB.SLV.TMA.ACUM.MES.MONTO>
						V.AMT.DJA.CRE = V.AMT.DJA.CRE + Y.TXN.AMT
					END
					;*Obteniendo el monto limite declarado
					;*---------------------------
					V.ARR = ''
					V.ARR = V.ARR.CRE
					V.TYPE = 'DEP'
					GOSUB AMT.DECLARADO.CLIENTE
					
					;*::::Log's:::::
					TEXTO.ARCHIVO = 'VALIDACION ACC CREDITO = ':V.AMT.DJA.CRE:' // ':V.AMT.LIMIT.DEP
					GOSUB LOG.WRITE
					;*Realizando validacion para el cliente Credito
					;*===========================================	
					IF V.AMT.DJA.CRE GT V.AMT.LIMIT.DEP THEN
						TEXT  = 'SLV.AML.DECL.JUR.ACUM':FM:Y.CRE.CUS:VM:Y.CREDI.ACC
						CURRNO = DCOUNT(R.NEW(FT.OVERRIDE),VM) + 1
						CALL STORE.OVERRIDE(CURRNO)
					END
				END
			END
		END			
	END
RETURN

V.DJA.TT:
	Y.TXN.AMT	= R.NEW(TT.TE.AMOUNT.LOCAL.1)
	
	V.CUS.1 	= R.NEW(TT.TE.CUSTOMER.1)
    V.CUS.2 	= R.NEW(TT.TE.CUSTOMER.2)
        
    ;*::::Log's:::::
	TEXTO.ARCHIVO = 'InfoTxn = ':Y.TXN.AMT:' // ':V.CUS.1:' // ':V.CUS.2
	GOSUB LOG.WRITE
    
    V.ACC.DEP 	= R.NEW(TT.TE.ACCOUNT.2)
    CALL F.READ(FN.ACC, V.ACC.DEP, RECORD.ACC.DEP, F.ACC, ERRO.ACC)
	V.ARR.DEP = RECORD.ACC.DEP<AC.ARRANGEMENT.ID>
	V.NOSTRO.DEP = RECORD.ACC.DEP<AC.LIMIT.REF>
	V.DEP.USD 	= SUBSTRINGS(V.ACC.DEP, 1, 3) 
	
	IF NOT(V.CUS.2) THEN
		V.CUS.2 = RECORD.ACC.DEP<AC.CUSTOMER>	
	END
	;*::::Log's:::::
	TEXTO.ARCHIVO = 'CuentaDeposito = ':V.ACC.DEP:' // ':V.ARR.DEP
	GOSUB LOG.WRITE
	
    V.ACC.RET 	= R.NEW(TT.TE.ACCOUNT.1)
	CALL F.READ(FN.ACC, V.ACC.RET, RECORD.ACC.RET, F.ACC, ERRO.ACC)
	V.ARR.RET = RECORD.ACC.RET<AC.ARRANGEMENT.ID>
	V.NOSTRO.RET = RECORD.ACC.RET<AC.LIMIT.REF>
	V.RET.USD 	= SUBSTRINGS(V.ACC.RET, 1, 3)
	
    IF NOT(RECORD.ACC.RET) THEN
    	V.CUS.1 = RECORD.ACC.RET<AC.CUSTOMER>
    END
    ;*::::Log's:::::
	TEXTO.ARCHIVO = 'CuentaRetiro = ':V.ACC.RET:' // ':V.ARR.RET
	GOSUB LOG.WRITE
	
    IF V.CUS.1 EQ V.CUS.2 THEN 
    	;*::::Log's:::::
    	TEXTO.ARCHIVO = 'Mismo Cliente'
		GOSUB LOG.WRITE
		
    	;*Obteniendo el acumulado actual del cliente 
    	;*------------------------------------------
    	CALL F.READ(FN.CONTA, V.CUS.1:'-DJA', RECORD.DJA, F.CONTA, ERROR.CONTA.DEB)
    	V.AMT.DJA.DEP = 0
    	IF RECORD.DJA THEN
			V.AMT.DJA.DEP = RECORD.DJA<EB.SLV.TMA.ACUM.MES.MONTO>
			V.AMT.DJA.DEP = V.AMT.DJA.DEP + Y.TXN.AMT
    	END
    	
    	;*Obteniendo el monto limite declarado
		;*---------------------------
		V.ARR = ''
		V.ARR = V.ARR.DEP
		V.TYPE = 'DEP'
		GOSUB AMT.DECLARADO.CLIENTE
		
    	;*Realizando validacion para el cliente Credito
		;*===========================================
		IF V.NOSTRO.DEP NE 'NOSTRO' THEN
			IF V.DEP.USD NE 'USD' THEN	
				IF V.AMT.DJA.DEP GT V.AMT.LIMIT.DEP  THEN
					TEXT  = 'SLV.AML.DECL.JUR.ACUM':FM:V.CUS.2:VM:V.ACC.DEP
					CURRNO = DCOUNT(R.NEW(TT.TE.OVERRIDE),VM) + 1
					CALL STORE.OVERRIDE(CURRNO)
					RETURN
				END
			END
		END
		;*Obteniendo el monto limite declarado
		;*---------------------------
		V.ARR = ''
		V.ARR = V.ARR.RET
		V.TYPE = 'RET'
		GOSUB AMT.DECLARADO.CLIENTE
		
		;*Realizando validacion para el cliente debito
		;*===========================================	
		IF V.NOSTRO.RET NE 'NOSTRO' THEN
			IF V.RET.USD NE 'USD' THEN
				IF V.AMT.DJA.DEP GT V.AMT.LIMIT.RET  THEN
					TEXT  = 'SLV.AML.DECL.JUR.ACUM':FM:V.CUS.2:VM:V.ACC.RET
					CURRNO = DCOUNT(R.NEW(TT.TE.OVERRIDE),VM) + 1
					CALL STORE.OVERRIDE(CURRNO)
					RETURN
				END
			END	
		END	
    END
    ELSE
;*****************************************Cliente Retiro******************************************************
		CALL F.READ(FN.CONTA, V.CUS.1:'-DJA', RECORD.DJA.RET, F.CONTA, ERROR.CONTA.RET)
		V.DJA.RET = 0
		IF RECORD.DJA.RET THEN
			V.AMT.DJA.RET = RECORD.DJA.RET<EB.SLV.TMA.ACUM.MES.MONTO>
			V.DJA.RET = V.AMT.DJA.RET + Y.TXN.AMT
		END
		
		;*::::Log's:::::
		TEXTO.ARCHIVO = 'Info Acumulado retiro = ':V.DJA.RET
		GOSUB LOG.WRITE
	
		;*Obteniendo el monto limite declarado
		;*---------------------------
		V.ARR = ''
		V.ARR = V.ARR.RET
		V.TYPE = 'RET'
		GOSUB AMT.DECLARADO.CLIENTE
		
		;*::::Log's:::::
		TEXTO.ARCHIVO = 'Validacion Cliente Retiro = ':V.DJA.RET:' // ':V.AMT.LIMIT.RET
		GOSUB LOG.WRITE
		
		;*Realizando validacion para el cliente debito/retiro
		;*---------------	
		IF V.NOSTRO.RET NE 'NOSTRO' THEN
			IF V.RET.USD NE 'USD' THEN
				IF V.DJA.RET GT V.AMT.LIMIT.RET THEN
					TEXT  = 'SLV.AML.DECL.JUR.ACUM':FM:V.CUS.1:VM:V.ACC.RET
					CURRNO = DCOUNT(R.NEW(TT.TE.OVERRIDE),VM) + 1
					CALL STORE.OVERRIDE(CURRNO)
					RETURN
				END
			END
		END
;*****************************************Cliente Deposito*****************************************************
		CALL F.READ(FN.CONTA, V.CUS.2:'-DJA', RECORD.DJA.DEP, F.CONTA, ERROR.CONTA.DEP)
		V.AMT.DJA.DEP = 0
		IF RECORD.DJA.DEP THEN
			V.AMT.DJA.DEP = RECORD.DJA.DEP<EB.SLV.TMA.ACUM.MES.MONTO>
			V.AMT.DJA.DEP = V.AMT.DJA.DEP + Y.TXN.AMT
		END
		
		;*::::Log's:::::
		TEXTO.ARCHIVO = 'Acumulado Deposito = ':V.AMT.DJA.DEP
		GOSUB LOG.WRITE
		
		;*Obteniendo el monto limite declarado
		;*---------------------------
		V.ARR = ''
		V.ARR = V.ARR.DEP
		V.TYPE = 'DEP'
		GOSUB AMT.DECLARADO.CLIENTE
		
		;*::::Log's:::::
		TEXTO.ARCHIVO = 'Validacion Deposito = ':V.AMT.DJA.DEP:' // ':V.AMT.LIMIT.DEP
		GOSUB LOG.WRITE
		
		;*Realizando validacion para el cliente credito/deposito	
		IF V.NOSTRO.DEP NE 'NOSTRO' THEN
			IF V.DEP.USD NE 'USD' THEN
				IF V.AMT.DJA.DEP GT V.AMT.LIMIT.DEP THEN
					TEXT  = 'SLV.AML.DECL.JUR.ACUM':FM:V.CUS.2:VM:V.ACC.DEP
					CURRNO = DCOUNT(R.NEW(TT.TE.OVERRIDE),VM) + 1
					CALL STORE.OVERRIDE(CURRNO)
					RETURN
				END
			END
		END	
	END
RETURN

AMT.DECLARADO.CLIENTE:
	CALL GET.LOC.REF ('AA.PRD.DES.ACCOUNT','LF.AML.DEP.PROY',PosAmlDepProy)
	CALL GET.LOC.REF ('AA.PRD.DES.ACCOUNT','LF.AML.RET.PROY',PosAmlRetProy)
	
	CALL AA.GET.ARRANGEMENT.CONDITIONS(V.ARR, 'ACCOUNT', '', TODAY, RETURN.IDS, RETURN.VALUES.ACC, RETURN.ER)
	REC.PRODUCT.LIMIT = RAISE(RETURN.VALUES.ACC)
	BEGIN CASE
		CASE V.TYPE EQ 'DEP'
			V.AMT.LIMIT.DEP 		= REC.PRODUCT.LIMIT<AA.AC.LOCAL.REF,PosAmlDepProy>
			V.AMT.TOLERANCIA.DEP 	= V.AMT.LIMIT.DEP 	* (V.TOLERANCE/100)
			V.AMT.LIMIT.DEP 		= V.AMT.TOLERANCIA.DEP + V.AMT.LIMIT.DEP 
			
			;*::::Log's:::::
			TEXTO.ARCHIVO = 'DECLARADO.CLIENTE.DEPOSITO = ':V.AMT.LIMIT.DEP:' // ':V.AMT.TOLERANCIA.DEP:' // ':V.AMT.LIMIT.DEP
			GOSUB LOG.WRITE
			
		CASE V.TYPE EQ 'RET'
			V.AMT.LIMIT.RET			= REC.PRODUCT.LIMIT<AA.AC.LOCAL.REF,PosAmlRetProy>
			V.AMT.TOLERANCIA.RET 	= V.AMT.LIMIT.RET 	* (V.TOLERANCE/100)
			V.AMT.LIMIT.RET 		= V.AMT.LIMIT.RET + V.AMT.TOLERANCIA.RET
			
			;*::::Log's:::::
			TEXTO.ARCHIVO = 'DECLARADO.CLIENTE.RETIRO = ':V.AMT.LIMIT.RET:' // ':V.AMT.TOLERANCIA.RET:' // ':V.AMT.LIMIT.RET
			GOSUB LOG.WRITE
	END CASE
	
RETURN

LOG.WRITE:
		;*Si el parametro de Log esta Activo Escribir Archivo
		IF LOG.ACTIVO EQ 'Y' THEN
		    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
		    WRITESEQ '[' : LEFT(TIMEDATE(),8) : ']  ' : TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
		    END
		    CLOSESEQ SEQ.PTR
		END
    RETURN

END
