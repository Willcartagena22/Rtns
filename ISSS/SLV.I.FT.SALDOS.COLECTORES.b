*-----------------------------------------------------------------------------
* <Rating>787</Rating>
*-----------------------------------------------------------------------------
    PROGRAM SLV.I.FT.SALDOS.COLECTORES
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
*-----------------------------------------------------------------------------

    GOSUB INICIALIZAR
    GOSUB OPENFILE
    GOSUB CUENTA.COLECTOR

    RETURN

INICIALIZAR:
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''

    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''
    
    FN.TELLER = 'F.TELLER'
    F.TELLER  = ''
    
    RETURN

OPENFILE:
    CALL OPF(FN.ACC,F.ACC)
    CALL OPF(FN.FT, F.FT)
    CALL OPF(FN.TELLER, F.TELLER)
    
    RETURN

CUENTA.COLECTOR:
;*LLAMADA AL CALLJ PARA OBTENER CUENTA CLIENTE POR COLECTOR
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector"
    THIS.METHOD.CLT= "getLowBalanceAccounts"
    CALLJ.ARGUMENTS.CLT = " "
    CALLJ.ERROR.SMS = ""
    CALLJ.RESPONSE.CLT = " "

;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA = CALLJ.RESPONSE.CLT
    COL.CUENTAS = CHANGE(RESPUESTA,'|',VM)
    CANTIDAD.REGISTROS = DCOUNT(COL.CUENTAS,VM) - 2 ;* MENOS EL PIPE DE INICIO Y FINAL

    FOR H = 1 TO CANTIDAD.REGISTROS
    	;*VARIABLE PARA GENERAR EL JSON A ENVIAR
    	JSON.SALDOS = '['
    	COL = ''
    	CTA = ''
    	FILA = ''
    	
        FILA = FIELD(COL.CUENTAS,VM,H+1)
        COL = FIELD(FILA,'-',1) ;* COLECTOR
        CTA = FIELD(FILA,'-',2) ;* CUENTA
        
        ;* CONSTRUIR JSON A ENVIAR
        GOSUB SALDO.CUENTA
        
	NEXT H
	

    RETURN

SALDO.CUENTA:
	DEBITOS = 0
	CREDITOS = 0
	MOVIMIENTOS = ''
	
	;* Recuperar el saldo actual de la cuenta
	CALL F.READ(FN.ACC, CTA, R.ACC, F.ACC, ERR.ACC)
    WORKING.BALANCE = R.ACC<AC.ONLINE.ACTUAL.BAL>
    
    IF WORKING.BALANCE EQ '' THEN
        WORKING.BALANCE = 0
    END
    
    ;********************************************************
    ;*						SALDOS FT 
    ;********************************************************
    
    ;* Recuperar el listado de FT del día para la cuenta
	CMND = 'SELECT ' : FN.FT : ' WITH DEBIT.ACCT.NO EQ "' : CTA : '" OR CREDIT.ACCT.NO EQ "' : CTA : '" AND DEBIT.VALUE.DATE EQ "':TODAY:'" ORDER BY DATE.TIME ASC'
	CALL EB.READLIST(CMND, F.FT.READ, '', F.SELECTED.FT, ERROR.READ)
	
	LOOP
        REMOVE FT.LIST FROM F.FT.READ SETTING POSFT
    WHILE FT.LIST:POSFT
    	;*Revisamos el detalle de cada FT
        CALL F.READ(FN.FT,FT.LIST,RS.FT,F.FT,FT.ERR)
        
        ;* Si la cuenta debito es la cuenta colector, entonces se debe debitar
        IF RS.FT<FT.DEBIT.ACCT.NO> = CTA THEN
        	DEBITOS = DEBITOS + CHANGE(RS.FT<FT.AMOUNT.DEBITED>,'USD','')
        END
        ELSE IF RS.FT<FT.CREDIT.ACCT.NO> = CTA THEN
        	CREDITOS = CREDITOS + CHANGE(RS.FT<FT.AMOUNT.CREDITED>,'USD','')
        END
        
        ;********************************************
        ;*			LINEA DE JSON FT
        ;********************************************
        
        ;* Fecha Real
        DATE.REAL = RS.FT<FT.DATE.TIME>
        GOSUB PARSE.DATETIME
        
        ;* Fecha Valor Core
        DATE.CORE = OCONV(ICONV(RS.FT<FT.DEBIT.VALUE.DATE>,'D'),'D4/E')
        
        ;* Agregar los movimientos al arreglo
        ;* fecha real - fecha core - cuenta origen - cuenta desitno - ft - monto - colector - cuenta colector
        MOVIMIENTOS<-1> = DATE.REAL:VM:DATE.CORE:VM:RS.FT<FT.DEBIT.ACCT.NO>:VM:RS.FT<FT.CREDIT.ACCT.NO>:VM:FT.LIST:VM:RS.FT<FT.DEBIT.AMOUNT>:VM:COL:VM:CTA
    REPEAT
    
    ;********************************************************
    ;*						SALDOS TT 
    ;********************************************************
    
    ;* Recuperar el listado de TT del día para la cuenta
    STMT.TT.DEBIT = 'SELECT ':FN.TELLER:' WITH ACCOUNT.1 EQ "':CTA:'" OR ACCOUNT.2 EQ "':CTA:'" AND VALUE.DATE.1 EQ "':TODAY:'" '
    CALL EB.READLIST(STMT.TT.DEBIT,ARR.LST.TT,'',RECORD.DEBIT.COUNT.TT,Y.ARR.LST.DB.TT)
    
    LOOP
        REMOVE TT.RECORD FROM ARR.LST.TT SETTING PosTT
    WHILE TT.RECORD:PosTT
    	;*Revisamos el detalle de cada TT
        CALL F.READ(FN.TELLER,TT.RECORD,RS.TT.DETAIL,F.TELLER,TT.ERR.DEBIT)
        
        MARKER = RS.TT.DETAIL<TT.TE.DR.CR.MARKER>
        ACCOUNT.1 = RS.TT.DETAIL<TT.TE.ACCOUNT.1>
        ACCOUNT.2 = RS.TT.DETAIL<TT.TE.ACCOUNT.2>
        ACCOUNT.ORIGEN = ""
        ACCOUNT.DESTINO = ""
        
        ;* Si el marcador es DEBIT
        IF MARKER = "DEBIT"THEN
        	;* Si ACCOUNT.1 es la cuenta colector entonces si es un debito
        	IF ACCOUNT.1 = CTA THEN
        		DEBITOS = DEBITOS + RS.TT.DETAIL<TT.TE.AMOUNT.LOCAL.1>
        	END
        	ELSE
        		CREDITOS = CREDITOS + RS.TT.DETAIL<TT.TE.AMOUNT.LOCAL.1>
        	END
        	
        	;* La cuenta origen es la que se debita
        	ACCOUNT.ORIGEN = ACCOUNT.1
        	ACCOUNT.DESTINO = ACCOUNT.2
        END
        ELSE IF MARKER = "CREDIT" THEN
        	;* Si ACCOUNT.1 es la cuenta colector entonces si es un credito
        	IF ACCOUNT.1 = CTA THEN
        		CREDITOS = CREDITOS + RS.TT.DETAIL<TT.TE.AMOUNT.LOCAL.1>
        	END
        	ELSE
        		DEBITOS = DEBITOS + RS.TT.DETAIL<TT.TE.AMOUNT.LOCAL.1>
        	END
        	
        	;* La cuenta origen es la que se debita
        	ACCOUNT.ORIGEN = ACCOUNT.2
        	ACCOUNT.DESTINO = ACCOUNT.1
        END
        
        ;********************************************
        ;*			LINEA DE JSON TT
        ;********************************************
        
        ;* Fecha Real
        DATE.REAL = RS.TT.DETAIL<TT.TE.DATE.TIME>
        GOSUB PARSE.DATETIME
        
        ;* Fecha Valor Core
        DATE.CORE = OCONV(ICONV(RS.TT.DETAIL<TT.TE.VALUE.DATE.1>,'D'),'D4/E')
        
        ;* Agregar los movimientos al arreglo
        ;* fecha real - fecha core - cuenta origen - cuenta desitno - ft - monto - colector - cuenta colector
        MOVIMIENTOS<-1> = DATE.REAL:VM:DATE.CORE:VM:ACCOUNT.ORIGEN:VM:ACCOUNT.DESTINO:VM:TT.RECORD:VM:RS.TT.DETAIL<TT.TE.AMOUNT.LOCAL.1>:VM:COL:VM:CTA:FM
    REPEAT
    
    ;* Calcular el saldo al inicio del día
    ;* Saldo antes de movimientos: SALDO ACTUAL + DEBITOS - CREDITOS
    ;* Antes de GENERAR.JSON, Si la transacción del R.NEW es un crédito, incluir ese monto en la sumatoria de créditos
    ;* Ya que ese monto aun no está incluido en el AC.ONLINE.ACTUAL.BAL a diferencia de los debitos que ya lo incluyen
    IF R.NEW(FT.CREDIT.ACCT.NO) = CTA THEN
    	CREDITOS = CREDITOS + R.NEW(FT.DEBIT.AMOUNT)
    END
    
    SALDO.INICIAL = WORKING.BALANCE + DEBITOS - CREDITOS
	
	;* Generar JSON
	GOSUB GENERAR.JSON
	
	;* SI LA CUENTA DE LA FT A REALIZARCE SON LAS DEL COLECTOR AGREGAR EL MOVIMIENTO
	IF R.NEW(FT.CREDIT.ACCT.NO) = CTA OR R.NEW(FT.DEBIT.ACCT.NO) = CTA THEN
		;*AGREGAR LA FILA QUE ESTA POR ESCRIBIRSE EN LA FT
		DATE.REAL = OCONV(DATE(),'D4/E'):" ":OCONV(TIME(),'MTS')
		
		;* Calcular el saldo despues de la transacción
		IF R.NEW(FT.DEBIT.ACCT.NO) = CTA THEN
			;* Sumar el monto de la transacción actual al saldo inicial
			;*SALDO.INICIAL = SALDO.INICIAL + R.NEW(FT.DEBIT.AMOUNT)
			
			SALDO.DESPUES = SALDO.INICIAL - R.NEW(FT.DEBIT.AMOUNT)
		END
		ELSE
			;* Quitar el monto de la transacción actual al saldo inicial
			;*SALDO.INICIAL = SALDO.INICIAL - R.NEW(FT.DEBIT.AMOUNT)
			
			SALDO.DESPUES = SALDO.INICIAL + R.NEW(FT.DEBIT.AMOUNT)
		END
		
		;* AGREGAR LA FILA AL JSON DE LA NUEVA TRANSACCION
		FILA = '{"FECHAR":"':DATE.REAL:'","FECHAC":"':OCONV(ICONV(TODAY,'D'),'D4/E'):'","CTAO":"':R.NEW(FT.DEBIT.ACCT.NO):'",'
		FILA :='"CTAD":"':R.NEW(FT.CREDIT.ACCT.NO):'","IDFT":"':ID.NEW:'","SALDOANTES":"':SALDO.INICIAL:'","MONTO":"':R.NEW(FT.DEBIT.AMOUNT):'",'
		FILA :='"SALDODESPUES":"':SALDO.DESPUES:'","COLECTOR":"':COL:'","CTACOL":"':CTA:'"}'	
		
		;*SALDO.DESPUES = WORKING.BALANCE + R.NEW(FT.DEBIT.AMOUNT)
		;*WORKING.BALANCE = SALDO.DESPUES
		
		JSON.SALDOS := FILA
	END
	ELSE
		MAXIMO = LEN(JSON.SALDOS) - 1
		JSON.SALDOS = SUBSTRINGS(JSON.SALDOS,1,MAXIMO)
	END
	
	;* CERRAR EL JSON
	JSON.SALDOS := ']'
	
	CRT 'JSON FINAL: ':JSON.SALDOS
	
	IF JSON.SALDOS NE ']' THEN
		GOSUB ENVIAR.JSON
	END
	
    RETURN

GENERAR.JSON:
	;* Recorrer el arreglo de los movimientos
	NUM.FT = DCOUNT(MOVIMIENTOS,FM) ;* Se agrega uno al final
	
	FOR I = 1 TO NUM.FT
		ROW = MOVIMIENTOS<I>
		CRT 'FILA FT: ':ROW
		
		;* ROW = fecha real - fecha core - cuenta origen - cuenta desitno - ft - monto - colector - cuenta colector
		DATE.REAL = FIELD(ROW,VM,1)
        DATE.CORE = FIELD(ROW,VM,2)
        CTA.ORIGEN = FIELD(ROW,VM,3)
        CTA.DESTINO = FIELD(ROW,VM,4)
        FT.ID = FIELD(ROW,VM,5)
        MONTO.FT = FIELD(ROW,VM,6)
        COLECTOR = FIELD(ROW,VM,7)
        CTA.COLECTOR = FIELD(ROW,VM,8)
        
        ;* Saldo después, si cuenta origen es la cuenta colector, entonces es un debito
        IF CTA.ORIGEN = CTA.COLECTOR THEN
        	SALDO.DESPUES = SALDO.INICIAL - MONTO.FT
        END
        ELSE IF CTA.DESTINO = CTA.COLECTOR THEN
        	SALDO.DESPUES = SALDO.INICIAL + MONTO.FT
        END
        
        
        ;* Crear el JSON
        FILA = '{"FECHAR":"':DATE.REAL:'","FECHAC":"':DATE.CORE:'","CTAO":"':CTA.ORIGEN:'",'
		FILA :='"CTAD":"':CTA.DESTINO:'","IDFT":"':FT.ID:'","SALDOANTES":"':SALDO.INICIAL:'","MONTO":"':MONTO.FT:'",'
		FILA :='"SALDODESPUES":"':SALDO.DESPUES:'","COLECTOR":"':COLECTOR:'","CTACOL":"':CTA.COLECTOR:'"},'
		JSON.SALDOS := FILA
        
        ;* Actualizar el saldo inicial, para el próximo movimiento
        SALDO.INICIAL = SALDO.DESPUES
    NEXT I
	
	CRT JSON.SALDOS
	
RETURN

PARSE.DATETIME:
    utcDateTime =  DATE.REAL
    UTC.FLAG = ''
	;*Evaluar UTC Time or Standard Time

    FINDSTR "." IN utcDateTime SETTING Ap, Vp THEN
        UTC.FLAG = '1'
    END

    IF UTC.FLAG EQ '1' THEN
        localZoneDate1 = OCONV(LOCALDATE(utcDateTime,localZone),'D4/E')
        localZoneTime1= OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
        DATE.REAL = localZoneDate1:' ':localZoneTime1
    END
    ELSE
	    Y.DAY.BC = utcDateTime[3,2]
	    Y.MONTH.BC = utcDateTime[5,2]
	    Y.YEAR.BC = utcDateTime[1,2]
	    Y.DATE.BC = OCONV(ICONV(utcDateTime[1,6],'D'),'D4/E')
	    Y.TIME.BC = utcDateTime[7,2]:':':utcDateTime[9,2]:':':'00'
	    DATE.REAL = Y.DATE.BC: ' ': Y.TIME.BC
    END

    RETURN

ENVIAR.JSON:
	;*LLAMADA AL CALLJ PARA ENVIAR LOS DATOS DE LAS TRANSACCIONES
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector"
    THIS.METHOD.CLT= "sendJsonTransactions"
    CALLJ.ARGUMENTS.CLT = JSON.SALDOS
    CALLJ.ERROR.SMS = ""
    CALLJ.RESPONSE.CLT = " "

	;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA = CALLJ.RESPONSE.CLT
    Y.ERR = FIELD(RESPUESTA,'|',2)
    
    IF Y.ERR EQ 'ERR' THEN
    	ETEXT = "Error al ingresar Operacion":RESPUESTA
    	CALL STORE.END.ERROR
    	
    	CRT RESPUESTA
    END
    
    RETURN
    
END
