*-----------------------------------------------------------------------------
* <Rating>-127</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.AUTH.OFS.CONT.DAP
*-----------------------------------------------------------------------------
*
* Nombre: SLV.AUTH.OFS.CONT.DAP
* Descripción: Rutina para Envio de OFS e Incremento en Contador de Liquidez
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* ocornejo	14.10.2016	Initial Code
* ocornejo	27.02.2016	Se agrega funcionalidad para envio de Fondeo por OFS eliminando Paso de FT
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.SLV.GEN.PARAM
$INSERT I_F.TAX
$INSERT I_F.SLV.F.CONTADOR.LIOF
$INSERT I_F.AA.ARRANGEMENT
*-----------------------------------------------------------------------------

GOSUB INITIALIZE
GOSUB OPENFILE
GOSUB GET.LOCAL.REF.FIELDS
GOSUB PROCESS
RETURN

	INITIALIZE:
		FN.TFS = 'F.TELLER.FINANCIAL.SERVICES'
		F.TFS  = ''
		FN.SLV.GEN.PARAM = 'F.SLV.GEN.PARAM'
    	F.SLV.GEN.PARAM  = ''
    	FN.TAX = 'F.TAX'
	    F.TAX  = ''
	    FN.COUNTER = 'F.SLV.F.CONTADOR.LIOF'
	    F.COUNTER  = ''
	    FN.AA      = 'F.AA.ARRANGEMENT'
	    F.AA       = ''
		
		EQU LIOF.PARAM TO 'SLV.TXN.LIOF'
		EQU LIOF       TO 'LIOF'
	    EQU CLIQ       TO 'CLIQ'
	    EQU LIOF.SUR.ACC TO 'LIOF.SUR.ACC'
	    EQU VersionCobro TO 'TELLER.FINANCIAL.SERVICES,SLV.TFS.RESERVA.CLIQ.ENQ'
	RETURN
	
	GET.LOCAL.REF.FIELDS:
		APPL.NAME = 'TELLER.FINANCIAL.SERVICES'
	    FLD.NAME  = 'LF.AMT.TAX':VM:'LF.TFS.LOAN.REF':VM:'LF.CHG.ACC':VM:'LF.NOM.PER'
	    FLD.POS   = ''
	    
	    CALL MULTI.GET.LOC.REF(APPL.NAME,FLD.NAME,FLD.POS)
	
	    Y.LF.AMT.TAX      = FLD.POS<1,1>
	    Y.LF.TFS.LOAN.REF = FLD.POS<1,2>
	    Y.LF.CHG.ACC      = FLD.POS<1,3>
	    Y.LF.NOM.PER      = FLD.POS<1,4>
	RETURN
	
	OPENFILE:
		CALL OPF(FN.TFS, F.TFS)
		CALL OPF(FN.SLV.GEN.PARAM, F.SLV.GEN.PARAM)
		CALL OPF(FN.TAX, F.TAX)
		CALL OPF(FN.COUNTER, F.COUNTER)
		CALL OPF(FN.AA, F.AA)
	RETURN
	
	PROCESS:
		;*ID.NEW    = 'TFS1535873TDC'							;*Debug
		Y.TFS.ID  = ID.NEW
		Y.VERSION = APPLICATION:PGM.VERSION
		;*Y.TFS.ID  = 'TFS15358SWYV5'							;*Debug
		
		;*Obtener ID de TFS Padre
		IF VersionCobro EQ Y.VERSION THEN
			Y.TFS.ID = FIELD(R.NEW(TFS.LOCAL.REF)<1, Y.LF.NOM.PER>, " ", 1)
		END
		
		;*Usar R.NEW si es la Version de Fondeo
		IF ID.NEW EQ Y.TFS.ID THEN
			GOSUB SET.FONDEO
		END
		ELSE
			GOSUB SET.COBRO
		END
		
		;*Consulta Arrangement para Extraer Id de Cliente
		CALL F.READ(FN.AA, Y.CUST.ACC, R.AA, F.AA, E.AA)
		Y.CUS.ID = R.AA<AA.ARR.CUSTOMER>
		
		;*Consultar Transacciones que Aplican a Impuesto y de que lado
		Y.AMT.LIOF    = 0
	    Y.AMT.CLIQ    = 0
	    Y.AMT.SUR.ACC = 0
		CALL F.READ(FN.SLV.GEN.PARAM, LIOF.PARAM, R.SLV.GEN.PARAM, F.SLV.GEN.PARAM, ERR.SLV.GEN.PARAM)
		
		;*Recorrer Transacciones para Acumular Impuestos CLIQ y LIOF
		Y.TOT.AMT = R.TFS<TFS.AMOUNT>
    	Y.MONTO   = SUM(Y.TOT.AMT)
		NO.TRANS  = DCOUNT(R.TFS<TFS.TRANSACTION>, VM)
		
		;*Es Version de Fondeo y
		;*Aplica Cobro de Liquidez 
		;*Entonces No Hacer Nada
		IF ID.NEW EQ Y.TFS.ID THEN
			;*Validar si Fondeo Aplica Cobro de Liquidez 
			GOSUB CLIQ.APLICADO
			IF Y.COBRO.APLICADO EQ 0 THEN
				RETURN
			END
		END
		
		;*Sino esta en Version de Cobro por tanto Recorrer, Calcular y Aplicar OFS segun Corresponda
		FOR I = 1 TO NO.TRANS		
			;*Asignando Variables de Trabajo
			Y.TXN     = R.TFS<TFS.TRANSACTION><1,I>
			Y.TXN.AMT = R.TFS<TFS.AMOUNT><1,I>
			Y.MNT     = R.TFS<TFS.LOCAL.REF><1, Y.LF.AMT.TAX, I>	
			Y.SUR.ACC = R.TFS<TFS.SURROGATE.AC><1,I>
			
			;*Si Hay Impuesto Acumular o Cobrar para la Surrogate Acc
	        IF Y.MNT GT 0 THEN
	        	GOSUB SUMA.IMPUESTO
	        END
			
			;*Incrementa Contador de Liquidez
			CALL SLV.GET.LIOF.TRX.PARAM(Y.TXN, Y.CATEG.ID, Y.OUT.TAX, Y.OUT.OPER, '', '')
			IF Y.CATEG.ID EQ 'LIOFCASH' THEN
				GOSUB ACTUALIZA.CONTADOR
			END
		NEXT I
		
		;*Envio de OFS para Cobro de Liquidez Si Aplicase
		GOSUB CREATE.OFS
		
		;*Envio de OFS para Fondeo de DAP
	    GOSUB CREATE.OFS.FONDEO
		
		;*Si fue Exito Enviar Validar que si el Autorizador es Otro 
		;*No se debe Cargar los Comprobantes Automaticamente
		Y.INPUTTER = R.NEW(TFS.INPUTTER)
		IF FIELD(Y.INPUTTER,'_',2) NE OPERATOR THEN
			RETURN
		END
		
		;*Si fue Exito Enviar a Enquiry para Impresion de Comprobantes
		CALL SLV.E.IR.A.IMPRESION.ENQ
	RETURN
	
	;*Validar, Si se va Aplicar Cobro de Liquidez
	CLIQ.APLICADO:
		Y.COBRO.APLICADO = 1 ;*Solo va importar Cuando este en Version de Fondeo		
		;*Recorrer Transacciones para Validar si Fondeo Aplica CLIQ
		FOR J = 1 TO NO.TRANS
			Y.TXN = R.TFS<TFS.TRANSACTION><1,J>
			Y.MNT = R.TFS<TFS.LOCAL.REF><1, Y.LF.AMT.TAX, J>
			
			;*Si Hay Impuesto Continuar
	        IF Y.MNT GT 0 THEN
	        	;*Buscar Txn en Parametros		
				FIND Y.TXN IN R.SLV.GEN.PARAM<SLV.GEN.TX.DATA.PARAM> SETTING Fp, Vp THEN
					;*Txn Aplica a CLIQ?
					IF R.SLV.GEN.PARAM<SLV.GEN.TX.TYPE.PARAM><Fp, Vp> EQ CLIQ THEN
						;*Cobrar Antes de Lanzar OFS
						Y.COBRO.APLICADO = 0
						RETURN
					END
				END
	        END
		NEXT J
	RETURN

	;*Metodo para Setear las Variables del R.NEW (Aplicacion de OFS desde Version de Fondeo)
	SET.FONDEO:		
		R.TFS      = ''
		R.TFS<TFS.AMOUNT.LCY>    = R.NEW(TFS.AMOUNT.LCY)
		R.TFS<TFS.TRANSACTION>   = R.NEW(TFS.TRANSACTION)
		R.TFS<TFS.AMOUNT>        = R.NEW(TFS.AMOUNT)
		R.TFS<TFS.LOCAL.REF>     = R.NEW(TFS.LOCAL.REF)
		R.TFS<TFS.SURROGATE.AC>  = R.NEW(TFS.SURROGATE.AC)
		R.TFS<TFS.RT.ACCOUNT.NO> = R.NEW(TFS.RT.ACCOUNT.NO)
		Y.CUST.ACC = R.NEW(TFS.LOCAL.REF)<1,Y.LF.TFS.LOAN.REF>
	RETURN
	
	;*Metodo para Setear las Variables del R.TFS
	SET.COBRO:
		;*Consultando TFS para Extraer Variables de la TFS de Fondeo		
		CALL F.READ(FN.TFS, Y.TFS.ID, R.TFS, F.TFS, E.TFS)
		Y.CUST.ACC = R.TFS<TFS.LOCAL.REF><1,Y.LF.TFS.LOAN.REF>
	RETURN
	
	ACTUALIZA.CONTADOR:		
		;*Contador de liquidez para el cliente
        CALL F.READ(FN.COUNTER, Y.CUS.ID, R.COUNTER, F.COUNTER, Y.ERR)
        R.COUNTER<1> += Y.TXN.AMT		
		CALL F.WRITE(FN.COUNTER, Y.CUS.ID, R.COUNTER)
	RETURN
	
	SUMA.IMPUESTO:
		;*Buscar Txn en Parametros		
		FIND Y.TXN IN R.SLV.GEN.PARAM<SLV.GEN.TX.DATA.PARAM> SETTING Fp, Vp ELSE END
		
		;*Si la Txn no Existe Retornar
		IF Fp EQ '' AND Vp EQ '' THEN
			RETURN
		END	
		
		;*Acumular en Liof o Cliq segun sea el Caso
		BEGIN CASE
			CASE R.SLV.GEN.PARAM<SLV.GEN.TX.TYPE.PARAM><Fp, Vp> EQ LIOF
				Y.AMT.LIOF += Y.MNT
				
			CASE R.SLV.GEN.PARAM<SLV.GEN.TX.TYPE.PARAM><Fp, Vp> EQ CLIQ
				Y.AMT.CLIQ += Y.MNT
			
			;*Flag para Aplicar Cobro en Surrogate.Acc, esta es una especializacion en el cobro ya que 
			;*para la primary debe cobrarse un solo impuesto acumulado pero en caso de depositos el cobro
			;*se hace a cada surrogate que lo aplique en la TSF
			CASE R.SLV.GEN.PARAM<SLV.GEN.TX.TYPE.PARAM><Fp, Vp> EQ LIOF.SUR.ACC
				Y.AMT.SUR.ACC = Y.MNT
				GOSUB CREATE.OFS
				Y.AMT.SUR.ACC = 0 ;*Limpiar Variable para Siguiente Iteracion
		END CASE
	RETURN
	
	CREATE.OFS.FONDEO:
		TRANS.ID     = ''
		R.FT         = ''
		Y.OUT        = ''
		ID.PARAM.OFS = 'OFS.FONDEO.DAP'
		
		;*Buscar Cta a Cargar
		GOSUB GET.CHARGE.ACC
		
		;*Setear Datos para Fondeo de DAP en OFS
		R.FT<FT.DEBIT.CURRENCY>   = LCCY
		R.FT<FT.CREDIT.CURRENCY>  = LCCY
		R.FT<FT.DEBIT.ACCT.NO>    = Y.ACC
		R.FT<FT.CREDIT.ACCT.NO>   = R.AA<AA.ARR.LINKED.APPL.ID>
		R.FT<FT.CREDIT.AMOUNT>    = Y.MONTO
		R.FT<FT.DEBIT.THEIR.REF>  = Y.TFS.ID
		R.FT<FT.ORDERING.CUST>    = ID.NEW
		GOSUB OFS.SEND
	RETURN
	
	CREATE.OFS:
		TRANS.ID     = ''
		R.FT         = ''
		Y.OUT        = ''
		R.FT<FT.DEBIT.CURRENCY>   = 'USD'
		R.FT<FT.DEBIT.THEIR.REF>  = Y.MONTO
		R.FT<FT.CREDIT.THEIR.REF> = Y.TFS.ID
		
		;*Cobro de Control de Liquidez en Primary Acc
		IF Y.AMT.CLIQ GT 0 AND Y.AMT.SUR.ACC EQ 0 THEN
		
			;*Buscar Cta a Cargar Impuesto
			GOSUB GET.CHARGE.ACC
			
			;*Obtener Cuenta Interna a la que se va Abonar Impuesto
			CALL SLV.GET.INTERNAL.LIOF.ACC(Y.CUST.ACC, CLIQ, Y.CREDIT.ACC)
		
			;*Armando OFS
			ID.PARAM.OFS = 'OFS.CARGO.CLIQ'
			R.FT<FT.DEBIT.ACCT.NO>  = Y.ACC
			R.FT<FT.DEBIT.AMOUNT>   = DROUND(Y.AMT.CLIQ,2)
			R.FT<FT.CREDIT.ACCT.NO> = Y.CREDIT.ACC
			GOSUB OFS.SEND
		END
		
		;*Cobro de LIOF en Surrogate Acc
		IF Y.AMT.SUR.ACC GT 0 THEN
			;*Obtener Cuenta Interna a la que se va Abonar Impuesto
			CALL SLV.GET.INTERNAL.LIOF.ACC(Y.SUR.ACC, LIOF, Y.CREDIT.ACC)
			
			;*Armando OFS
			ID.PARAM.OFS = 'OFS.CARGO.LIOF'
			R.FT<FT.DEBIT.ACCT.NO>  = Y.SUR.ACC
			R.FT<FT.DEBIT.AMOUNT>   = DROUND(Y.AMT.SUR.ACC,2)
			R.FT<FT.CREDIT.ACCT.NO> = Y.CREDIT.ACC
			GOSUB OFS.SEND
		END
	RETURN
	
	GET.CHARGE.ACC:
		;*Si hay cuenta especifica para pagar impuesto aplicar a ella el cargo
		IF R.TFS<TFS.LOCAL.REF><1, Y.LF.CHG.ACC> NE '' THEN
			Y.ACC = R.TFS<TFS.LOCAL.REF><1, Y.LF.CHG.ACC>
		END
		ELSE
			;*Id de Cta Transitoria a Cargar USD : CATEGORY (Estas no van a Cambiar en T24)
			Y.ID.CTA = 'USD14030'
			
			;*Extraer Cuenta Interna del MV
			Y.RT.ACC = R.TFS<TFS.RT.ACCOUNT.NO>
			FINDSTR Y.ID.CTA IN Y.RT.ACC SETTING Ap, Vp THEN
				Y.ACC = Y.RT.ACC<Ap, Vp>
			END
		END
	RETURN
	
	OFS.SEND:
		;*Envio de OFS en Linea
    	CALL SLV.UTIL.OFS.TRX(TRANS.ID, R.FT, ID.PARAM.OFS, Y.OUT)
	RETURN
END
