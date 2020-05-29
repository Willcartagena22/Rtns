*-----------------------------------------------------------------------------
* <Rating>-151</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.A.OFS.SEG.UNC
*-----------------------------------------------------------------------------
*
* Nombre: SLV.A.OFS.SEG.UNC
* Descripción: Rutina para Envio de OFS por Monto de Seguro a UNC y Registro de 
*				Movimiento en App Local
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* ocornejo	12.12.2017	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.OFS.PARAM.BUILD
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.EB.SLV.ANTICIPATED.LOAN.PAY
$INSERT I_F.AA.PAYMENT.SCHEDULE
$INSERT I_GTS.COMMON
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.EB.SLV.ACCOUNT.PARAM
$INSERT I_F.EB.SLV.GLOBAL.PARAM
$INSERT I_F.AA.SETTLEMENT
$INSERT I_F.AA.CHARGE
$INSERT I_F.AA.TERM.AMOUNT
$INSERT I_F.TAX
*-----------------------------------------------------------------------------

;*Validar que esta Rutina solo se ejecute si hay Pago Anticipado
IF LEFT(R.NEW(FT.DEBIT.THEIR.REF),3) NE 'ALP' AND LEFT(R.NEW(FT.CREDIT.THEIR.REF),3) NE 'ALP' THEN
	RETURN
END

IF OFS$OPERATION EQ 'PROCESS' THEN
	GOSUB INIT
	GOSUB OPENFILE
	GOSUB GETTER
	
	;*Si el Registro es Reversa ir al Proceso de Reversa
	IF R.NEW(FT.RECORD.STATUS) EQ 'RNAU' THEN
		GOSUB REVE.PROCESS
	END
	ELSE
		GOSUB PROCESS
	END
END
RETURN

	INIT:
		FN.OFS.PARAM = 'F.EB.SLV.OFS.PARAM.BUILD'
		F.OFS.PARAM  = ''
		FN.ALP$NAU   = 'F.EB.SLV.ANTICIPATED.LOAN.PAY$NAU'
		F.ALP$NAU    = ''
		FN.ALP   	 = 'F.EB.SLV.ANTICIPATED.LOAN.PAY'
		F.ALP    	 = ''
		FN.TFS   	 = 'F.TELLER.FINANCIAL.SERVICES'
		F.TFS        = ''
		FN.ACC.PARAM = 'F.EB.SLV.ACCOUNT.PARAM'
		F.ACC.PARAM  = ''
		FN.TAX     = 'F.TAX'
		F.TAX      = ''
		FN.ARR     = 'F.AA.ARRANGEMENT'
		F.ARR      = ''
		
		FN.GBL       = 'F.EB.SLV.GLOBAL.PARAM'
		F.GBL        = ''
		CALL OPF(FN.GBL, F.GBL)
		
		;*Extraer Parametrizacion de Log
		CALL F.READ(FN.GBL, 'LOG.ALP', R.GBL, F.GBL, E.GBL)
		DIR.NAME   = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',1)	;*Ruta de Salida del Archivo
		R.ID       = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',3) : '.' : TODAY : '.log'	;*Nombre de Archivo Log
		LOG.ACTIVO = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',2)	;*Bandera de Log Activo/Inactivo
		
		EQU OFS.PARAM.ID TO 'OFS.CTA.RECHAZO.CHQ'
		EQU IVA TO '11'
		TRANS.ID = '' ; R.FT = '' ; Y.OUT = ''
	RETURN
	
	OPENFILE:
		CALL OPF(FN.OFS.PARAM, F.OFS.PARAM)
		CALL OPF(FN.ALP$NAU, F.ALP$NAU)
		CALL OPF(FN.ALP, F.ALP)
		CALL OPF(FN.TFS, F.TFS)
		CALL OPF(FN.ACC.PARAM, F.ACC.PARAM)
		CALL OPF(FN.TAX, F.TAX)
		CALL OPF(FN.ARR, F.ARR)
	RETURN
	
	GETTER:		
		;*Obtener Posiciones de Campos Locales
		APPL.NAME = 'FUNDS.TRANSFER'
	    FLD.NAME  = 'LF.AMT.TAX':VM:'LF.AMOUNT'
	    FLD.POS   = ''
	    CALL MULTI.GET.LOC.REF(APPL.NAME,FLD.NAME,FLD.POS)	
	    LF.AMT.TAX = FLD.POS<1,1>
	    LF.AMOUNT  = FLD.POS<1,2>	    
	    Y.UNC.AMT  = 0
	    
	    Y.REF.ALP = R.NEW(FT.DEBIT.THEIR.REF)
	    IF LEFT(Y.REF.ALP,3) NE 'ALP' THEN
	    	Y.REF.ALP = R.NEW(FT.CREDIT.THEIR.REF)
	    END
	    
	    ;*Obtener Cuentas UNC
	    GOSUB GET.UNC.ACC
	    
	    ;*Obtener Datos de Transaccion
	    Y.ALP.ID     = R.NEW(FT.CREDIT.ACCT.NO) : '.' : TODAY : '.' : Y.REF.ALP[4,LEN(Y.REF.ALP)]
		Y.PAY.DATE   = R.NEW(FT.CREDIT.VALUE.DATE)
		Y.ARR        = R.NEW(FT.CREDIT.ACCT.NO)
		Y.PAY.AMT    = R.NEW(FT.LOCAL.REF)<1,LF.AMOUNT>
		CALL SLV.UTIL.GET.ARR.X.ACC(Y.ARR)
	RETURN
	
	GET.UNC.ACC:
		;*Obtener Cuenta Seguro Vida
	    CALL CACHE.READ(FN.ACC.PARAM, 'UNC.VIDA', R.ACC.PARAM, E.ACC.PARAM)
	    Y.SEG.VIDA.ACC = R.ACC.PARAM<EB.SLV47.MONEDA> : R.ACC.PARAM<EB.SLV47.CATEGORIA> : R.ACC.PARAM<EB.SLV47.IDENTIF.CTA> : RIGHT(R.ACC.PARAM<EB.SLV47.AGENCIA>,4)
	    
	    ;*Obtener Cuenta Seguro Daño
	    CALL CACHE.READ(FN.ACC.PARAM, 'UNC.DANIO', R.ACC.PARAM, E.ACC.PARAM)
	    Y.SEG.DANIO.ACC = R.ACC.PARAM<EB.SLV47.MONEDA> : R.ACC.PARAM<EB.SLV47.CATEGORIA> : R.ACC.PARAM<EB.SLV47.IDENTIF.CTA> : RIGHT(R.ACC.PARAM<EB.SLV47.AGENCIA>,4)
	    
	    ;*En reunion sostenida con Raul de Conta el dia 09.01.2018 a las 6.00 PM se envia monto de IVA
	    ;*a cuenta de seguro de daños ya que es una retencion por cuenta de terceros : OCORNEJO 09.01.2018
	    ;*------------------------------------------------------------------------------------------------
	    ;*Obtener Cuenta Iva Seguro Daño (Ya no es necesario)
	    ;*CALL CACHE.READ(FN.ACC.PARAM, 'UNC.IVA.DANIO', R.ACC.PARAM, E.ACC.PARAM)
	    ;*Y.IVA.DANIO.ACC = R.ACC.PARAM<EB.SLV47.MONEDA> : R.ACC.PARAM<EB.SLV47.CATEGORIA> : R.ACC.PARAM<EB.SLV47.IDENTIF.CTA> : RIGHT(R.ACC.PARAM<EB.SLV47.AGENCIA>,4)
	    ;*------------------------------------------------------------------------------------------------
	    
	    ;*Obtener Cuenta UNC
	    CALL CACHE.READ(FN.ACC.PARAM, 'UNC.ACC', R.ACC.PARAM, E.ACC.PARAM)
	RETURN
	
	PROCESS:		
		;*Si es Primer Pago Anticipado y con Adelanto de Capital enviar OFS por Monto de Seguros a UNC
		IF Y.REF.ALP EQ 'ALP1' OR Y.REF.ALP EQ 'ALPC' THEN
			;*Preparando Registro de FT a Enviar por OFS
			Y.FLG.MARCAR = 'N'
			Y.UNC.AMT = R.NEW(FT.LOCAL.REF)<1,LF.AMT.TAX>
			R.FT<FT.DEBIT.ACCT.NO>    = R.NEW(FT.DEBIT.ACCT.NO)
			R.FT<FT.DEBIT.VALUE.DATE> = TODAY
			R.FT<FT.DEBIT.THEIR.REF>  = Y.PAY.AMT
			R.FT<FT.CREDIT.THEIR.REF> = ID.NEW
			R.FT<FT.TRANSACTION.TYPE> = 'AC'
			R.FT<FT.DEBIT.CURRENCY> = LCCY
			
			GOSUB GET.COMPANY	;*Buscar Agencia que Proceso el Pago
			R.FT<FT.DEBIT.AMOUNT>     = Y.UNC.AMT
			R.FT<FT.CREDIT.ACCT.NO>   = R.ACC.PARAM<EB.SLV47.MONEDA> : R.ACC.PARAM<EB.SLV47.CATEGORIA> : R.ACC.PARAM<EB.SLV47.IDENTIF.CTA> : RIGHT(Y.COMPANY,4)
			
			;*Si es Cancelacion Anticipada Buscar y Enviar Monto a Cuenta de Producto Seguros
			IF Y.REF.ALP EQ 'ALPC' THEN
				GOSUB CANCELACION.ANTICIPADA
			END
		
			IF Y.FLG.MARCAR EQ 'N' AND Y.UNC.AMT GT 0 THEN
				;*Enviar OFS para Traslado de Fondos a UNC
	        	CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID, R.FT, OFS.PARAM.ID, Y.OUT)
	        END
       	END
        
        ;*Escribir Registro en Aplicacion de Pagos Anticipados
        GOSUB GET.AAA.PAY.INFO
	RETURN
	
	CANCELACION.ANTICIPADA:
		;*Buscar Montos de Seguros a Aplicar
		GOSUB GET.SEG.AMT
		
		;*Si no Hay Registro de Pago Anticipado Previo Retornar sin hacer nada 
		IF R.ALP EQ '' THEN
			RETURN
		END
		
		;*OFS Seguro de Vida
		IF Y.SEG.VIDA GT 0 THEN
			R.FT<FT.DEBIT.AMOUNT>     = Y.SEG.VIDA
			R.FT<FT.CREDIT.ACCT.NO>   = Y.SEG.VIDA.ACC
			CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID, R.FT, OFS.PARAM.ID, Y.OUT)
		END
		
		;*Validar si debe Aplicar Seguro de Daños e IVA
		IF Y.SEG.DANIO GT 0 THEN
			;*OFS Seguro de Daños
			R.FT<FT.DEBIT.AMOUNT>     = Y.SEG.DANIO + Y.IVA.DANIO
			R.FT<FT.CREDIT.ACCT.NO>   = Y.SEG.DANIO.ACC
			CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID, R.FT, OFS.PARAM.ID, Y.OUT)
			
			;*Segun conta el IVA debe ir junto con el Seguro de Daños ver comentario de fecha 09.01.2018 por OCORNEJO
			;*-------------------------------------------------------------------------------------------------------
			;*OFS IVA Seguro de Daños
			;*R.FT<FT.DEBIT.AMOUNT>     = Y.IVA.DANIO
			;*R.FT<FT.CREDIT.ACCT.NO>   = Y.IVA.DANIO.ACC
			;*CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID, R.FT, OFS.PARAM.ID, Y.OUT)
			;*-------------------------------------------------------------------------------------------------------
		END
		
		;*Marcar todos los Registros como Aplicados en ALP
		Y.FLG.MARCAR = 'S'
	RETURN
	
	GET.SEG.AMT:
		;*Buscar Monto en Aplicacion Local
		Y.ID.1 = R.NEW(FT.CREDIT.ACCT.NO) : '.' : TODAY : '.1'
		CALL CACHE.READ(FN.ALP, Y.ID.1, R.ALP, E.ALP)
		Y.SEG.VIDA  = R.ALP<EB.SLV20.SEC.LIFE>
		Y.SEG.DANIO = R.ALP<EB.SLV20.SEC.DAMAGE>
		Y.IVA.DANIO = R.ALP<EB.SLV20.TAX.IVA.SD>
	RETURN
	
	GET.COMPANY:
		;*Si la FT viene originada por una TFS extraer Agencia de TFS
		Y.TFS = R.NEW(FT.ORDERING.CUST)		
		IF Y.TFS EQ '' THEN
			Y.COMPANY = ID.COMPANY
			RETURN
		END
		
		;*Buscar TFS y Extraer Agencia
		CALL CACHE.READ(FN.TFS, Y.TFS, R.TFS, E.TFS)
		Y.COMPANY = R.TFS<TFS.CO.CODE>
	RETURN
	
	GET.AAA.PAY.INFO:
		;*Obtener Monto de la Cuota
		CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ARR, 'PAYMENT.SCHEDULE', 'SCHEDULE', TODAY, RETURN.IDS, RETURN.VALUES.PYMT, RETURN.ER)
		R.PAY.SCHE = RAISE(RETURN.VALUES.PYMT)
		Y.FEE.AMT  = R.PAY.SCHE<AA.PS.ACTUAL.AMT>
		IF Y.FEE.AMT EQ '' THEN
			Y.FEE.AMT  = R.PAY.SCHE<AA.PS.CALC.AMOUNT>
		END
		
		;*Validar si Es Numero ya que en Cronograma Capital Vencimiento Funciona un poco Extraño
		Y.IS.AMT = CHANGE(Y.FEE.AMT,',','')
		IF NUM(Y.IS.AMT) EQ 0 THEN
			Y.FEE.AMT = 0
		END
		
		;*Obtener Capital a Pagar de la Cuota Vigente
		GOSUB GET.CAP.FEE
		
		;*Buscar Registro en Aplicacion $NAU
		CALL F.READ (FN.ALP$NAU, Y.ALP.ID, R.ALP$NAU, F.ALP$NAU, E.ALP$NAU)
		IF R.ALP$NAU EQ '' THEN
			GOSUB GET.INSURANCE.AMT
			R.ALP$NAU<EB.SLV20.SEC.LIFE>   = Y.SEG.VIDA	   	   ;*Seguro Vida
			R.ALP$NAU<EB.SLV20.SEC.DAMAGE> = Y.SEG.DANIO	   ;*Seguro Daño
			R.ALP$NAU<EB.SLV20.TAX.IVA.SD> = Y.IVA		       ;*Iva Seguro Daño
		END
		TEXTO.ARCHIVO = Y.ALP.ID : ' - ' : R.ALP$NAU
		GOSUB ESCRIBIR.ARCHIVO
		
		;*Completar demas Datos de la Aplicacion
		R.ALP$NAU<EB.SLV20.PAY.AMT>      = Y.PAY.AMT 		   ;*Monto del Pago
		R.ALP$NAU<EB.SLV20.FEE.AMT>      = Y.FEE.AMT		   ;*Monto de la Cuota
		R.ALP$NAU<EB.SLV20.FEE.CAPITAL>  = Y.CAPITAL.FEE	   ;*Monto Capital de la Cuota
		R.ALP$NAU<EB.SLV20.FEE.INTEREST> = Y.INTEREST.FEE	   ;*Monto Interes de la Cuota
		R.ALP$NAU<EB.SLV20.UNC.AMT>      = Y.UNC.AMT		   ;*Monto de Deducciones (de la Cuota) a Aplicar el dia de Pago
		R.ALP$NAU<EB.SLV20.STATUS>       = 'P'				   ;*Pendiente de Aplicacion en COB de dia de Pago		
		R.ALP$NAU<EB.SLV20.APPL.DATE>   = Y.NEXT.PAY.DATE	   ;*Fecha en que se Aplicara el Pago en COB
		;*Cancelacion Anticipada
		IF Y.REF.ALP EQ 'ALPC' THEN
			R.ALP$NAU<EB.SLV20.STATUS>  = 'A'			   	   ;*Aplicado ya que se va Directamente a la Cta Producto Seguros
			R.ALP$NAU<EB.SLV20.APPL.DATE> = TODAY
		END	
		R.ALP$NAU<EB.SLV20.FT.PAY>  	= ID.NEW			   ;*Relacionar Registro con FT de Pago
		R.ALP$NAU<EB.SLV20.RECORD.STATUS> = ''
		R.ALP$NAU<EB.SLV20.CURR.NO>       = ''
		R.ALP$NAU<EB.SLV20.INPUTTER>      = ''
		R.ALP$NAU<EB.SLV20.DATE.TIME>     = ''
		R.ALP$NAU<EB.SLV20.DEPT.CODE>     = ''
		
		TEXTO.ARCHIVO = Y.ALP.ID : ' - ' : R.ALP$NAU
		GOSUB ESCRIBIR.ARCHIVO
		
		;*Eliminar Registro a traves de OFS de la Aplicacion $NAU
		CALL SLV.OFS.UTIL.OL.TRX(Y.ALP.ID, '', 'OFS.ALP.DEL', Y.OUT)
		
		TEXTO.ARCHIVO = 'OFS.ALP.DEL --> ' : Y.OUT
		GOSUB ESCRIBIR.ARCHIVO
		
		;*Escribir Registro a traves de OFS
		CALL SLV.OFS.UTIL.OL.TRX(Y.ALP.ID, R.ALP$NAU, 'OFS.ALP.AUTH', Y.OUT)
		
		TEXTO.ARCHIVO = 'OFS.ALP.AUTH --> ' : Y.OUT
		GOSUB ESCRIBIR.ARCHIVO
		
		;*Si fue Cancelacion Anticipada y se Envió OFS a Cuenta de Producto marcar los Registros como Aplicados
		IF Y.FLG.MARCAR EQ 'S' THEN
			GOSUB MARK.ALP.A
		END
	RETURN
	
	MARK.ALP.A:
		;*Marcar los Registros del Mes o Cuota en Curso como Aplicados
		SELECT.ALP = "SELECT " : FN.ALP : " WITH @ID LIKE '" : Y.ARR :'.': LEFT(TODAY, 6) : "...'"
		CALL EB.READLIST (SELECT.ALP, ALP.LIST, '', NO.ALP, SYSTEM.RETURN.CODE)
		FOR I = 1 TO NO.ALP
			CALL F.READ(FN.ALP, ALP.LIST<I>, R.ALP, F.ALP, E.ALP)
			R.ALP<EB.SLV20.STATUS>    = 'A'			;*Aplicado ya que se va Directamente a la Cta Producto Seguros
			R.ALP<EB.SLV20.APPL.DATE> = TODAY
			CALL F.WRITE(FN.ALP, ALP.LIST<I>, R.ALP)
		NEXT I
	RETURN
	
	GET.CAP.FEE:
		;*Obtener Datos del Cronograma de Pagos
		CALL AA.SCHEDULE.PROJECTOR(Y.ARR, SIM.REF, "", Y.PAY.DATE, TOT.PAYMENT, DUE.DATES, DUE.TYPES, DUE.METHODS, DUE.TYPE.AMTS, DUE.PROPS, DUE.PROP.AMTS, DUE.OUTS)
		Y.NEXT.PAY.DATE = DUE.DATES<1>
		
		;*Pago Constante es Decir Capital, Interes y Deducciones pero esta ultima no interes para esta Rutina
		FIND 'CONSTANT' IN DUE.TYPES<1> SETTING Ap, Vp THEN	
			Y.PROPS = CHANGE(DUE.PROPS<Ap,Vp>, SM, VM)
			Y.AMTS  = CHANGE(DUE.PROP.AMTS<Ap,Vp>, SM, VM)
			
			;*Capital de la Cuota
			FIND 'ACCOUNT' IN Y.PROPS SETTING Xp, Yp THEN
				Y.CAPITAL.FEE = Y.AMTS<Xp, Yp>
			END
			
			;*Interes de la Cuota
			FIND 'PRINCIPALINT' IN Y.PROPS SETTING Xp, Yp THEN
				Y.INTEREST.FEE = Y.AMTS<Xp, Yp>
			END
		END
		
		;*Para Cronogramas con Capital Vencimiento
		FIND 'ACTUAL' IN DUE.TYPES<1> SETTING Ap, Vp THEN	
			Y.PROPS = CHANGE(DUE.PROPS<Ap,Vp>, SM, VM)
			Y.AMTS  = CHANGE(DUE.PROP.AMTS<Ap,Vp>, SM, VM)
			
			;*Interes de la Cuota
			FIND 'PRINCIPALINT' IN Y.PROPS SETTING Xp, Yp THEN
				Y.INTEREST.FEE = Y.AMTS<Xp, Yp>
			END
			
			Y.CAPITAL.FEE = 0
			FIND 'LINEAR' IN DUE.TYPES<1> SETTING Ap, Vp THEN
				Y.PROPS = CHANGE(DUE.PROPS<Ap,Vp>, SM, VM)
				Y.AMTS  = CHANGE(DUE.PROP.AMTS<Ap,Vp>, SM, VM)
				
				;*Capital de la Cuota
				FIND 'ACCOUNT' IN Y.PROPS SETTING Xp, Yp THEN
					Y.CAPITAL.FEE = Y.AMTS<Xp, Yp>
				END
			END
		END
	RETURN
	
	REVE.PROCESS:
       	;*Enviar OFS de Reversa para la UNC
       	CALL SLV.A.REV.OFS.SEG.UNC(ID.NEW)
	RETURN
	
	GET.INSURANCE.AMT:
		;*Obtener Monto del Prestamo
		CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ARR, 'TERM.AMOUNT', 'COMMITMENT','',RETURN.IDS,RETURN.VALUES,RETURN.ER)
		R.AA.TERM = RAISE(RETURN.VALUES)
		
		;*Obtener  Seguro de Vida
		CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ARR, 'CHARGE', 'ALSEGUROVIDA','',RETURN.IDS,RETURN.VALUES,RETURN.ER)
		R.AA.CHARGE = RAISE(RETURN.VALUES)
		Y.SEG.VIDA  = DROUND(((R.AA.CHARGE<AA.CHG.CHARGE.RATE> * R.AA.TERM<AA.AMT.AMOUNT>) / 100), 2)
		
		;*Obtener Seguro de Daños
		CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ARR, 'CHARGE', 'ALSEGURODANO','',RETURN.IDS,RETURN.VALUES,RETURN.ER)
		R.AA.CHARGE = RAISE(RETURN.VALUES)
		Y.SEG.DANIO = 0 ; Y.IVA = 0
		
		;*Si el Credito posee Cobro de Seguro de Daños
		IF R.AA.CHARGE NE '' THEN
			;*Asignar Seguro y Calcular IVA
			Y.SEG.DANIO = R.AA.CHARGE<AA.CHG.FIXED.AMOUNT>
			
			;*Calcular IVA
			GOSUB GET.IVA
			Y.IVA = DROUND(((R.AA.CHARGE<AA.CHG.FIXED.AMOUNT> * R.TAX<EB.TAX.RATE>) / 100),2)
		END
	RETURN
	
	GET.IVA:
		SELECT.STMT = "SELECT " : FN.TAX : " WITH @ID LIKE '" : IVA : "...'"
		CALL EB.READLIST(SELECT.STMT, Y.LIST, '', NO.REC, SYSTEM.RETURN.CODE)
		Y.LAST = ''
		FOR I = 1 TO NO.REC 			;*Buscar Registro de Tax mas Actualizado
			IF Y.LAST EQ '' OR Y.LAST LT FIELD(Y.LIST<I>,'.',2) THEN
				Y.LAST = FIELD(Y.LIST<I>,'.',2)
			END
		NEXT I		
		CALL F.READ (FN.TAX, IVA : '.' : Y.LAST, R.TAX, F.TAX, ERR.TAX)
	RETURN
	
	ESCRIBIR.ARCHIVO:
		;*Si el parametro de Log esta Activo Escribir Archivo
		IF LOG.ACTIVO EQ 'Y' THEN
		    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
		    WRITESEQ '[' : LEFT(TIMEDATE(),8) : ']  ' : TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
		    END
		    CLOSESEQ SEQ.PTR
		END
    RETURN
END
