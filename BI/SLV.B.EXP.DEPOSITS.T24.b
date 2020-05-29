*-----------------------------------------------------------------------------
* <Rating>18023</Rating>
*--------------------------------------------------------------------------------------------------------
* Nombre: SLV.B.EXP.DEPOSITS.T24.b
* Descripcion: Rutina encargada de generar archivo .csv con informacion de Arrangemente de Depositos, Creditos y Cuentas de
*				 T24 , para volcado de data a SIC
*---------------------------------------------------------------------------------------------------------
* Version	    Autor		 Fecha			Comentario
*---------------------------------------------------------------------------------------------------------
* 1.0		ITurcios		04.04.2016	Version inicial
* 1.1		ITurcios		06.05.16	Se agrega parseo de campo DateTime debido a la data erronea que viene de los ambientes.
* 1.2 		ITurcios		23.08.16	Se agregan nuevos campos para DAPs y se cambia logica para leer idArrangement desde Activity History.
* 1.3 		Mgarcia			14.11.16	Se agregan nuevos campos para Creditos para proyecto Cobranza.
* 1.4 		ITurcios		22.11.16	Se agregan modifica campo AccInterestRate para colocar en 0 las tasas de las cuentas que no tienen bandas (corrientes).
* 1.5 		ITurcios		03.12.16	Se elimina parseo para fecha de auditoria ArrDateTime por problemas en PROD.
* 1.6		ITurcios		06.12.2016	Se modifica el campo ArrDateTime para que lea la fecha y hora del servidor.
* 1.7		Mgarcia			08.12.2016	Se modifica campo de Dias de Gracia para que no realice funcion CDD que generó problema en COB.
* 1.8 		MMenjivar		20.01.17	Se modifica para que genere únicamente arrangements con ArrStatus válido.
* 1.9		Ocornejo		02.03.2017	Se agrega extraccion de Motivo de Cancelacion desde AA.ACCOUNT.CLOSURE.DETAILS 
* 2.0 		MMenjivar		03.03.17	Se modifica para incluir campos de depositos y cuentas adicionales (DapActualBalance, DapReserva, DapTipoInteres, DapIntAcum, DapFechaVencimiento		
*										DapDiasAntVencimiento, DapNumTitulares, DapMontoGar, DapMontoNoGar, DapIntPagadoMes, DapISRPagadoMes, AccActualBalance, AccReserva, AccTipoInteres, AccIntAcum, AccSaldoPromedio		
*										AccNumTitulares, AccMontoGar, AccMontoNoGar, AccIntPagadoMes, AccISRPagadoMes)
* 2.1		MMenjivar 		31.03.17	Se modifica para incluir campo CrdPrinIntAcc
* 2.2 		MMenjivar 		11.05.17	Se modifica para incluir campos CrdSaldoIntereses,  CrdAdelantoK, CrdSeguroDanio, CrdSeguroDanioIVA y se corrigen campos CrdIntVencidos, CrdInterestRate (Interes Nominal), 
*										CrdSaldoCapital, CrdPrinIntAcc, CrdOverCap, CrdIntMoratorio, CrdPayAmount, CrdInsuranceFee
* 2.3		MMenjivar		07.06.17	Se modifica para llenar campos CrdAmountMoraCap y CrdAmountMoraInt.
* 2.4 		MMenjivar		29.06.17	Se modifica rutina para ejecutarse como multihilos.
* 2.5		MMenjivar		27.10.17	Se agregan campos AccAccruedInt y ArrFechaProceso.
*--------------------------------------------------------------------------------------------------------
SUBROUTINE SLV.B.EXP.DEPOSITS.T24(ARR.ID)
  
$INSERT I_COMMON 
$INSERT I_EQUATE     
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY  
$INSERT I_F.AA.ACTIVITY.HISTORY 
$INSERT I_AA.LOCAL.COMMON     
$INSERT I_F.ACCOUNT        
$INSERT I_F.AA.ACCOUNT 
$INSERT I_F.AA.ACCOUNT.DETAILS 
$INSERT I_AA.APP.COMMON	
$INSERT I_F.AA.ARRANGEMENT   
$INSERT I_F.AA.ACCOUNT
$INSERT I_F.AA.INTEREST    
$INSERT I_F.AA.SETTLEMENT
$INSERT I_F.AA.PRODUCT
$INSERT I_F.AA.TERM.AMOUNT
$INSERT I_F.AA.CHANGE.PRODUCT    
$INSERT I_F.AA.ACCOUNT.DETAILS
$INSERT I_F.AA.SETTLEMENT 
$INSERT I_F.AA.PAYMENT.SCHEDULE
$INSERT I_F.CUSTOMER
$INSERT I_F.AA.CUSTOMER 
$INSERT I_F.EB.LOOKUP
$INSERT I_F.AA.INTEREST.ACCRUALS
$INSERT I_F.SLV.REGULATORY.INFO
$INSERT I_F.AA.BILL.DETAILS 
$INSERT I_F.SLV.LOAN.STATUS
$INSERT I_F.SLV.COND.ASSET
$INSERT I_F.EB.CONTRACT.BALANCES
$INSERT I_F.AA.SCHEDULED.ACTIVITY
$INSERT I_F.AA.ACTIVITY.BALANCES 
$INSERT I_F.SLV.AA.ACT.REVE.BAL.DET
$INSERT I_SLV.E.LOAN.COMMON
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.DATES 
$INSERT I_F.AA.ACCOUNT.CLOSURE.DETAILS
$INSERT I_F.AA.CHARGE
$INSERT I_F.AA.TAX
$INSERT I_SLV.B.EXP.DEPOSITS.T24.COMMON
$INSERT I_F.SLV.RET.ISR.AVERAGE.BALANCE
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
INIT:
	DIR.NAME			= 'SICTemp'  
*	DIR.NAME 			= 'C:\Users\mamenjivar\Desktop\TEST\EXPARR\'

	
	;*Para guardar arreglos de propiedades	 
	GOSUB SET.VARIABLES
	
	
RETURN
*------------------------------------------------------------------------------- 
PROCESS:       

		;*Obtener informacion de BILL.DETAILS para Lendings
		CRT 'Inicio BILLDET'
		GOSUB BILLDET
		CRT 'FIN BILLDET'
		
		;*Obtener informacion de BILL.DETAILS para Daps y Accounts
		CRT 'Inicio INT.PAGADO'
		GOSUB INT.PAGADO
		CRT 'Fin INT.PAGADO'
	

		;*Leyendo Data de Arrangement entrante
		CALL F.READ(FN.ARRANGEMENT, ARR.ID, R.ARRANGEMENT_INI, F.ARRANGEMENT, Y.ARRANGEMENT.ERR2)
		;*Creando arreglo con ids de Arrangement para posterior uso
		
		IF R.ARRANGEMENT_INI<AA.ARR.ARR.STATUS> NE '' THEN 
			J 						= J + 1 
			ARRAY.ID.ARRAG<J> 		= ARR.ID
			;*Account 	 
			Y.REF.ID	  			= R.ARRANGEMENT_INI<AA.ARR.LINKED.APPL.ID>
			CALL F.READ(FN.ACC, Y.REF.ID, R.ACC.ARR.NEW , F.ACC, ERR.ACC)
	
			IF R.ACC.ARR.NEW EQ '' THEN
				;*Se lee del History para abarcar Arrangements cerrados
				CALL F.READ.HISTORY(FN.ACC.HIS, Y.REF.ID, R.ACC.ARR.NEW, F.ACC.HIS, ERR.ACC.HIS)
			END	
			;*Propiedad Account del Arrangement
			RETURN.ER.A = ''
			RETURN.VALUES.ACC = ''
			RETURN.IDS= ''			
			CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'ACCOUNT', '', TODAY, RETURN.IDS, RETURN.VALUES.ACC, RETURN.ER)
			R.ARR.ACCOUNT = RAISE(RETURN.VALUES.ACC)
			
			CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'CUSTOMER', '', TODAY, RETURN.IDS, RETURN.VALUES.CUST, RETURN.ER)		
			;*-------------------------------------------------------------------ARRANGEMENT----------------------------------------------------------------------
			;*Id de Arrangement
			Arrangement 		= ARR.ID			
			;*Numero de Cuenta
	  		ArrNumReference 	= R.ARRANGEMENT_INI<AA.ARR.LINKED.APPL.ID>
		  	;*Customer ID
	  		CustomerId 			= R.ARRANGEMENT_INI<AA.ARR.CUSTOMER>
		  	;*Estatus del Arrangement
		  	ArrStatus 			= R.ARRANGEMENT_INI<AA.ARR.ARR.STATUS>		  
	  		;*Usuario que ingreso
		  	ArrUserInput		= FIELD(R.ACC.ARR.NEW<AC.INPUTTER>, "_", 2) 
	  		;*Start Date
			ArrStartDate		= R.ARRANGEMENT_INI<AA.ARR.START.DATE>
			;*Id de la Agencia
			ArrBranchId 		= R.ARRANGEMENT_INI<AA.ARR.CO.CODE>
			;*Balance Actual de la Cuenta
			ArrWorkingBalance   = R.ACC.ARR.NEW<AC.WORKING.BALANCE>
			;*-----------------------AUDITORIA ARRANGEMENT------------------------------------------------
			;*Digitador			
	  		ArrInputter 		= FIELD(R.ACC.ARR.NEW<AC.INPUTTER>, "_", 2) 		
			;*DateTime Auditoria 
			ArrDateTime			= UsLocGetDate 		
		 	;*Autorizador
			ArrAuthorizer		= FIELD(R.ACC.ARR.NEW<AC.AUTHORISER>, "_", 2)	
			;*Agencia		
	  		ArrCompany	 		= R.ACC.ARR.NEW<AC.CO.CODE>
	  		;*Numero Actual del registro
		  	ArrCurrNum   		= R.ACC.ARR.NEW<AC.CURR.NO>  
		  	;*Ultima Alerta Enviada por el Arrangement	
			CantOver			= DCOUNT(R.ACC.ARR.NEW<AA.AC.OVERRIDE>, VM)
	  		ArrOverride  		= R.ACC.ARR.NEW<AA.AC.OVERRIDE><1,CantOver>  			 			
		  	;*--------------------------PRODUCTO------------------------------------------------  	
		  	;*Tipo de Producto
		  	Product				= R.ARRANGEMENT_INI<AA.ARR.PRODUCT>
		  	;*Grupo de Producto
	  		ProductGroup 		= R.ARRANGEMENT_INI<AA.ARR.PRODUCT.GROUP>
	  		;*Linea del Producto
	  		ProductLine 		= R.ARRANGEMENT_INI<AA.ARR.PRODUCT.LINE>
	  		;*Fecha Efectiva del producto
	  		ProEfectDate 		= R.ARRANGEMENT_INI<AA.ARR.PROD.EFF.DATE>			
			;*Estatus del Producto 
	  		ProductStatus 		= R.ARRANGEMENT_INI<AA.ARR.PRODUCT.STATUS> 
	  		;*--------------------------CUENTA-------------------------------------------------	
			;*Titulo de la Cuenta
			AccTitle			= R.ACC.ARR.NEW<AC.ACCOUNT.TITLE.1>				
			;*Titlo corto de la cuenta
			AccShortTitle		= R.ACC.ARR.NEW<AC.SHORT.TITLE>	 
			
			ArrCategory			= R.ACC.ARR.NEW<AC.CATEGORY>
			;* Fecha en que se extrajo la información
			ArrFechaProceso		= TODAY

			;*------------------------------------------------------FIN DE INFORMACION BASICA DE ARRANGEMENT---------------------------------------------------------------
			
			;**********************************************************************--PRESTAMOS--*****************************************************************************
			IF(R.ARRANGEMENT_INI<AA.ARR.PRODUCT.LINE> EQ Y.LENDING) THEN
	
				;*Obteniendo data de las propiedades
				;* Term Amount
				CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'TERM.AMOUNT', 'COMMITMENT', TODAY, RETURN.IDS, RETURN.VALUES.TERM, RETURN.ER)
				R.TERM.AM = RAISE(RETURN.VALUES.TERM)
				;*Tasa de Interes
				CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'INTEREST', '', TODAY, RETURN.IDS, RETURN.VALUES, RETURN.ER)
				R.INTEREST = RETURN.VALUES				
				
				CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.LIST.IDS<LK>, 'SETTLEMENT','SETTLEMENT', TODAY, RETURN.IDS, RETURN.VALUES.SETT, RETURN.ER)
				R.SETTL.NEW = RAISE(RETURN.VALUES.SETT)	
								   
				    
				;*Obteniendo posicion de campos Locales
				CALL GET.LOC.REF('AA.ARR.INTEREST','LF.EFEC.INTRATE', LOCPOSTasaInteresEfec)
				CALL GET.LOC.REF('AA.ARR.INTEREST','LF.REF.INT.RATE', LOCPOSTasaReferencia)
				CALL GET.LOC.REF('AA.ARR.CUSTOMER','LF.FORMA.PAGO', LOCPOSForPag)
				CALL GET.LOC.REF('AA.ARR.TERM.AMOUNT','LF.DESTINO.SSF', LOCPOSDestinoSSF)
				CALL GET.LOC.REF('ACCOUNT','LF.LOAN.STATUS',POS.EST)
				
*				CRT 'OTHER.PARTY: ' : RETURN.VALUES.CUST<1,AA.CUS.OTHER.PARTY>
				
			    APPL.ARR		='AA.ARR.ACCOUNT'
			    FIELDNAME.ARR	='LF.AML.DEP.PROY':VM:'LF.COND.ASSET':VM:'LF.AML.DEP.PROY':VM:'LF.AML.RET.PROY':VM:'LF.AML.PROC.FON':VM:'LF.LOAN.STATUS'
			    CALL MULTI.GET.LOC.REF(APPL.ARR, FIELDNAME.ARR, POS.ARR)
				
				;*Obteniendo dias de mora/omisos 
				CALL F.READ(FN.REGUL.INFO, ARR.ID, R.REG.INFO, F.REGUL.INFO, ERROR.REG.INFO)
				
	   			CALL F.READ(FN.ACC.DET, ARR.ID, R.ACC.DET, F.ACC.DET, ERR.ACCDET)
	   			;*Obteniendo Descripcion del Estado del prestamo
*			    ID.STATUS.CRE = FIELD(RETURN.VALUES.ACC<1,AA.AC.LOCAL.REF>,SM,POS.ARR<1,6>)
			    ID.STATUS.CRE = R.ACC.ARR.NEW<AC.LOCAL.REF><1, POS.EST>			    
			    CALL F.READ(FN.LOAN.STATUS, ID.STATUS.CRE, DET.LOAF.STATUS, F.LOAN.STATUS, LOAN.STATUS.ERR)
			    ;*Obteniendo el ID y descripcion de la condicion del Credito
			    ID.CRE.COND = FIELD(RETURN.VALUES.ACC<1,AA.AC.LOCAL.REF>,SM,POS.ARR<1,2>)
			    CALL F.READ(FN.COND.ASSET, ID.CRE.COND, DET.COND.ASSET, F.COND.ASSET, COND.ASSET.ERR)		   
			    ;*Obteniendo la cantidad de disponibilidad del Credito siempre y cuando sea Revolvente
			    CALL F.READ(FN.ECB, ArrNumReference, R.FN.ECB, F.ECB, FN.ECB.ERR)
			    ;*Obteniendo fecha siguiente de pago
				CALL F.READ(FN.SCHE.ACT, ARR.ID, R.SCHE.ACT, F.SCHE.ACT, RET.ERR)
				
				;* Tasa de interes
	        	CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.LIST.IDS<LK>, 'INTEREST', 'PRINCIPALINT', TODAY, RETURN.ID.INT, R.AA.INT, Y.ERR.INT)				
				;* Cuota de Capital + intereses
	        	CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'AA.ARR.PAYMENT.SCHEDULE', 'SCHEDULE', TODAY, RETURN.ID.REPAY, R.AA.REPAY, Y.ERR.REP)					
							
				;*Cuota de Seguro de Danio
		        CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'AA.ARR.CHARGE', 'ALSEGURODANO', TODAY, RETURN.ID.REPAY, R.CHARGE, Y.ERR.CHAR)
		        
				IF R.CHARGE<1, AA.CHG.CHARGE.TYPE> EQ "FIXED" THEN
					S.SEG.DANO = R.CHARGE<1, AA.CHG.FIXED.AMOUNT>
				END ELSE
					IF R.CHARGE<1, AA.CHG.CHARGE.TYPE> EQ "CALCULATED" THEN
						S.SEG.DANO = DROUND((RETURN.VALUES.TERM<1,AA.AMT.AMOUNT> * R.CHARGE<1, AA.CHG.CHARGE.RATE>) / 100, 2) 
					END
				END
				
		        ;*Cuota de Seguro de Deuda-
		        CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'AA.ARR.CHARGE', 'ALSEGUROVIDA', TODAY, RETURN.ID.REPAY, R.CHARGE, Y.ERR.CHAR)
				
				IF R.CHARGE<1, AA.CHG.CHARGE.TYPE> EQ "FIXED" THEN
					S.SEG.VIDA = R.CHARGE<1, AA.CHG.FIXED.AMOUNT>
				END ELSE
					IF R.CHARGE<1, AA.CHG.CHARGE.TYPE> EQ "CALCULATED" THEN	
						S.SEG.VIDA = DROUND((RETURN.VALUES.TERM<1,AA.AMT.AMOUNT> * R.CHARGE<1, AA.CHG.CHARGE.RATE>) / 100, 2) 
					END
				END
	
		        ;*Cuota de IVA Seguro de DenoAC
		        CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'AA.ARR.TAX', 'IVA', TODAY, RETURN.ID.REPAY, R.TAX, Y.ERR.TAX)
				S.SEG.DANO.IVA = DROUND(S.SEG.DANO * (R.TAX<1, AA.TAX.PROP.TAX.CODE> / 100 ), 2) 
				
				;*Sumatoria de cargos
				S.TOT.CARGO = S.SEG.DANO + S.SEG.DANO.IVA + S.SEG.VIDA
								
				;*Verificar si el prestamos tiene PENALTYINT
				N.PENALTYINT = 0
				LOCATE ARR.ID IN S.ARR2 SETTING N.POS ELSE N.POS=''
				
				IF N.POS THEN
				  	;*N.PENALTYINT = DROUND(FIELD(STR.PENALTYINT<N.POS>,'*',2),2)
				  	N.PENALTYINT = FIELD(STR.PENALTYINT<N.POS>,'*',2)
				END											
				
				;*Verificar si el prestamos tiene cuota en periodo de gracia
				LOCATE ARR.ID IN S.ARR3 SETTING N.POS ELSE N.POS=''
				
				N.GRACE.K = 0
				N.GRACE.I = 0
				N.VENC.I  = 0
				IF N.POS THEN
				   N.GRACE.K = FIELD(STR.GRACE<N.POS>,'*',2)
				   N.GRACE.I = FIELD(STR.GRACE<N.POS>,'*',3)
				   N.VENC.I  = FIELD(STR.GRACE<N.POS>,'*',4)
				END
				
				N.CUOTA = 0.00
				;*Cuota con modificacion manual
				N.CUOTA = SUM(R.AA.REPAY<1,AA.PS.ACTUAL.AMT>)
				
				;*Cuota calculada
				IF N.CUOTA EQ 0 THEN
					N.CUOTA = SUM(R.AA.REPAY<1,AA.PS.CALC.AMOUNT>)
				END
	
				N.CUOTA -= S.TOT.CARGO 
		       	
		       	;*Obtener la provision de intereses
		       	CALL F.READ(FN.INTEREST.ACCRU, ARR.ID:"-PRINCIPALINT", R.INT.ACCR, F.INTEREST.ACCRU, Y.ERR.ACCR)
	
				;*Obtener valor de sobrepago de credito
				N.PRINCIPALINT = 0
		     	N.SAL.VIG.K   = 0
		     	N.SAL.VEN.K   = 0
		     	N.SAL.MORA.K  = 0
		     	N.SAL.GRACE.K = N.GRACE.K
		     	N.SAL.VIG.I   = 0
		     	N.SAL.VEN.I   = 0
		     	N.SAL.MORA.I  = 0
		     	N.UNCACCOUNT  = 0
		     	N.SAL.GRACE.I = N.GRACE.I
		     	
		     	;* Si se encuentra cerrado el estado local irá vacío.
		     	IF R.ARRANGEMENT_INI<AA.ARR.ARR.STATUS> EQ "CLOSE" THEN
		     		ID.STATUS.CRE 		= ""
					DET.LOAF.STATUS 	= ""
		     	END
				
		     	;*Para estos estados de toman los saldos de EB.CONTRACT.BALANCES mientras TENEMOS resuelve
		     	;*la observacion de actualización en la aplicacion SLV.REGULATORY.INFO.
		     	IF (R.ARRANGEMENT_INI<AA.ARR.ARR.STATUS> EQ "PENDING.CLOSURE" OR R.ARRANGEMENT_INI<AA.ARR.ARR.STATUS> EQ "CLOSE") THEN
	
			     	;* Para estados PENDING.CLOSURE y CLOSE se dejan los saldo a CERO ya que el sistema los dejara con cero
			     	;* despues de los 5 dias del pago total del prestamo, luego de instalado el desarrollo de TEMENOS se hara
			     	;* revision para determinar si es posible obtener los saldos de SLV.REGULATORY.INFO o EB.CONTRACT.BALANCES
			     	N.CURACCOUNT =  0
			     	N.NABACCOUNT =	0
			     	N.PRINCIPALINT 	  = 0
			     	N.DELPRINCIPALINT = 0
			     	N.NABPRINCIPALINT = 0
												
					N.DIAS.MORA.K = 0	
					N.DIAS.MORA.I = 0
					S.SEG.DANO		= 0
					S.SEG.VIDA		= 0
					S.SEG.DANO.IVA	= 0
					N.SAL.VIG.K   = N.CURACCOUNT
					N.SAL.VEN.K   = N.CURACCOUNT + N.NABACCOUNT
					N.SAL.MORA.K  = N.NABACCOUNT
					N.SAL.VIG.I   = N.PRINCIPALINT
	 				N.SAL.VEN.I   = N.PRINCIPALINT+N.NABPRINCIPALINT
					N.SAL.MORA.I  = N.NABPRINCIPALINT
				END ELSE
			        N.SAL.VIG.K  = R.REG.INFO<SLV.AA.REG.SALDO.VIGENTE.K>
			        N.SAL.VEN.K  = R.REG.INFO<SLV.AA.REG.SALDO.VENCIDO.K>
			        N.SAL.MORA.K = R.REG.INFO<SLV.AA.REG.SALDO.MORA.K>        
					N.SAL.VIG.I  = R.REG.INFO<SLV.AA.REG.SALDO.VIGENTE.I> - N.VENC.I
			        N.SAL.VEN.I  = R.REG.INFO<SLV.AA.REG.SALDO.VENCIDO.I>
			        N.SAL.MORA.I = R.REG.INFO<SLV.AA.REG.SALDO.MORA.I>        
			        N.DIAS.MORA.K = R.REG.INFO<SLV.AA.REG.DIAS.MORA.K>
					N.DIAS.MORA.I = R.REG.INFO<SLV.AA.REG.DIAS.MORA.I>
		       	END
		       	
	    		;*Evaluar tasa de interes para credito pignorados - Tasa de Interes Nominal
	    		N.TASA.INT = 0
				IF R.ARRANGEMENT_INI<AA.ARR.PRODUCT.GROUP> EQ "PIGNORADO" THEN
					N.TASA.INT = OCONV(R.INTEREST<1,AA.INT.EFFECTIVE.RATE>*100,"MD2")
				END ELSE
					N.TASA.INT = OCONV(R.INTEREST<1,AA.INT.FIXED.RATE>*100,"MD2")
				END			
		       	
				;*Obtener valor de sobrepago de credito
				GOSUB SOBREPAGO.PRESTAMO
				
				N.UNCACCOUNT = Y.SALDO.TOTAL * -1	
				
				IF N.UNCACCOUNT LT 0 AND N.SAL.VEN.K EQ 0 THEN
					N.SAL.VIG.K  = N.UNCACCOUNT
					N.SAL.VEN.K  = N.UNCACCOUNT
				END
	
		       	IF D.LAST.PERIOD.END GE R.INT.ACCR<AA.INT.ACC.FROM.DATE><1,1> AND D.LAST.PERIOD.END LE R.INT.ACCR<AA.INT.ACC.TO.DATE><1,1> THEN
			        STR.ACCR = R.INT.ACCR<AA.INT.ACC.ACCRUAL.AMT><1,1>		;*<24>Monto de intereses - ultimo calculo
			    END ELSE
			    	STR.ACCR = ""
			    END
			    
								    				  
				;*Monto del Credito
				CrdAmount			= R.TERM.AM<AA.AMT.AMOUNT>				
										
				;*Tasa de Interes Efectiva
				CrdEffectiveRate 	= FIELD(R.INTEREST<1,AA.INT.LOCAL.REF>,SM,LOCPOSTasaInteresEfec)
				;*Tasa de Referencia
				CrdReferenceRate 	= FIELD(R.INTEREST<1,AA.INT.LOCAL.REF>,SM,LOCPOSTasaReferencia)
				;*Fecha de desembolso
				CrdDisburstDate 	= ""
				;*Fecha de vencimiento
				CrdDueDate 			= R.TERM.AM<AA.AMT.MATURITY.DATE>
				;*Plazo
				CrdTermLending 		= R.TERM.AM<AA.AMT.TERM>									
				;*Saldo
				CrdBalance 			= ""
				;*Forma de pago
				CrdPaymentMethod 	= FIELD(RETURN.VALUES.CUST<1,AA.CUS.LOCAL.REF>,SM,LOCPOSForPag)
				;*Fecha de pago
				CrdPaymentDate 		= ""
				;*Tipo de credito
				CrdType 			= R.ARRANGEMENT_INI<AA.ARR.PRODUCT>
				;*Estado del credito
				CrdStatus 			= R.ARRANGEMENT_INI<AA.ARR.PRODUCT.STATUS>
				;*Bandera que indica si el credito es via judicial
				CrdJudicial 		= ""
				;*Saldo en Mora Capital
		      	CrdAmountMoraCap 	= N.SAL.MORA.K
		      	;*Saldo en Mora Interes
		      	CrdAmountMoraInt 	= N.SAL.MORA.I
		      	;*Dias Mora Capital
		      	CrdCapMorDays 		= N.DIAS.MORA.K
		      	;*CRT 'CrdCapMorDays: ' : CrdCapMorDays
		      	;*Dias Mora Interes
			    CrdIntMorDays 		= N.DIAS.MORA.I
			    ;*CRT 'CrdIntMorDays: ' : CrdIntMorDays
			    ;*Interes Vigente
			    CrdCurrInteres 		= ""
			    ;*Interes Vencido
			    CrdOverInteres 		= ""
			    ;*Saldos Seguros vigente
			    CrdCurrTax 			= ""
			    ;*Saldos Seguros Vencido
			    CrdOverTax 			= ""
			    ;*Saldos de Capital Vigente
			    CrdCurrCap 			= ""

			    ;*Fecha de Cancelacion
			    CrdCancelDate 		= R.ACC.ARR.NEW<AC.CLOSURE.DATE>		    
		    	;*Destino del credito
		    	CrdDestinySSF 		= FIELD(RETURN.VALUES.TERM<1,AA.AMT.LOCAL.REF>,SM,LOCPOSDestinoSSF)
			    ;*Condicion del Prestamo				
				CrdCreditCondition 	= DET.COND.ASSET<SLV.COND.DESCRIPTION>
			    ;*Cuota de gasto / Frecuencia de Pago
				CrdPaymentAmountDJ 	= FIELD(R.ARR.ACCOUNT<1,AA.AC.LOCAL.REF>,SM,POS.ARR<1,3>)
			    ;*Monto de pago adelantado			   
				CrdAdvancedAmountDJ = FIELD(R.ARR.ACCOUNT<1,AA.AC.LOCAL.REF>,SM,POS.ARR<1,4>)
			    ;*Procedencia de fondos
				CrdSourceFundsDJ 	= FIELD(R.ARR.ACCOUNT<1,AA.AC.LOCAL.REF>,SM,POS.ARR<1,5>)
				;*Ejecutivo de Credito
			    CrdSellingOfficer 	= ""
			    ;*Codigo de Gasto			    
				CrdExpenseId 		= ""
				;*Tipo de Gasto				
				CrdExpenseType 		= ""
				;*Monto del Gasto				
				CrdExpenseAmount 	= ""
				;*Short Title de la Referencia				
				CrdNameAccount 		= ""
				;*Frecuencia de Pago para Todos los Productos						
				PaymentFreq 		= ""
				;*Ejecutivo Primario para Todos los Productos				
				PrimaryOfficer 		= ""
				;*Deposito Garante del Credito				
				CrdDepGuarantee 	= ""
				;*Hipoteca Garante del Credito				
				CrdAssetGuarantee 	= ""
				;*Cuenta a Debitar (Garante del Credito)				
				CrdPayOutAccount 	= ""
				;*Se envio vacio
				CrdNoFolio   		= ""
				;*Cantidad de amortizaciones vencidas
				CrdOmisos 			= ""
				;*Pago Primer Cuota
	   			CrdFechaAsignacion = R.ACC.DET<AA.AD.PAYMENT.START.DATE>
	   			;*Fecha Desembolso
			    CrdFechaApertura   = R.ACC.DET<AA.AD.START.DATE>			    
			    ;*ID BILLS
			    MY.BILL.ID         = R.ACC.DET<AA.AD.BILL.ID>			    
			   		    				
				;*Intereses diarios del credito
				CrdIntCorrientes   = R.REG.INFO<SLV.AA.REG.SALDO.VIGENTE.I>
								
				;*Ciclo Pago K
				CrdCicloPK         = R.REG.INFO<SLV.AA.REG.PAGO.CAPITAL>
				
				;*Ciclo Pago I
				CrdCicloPI         = R.REG.INFO<SLV.AA.REG.PAGO.INTERES>
				
				;*Estado actual Prestamo 
				CrdEstPrestamo     = DET.LOAF.STATUS<SLV.STAT.DESCRIPTION>
				
				;*Principal Interes(Delinquet) 
				CrdPrinIntDel      = R.REG.INFO<SLV.AA.REG.SALDO.MORA.I>
				
				;*Cuando tiene fiador: Para garantia fiduciaria
				CrdOtherParty      = RETURN.VALUES.CUST<1,AA.CUS.OTHER.PARTY, 1>
				
	   			;*Valor maximo de los dos valores: DiasMoraI vs DiasMoraK
	   			CrdDiasMoraKI = CrdIntMorDays
	   			IF CrdCapMorDays GT CrdIntMorDays THEN	
	   			   CrdDiasMoraKI = CrdCapMorDays 
	   			END
					     
			    ;*Carga Info BILLS.DETAILS: seguros pendientes, Penaltys
*				GOSUB GET.BILLS.PROCESS

			
				;*Saldo disponible cuando el prestamo es revolvente
				CrdDispRevolvente = 0.0
			    FINDSTR "CURCOMMITMENTBL" IN R.FN.ECB<ECB.TYPE.SYSDATE> SETTING Ap, Vp THEN
			        CrdDispRevolvente = R.FN.ECB<ECB.OPEN.BALANCE><1,Vp>
			    END	
	
				;*Fecha Pago Siguiente
		    	FINDSTR "LENDING-MAKEDUE-SCHEDULE" IN R.SCHE.ACT<AA.SCH.ACTIVITY.NAME> SETTING Ap, Vp THEN
		        	CrdFechaSigPago = R.SCHE.ACT<AA.SCH.NEXT.DATE><1,Vp>                    
		     	END 
		     	;*Cargar ultimo pago Neto realizado por Cliente
		     	V.REFERENCIA = ARR.ID
*		     	GOSUB ACT.BAL.PROCESS

		     	;*Ulitmo pago realizado
		     	CrdMontUltPago = ULTIMO.PAGO
		     	;*CRT 'CrdMontUltPago: ' : CrdMontUltPago
		     			     	
		     	;*Saldo Capital
			    CrdSaldoCapital    		= N.SAL.VEN.K	
		     	
		     	;*Saldo intereses 												(Nuevo)
		     	CrdSaldoIntereses 		= N.SAL.VEN.I + N.SAL.GRACE.I
			    
				;*Interes vencido
				CrdIntVencidos     		= N.SAL.VEN.I + N.SAL.GRACE.I - N.SAL.VIG.I
				
				;*Tasa de Interes Nominal
				CrdInterestRate 		= N.TASA.INT	
				
		     	;* Prov. Dia Int. COB
		     	CrdPrinIntAcc = STR.ACCR
		     	
		     	;* Anticipo o Adelanto Capital 									(Nuevo)
		     	CrdAdelantoK = R.REG.INFO<SLV.AA.REG.ADELANTO.CAPITAL>
		     	
		     	;*Saldos de Capital Vencido		     	
		     	IF (N.SAL.VEN.K - N.SAL.VIG.K) EQ 0 THEN
					CrdOverCap := (N.SAL.VEN.K - N.SAL.VIG.K) 				;*<18>Capital vencido
				END ELSE
					CrdOverCap := (N.SAL.VEN.K-N.SAL.VIG.K-N.SAL.GRACE.K) 	;*<18>Capital vencido
				END
				
				;* Interes Moratorio
				CrdIntMoratorio   	= N.PENALTYINT    
		     	
		     	;*Cuota	
				CrdPayAmount 		= N.CUOTA
				
				;*Cuota de Seguro de vida										
				CrdInsuranceFee		= S.SEG.VIDA
				
				;*Cuota de Seguro de Danio										(Nuevo)
				CrdSeguroDanio		= S.SEG.DANO
				
				;*Seguro de Danio - IVA											(Nuevo)
		     	CrdSeguroDanioIVA	= S.SEG.DANO.IVA
		     	
		     	CrdPayInAccount = ""
			    FINDSTR "CONSTANT" IN R.SETTL.NEW<AA.SET.PAYMENT.TYPE> SETTING Ap, Vp THEN
			    	IF (R.SETTL.NEW<AA.SET.PAYIN.SETTLEMENT,1, Vp> EQ 'YES') THEN
			    		CrdPayInAccount	= R.SETTL.NEW<AA.SET.PAYIN.ACCOUNT, 1, Vp>
			       END 
			    END	
			END	 
			;**********************************************************************--FIN PRESTAMOS--***************************************************************************
			
			;**********************************************************************--DEPOSITOS--*******************************************************************************
			 
			IF(R.ARRANGEMENT_INI<AA.ARR.PRODUCT.LINE> EQ Y.DEPOSITS) THEN
				;*Leyendo Posiciones de Campos Locales
				CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.NO.FORM',LOCPOSNumForm)	
				CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.AML.DEP.PROY',LOCPOSMonPro)
				CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.AML.PROC.FON', LOCPOSPreFon) 
				CALL GET.LOC.REF('ACCOUNT','L.DEP.STATUS', LOCPOSDepStatus)
				;*Obteniendo data de las propiedades
				;*AA.ACCOUNT.DETAILS
				CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'AA.ACCOUNT.DETAILS', '', TODAY, RETURN.IDS.ACCDET, RETURN.VALUES.ACCDET, RETURN.ER)
				R.ACC.DET.NEW = RAISE(RETURN.VALUES.ACCDET)
			
				CALL F.READ(FN.ACC.DET, ARR.ID, R.ACC.DET.NEW , F.ACC.DET, ERR.ACCDET)
				;*PAYMENT.SCHEDULE PROPERTY
				CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'PAYMENT.SCHEDULE', 'SCHEDULE', TODAY, RETURN.IDS, RETURN.VALUES.PYMT, RETURN.ER)
				R.PAY.SCHE.NEW = RAISE(RETURN.VALUES.PYMT)	
				;*TERM.AMOUNT
				CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'TERM.AMOUNT', 'COMMITMENT', TODAY, RETURN.IDS, RETURN.VALUES.TERM, RETURN.ER)
				R.TERM.AM = RAISE(RETURN.VALUES.TERM)				
				;*CHANGE.PRODUCT(Plazo del deposito)
				CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'CHANGE.PRODUCT','RENEWAL', TODAY, RETURN.IDS, RETURN.VALUES.CHG.PROD, RETURN.ER)
				R.CHG.PRD.NEW = RAISE(RETURN.VALUES.CHG.PROD)				
				;*INTEREST
				CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'INTEREST', 'DEPOSITINT', TODAY, RETURN.IDS, RETURN.VALUES.INT, RETURN.ER)
				R.DEPO.INT = RAISE(RETURN.VALUES.INT)							
				;*SETTLEMENT
				CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'SETTLEMENT','SETTLEMENT', TODAY, RETURN.IDS, RETURN.VALUES.SETT, RETURN.ER)
				R.SETTL.NEW = RAISE(RETURN.VALUES.SETT)	
							
				;*EB.LOOKUP (para Status del Deposito)
				STR.STATUS.ID = 'L.DEP.STATUS*': R.ACC.ARR.NEW<AC.LOCAL.REF><1,LOCPOSDepStatus> 
				CALL F.READ(FN.EBLOOKUP, STR.STATUS.ID , R.STATUS.LOOK, F.EBLOOKUP, F.ERR.LOOK)						
				;*Interes Accruals del Arrangement
				CALL F.READ(FN.INTEREST.ACCRU, ARR.ID:"-" : PROP.DEPINT, R.INT.ACCRUALS, F.INTEREST.ACCRU, ACCRUALS.ERR)
													
				;*Obteniendo la data	
				;*Monto del Deposito 
				DapAmount 				= R.TERM.AM<AA.AMT.AMOUNT>
				;*Fecha de vencimiento		
				DapDueDate 				= R.ACC.DET.NEW<AA.AD.RENEWAL.DATE>
				;*Plazo del deposito
				DapTermDeposit		 	= R.CHG.PRD.NEW<AA.CP.CHANGE.PERIOD>
				;*Interes
				DapInterestRate 		= R.DEPO.INT<AA.INT.EFFECTIVE.RATE>
				;*Periodo de capitalizacion
				DapCapitalizationPeriod	= R.PAY.SCHE.NEW<AA.PS.PAYMENT.FREQ>			
				;*Fecha de apertura
				DapOpeningDate 			= R.ACC.DET.NEW<AA.AD.CONTRACT.DATE>
				;*Forma de pago
				CantPayMethod			= DCOUNT(R.ACC.DET.NEW<AA.AD.PAY.METHOD>, VM)
				DapPaymentMethod 		= R.ACC.DET.NEW<AA.AD.PAY.METHOD><1,CantPayMethod>				
				;*Cuenta a Abonar Intereses
				DapPaymentAccount 		= R.SETTL.NEW<AA.SET.PAYOUT.ACCOUNT>
				;*Fecha de capitalizacion
				DapCapitalizationDate 	= R.ACC.DET.NEW<AA.AD.PAYMENT.START.DATE>
				;*Interes diario			
				DapDailyInt 			= ""	
				;*Categoria del deposito a plazo
				DapCategory 			= R.ACC.ARR.NEW<AA.AC.CATEGORY>				
				;*Numero de formulario del deposito				
				DapFormNo 				= R.ARR.ACCOUNT<AA.AC.LOCAL.REF><1,LOCPOSNumForm>				
				;*Tasa negociada
				DapEffectiveRate 		= R.DEPO.INT<AA.INT.EFFECTIVE.RATE>
				;*Margen de negociacion
				DapMarginRate 			= R.DEPO.INT<AA.INT.MARGIN.RATE>				
				;*Rango del deposito
				DapPeriodicIndex 		= R.DEPO.INT<AA.INT.PERIODIC.INDEX>
				;*Estado del deposito
				DapStatus				= R.STATUS.LOOK<EB.LU.DESCRIPTION><1,1>
				;*Fecha de cancelacion
				DapCancelDate 			= R.ACC.DET.NEW<AA.AD.REPORT.END.DATE>
				;*Monto Proyectado de Depositos Según Declaracion Jurada			
				DapProjectAmountDJ 		= R.ARR.ACCOUNT<AA.AC.LOCAL.REF><1,LOCPOSMonPro>
				;*Procedencia de fondos		    	
				DapSourceFundsDJ 		= R.ARR.ACCOUNT<AA.AC.LOCAL.REF><1,LOCPOSPreFon>
				;*Fecha Ultima Renovacion 
				PosLastRenew			= DCOUNT(R.ACC.DET.NEW<AA.AD.LAST.RENEW.DATE>, VM)
				DapLastRewnewDate		= R.ACC.DET.NEW<AA.AD.LAST.RENEW.DATE><1,PosLastRenew>				 	
				;*Fecha Base del DAP (Contiene la Fecha apertura en primera vez, segunda contiene la fecha de renovacion)
				DapBaseDate			 	= R.ACC.DET.NEW<AA.AD.BASE.DATE>
				
				;*Saldo Real del Arrangement
				DapActualBalance		= R.ACC.ARR.NEW<AC.ONLINE.ACTUAL.BAL>														
				;*Reserva
				DapReserva 				= R.ACC.ARR.NEW<AC.ONLINE.ACTUAL.BAL> - R.ACC.ARR.NEW<AC.ONLINE.CLEARED.BAL> 					
				;*Tipo Interes
				DapTipoInteres			= R.DEPO.INT<AA.INT.RATE.TIER.TYPE>										
								
				GOSUB INTERES.ACUMULADO 
				;*Interes Acumulado
				DapIntAcum 					= Y.INTERES.ACUMULADO																	

				;*Fecha Vencimiento				
				DapFechaVencimiento 		= R.ACC.DET.NEW<AA.AD.MATURITY.DATE>

				IF DapFechaVencimiento EQ '' THEN
					DapFechaVencimiento 	= R.ACC.DET.NEW<AA.AD.REPORT.END.DATE>
				END

				Y.FVENCIM  = DapFechaVencimiento
				TYPE.RETURN = 'C'
				Y.REG=''   
				Y.DIA.ANT = '-'

				IF Y.FVENCIM NE '' THEN
					CALL CDD(Y.REG, TODAY, Y.FVENCIM, TYPE.RETURN)           

					DapDiasAntVencimiento = TYPE.RETURN 		;*Y.FECHA.VENCIMIENTO - OCONV(ICONV(TODAY,"DMDY"),"D/E[2,A3,2]")
				END

				Y.NUM.OTH.TITULAR 			= DCOUNT(RETURN.VALUES.CUST<1, AA.CUS.OTHER.PARTY>, SM)
				DapNumTitulares 			= Y.NUM.OTH.TITULAR + 1	

				;*Obtener el monto maximo garantizado
    			CALL F.READ(FN.DEPGAR, "SYSTEM", R.DEPGAR, F.DEPGAR, AA.DEPGAR)
   				Y.MONTO.MAX.GAR 			= R.DEPGAR<2>

				IF DapActualBalance GT Y.MONTO.MAX.GAR THEN
					DapMontoGar				= Y.MONTO.MAX.GAR
					DapMontoNoGar			= DapActualBalance - Y.MONTO.MAX.GAR
				END ELSE
					DapMontoGar				= DapActualBalance
					DapMontoNoGar			= 0.00
				END	

				;*INTERES.PAGADO
				;*Verificar si la cuenta tiene intereses pagados
				LOCATE ARR.ID IN S.ARR SETTING N.POS ELSE N.POS=''

				IF N.POS THEN
					Y.INTERES.PAGADO = FIELD(STR.INTPAG<N.POS>,'*',2)
					Y.ISR.AMOUNT     = FIELD(STR.INTPAG<N.POS>,'*',3)
				END
				
				;*Interes Devengado			
				DapAccruedInt 				= Y.INTERES.PAGADO
				
				;* Interes Pagado
				DapIntPagadoMes				= Y.INTERES.PAGADO - Y.ISR.AMOUNT 	;* Interes devengado - ISR
				
				;* Monto ISR
				DapISRPagadoMes				= Y.ISR.AMOUNT
				
				CALL GET.LOC.REF('ACCOUNT','L.DEP.STATUS',LOCPOSEstado)			
				DapEstado = R.ACC.ARR.NEW<AC.LOCAL.REF><1,LOCPOSEstado>

			END
			;**********************************************************************-- FIN DEPÓSITOS--**************************************************************************		

			;***********************************************************************--ACCOUNTS--*********************************************************************************
			;*Leyendo  accounts
			IF(R.ARRANGEMENT_INI<AA.ARR.PRODUCT.LINE> EQ Y.ACCOUNTS) THEN		
				;*Obteniendo data de las propiedades
				;*AA.ACCOUNT.DETAILS
				CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'AA.ACCOUNT.DETAILS', '', TODAY, RETURN.IDS.ACCDET, RETURN.VALUES.ACCDET, RETURN.ER)
				R.ACC.DET.NEW = RAISE(RETURN.VALUES.ACCDET)
			
				CALL F.READ(FN.ACC.DET, ARR.ID, R.ACC.DET.NEW , F.ACC.DET, ERR.ACCDET)	
				;*CINTEREST->AA.ARR.INTEREST
				CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID, 'INTEREST', 'CRINTEREST' , TODAY, RETURN.IDS, RETURN.VALUES.INT, RETURN.ER)
				R.INTEREST = RAISE(RETURN.VALUES.INT)
								
													
				CALL F.READ(FN.INTEREST.ACCRU, ARR.ID:"-":PROP.ACTINT, R.INT.ACCRUALS, F.INTEREST.ACCRU, ACCRUALS.ERR)
					
				;*Obteniendo posiciones de campos locales
				CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.ESTADO.CTA',	 LOCPOSEstCta)
				CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.MONTO.APER',   LOCPOSAmtOpen)				
				CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.AML.DEP.PROY', LOCPOSDepProy)
				CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.AML.RET.PROY', LOCPOSRetProy)
				CALL GET.LOC.REF('AA.ARR.ACCOUNT','LF.AML.PROC.FON', LOCPOSPreFon)
					
				;*EB.LOOKUP (para Status de la Cuenta)
				STR.STATUS.ACC.ID = 'SLV.AC.ESTADO.CUENTA*': R.ARR.ACCOUNT<AA.AC.LOCAL.REF><1,LOCPOSEstCta> 
				CALL F.READ(FN.EBLOOKUP, STR.STATUS.ACC.ID , R.STATUS.LOOK, F.EBLOOKUP, F.ERR.LOOK)	
				;*Obteniendo data	
				;*Saldo disponible
				AccWorkBalance 		= R.ACC.ARR.NEW<AC.WORKING.BALANCE>				
				;*Saldo Bloqueado
				AccBlockBalance 	= FIELD(R.ACC.ARR.NEW<AC.LOCKED.AMOUNT>, @VM, 1)			
				;*Fecha de apertura	
				AccOpeningDate		= R.ACC.ARR.NEW<AC.OPENING.DATE>												
				;*Tasa de Interes de la Cuenta (La tasa menor de la banda que no sea cero, si es cero se coloca la siguiente. Si no tiene banda es cero)
				NO.REC.INT			= DCOUNT(R.INTEREST<AA.INT.FIXED.RATE>, @VM)
				Y.INTEREST.RATE 	= "" ;*Inicializando Variable
				IF NO.REC.INT EQ 0 THEN ;*sino tiene registro es porque no tiene banda
					Y.INTEREST.RATE = 0.0 ;*Se coloca tasa 0 para cuentas Corrientes (no poseen banda)			
				END
				ELSE ;*Posee banda 
					;*Se elige el valor menor de toda la banda 
					;*Si la posicion 1 de la banda es 0 se elige la 2 sino deja la 1
					IF R.INTEREST<AA.INT.FIXED.RATE, 1> EQ 0.0 THEN
						Y.INTEREST.RATE = R.INTEREST<AA.INT.FIXED.RATE, 2> 
					END
					ELSE
						Y.INTEREST.RATE = R.INTEREST<AA.INT.FIXED.RATE, 1>
					END 
				END
				AccInterestRate		= Y.INTEREST.RATE					 
				;*Saldo restringuido
				AccRestrictBalance  = FIELD(R.ACC.ARR.NEW<AC.LOCKED.AMOUNT>,@VM, 1)
				;*Estado de la cuenta				
				AccStatus 			= R.STATUS.LOOK<EB.LU.DESCRIPTION><1,1>
				;*Fecha de cancelacion
				AccCancelDate 		= R.ACC.ARR.NEW<AC.CLOSURE.DATE>				 
				;*Fecha de Inactivacion
				AccInactiveDate 	= ''
				;*Fecha de Reactivacion
				AccRenewalDate 		= R.ACC.DET.NEW<AA.AD.RENEWAL.DATE>
				;*Numero de dias que estaran restringuidos los fondos
				AccRestrictPeriod 	= ''
				;*Depositos proyectados
				AccProjectDepDJ 	= R.ARR.ACCOUNT<AA.AC.LOCAL.REF><1,LOCPOSDepProy>
				;*Retiros proyectados 
				AccProjectWithdrwDJ = R.ARR.ACCOUNT<AA.AC.LOCAL.REF><1,LOCPOSRetProy>		
			    ;*Precedencia de fondos
				AccSourceFundsDJ 	= R.ARR.ACCOUNT<AA.AC.LOCAL.REF><1,LOCPOSPreFon>				
				;*Monto de Apertura
				AccOpenAmount		= R.ARR.ACCOUNT<AA.AC.LOCAL.REF><1,LOCPOSAmtOpen>				
				;*Motivo de Cancelacion de Cta
				GOSUB GET.ACC.REASON.CANCEL
				AccReasonCancel            = Y.REASON.CANCEL				
				;*Saldo Real del Arrangement
				AccActualBalance			= R.ACC.ARR.NEW<AC.ONLINE.ACTUAL.BAL>														
				;*Reserva
				AccReserva 					= R.ACC.ARR.NEW<AC.ONLINE.ACTUAL.BAL> - R.ACC.ARR.NEW<AC.ONLINE.CLEARED.BAL> 					
				;*Tipo Interes
				AccTipoInteres				= R.INTEREST<AA.INT.RATE.TIER.TYPE>											
				GOSUB INTERES.ACUMULADO
				;*Interes Acumulado
				AccIntAcum 					= Y.INTERES.ACUMULADO
				
				GOSUB GETAVERAGEBALANCE
				;*Saldo Promedio 
				AccSaldoPromedio			= saldoPromedioxCuenta																									
				
				Y.NUM.OTH.TITULAR 			= DCOUNT(RETURN.VALUES.CUST<1, AA.CUS.OTHER.PARTY>, SM)
				AccNumTitulares 			= Y.NUM.OTH.TITULAR + 1	
				
				;*Obtener el monto maximo garantizado
    			CALL F.READ(FN.DEPGAR, "SYSTEM", R.DEPGAR, F.DEPGAR, AA.DEPGAR)
   				Y.MONTO.MAX.GAR 			= R.DEPGAR<2>
					
				IF AccActualBalance GT Y.MONTO.MAX.GAR THEN
					AccMontoGar				= Y.MONTO.MAX.GAR
					AccMontoNoGar			= AccActualBalance - Y.MONTO.MAX.GAR
				END ELSE
					AccMontoGar				= AccActualBalance
					AccMontoNoGar			= 0.00
				END	
				
				;*INTERES.PAGADO
				;*Verificar si la cuenta tiene intereses pagados
				LOCATE ARR.ID IN S.ARR SETTING N.POS ELSE N.POS=''
				
				IF N.POS THEN
					Y.INTERES.PAGADO = FIELD(STR.INTPAG<N.POS>,'*',2)
					Y.ISR.AMOUNT   	 = FIELD(STR.INTPAG<N.POS>,'*',3)
				END
				
				;*Interes Devengado			
				AccAccruedInt 				= Y.INTERES.PAGADO	

				;* Interes Pagado			
				AccIntPagadoMes				= Y.INTERES.PAGADO - Y.ISR.AMOUNT   ;* Interes Devengado - ISR
				
				;* Monto ISR
				AccISRPagadoMes				= Y.ISR.AMOUNT		
				
				CALL GET.LOC.REF('ACCOUNT','LF.ESTADO.CTA',LOCPOSEstado)
				AccEstado = R.ACC.ARR.NEW<AC.LOCAL.REF><1,LOCPOSEstado>																				
																	
				

			END
					
			;**********************************************************************--FIN ACCOUNTS--*******************************************************************************
			
			;***********************************************************************Generando Archivo .CSV************************************************************************
			;*Contrucccion de arreglos para generar archivo
			STR.ARR  = Arrangement			   : ";"	;*01
		    STR.ARR := ArrNumReference	       : ";"	;*02
		    STR.ARR := CustomerId		 	   : ";"	;*03
		    STR.ARR := ArrStatus               : ";"	;*04
		    STR.ARR := ArrUserInput		       : ";"	;*05
		    STR.ARR := ArrStartDate            : ";"	;*06
		    STR.ARR := ArrBranchId             : ";"	;*07
		    STR.ARR := Product                 : ";"	;*08
		    STR.ARR := ProductGroup		       : ";"	;*09
		    STR.ARR := ProductLine             : ";"	;*10
		    STR.ARR := ProEfectDate            : ";"	;*11
		    STR.ARR := ProductStatus           : ";"	;*12
		    STR.ARR := CrdAmount               : ";"	;*13
		    STR.ARR := CrdInterestRate         : ";"	;*14
		    STR.ARR := CrdEffectiveRate        : ";"	;*15
		    STR.ARR := CrdReferenceRate		   : ";"	;*16	
		    STR.ARR := CrdDisburstDate         : ";"	;*17
		    STR.ARR := CrdDueDate			   : ";"	;*18
		    STR.ARR := CrdTermLending          : ";"	;*19
		    STR.ARR := CrdBalance              : ";"	;*20
		    STR.ARR := CrdPayAmount            : ";"	;*21
		    STR.ARR := CrdPaymentMethod        : ";"	;*22
		    STR.ARR := CrdPaymentDate          : ";"	;*23
		    STR.ARR := CrdType                 : ";"	;*24
		    STR.ARR := CrdStatus               : ";"	;*25
		    STR.ARR := CrdJudicial             : ";"	;*26
		    STR.ARR := CrdAmountMoraCap        : ";"	;*27
		    STR.ARR := CrdAmountMoraInt        : ";"	;*28
		    STR.ARR := CrdCapMorDays           : ";"	;*29
		    STR.ARR := CrdIntMorDays           : ";"	;*30
		    STR.ARR := CrdCurrInteres          : ";"	;*31
		    STR.ARR := CrdOverInteres          : ";"	;*32
		    STR.ARR := CrdCurrTax              : ";"	;*33
		    STR.ARR := CrdOverTax			   : ";"	;*34
		    STR.ARR := CrdCurrCap              : ";"	;*35
		    STR.ARR := CrdOverCap              : ";"	;*36
		    STR.ARR := CrdCancelDate           : ";"	;*37
		    STR.ARR := CrdDestinySSF           : ";"	;*38
		    STR.ARR := CrdCreditCondition      : ";"	;*39
		    STR.ARR := CrdPaymentAmountDJ      : ";"	;*40
		    STR.ARR := CrdAdvancedAmountDJ     : ";"	;*41
		    STR.ARR := CrdSourceFundsDJ        : ";"	;*42
		    STR.ARR := CrdSellingOfficer       : ";"	;*43
		    STR.ARR := CrdExpenseId            : ";"	;*44
		    STR.ARR := CrdExpenseType          : ";"	;*45
		    STR.ARR := CrdExpenseAmount        : ";"	;*46
		    STR.ARR := CrdNameAccount          : ";"	;*47
		    STR.ARR := CrdInsuranceFee         : ";"	;*48
		    STR.ARR := PaymentFreq             : ";"	;*49
		    STR.ARR := PrimaryOfficer          : ";"	;*50
		    STR.ARR := CrdDepGuarantee         : ";"	;*51
		    STR.ARR := CrdAssetGuarantee       : ";"	;*52
		    STR.ARR := CrdPayOutAccount        : ";"	;*53
		    STR.ARR := ArrInputter             : ";"	;*54
		    STR.ARR := ArrDateTime             : ";"	;*55
		    STR.ARR := ArrAuthorizer           : ";"	;*56
		    STR.ARR := ArrCompany              : ";"	;*57
		    STR.ARR := ArrCurrNum              : ";"	;*58
		    STR.ARR := ArrOverride             : ";"	;*59
		    STR.ARR := DapAmount               : ";"	;*60
		    STR.ARR := DapDueDate              : ";"	;*61
		    STR.ARR := DapTermDeposit          : ";"	;*62
		    STR.ARR := DapInterestRate         : ";"	;*63
		    STR.ARR := DapCapitalizationPeriod : ";"	;*64
		    STR.ARR := DapOpeningDate     	   : ";"	;*65
		    STR.ARR := DapPaymentMethod		   : ";"	;*66
		    STR.ARR := DapPaymentAccount       : ";"	;*67
		    STR.ARR := DapCapitalizationDate   : ";"	;*68
		    STR.ARR := DapAccruedInt           : ";"	;*69
		    STR.ARR := DapDailyInt             : ";"	;*70
		    STR.ARR := DapCategory             : ";"	;*71
		    STR.ARR := DapFormNo               : ";"	;*72
		    STR.ARR := DapEffectiveRate        : ";"	;*73
		    STR.ARR := DapMarginRate           : ";"	;*74
		    STR.ARR := DapPeriodicIndex        : ";"	;*75
		    STR.ARR := DapStatus               : ";"	;*76  
		    STR.ARR := DapCancelDate           : ";"	;*77
		    STR.ARR := DapProjectAmountDJ      : ";"	;*78
		    STR.ARR := DapSourceFundsDJ        : ";"	;*79 
		    STR.ARR := AccWorkBalance          : ";"	;*80
		    STR.ARR := AccBlockBalance         : ";"	;*81
		    STR.ARR := AccOpeningDate          : ";"	;*82 
		    STR.ARR := AccInterestRate         : ";"	;*83
		    STR.ARR := AccRestrictBalance      : ";"	;*84
		    STR.ARR := AccStatus               : ";"	;*85
		    STR.ARR := AccCancelDate           : ";"	;*86
		    STR.ARR := AccInactiveDate         : ";"	;*87
		    STR.ARR := AccRenewalDate          : ";"	;*88
		    STR.ARR := AccRestrictPeriod       : ";"	;*89
		    STR.ARR := AccProjectDepDJ         : ";"	;*90
		    STR.ARR := AccProjectWithdrwDJ     : ";"	;*91
		    STR.ARR := AccSourceFundsDJ		   : ";"	;*92 
		    STR.ARR := DapLastRewnewDate	   : ";"	;*93
		    STR.ARR := DapBaseDate			   : ";"	;*94
		    STR.ARR := AccOpenAmount		   : ";"	;*95
		    STR.ARR := AccTitle		   		   : ";"	;*96
			STR.ARR := AccShortTitle		   : ";"	;*97
			STR.ARR := ArrWorkingBalance       : ";"	;*98	
			STR.ARR := CrdNoFolio		       : ";"   	;*99
			STR.ARR := CrdOmisos		       : ";"   	;*100
			STR.ARR := CrdSaldoBillsPenalty    : ";"    ;*101
			STR.ARR := CrdDiasMoraKI           : ";"    ;*102
			STR.ARR := CrdFechaAsignacion      : ";"    ;*103
			STR.ARR := CrdFechaApertura        : ";"    ;*104 
			STR.ARR := CrdSaldoCapital         : ";"    ;*105
			STR.ARR := CrdIntMoratorio         : ";"    ;*106
			STR.ARR := CrdCicloPK              : ";"    ;*107
			STR.ARR := CrdCicloPI              : ";"    ;*108
			STR.ARR := CrdDispRevolvente       : ";"    ;*109
			STR.ARR := CrdFechaSigPago         : ";"    ;*110
			STR.ARR := CrdTtlSeguPendiente     : ";"    ;*111
			STR.ARR := CrdMontUltPago          : ";"    ;*112
			STR.ARR := CrdSaldoTtl             : ";"    ;*113
			STR.ARR := CrdDelegacion           : ";"    ;*114
			STR.ARR := CrdRelacion             : ";"    ;*115
			STR.ARR := CrdFechaProceso         : ";"    ;*116
			STR.ARR := CrdOtherParty           : ";"    ;*117
			STR.ARR := CrdIntVencidos          : ";"    ;*118
			STR.ARR := CrdIntCorrientes        : ";"    ;*119
			STR.ARR := CrdEstPrestamo          : ";"    ;*120
			STR.ARR := AccReasonCancel		   : ";"	;*121
			STR.ARR := DapActualBalance		   : ";"	;*122
			STR.ARR := DapReserva			   : ";"	;*123
			STR.ARR := DapTipoInteres		   : ";"	;*124
			STR.ARR := DapIntAcum			   : ";"	;*125
			STR.ARR := DapFechaVencimiento	   : ";"	;*126
			STR.ARR := DapDiasAntVencimiento	: ";"	;*127
			STR.ARR := DapNumTitulares          : ";"   ;*128
			STR.ARR := DapMontoGar              : ";"   ;*129
			STR.ARR := DapMontoNoGar            : ";"   ;*130
			STR.ARR := DapIntPagadoMes          : ";"   ;*131
			STR.ARR := DapISRPagadoMes          : ";"   ;*132			
			STR.ARR := AccActualBalance	        : ";"	;*133 
			STR.ARR := AccReserva			    : ";"	;*134
			STR.ARR := AccTipoInteres		    : ";"	;*135
			STR.ARR := AccIntAcum			    : ";"	;*136
			STR.ARR := AccSaldoPromedio		   	: ";"	;*137
			STR.ARR := AccNumTitulares          : ";"   ;*138
			STR.ARR := AccMontoGar              : ";"   ;*139
			STR.ARR := AccMontoNoGar            : ";"   ;*140
			STR.ARR := AccIntPagadoMes          : ";"   ;*141
			STR.ARR := AccISRPagadoMes			: ";"   ;*142
			STR.ARR := AccEstado			    : ";"   ;*143
			STR.ARR := DapEstado				: ";"	;*144
			STR.ARR := CrdPrinIntAcc			: ";"	;*145
			STR.ARR := CrdAdelantoK				: ";"	;*147
			STR.ARR := CrdSaldoIntereses		: ";"	;*146			
			STR.ARR := CrdSeguroDanio			: ";"	;*148
			STR.ARR := CrdSeguroDanioIVA		: ";"	;*149
			STR.ARR := CrdPayInAccount			: ";"   ;*150
			STR.ARR := ArrCategory				: ";"	;*151
			STR.ARR := AccAccruedInt			: ";"	;*152
			STR.ARR := ArrFechaProceso					;*153
		    STR.ARR := EndLine	
		    ARRAY.DATA<LK> 		= STR.ARR
		    STR.ARR 			= ''
		    	    
		    ;*Limpiando	    	    
			GOSUB SET.VARIABLES 
		END
	
	CRT '-----------Creando Archivos--------------------'
	;****************************************--Guardando Archivos--********************************************					
	FOR LM = 1 TO J 
		;*Creando nombre para Archivo
		NAME.FILE = '_T24Arrangement.' : ARRAY.ID.ARRAG<LM>: '.csv'
		;*Eliminando archivo existente
	    DELETESEQ DIR.NAME, NAME.FILE THEN
	    END  
		;* Abriendo archivo para escritura
	    OPENSEQ DIR.NAME, NAME.FILE TO SEQ.PTR THEN
	        WEOFSEQ NAME.FILE
	    END
	    ;*Escribiendo
	  	WRITEBLK ARRAY.DATA<LM> ON SEQ.PTR THEN 
	  	;*sin retorno de carro
	  	END 
		CLOSESEQ SEQ.PTR
		NAME.FILE = ''
	NEXT LM	
	CRT 'Cantidad de arhivos creados: ' : LM
RETURN
  
SET.VARIABLES:
	;*Se inicializan/limpian las variables que contienen la informacion para evitar data erronea dentro del ciclo
	R.ARRANGEMENT_INI 		= ''
	R.ACC.ARR.NEW			= ''
	RETURN.VALUES.ACC		= ''
	R.ARR.ACCOUNT			= ''
	RETURN.VALUES.TERM 		= ''
	RETURN.VALUES			= ''
	R.ACC.DET.NEW			= ''
	RETURN.VALUES.INT		= ''
	RETURN.VALUES.PYMT		= ''
	R.PAY.SCHE.NEW			= ''
	R.TERM.AM				= ''
	RETURN.VALUES.CHG.PROD	= ''
	R.CHG.PRD.NEW			= ''
	R.DEPO.INT				= ''
	RETURN.VALUES.SETT		= ''
	R.SETTL.NEW				= ''
	R.STATUS.LOOK			= ''
	R.INT.ACCRUALS			= ''
	RETURN.VALUES.ACCDET 	= ''
	R.INTEREST				= ''
	;*Variables de informacion a generar
	Arrangement 			= ""		;*01	
	ArrNumReference 		= ""        ;*02
	CustomerId 				= ""        ;*03
	ArrStatus 				= ""        ;*04
	ArrUserInput 			= ""        ;*05
	ArrStartDate 			= ""        ;*06
	ArrBranchId 			= ""        ;*07
	Product 				= ""        ;*08
	ProductGroup 			= ""        ;*09
	ProductLine 			= ""        ;*10
	ProEfectDate 			= ""        ;*11
	ProductStatus 			= ""        ;*12
	CrdAmount 				= ""        ;*13
	CrdInterestRate 		= ""        ;*14
	CrdEffectiveRate 		= ""        ;*15
	CrdReferenceRate		= ""        ;*16	
	CrdDisburstDate 		= ""        ;*17
	CrdDueDate 				= ""        ;*18
	CrdTermLending 			= ""        ;*19
	CrdBalance 				= ""        ;*20
	CrdPayAmount 			= ""        ;*21
	CrdPaymentMethod 		= ""        ;*22
	CrdPaymentDate 			= ""        ;*23
	CrdType 				= ""        ;*24
	CrdStatus 				= ""        ;*25
	CrdJudicial 			= ""        ;*26
	CrdAmountMoraCap 		= ""        ;*27
	CrdAmountMoraInt 		= ""        ;*28
	CrdCapMorDays 			= ""        ;*29
	CrdIntMorDays 			= ""        ;*30
	CrdCurrInteres 			= ""        ;*31
	CrdOverInteres 			= ""        ;*32
	CrdCurrTax 				= ""        ;*33
	CrdOverTax 				= ""        ;*34
	CrdCurrCap 				= ""        ;*35
	CrdOverCap 				= ""        ;*36
	CrdCancelDate 			= ""        ;*37
	CrdDestinySSF 			= ""        ;*38
	CrdCreditCondition 		= ""        ;*39
	CrdPaymentAmountDJ 		= ""        ;*40
	CrdAdvancedAmountDJ 	= ""        ;*41
	CrdSourceFundsDJ 		= ""        ;*42
	CrdSellingOfficer 		= ""        ;*43
	CrdExpenseId 			= ""        ;*44
	CrdExpenseType 			= ""        ;*45
	CrdExpenseAmount 		= ""        ;*46
	CrdNameAccount 			= ""        ;*47
	CrdInsuranceFee 		= ""        ;*48
	PaymentFreq 			= ""        ;*49
	PrimaryOfficer 			= ""        ;*50
	CrdDepGuarantee 		= ""        ;*51
	CrdAssetGuarantee 		= ""        ;*52
	CrdPayOutAccount 		= ""        ;*53
	ArrInputter 			= ""        ;*54
	ArrDateTime 			= ""        ;*55
	ArrAuthorizer 			= ""        ;*56
	ArrCompany 				= ""        ;*57
	ArrCurrNum 				= ""        ;*58
	ArrOverride 			= ""        ;*59
	DapAmount 				= ""        ;*60
	DapDueDate 				= ""        ;*61
	DapTermDeposit 			= ""        ;*62
	DapInterestRate 		= ""        ;*63
	DapCapitalizationPeriod = ""        ;*64
	DapOpeningDate 			= ""        ;*65
	DapPaymentMethod 		= ""        ;*66
	DapPaymentAccount 		= ""        ;*67
	DapCapitalizationDate 	= ""        ;*68
	DapAccruedInt			= ""        ;*69
	DapDailyInt 			= ""        ;*70
	DapCategory 			= ""        ;*71
	DapFormNo 				= ""        ;*72
	DapEffectiveRate 		= ""        ;*73
	DapMarginRate 			= ""        ;*74
	DapPeriodicIndex 		= ""        ;*75
	DapStatus 				= ""        ;*76  
	DapCancelDate 			= ""        ;*77
	DapProjectAmountDJ 		= ""        ;*78
	DapSourceFundsDJ 		= ""        ;*79 
	AccWorkBalance 			= ""        ;*80
	AccBlockBalance 		= ""        ;*81
	AccOpeningDate 			= ""        ;*82 
	AccInterestRate 		= ""        ;*83
	AccRestrictBalance 		= ""        ;*84
	AccStatus 				= ""        ;*85
	AccCancelDate 			= ""        ;*86
	AccInactiveDate 		= ""        ;*87
	AccRenewalDate 			= ""        ;*88
	AccRestrictPeriod 		= ""        ;*89
	AccProjectDepDJ 		= ""        ;*90
	AccProjectWithdrwDJ 	= ""        ;*91
	AccSourceFundsDJ 		= ""        ;*92 
	DapLastRewnewDate		= ""        ;*93
	DapBaseDate				= ""        ;*94
	AccOpenAmount			= ""		;*95
	AccTitle				= ""        ;*96
	AccShortTitle			= ""        ;*97
	ArrWorkingBalance       = ""        ;*98	
	CrdNoFolio              = ""	    ;*99
	CrdOmisos               = ""        ;*100
	CrdSaldoBillsPenalty    = ""        ;*101
	CrdDiasMoraKI           = ""        ;*102
	CrdFechaAsignacion      = ""        ;*103
	CrdFechaApertura        = ""        ;*104 
	CrdSaldoCapital         = ""        ;*105
	CrdIntMoratorio         = ""        ;*106
	CrdIntCorrientes        = ""        ;*107
	CrdCicloPK              = ""        ;*108
	CrdCicloPI              = ""        ;*109
	CrdEstPrestamo          = ""        ;*110
	CrdDispRevolvente       = ""        ;*111
	CrdFechaSigPago         = ""        ;*112
	CrdTtlSeguPendiente     = ""        ;*113
	CrdMontUltPago          = ""	    ;*114
	CrdSaldoTtl             = ""        ;*115
	CrdDelegacion           = ""        ;*116
	CrdRelacion             = ""        ;*117
	CrdFechaProceso         = ""        ;*118	
	CrdOtherParty           = ""	    ;*119
	CrdIntVencidos			= ""        ;*120
	AccReasonCancel		    = ""        ;*121	
	DapActualBalance		= ""        ;*122
	DapReserva			    = ""        ;*123
	DapTipoInteres		    = ""        ;*124
	DapIntAcum			    = ""        ;*125
	DapFechaVencimiento		= ""        ;*126
	DapDiasAntVencimiento	= ""        ;*127
	DapNumTitulares         = ""        ;*128 
	DapMontoGar             = ""        ;*129
	DapMontoNoGar           = ""        ;*130
	DapIntPagadoMes         = ""		;*131
	DapISRPagadoMes         = ""        ;*132	
	AccActualBalance	    = ""        ;*133 
	AccReserva			    = ""        ;*134
	AccTipoInteres		    = ""        ;*135
	AccIntAcum			    = ""        ;*136
	AccSaldoPromedio		= ""        ;*137
	AccNumTitulares         = ""        ;*138
	AccMontoGar             = ""        ;*139
	AccMontoNoGar           = ""        ;*140
	AccIntPagadoMes         = ""        ;*141
	AccISRPagadoMes			= ""        ;*142
	AccEstado			    = ""		;*143
	DapEstado				= ""		;*144
	CrdPrinIntAcc			= ""		;*145
	CrdAdelantoK			= ""		;*146 
	CrdSaldoIntereses		= ""		;*147
	CrdSeguroDanio			= ""		;*148
	CrdSeguroDanioIVA		= ""		;*149
	CrdPayInAccount			= ""		;*150
	ArrCategory				= ""		;*151
	AccAccruedInt			= "" 		;*152
	ArrFechaProceso			= ""		;*153
RETURN


;*Metodo para Extraccion de Motivo de Cancelacion en Cuentas : OCORNEJO - 02.03.2017
GET.ACC.REASON.CANCEL:
       SELECT.STMT = "SELECT " : FN.CLOSE.DET : " WITH ARRANGEMENT.ID EQ " : ARR.ID
       CALL EB.READLIST(SELECT.STMT, Y.LIST, '', NO.REC, SYSTEM.RETURN.CODE) 
       CALL CACHE.READ(FN.CLOSE.DET, Y.LIST<NO.REC>, R.CLOSE.DET, E.CLOSE.DET)
       Y.REASON.CANCEL = SUBSTRINGS(CHANGE(R.CLOSE.DET<AA.ACC6.REASON.CANCEL>,VM,''),1,250)
       
       ;* Codificar caracteres especiales
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(465), 'NI')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(497), 'ni')
    
       ;* tratamiento de letras tildadas o semejantes
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(193), 'A')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(201), 'E')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(205), 'I')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(211), 'O')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(218), 'U')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(192), 'A')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(200), 'E')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(204), 'I')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(210), 'O')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(217), 'U')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(220), 'U')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(219), 'U')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(214), 'O')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(212), 'O')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(207), 'I')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(206), 'I')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(203), 'E')

    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(225), 'A')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(233), 'E')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(237), 'I')
    Y.REASON.CANCEL = CHANGE(Y.REASON.CANCEL, CHAR(243), 'O')
    Y.REASON.CANCEL = UPCASE(CHANGE(Y.REASON.CANCEL, CHAR(250), 'U'))
RETURN


*-----------------------------------------------------------------------------
;*Calcular total de interes acumulados de EB.CONTRACT.BALANCES
INTERES.ACUMULADO:
*-----------------------------------------------------------------------------
	Y.INTERES.ACUMULADO = 0
	Y.TOT.CRE.AMT = 0
	Y.TOT.DEB.AMT = 0
	Y.OPEN.BALANCE = 0
 
    ;*Obtener informacion del EB.CONTRACT.BALANCE
    CALL F.READ(FN.ECB, ArrNumReference, R.CON.BAL, F.ECB, Y.ERR.CB)

	Y.COUNT.CUR.TYPE = DCOUNT(R.CON.BAL<ECB.TYPE.SYSDATE>, VM)

	FOR I = 1 TO Y.COUNT.CUR.TYPE
		IF (R.CON.BAL<ECB.CURR.ASSET.TYPE><1, I> EQ PROP.ACCCRINT OR R.CON.BAL<ECB.CURR.ASSET.TYPE><1, I> EQ PROP.ACCDEPIN) THEN
			Y.OPEN.BALANCE += R.CON.BAL<ECB.OPEN.BALANCE><1, I>
			Y.TOT.CRE.AMT  += R.CON.BAL<ECB.CREDIT.MVMT><1, I>
			Y.TOT.DEB.AMT  += R.CON.BAL<ECB.DEBIT.MVMT><1, I>
		END
	NEXT I

	Y.INTERES.ACUMULADO = ABS(Y.OPEN.BALANCE) - ABS(Y.TOT.DEB.AMT) + ABS(Y.TOT.CRE.AMT) 

RETURN



*-----------------------------------------------------------------------------
;*Calcular si el prestamo tiene sobre pago de EB.CONTRACT.BALANCES
SOBREPAGO.PRESTAMO:
*-----------------------------------------------------------------------------
	Y.SALDO.TOTAL = 0
	Y.TOT.CRE.AMT = 0
	Y.TOT.DEB.AMT = 0
	Y.OPEN.BALANCE = 0
 
    ;*Obtener informacion del EB.CONTRACT.BALANCE
    CALL F.READ(FN.ECB, ArrNumReference, R.CON.BAL, F.ECB, Y.ERR.CB)

	Y.COUNT.CUR.TYPE = DCOUNT(R.CON.BAL<ECB.TYPE.SYSDATE>, VM)

	FOR I = 1 TO Y.COUNT.CUR.TYPE
		IF (R.CON.BAL<ECB.CURR.ASSET.TYPE><1, I> EQ "UNCACCOUNT") THEN
			Y.OPEN.BALANCE += R.CON.BAL<ECB.OPEN.BALANCE><1, I>
			Y.TOT.CRE.AMT  += R.CON.BAL<ECB.CREDIT.MVMT><1, I>
			Y.TOT.DEB.AMT  += R.CON.BAL<ECB.DEBIT.MVMT><1, I>
		END
	NEXT I

	Y.SALDO.TOTAL = ABS(Y.OPEN.BALANCE) - ABS(Y.TOT.DEB.AMT) + ABS(Y.TOT.CRE.AMT) 

RETURN


*-----------------------------------------------------------------------------
GETAVERAGEBALANCE:
*-----------------------------------------------------------------------------
;*PRECISION 2
;*logica para obtener saldo promedio de los ultimos 3 meses anteriores	
	anioActual=TODAY[1,4]
	mesActual =TODAY[5,2]
	periodosEvaludados = ''
	evalAnioanterior = FALSE
	FOR I = 1 TO MESES
		mesActualAux = FMT(mesActual-1,'R%2')
		CALL GET.LAST.DOM(anioActual:mesActualAux,LAST.DATE,LAST.DAY,MONTH.NAME)
		periodosEvaludados := anioActual:mesActualAux:LAST.DAY:FM
		IF(mesActualAux EQ 1)THEN;* si se llega a enero saltar al año anterior y cambiar al año anterior
			mesActual = 13
			anioActual = anioActual-1
			evalAnioanterior = TRUE;*evaluar anio anterior SI
			iteracionCambioAnio = I + 1 ;*iteracion en la que cambio el año
		END ELSE
			mesActual = mesActual-1
		END
	NEXT I
	
*-----------------------------------------------------------------------------
	;* logica para extraer los saldos promedios por cuenta
	;*restablecer variables
	anioActual=TODAY[1,4]
	mesActual =TODAY[5,2]

	;*numPeriodos = DCOUNT(periodosEvaludados,FM)
	saldoPromedioxCuenta=0
	
	customer=''
	id = CustomerId:'-':anioActual
    aplicacionData=''
    Error=''
    CALL F.READ(FN.RET.ISR.AVE,id,aplicacionData,F.RET.ISR.AVE,Error)
      
	saldoPromedio = 0  
	FOR I = 1 TO MESES
		IF(iteracionCambioAnio EQ I)THEN
			anioActual = anioActual-1
			id = CustomerId:'-':anioActual
    		aplicacionData='' 
    		Error=''
     		CALL F.READ(FN.RET.ISR.AVE,id,aplicacionData,F.RET.ISR.AVE,Error)
		END
		periodo = periodosEvaludados<I>;*anioActual:FMT(mesActual-1,"R%2"):LAST.DAY
	 	FIND periodo IN aplicacionData<SLV.AVG.ISR.PERIOD> SETTING Ap, Vp THEN
	 		;*buscar la cuenta especifica que se esta consultando
	 		poolCuentas = aplicacionData<SLV.AVG.ISR.ACCOUNT><1,Vp>
	 		FIND ArrNumReference IN poolCuentas SETTING Ap2, Vp2, Mv3 THEN
	 			montoAux = aplicacionData<SLV.AVG.ISR.AVERAGE.BALANCE><1,Vp,Mv3>
   				saldoPromedio = saldoPromedio + montoAux
	 		END
		END
		periodo =''
		mesActual = mesActual-1
	NEXT I	
	IF(saldoPromedio>0)THEN
		saldoPromedioxCuenta = FMT(saldoPromedio/MESES, 'L2')  
	END ELSE
		saldoPromedioxCuenta = 0
	END
*-----------------------------------------------------------------------------

RETURN

**---------------*
INT.PAGADO:
**---------------*
	
	GOSUB OBTDATECOBB

	;*Seleccionar ID´s de AA.BILL.DETAILS
    SELECT.BILL = "SELECT " : FN.BILL.DET : ' WITH ARRANGEMENT.ID EQ ': ARR.ID
	;* Extrayendo IDs de BILL
    CALL EB.READLIST(SELECT.BILL, LIST.BILL, '', NO.REG.BILL, Y.ERR.BILL)
    
    CRT 'Cantidad BILL DETAILS : ' : NO.REG.BILL
	
    ;*Obtener el total de interes pagado
    ;*Para mejorar rendimiento en extracion de informacion
	STR.INTPAG = ''
	S.ARR = ''
	
    FOR I=1 TO NO.REG.BILL

	    ;*Seleccionar registro de AA.BILL.DETAILS
        CALL F.READ(FN.BILL.DET, LIST.BILL<I>, R.AABD, F.BILL.DET, BILL.DET.ERR)

		IF R.AABD<AA.BD.BILL.STATUS> NE 'ISSUED' THEN ;* Solamente los pagados

	        IF R.AABD<AA.BD.ACTUAL.PAY.DATE> GE Y.FECHA.INF AND R.AABD<AA.BD.ACTUAL.PAY.DATE> LE Y.FECHA.SUP THEN
	            N.INTERES.PAGADO = 0
	            N.ISR.AMOUNT = 0

	            LOCATE PROP.BD IN R.AABD<AA.BD.PROPERTY,1> SETTING Y.POS.DEPINT ELSE Y.POS.DEPINT = ''
	        	Y.PROP = R.AABD<AA.BD.PROPERTY,1,Y.POS.DEPINT>
	        	
        	    IF Y.PROP EQ PROP.BD  THEN 

	            	LOCATE PROP.PAY IN R.AABD<AA.BD.BILL.TYPE,1> SETTING Y.POS.PAYM ELSE Y.POS.PAYM = ''
	           	 	IF Y.POS.PAYM NE '' THEN
	           	 		N.INTERES.PAGADO = R.AABD<AA.BD.OR.PR.AMT,1,Y.POS.PAYM>
	           	 	END ELSE
	           	 		N.INTERES.PAGADO = 0
	           	 	END

	            	LOCATE PROP.TAX.DEP IN R.AABD<AA.BD.PAY.PROPERTY,1,1> SETTING Y.POS.PAYTAX ELSE Y.POS.PAYTAX = ''
	     	 		IF Y.POS.PAYTAX NE '' THEN
	     	 			N.ISR.AMOUNT = R.AABD<AA.BD.OR.PR.AMT,1,Y.POS.PAYTAX>
	     	 		END ELSE
	     	 			N.ISR.AMOUNT = 0 
	     	 		END
	            
	            END
	            Y.PROP = ''
	            
	            IF N.INTERES.PAGADO EQ 0 THEN    

			    	LOCATE PROP.ACTINT IN R.AABD<AA.BD.PAY.PROPERTY,1,1> SETTING Y.POS.PAYMPRO ELSE Y.POS.PAYMPRO = ''
		     	 	IF Y.POS.PAYMPRO NE '' THEN
		     	 		N.INTERES.PAGADO = R.AABD<AA.BD.OR.PR.AMT,1,Y.POS.PAYMPRO>
		     	 	END ELSE
		     	 		N.INTERES.PAGADO = 0
		     	 	END
		     	 	
		     	 	LOCATE PROP.TAX IN R.AABD<AA.BD.PAY.PROPERTY,1,1> SETTING Y.POS.PAYTAX ELSE Y.POS.PAYTAX = ''
		     	 	IF Y.POS.PAYTAX NE '' THEN
		     	 		N.ISR.AMOUNT = R.AABD<AA.BD.OR.PR.AMT,1,Y.POS.PAYTAX>
		     	 	END ELSE
		     	 		N.ISR.AMOUNT = 0
		     	 	END
	            END
	
	         	LOCATE R.AABD<AA.BD.ARRANGEMENT.ID> IN S.ARR SETTING N.POS ELSE N.POS=''		
		
	  			N.TOT.ISR.AMOUNT  = 0.00
	  			N.TOT.INT.PAGADO  = 0.00
				IF N.POS THEN
					S.ARR.POS 	= STR.INTPAG<N.POS>
				  	N.TOT.INT.PAGADO  = DROUND(FIELD(S.ARR.POS,'*',2),2) + N.INTERES.PAGADO
				  	N.TOT.ISR.AMOUNT  = DROUND(FIELD(S.ARR.POS,'*',3),2) + N.ISR.AMOUNT
				  	
				  	STR.INTPAG<N.POS> = FIELD(S.ARR.POS,'*',1) : '*' : N.TOT.INT.PAGADO : '*' : N.TOT.ISR.AMOUNT
		
				END ELSE
					STR.INTPAG<-1> = R.AABD<AA.BD.ARRANGEMENT.ID> : '*' : N.INTERES.PAGADO : '*' : N.ISR.AMOUNT
					S.ARR<-1> 	   = R.AABD<AA.BD.ARRANGEMENT.ID>
				END
			END
		END
    NEXT I

RETURN

*--------------
OBTDATECOBB:
*--------------
CALL F.READ (FN.DATES, Y.COD.DTS, R.DTS, F.DATES, ER)

Y.FECHA.COBB = OCONV(ICONV(R.DTS<EB.DAT.TODAY>,"DMDY"),"D/E[2,A3,2]")
Y.FECHA.INF  = R.DTS<EB.DAT.LAST.WORKING.DAY>
Y.FECHA.SUP  = R.DTS<EB.DAT.PERIOD.END>


RETURN


**----------------------------------------------------------------------------------------------------
BILLDET:
**----------------------------------------------------------------------------------------------------  	
  	
  	
  	;*Seleccionar ID´s de AA.BILL.DETAILS
    SELECT.BILL = "SELECT " : FN.BILL.DET : ' WITH ARRANGEMENT.ID EQ ': ARR.ID
	;* Extrayendo IDs de BILL
    CALL EB.READLIST(SELECT.BILL, LIST.BILL, '', NO.REG.BILL, Y.ERR.BILL)
    
    CRT 'Cantidad BILL DETAILS : ' : NO.REG.BILL 	

    ;*Obtener el monto pendiente de PENALTYINT
    ;*Para mejorar rendimiento en extracion de informacion
	STR.PENALTYINT = ''
	S.ARR2 = ''
	
	STR.GRACE = ''
	S.ARR3 = ''

    LOOP
        REMOVE BILL.ID FROM LIST.BILL SETTING POS01
    WHILE BILL.ID:POS01
	
    	R.BILL.DET = ''
	    ;*Seleccionar registro de AA.BILL.DETAILS
	    CALL CACHE.READ(FN.BILL.DET, BILL.ID, R.BILL.DET, Y.ERR.BILL2)

		IF R.BILL.DET<AA.BD.BILL.STATUS> NE 'SETTLED' AND R.BILL.DET<AA.BD.BILL.STATUS> NE 'ISSUED' THEN

			N.PENALTYINT  = 0
			
        	FINDSTR "PENALTYINT" IN R.BILL.DET<AA.BD.PROPERTY> SETTING Ap, Vp THEN
            	N.PENALTYINT = R.BILL.DET<AA.BD.OS.PROP.AMOUNT><1,Vp>                    
         	END 
			
         	LOCATE R.BILL.DET<AA.BD.ARRANGEMENT.ID> IN S.ARR2 SETTING N.POS ELSE N.POS=''		
	
  			N.TOT.PENALTYINT = 0.00
			IF N.POS THEN
				S.ARR.POS 	= STR.PENALTYINT<N.POS>
			  	N.TOT.PENALTYINT = FIELD(S.ARR.POS,'*',2) + N.PENALTYINT
			  	STR.PENALTYINT<N.POS> = FIELD(S.ARR.POS,'*',1) : '*' : N.TOT.PENALTYINT
	
			END ELSE
				STR.PENALTYINT<-1> = R.BILL.DET<AA.BD.ARRANGEMENT.ID>:"*":N.PENALTYINT
				S.ARR2<-1> = R.BILL.DET<AA.BD.ARRANGEMENT.ID>
			END

			;*Manejo de cuotas en periodo de gracia, se debera sumar el interes al interes acumulado
			;*y el capital debera restarse del vencido
 
        	N_DAYS_GRC = 0
        	D_PAYMENTDATE=''

	        ;*Verificando el estado del bill
	        SRTGRC = R.BILL.DET<AA.BD.AGING.STATUS><1,1>
	                          
	        D_PAYMENTDATE =   R.BILL.DET<AA.BD.PAYMENT.DATE>
			D_SYSTODAY = D.TODAY             
		    
		    ;*CRT 'PAYMENTDATE: ' : D_PAYMENTDATE
		    ;*calculo de plazo en dias en gracia
		    NO_DATE_GRC='C'                   
		    CALL CDD(Y.REG, D_PAYMENTDATE, D_SYSTODAY, NO_DATE_GRC)           
		   
		    N_DAYS_GRC = NO_DATE_GRC

         	;*logica para superar el problema de AL0016 en los cierres (no toma creditos en GRACE o DEL bajo las siguientes condiciones)
        	IF R.BILL.DET<AA.BD.SETTLE.STATUS> EQ 'UNPAID' THEN
				N.GRACE.K = 0
				N.GRACE.I = 0
				N.INT.VENCIDO = 0
                IF (SRTGRC EQ 'GRC' AND N_DAYS_GRC GT 1) OR (SRTGRC EQ 'DEL' AND N_DAYS_GRC EQ 6)  THEN
		        	
		        	FINDSTR "ACCOUNT" IN R.BILL.DET<AA.BD.PROPERTY> SETTING Ap, Vp THEN
		        		IF N_DAYS_GRC LT 6 THEN
		            		N.GRACE.K = R.BILL.DET<AA.BD.OS.PROP.AMOUNT><Ap,Vp>
		            	END                    
		         	END 
					
		        	FINDSTR "PRINCIPALINT" IN R.BILL.DET<AA.BD.PROPERTY> SETTING Ap, Vp THEN

		        		IF N_DAYS_GRC LE 6 THEN
		        			N.GRACE.I = R.BILL.DET<AA.BD.OS.PROP.AMOUNT><Ap,Vp>
		            	END                    
		         	END 
		
					;*  20170830 MMenjivar
				    LOCATE R.BILL.DET<AA.BD.ARRANGEMENT.ID> IN S.ARR3 SETTING N.POS ELSE N.POS=''		
			
		  			N.TOT.GRACE.I = 0.00
		  			N.TOT.GRACE.K = 0.00
					IF N.POS AND (N.GRACE.I NE 0 OR N.GRACE.K NE 0) THEN
						S.ARR.POS 	= STR.GRACE<N.POS>
					  	N.TOT.GRACE.I = FIELD(S.ARR.POS,'*',3) + N.GRACE.I
					  	N.TOT.GRACE.K = FIELD(S.ARR.POS,'*',2) + N.GRACE.K
					  	STR.GRACE<N.POS> = FIELD(S.ARR.POS,'*',1) : '*' :N.TOT.GRACE.K: '*' : N.TOT.GRACE.I : '*' : FIELD(S.ARR.POS,'*',4)
			
					END ELSE
						IF N.GRACE.K NE 0 OR N.GRACE.I NE 0 THEN
							STR.GRACE<-1> = R.BILL.DET<AA.BD.ARRANGEMENT.ID>:"*":N.GRACE.K:"*":N.GRACE.I:"*":N.INT.VENCIDO
							S.ARR3<-1> = R.BILL.DET<AA.BD.ARRANGEMENT.ID>
						END
					END		
					
				END ELSE
					IF (SRTGRC EQ 'GRC' AND N_DAYS_GRC EQ 1) THEN
					  	FINDSTR "PRINCIPALINT" IN R.BILL.DET<AA.BD.PROPERTY> SETTING Ap, Vp THEN
		        			N.INT.VENCIDO = R.BILL.DET<AA.BD.OS.PROP.AMOUNT><Ap,Vp>
			         	END 
						IF N.INT.VENCIDO NE 0 THEN
							STR.GRACE<-1> = R.BILL.DET<AA.BD.ARRANGEMENT.ID>:"*":N.GRACE.K:"*":N.GRACE.I:"*":N.INT.VENCIDO
							S.ARR3<-1> = R.BILL.DET<AA.BD.ARRANGEMENT.ID>
						END
					END
			 	END
			END	
		END
	REPEAT	
	
RETURN

END