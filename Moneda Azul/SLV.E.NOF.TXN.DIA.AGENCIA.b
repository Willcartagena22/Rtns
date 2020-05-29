*----------------------------------------------------------------------------------------------------
* <Rating>14900</Rating>
*----------------------	------------------------------------------------------------------------------
* Nombre: 		SLV.E.NOF.TXN.DIA.AGENCIA
* Descripcion: 	Transacciones del día - agencia.
* EQUIRY:		SLV.E.TXN.DIA.AGENCIA
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* V3		Jonas		26.03.15	Version inicial.
* V4		Jonas		28.03.15	Dejar solamente transacciones de cajero
*						09.04.15	Definicion de grupos por transaccion y categoría.
* V5		Jonas		06.07.15	Revisión de registro de transacciones y agrupamiento.
* 1.0		Jonas		15.07.15	Agregar transacciones de cajero que no se muestran.
* 2.0		Jonas		22.07.15	Revisión general por duplicidad de registros. 
* 3.0		Jonas		10.11.15	Revisión de registros revesados TRANSTFS.
* 3.1		Jonas 		19.04.16	Agregar registros de pago masivo de planillas y prestamos.
* 4.0		Jonas 		28.01.17	No mostrar registros de TFS reversadas.
* 4.1		Jonas		31.01.17	Revision de descripciones por transacciones LIOF.
* 4.2		CSurio	    13.07.17	Mostrar TFS de Colectores Ventanilla por separado.
* 4.3		Javier		17.10.17	Se agrega Forma de Pago para Transferencias Moviles.
*-----------------------------------------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.TXN.DIA.AGENCIA(A.INFO)
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
	$INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.AA.ARRANGEMENT   
    $INSERT I_F.AA.PRODUCT.LINE
    $INSERT I_F.ACCOUNT
	$INSERT I_F.AC.CHARGE.REQUEST    
    $INSERT I_F.DATES
    $INSERT I_F.FT.COMMISSION.TYPE
	$INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $INSERT I_F.TELLER.ID
    $INSERT I_F.TELLER.PARAMETER
    $INSERT I_F.TELLER.TRANSACTION
    $INSERT I_F.TFS.TRANSACTION
    $INSERT I_F.TRANSACTION
    $INSERT I_F.USER
    $INSERT I_F.EB.SLV.TRX.LOG
    $INSERT I_F.SLV.GEN.PARAM
    $INSERT I_F.EB.SLV.KEYS.PARAMS	
	$INSERT I_F.EB.SLV.RANG.PARAM			
	$INSERT I_F.EB.SLV.COLECTOR
    $INSERT I_F.EB.SLV.ACCOUNT.PARAM    
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

    RETURN
*-----------------------------------------------------------------------------
      INIT:
*-----------------------------------------------------------------------------
    FN.ACC 	= 'F.ACCOUNT'
    F.ACC  	= ''
    FN.ACCHR 	= 'F.AC.CHARGE.REQUEST'
    F.ACCHR  	= ''
    FN.ACC.HIS 	= 'F.ACCOUNT$HIS'
    F.ACC.HIS  	= ''
    FN.ARR 		= 'F.AA.ARRANGEMENT'
    F.ARR  		= ''
    FN.DATES 	= 'F.DATES'
    F.DATES  	= ''
    FN.FT 		= 'F.FUNDS.TRANSFER'
    F.FT  		= ''
	FN.FT.COMM	= 'F.FT.COMMISSION.TYPE'
	F.FT.COMM 	= ''
	FN.FT.TXN	= 'F.FT.TXN.TYPE.CONDITION'
	F.FT.TXN	= ''
    FN.TELLER 	= 'F.TELLER'
    F.TELLER  	= ''
    FN.TELL.ID 	= 'F.TELLER.ID'
    F.TELL.ID  	= ''
    FN.TELL.TXN	= 'F.TELLER.TRANSACTION'
    F.TELL.TXN 	= ''
    FN.TFS	= 'F.TELLER.FINANCIAL.SERVICES'
    F.TFS 	= ''
    FN.TELL.PAR	= 'F.TELLER.PARAMETER'
    F.TELL.PAR	= ''
    FN.TFS.TRA	= 'F.TFS.TRANSACTION'
    F.TFS.TRA 	= ''
    FN.PRD.L 	= 'F.AA.PRODUCT.LINE'
    F.PRD.L  	= ''
    FN.TXN	= 'F.TRANSACTION'
    F.TXN 	= ''
    FN.USER 	= 'F.USER'
    F.USER  	= ''
    FN.GEN.PARAM= 'F.SLV.GEN.PARAM'
	F.GEN.PARAM = ''
	FN.KEY.PARAM= 'F.EB.SLV.KEYS.PARAMS'
	F.KEY.PARAM = ''
	FN.RNG.PARAM = 'F.EB.SLV.RANG.PARAM'
	F.RNG.PARAM  = ''
	FN.TRX.LOG  = 'F.EB.SLV.TRX.LOG'
	F.TRX.LOG = ''	
	FN.SLV.COLECTOR  = 'F.EB.SLV.COLECTOR'
    F.SLV.COLECTOR   = ''
    FN.ACCOUNT.PARAM = 'F.EB.SLV.ACCOUNT.PARAM'
    F.ACCOUNT.PARAM  = ''

	Y.GEN.PARAM	= 'SLV.TFS.VERSION.TYPE'
	Y.KEY.PARAM = 'SLV.LIOF.TRX.PARAMETERS'
	Y.TRANS.SAL = 'RETIRO NOTACARGO PAGOSTM'
	Y.ID.CTA.PRO = 'ACuentaPropia DeCuentaPropia DepChPropio AcredChCaja AcredChCert'
	Y.ID.CTA.TER = 'ACuentaTercero DeCuentaTercero AcredChAjeno'
	Y.DES.CTA.SAL = 'Transferencia de'
	Y.ID.CHQ = 'DepChPropio AcredChCaja AcredChAjeno AcredChCert'
	;*Para manejo de descripcion de TFS
	Y.TXT.RETIRO  = 'R'
	Y.TXT.DEPOSIT = 'D'
	Y.TXT.CHQ.DEP = 'Acreditacion'
	Y.TXT.CHQ.RET = 'Pago'
	Y.TXT.CCTE.RET = 'Nota de cargo'
	Y.TXT.CAHO.RET = 'Retiro'
	Y.TXT.CCTE.DEP = 'Remesa en'
	Y.TXT.CAHO.DEP = 'Deposito en'
	Y.TXT.RET.TAR  = 'Retiro tarjeta debito'
	
	;*Manejo de versiones
	Y.VER.RET.TAR = 'TELLER.FINANCIAL.SERVICES,SLV.INPUT.HM.RI'

	EQU RANGO TO 'SLV.CAT.CUENTAS'
	EQU CTA.AHO TO 'CTA.AHO'
	EQU CTA.COR TO 'CTA.COR'
	Y.DES.AHO = 'cuenta de ahorro'
	Y.DES.CTE = 'cuenta corriente'

*Definicion de grupos de transacciones por rango de categorias ACC.PARAMETERS
*-----------------------------------------------------------------------------
STR.GRP<-1> = "COD03*Cuenta DAP**" ;*Por convencion
STR.GRP<-1> = "COD04*Prestamo*3000*3999"

;*Cuenta interna de transacciones CNR
S.ACC.CNR = 'USD1760700030001'	
STR.CTAS.INT<-1> = S.ACC.CNR
;*Cuenta interna de depositos pago de prestamo y apertura DAP
S.ACC.DEP = 'USD1403000010001'
STR.CTAS.INT<-1> = S.ACC.DEP 
;*Cuenta interna de Aporte de Capital (Compra de acciones)
S.ACC.ACAP = 'USD1762600010001'
STR.CTAS.INT<-1> = S.ACC.ACAP
;*Control forma de pago o depósito
S.TRA.EFE = 'DepEfectivo'
S.TRA.CPR = 'DepChPropio'
S.TRA.CAJ = 'AcredChCaja'
S.TRA.CCE = 'AcredChCert'
S.TRA.AJE = 'AcredChAjeno'

;*Control de registros procesado TFS, TT, FT
STR.REGIS = ''
S.TODAY = TODAY

;*Definicion de categorias de pagos masivos
Y.CATEG.PLANILLA = '17923'  ;*Planilla
Y.CATEG.PRESTAMO = '17610'	;*Prestamos
Y.SUBCATEG.PREST = '0003'

*-----------------------------------------------------------------------------
    LOCATE "AGENCIA.ID" IN D.FIELDS<1> SETTING POS THEN
    	S.AGENCIA.ID = D.RANGE.AND.VALUE<POS>
    END
*-----------------------------------------------------------------------------
* DEBUG
*-----------------------------------------------------------------------------
*S.AGENCIA.ID = 'SV0010406'
*S.COMPANY 	 = 'SV0010406'
*S.TODAY 	 = '20170401'
*-----------------------------------------------------------------------------
  	;*Obtener la ultima fecha cerrada
	CALL F.READ(FN.DATES, S.AGENCIA.ID, R.DATES, F.DATES, Y.ERR.R0)
	S.LAST.PERIOD.END = R.DATES<EB.DAT.LAST.PERIOD.END>
	;*Definicion de campos locales
	CALL GET.LOC.REF('TELLER.FINANCIAL.SERVICES','LF.NOM.PER',POS.NOMP)

RETURN
*-----------------------------------------------------------------------------
OPENFILE:
*-----------------------------------------------------------------------------
    CALL OPF(FN.ACC, F.ACC)
    CALL OPF(FN.ACCHR, F.ACCHR)
    CALL OPF(FN.ACC.HIS, F.ACC.HIS)
	CALL OPF(FN.ARR, F.ARR)
    CALL OPF(FN.DATES, F.DATES)
    CALL OPF(FN.FT, F.FT)
    CALL OPF(FN.FT.COMM, F.FT.COMM)
    CALL OPF(FN.FT.TXN, F.FT.TXN)
    CALL OPF(FN.TXN, F.TXN)    
    CALL OPF(FN.PRD.L, F.PRD.L)
    CALL OPF(FN.TELLER, F.TELLER)
    CALL OPF(FN.TELL.ID, F.TELL.ID)    
    CALL OPF(FN.TELL.PAR, F.TELL.PAR)
    CALL OPF(FN.TELL.TXN, F.TELL.TXN)
    CALL OPF(FN.TFS, F.TFS)
    CALL OPF(FN.TFS.TRA, F.TFS.TRA)
    CALL OPF(FN.USER, F.USER)
    CALL OPF(FN.TRX.LOG, F.TRX.LOG)
    CALL OPF(FN.GEN.PARAM,F.GEN.PARAM)
    CALL OPF(FN.KEY.PARAM, F.KEY.PARAM)
	CALL OPF(FN.RNG.PARAM, F.RNG.PARAM)	
	CALL OPF(FN.SLV.COLECTOR, F.SLV.COLECTOR)
    CALL OPF(FN.ACCOUNT.PARAM, F.ACCOUNT.PARAM)
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
		;*Categorias de cuenta corriente
	;*-------------------------------------------
	Y.TIPO.ACC = ''
	CALL F.READ(FN.RNG.PARAM, RANGO, R.RNG.PARAM, F.RNG.PARAM, ERROR.RNG.PRM)
	
	;*Buscar Rango Cta de Ahorro
	FIND CTA.AHO IN R.RNG.PARAM<EB.SLV56.ID.RANGO> SETTING Ap, Vp THEN
		STR.GRP<-1> = "COD02*cuenta ahorro*":R.RNG.PARAM<EB.SLV56.RANGO.INF><Ap, Vp>:"*":R.RNG.PARAM<EB.SLV56.RANGO.SUP><Ap, Vp>
	END
	
	;*Buscar Rango Cta Corriente
	FIND CTA.COR IN R.RNG.PARAM<EB.SLV56.ID.RANGO> SETTING Ap, Vp THEN			
		STR.GRP<-1> = "COD01*cuenta corriente*":R.RNG.PARAM<EB.SLV56.RANGO.INF><Ap, Vp>:"*":R.RNG.PARAM<EB.SLV56.RANGO.SUP><Ap, Vp>
	END
	NO.GRP = DCOUNT(STR.GRP, FM)
	;*Obtener parametro de transaccion
	CALL F.READ(FN.KEY.PARAM, Y.KEY.PARAM, R.KEY.PARAM, F.KEY.PARAM, ERR.KEYPARAM)
	Y.PARAM.ID  = R.KEY.PARAM<EB.SLV18.PARAM.ID>
	Y.PARAM.VAL = R.KEY.PARAM<EB.SLV18.VALOR>
	
	;*NOTA: se utiliza S.TIPO.APP para orden de presentación en reporte
	S.TIPO.APP = ''
	;*Arreglos de control de registros
	A.INFO 	   = ''
	STR.ARR2   = ''
	STR.REGIS  = ''

	S.CATEG.CAJA = '10001'	;*Caja
	S.CATEG.BOV  = '10011'	;*Boveda
	S.CATEG.CHQ	 = '14025'	;*Cheque

	;*Agregar registro de saldo inicial de Cajeros y sobrante/faltante de caja
	GOSUB SALDO.INIFIN.CAJA
	
	;*Agregar registro de saldo inicial de Bóveda
	GOSUB SALDO.INIFIN.BOV
	
	;*Agregar registro de TFS colectores en Ventanilla
	GOSUB TFS.COLECTOR.VEN

	;*Agregar registro FT de pago de préstamos y depósitos a plazo
	GOSUB TRANSFT.PRE.DAP

	;*Agregar registro de TFS pagos al CNR
	GOSUB TRANSTFS.CNR
	
	;*Agregar registro de TELLER.FINANCIAL.SERVICE Remesas, Depositos y Retiros
	GOSUB TRANSTFS

	;*Agregar registro de AC.CHARGE.REQUEST Cargos - Comisiones
	GOSUB TRANSCHARGES

	;*Agregar registro de TELLER Remesas, Depositos y Retiros
	GOSUB TRANSTELL ;* Revisar exclusion de cargos segun Rpt Cajero

	;*Agregar registro de FUNDS.TRANSFER
	GOSUB TRANSFT	;*Revision Vs Rpt Cajero
*******************************************************************************
	;*Agregar detalle de cheques
*	GOSUB TRANSCHEQ

	;*Ordenar registros por cliente
	A.INFO = SORT(A.INFO)
	
RETURN
*-----------------------------------------------------------------------------
SALDO.INIFIN.CAJA:
*-----------------------------------------------------------------------------		    
	;*Obtener ID de cajeros de la agencia
	SELECT.TELL.ID  = "SELECT " : FN.TELL.ID : " WITH CO.CODE EQ '": S.AGENCIA.ID:"'"
	CALL EB.READLIST(SELECT.TELL.ID, LIST.TELL.ID, '', NO.OF.TELL.ID, ERR.TELL.ID)
	STR.M.INI.CAJA = ''
	STR.M.FIN.CAJA = ''
	MON.INI.CAJA = 0.00
	MON.FIN.CAJA = 0.00
	STR.CAJEROS = ''

	;*Control de sobrante/faltante
	S.MON.SOB = 0.00
	S.MON.FAL = 0.00	
	S.COD.GRP.  = "120000"
	
	FOR I = 1 TO NO.OF.TELL.ID
		;*Registro de Cajero
		CALL F.READ(FN.TELL.ID, LIST.TELL.ID<I>, R.TELL.ID, F.TELL.ID, ERR.TELLID)
		
	    ;*Obtener cuenta de cajero
	    S.MONEDA 	= "USD"
		S.AGENCIA 	= RIGHT(R.TELL.ID<TT.TID.CO.CODE>,4)
	    S.INT.CTA.TID = S.MONEDA:S.CATEG.CAJA:LIST.TELL.ID<I>:S.AGENCIA
		S.USER.ID 	= R.TELL.ID<TT.TID.USER>

		;*Obtener informacion de la cuenta de cajero
		CALL F.READ(FN.ACC, S.INT.CTA.TID, R.ACC.TID, F.ACC, ERR.ACC.TID)
		
		;*Obtener el saldo incial del día de cajeros
		CALL EB.GET.ACCT.BALANCE(S.INT.CTA.TID, R.ACC.TID,'BOOKING', S.LAST.PERIOD.END,'', BALANCE, CREDIT.MVMT, DEBIT.MVMT, ERR.MSG)
		MON.INI.CAJA += ABS(BALANCE)

		;*Obtener el saldo final del día de cajeros
		CALL EB.GET.ACCT.BALANCE(S.INT.CTA.TID, R.ACC.TID,'BOOKING', S.TODAY,'', BALANCE, CREDIT.MVMT, DEBIT.MVMT, ERR.MSG)
		MON.FIN.CAJA += ABS(BALANCE)
		
		;*Obtener cajeros de la agencia
		STR.CAJEROS<-1> = S.USER.ID:"*"
		
		;*Obtener monto de sobrante/faltante de caja
		GOSUB TELLER.ID.SBRFAL
	NEXT I
	
	IF NO.OF.TELL.ID GT 0 THEN
		;*Ingresar registro de saldo inicial de caja
		S.KEY1 = "01100000"
		STR.M.INI.CAJA = S.KEY1:"*":"01":"*":"00":"*":"Saldo Inicial Cajas":"*":"1":"*":MON.INI.CAJA

		A.INFO<-1> 	 = STR.M.INI.CAJA
		STR.ARR2<-1> = S.KEY1
	
		;*Ingresar registro de saldo final de caja
		S.KEY5 = "05100000"
		STR.M.FIN.CAJA = S.KEY5:"*":"05":"*":"00":"*":"Saldo Final Cajas":"*":"1":"*":MON.FIN.CAJA
		
		A.INFO<-1> 	 = STR.M.FIN.CAJA
		STR.ARR2<-1> = S.KEY5
		
		;*Agregar registro por sobrantes
		S.INT.CTA = ""
		IF S.MON.SOB GT 0 THEN
			S.TIPO.APP = "01"
			S.DES.TRA  = "Sobrante de Caja"
			S.KEY  	= S.TIPO.APP:S.COD.GRP:S.DES.TRA 
			STR.DIF = S.KEY:"*":S.TIPO.APP:"*":S.COD.GRP:"*":S.DES.TRA:"*":"1":"*":ABS(S.MON.SOB)

			A.INFO<-1> 	 = STR.DIF
			STR.ARR2<-1> = S.KEY
		END
		
		;*Agregar registro por faltantes
		IF S.MON.FAL LT 0 THEN
			S.TIPO.APP = "02"
			S.DES.TRA  = "Faltante de Caja"
			S.KEY  	= S.TIPO.APP:S.COD.GRP:S.DES.TRA 
			STR.DIF = S.KEY:"*":S.TIPO.APP:"*":S.COD.GRP:"*":S.DES.TRA:"*":"1":"*":ABS(S.MON.FAL)

			A.INFO<-1> 	 = STR.DIF
			STR.ARR2<-1> = S.KEY
		END				
	END
RETURN
*-----------------------------------------------------------------------------
SALDO.INIFIN.BOV:
*-----------------------------------------------------------------------------		    
	;*Obtener ID de bovedas de la agencia
	SELECT.TELL.PAR  = "SELECT " : FN.TELL.PAR : " WITH CO.CODE EQ '": S.AGENCIA.ID:"'"
	CALL EB.READLIST(SELECT.TELL.PAR, LIST.TELL.PAR, '', NO.OF.TELL.PAR, ERR.TELL.PAR)
	
	STR.M.INI.BOV = ''
	STR.M.FIN.BOV = ''
	MON.INI.BOV = 0.00	
	MON.FIN.BOV = 0.00
	
	FOR I = 1 TO NO.OF.TELL.PAR
		;*Registro de BOVEDA
		CALL F.READ(FN.TELL.PAR, LIST.TELL.PAR<I>, R.TELL.PAR, F.TELL.PAR, ERR.TELLPAR)
	
	    ;*Obtener cuenta de boveda
	    S.MONEDA 	= "USD"
		S.AGENCIA 	= RIGHT(S.AGENCIA.ID,4)
	    S.INT.CTA.BOV = S.MONEDA:S.CATEG.BOV:R.TELL.PAR<TT.PAR.VAULT.ID>:S.AGENCIA
		
		;*Obtener informacion de la cuenta de Boveda
		CALL F.READ(FN.ACC, S.INT.CTA.BOV, R.ACC.BOV, F.ACC, ERR.ACC.BOV)
		
		;*Obtener el saldo inicial del día de boveda
		CALL EB.GET.ACCT.BALANCE(S.INT.CTA.BOV, R.ACC.BOV,'BOOKING', S.LAST.PERIOD.END,'', BALANCE, CREDIT.MVMT, DEBIT.MVMT, ERR.MSG)
		MON.INI.BOV += ABS(BALANCE)
		
		;*Obtener el saldo final del día de boveda
		CALL EB.GET.ACCT.BALANCE(S.INT.CTA.BOV, R.ACC.BOV,'BOOKING', S.TODAY,'', BALANCE, CREDIT.MVMT, DEBIT.MVMT, ERR.MSG2)
		MON.FIN.BOV += ABS(BALANCE)

	NEXT I

	IF NO.OF.TELL.PAR GT 0 THEN
		;*Ingresar registro de saldo inicial de boveda
		S.KEY11 = "011000000"
		STR.M.INI.BOV = S.KEY11:"*":"01":"*":"000":"*":"Saldo Inicial Boveda":"*":"1":"*":MON.INI.BOV
		
		A.INFO<-1> 	 = STR.M.INI.BOV
		STR.ARR2<-1> = S.KEY11
		
		;*Ingresar registro de saldo final de boveda
		S.KEY6 = "051000000"
		STR.M.FIN.BOV = S.KEY6:"*":"05":"*":"000":"*":"Saldo Final Boveda":"*":"1":"*":MON.FIN.BOV
		
		A.INFO<-1> 	 = STR.M.FIN.BOV
		STR.ARR2<-1> = S.KEY6
	END	
RETURN
*-----------------------------------------------------------------------------
TELLER.ID.SBRFAL:
*-----------------------------------------------------------------------------		    
	;*Obtener el monto de sobrante o faltante de caja
	S.MON.DIF = R.TELL.ID<TT.TID.DIFFERENCE>
	
	;*Validar si hay sobrante o faltante
	IF ABS(S.MON.DIF) GT 0 THEN
		IF S.MON.DIF GT 0 THEN
			S.MON.SOB += ABS(S.MON.DIF) 
		END ELSE
			S.MON.FAL += ABS(S.MON.DIF)		
		END				
	END
RETURN
*-----------------------------------------------------------------------------
;*Agregar registros de pago de préstamos y apertura de DAP
TRANSFT.PRE.DAP:
*-----------------------------------------------------------------------------		    
	SELECT.FT.PD  = "SELECT ": FN.FT:" WITH PROCESSING.DATE EQ '": S.TODAY :"' AND (TRANSACTION.TYPE EQ 'ACDF' OR TRANSACTION.TYPE EQ 'ACRP')"
*	SELECT.FT.PD := " AND @ID EQ 'FT17049ZBB0M;1'"
	CALL EB.READLIST(SELECT.FT.PD, LIST.FT.PD, '', NO.OF.RECS, ERR.FTPD)
	
	STR.FT.PD = ''
	S.KEY = ''
	LOOP
        REMOVE FT.ID FROM LIST.FT.PD SETTING POS01
    WHILE FT.ID:POS01
		S.REG.VALIDO = 'S'
		Y.TOT.MONTO = 0.00
		Y.POS = ''
		Y.ORDER.CUS = ''

		;*Extraer informacion de FUNDS.TRANSFER
        CALL F.READ(FN.FT, FT.ID, R.FT.PD, F.FT, Y.ERR.FTPD)
		
		;*Evaluar el estado de los registros 
		IF R.FT.PD<FT.RECORD.STATUS> EQ 'INAU' OR R.FT.PD<FT.RECORD.STATUS> EQ 'INAO' OR R.FT.PD<FT.RECORD.STATUS> EQ 'RNAU' OR R.FT.PD<FT.RECORD.STATUS> EQ 'RNAO' OR R.FT.PD<FT.RECORD.STATUS> EQ 'REVE' THEN
			S.REG.VALIDO = 'N'
		END
		Y.ORDER.CUS = R.FT.PD<FT.ORDERING.CUST>
		;*Obtener TFS Padre de DAP
		CALL SLV.GET.TFS.PADRE.DAP(Y.ORDER.CUS)
		
		;*Excluir ademas TT o FT relacionado al TFS
    	CALL F.READ(FN.TFS, Y.ORDER.CUS, R.TFS.PD, F.TFS, Y.ERR.TFSPD)
    	Y.TFS.CO.CODE = R.TFS.PD<TFS.CO.CODE>

		IF S.REG.VALIDO EQ 'S' AND R.FT.PD<FT.DEBIT.ACCT.NO> EQ S.ACC.DEP AND Y.TFS.CO.CODE EQ S.AGENCIA.ID THEN
			STR.REGIS<-1> =  Y.ORDER.CUS:"*":FT.ID
			S.TIPO.APP  = "01"
			S.COD.GRP   = R.FT.PD<FT.TRANSACTION.TYPE>

			IF R.FT.PD<FT.TRANSACTION.TYPE> EQ 'ACRP' THEN
				S.DES.TRA = "Pago de Prestamo" 
			END ELSE
				S.DES.TRA = "Deposito a Plazo Fijo"
			END

			N.MONTO	   = R.FT.PD<FT.LOC.AMT.DEBITED>			

			S.KEY = S.TIPO.APP:S.COD.GRP:S.DES.TRA
			STR.FT.PD = S.KEY:"*":S.TIPO.APP:"*":S.COD.GRP:"*":S.DES.TRA:"*":"1":"*":N.MONTO
			Y.MON.REG = N.MONTO
			STR.DATO  = STR.FT.PD 
			GOSUB REGISTRO.TRANS
			;*Agregar detalle de forma de pago
			S.COD.GRP = ''
			Y.TFS.REF = Y.ORDER.CUS
			GOSUB DET.FORMA.PAGO
		END
	REPEAT
RETURN
*-----------------------------------------------------------------------------
;*Evaluar los TFS pagos CNR
TRANSTFS.CNR:
*-----------------------------------------------------------------------------		    
	SELECT.TFS.CNR  = "SELECT " : FN.TFS : " WITH (CO.CODE EQ '": S.AGENCIA.ID :"') AND (BOOKING.DATE EQ '":S.TODAY:"')"
	SELECT.TFS.CNR := " AND (PRIMARY.ACCOUNT EQ '":S.ACC.CNR:"')"
	
	CALL EB.READLIST(SELECT.TFS.CNR, LIST.TFS.CNR, '', NO.OF.RECS, ERR.TFSCNR)

	S.TIPO.APP = "01"
	S.COD.GRP = "130000"
	S.DES.TRA = 'Captacion de Fondos CNR'		

	FOR I = 1 TO NO.OF.RECS
		S.MONTO.CNR = 0.00
		Y.POS 	= ''
		STR.CNR	= '' 
		S.KEY	= '' 
		S.REG.VALIDO = 'S'		
		
		;*Extraer informacion de TFS
       	CALL F.READ(FN.TFS, LIST.TFS.CNR<I>, R.TFS.CNR, F.TFS, Y.ERR.TFSCNR)
       	Y.TFS.STAT = R.TFS.CNR<TFS.RECORD.STATUS>
		Y.TFS.REV  = R.TFS.CNR<TFS.REVERSAL.MARK>
		
		;*Obtener código de cajero de TELLER
		CALL F.READ(FN.TELLER, FIELD(R.TFS.CNR<TFS.UNDERLYING>, VM, 1), R.TELLER, F.TELLER, Y.ERR.TELL)

		S.COD.CAJERO = R.TELLER<TT.TE.TELLER.ID.1>

		;*Evaluar el estado de los registros equivale a R.TELLER<TT.TE.RECORD.STATUS> 
		IF Y.TFS.STAT EQ 'INAU' OR Y.TFS.STAT EQ 'INAO' OR Y.TFS.STAT EQ 'RNAU' OR Y.TFS.STAT EQ 'RNAO' OR Y.TFS.STAT EQ 'REVE' OR Y.TFS.REV EQ 'R' THEN
			S.REG.VALIDO = 'N'
		END

		;*Evaluar si el registro ya fue agregado
		FINDSTR LIST.TFS.CNR<I> IN STR.REGIS SETTING Ap1, Vp1 THEN
			Y.POS = 1
		END ELSE
			Y.POS = 0
		END

		IF S.REG.VALIDO EQ "S" AND Y.POS EQ 0 THEN
			S.NO.REG.REF = DCOUNT(R.TFS.CNR<TFS.UNDERLYING>, VM)
			
			FOR J = 1 TO S.NO.REG.REF
				IF FIELD(R.TFS.CNR<TFS.RUNNING.TOTAL>, VM, J) GT 0 THEN
					;*Monto total del TFS
					S.MONTO.CNR += FIELD(R.TFS.CNR<TFS.RUNNING.TOTAL>, VM, J)

					;*Arreglo para validar TFS que ya fueron agregados
					STR.REGIS<-1> =  LIST.TFS.CNR<I>:"*":FIELD(R.TFS.CNR<TFS.UNDERLYING>, VM, J)
				END
			NEXT J
			
			IF S.MONTO.CNR GT 0 THEN
				S.KEY = S.TIPO.APP:S.COD.GRP:S.DES.GRP
				STR.CNR = S.KEY:"*":S.TIPO.APP:"*":S.COD.GRP:"*":TRIM(S.DES.TRA):"*":1:"*":S.MONTO.CNR
				Y.MON.REG = S.MONTO.CNR
				STR.DATO  = STR.CNR 
				GOSUB REGISTRO.TRANS
				;*Agregar detalle de forma de pago
				S.COD.GRP = ''
				Y.TFS.REF = LIST.TFS.CNR<I>
				GOSUB DET.FORMA.PAGO
			END						
		END
	NEXT I
RETURN


*-----------------------------------------------------------------------------
;*Evaluar los TFS solamente para Depositos de cuentas de clientes
TRANSTFS:
*-----------------------------------------------------------------------------		    
	SELECT.TFS  = "SELECT " : FN.TFS : " WITH (CO.CODE EQ '": S.AGENCIA.ID :"') AND (BOOKING.DATE EQ '":S.TODAY:"')"
	CALL EB.READLIST(SELECT.TFS, LIST.TFS, '', NO.OF.RECS, ERR.TFS)

	LOOP
        REMOVE TFS.ID FROM LIST.TFS SETTING POS01
    WHILE TFS.ID:POS01
		Y.TOT.MONTO = 0.00
		Y.POS 		= ''
		S.COD.GRP 	= ''
		S.DES.GRP 	= ''
		S.DES.TRA 	= ''		
		S.MONTO.TFS = 0
		R.TFS		= ''
   		S.ARR.ID 	= ''
   		S.COD.CAT 	= ''
		S.INT.CTA 	= ''
   		Y.TFS.PAG.IMP = ''
		Y.IMPUESTO 	= 0

		;*Evaluar si el TFS ya fue considerado para CNR
		FINDSTR TFS.ID IN STR.REGIS SETTING Ap1, Vp1 THEN
			Y.POS = 1
		END ELSE
			Y.POS = 0
		END

		IF Y.POS EQ 0 THEN
			S.REG.VALIDO = 'S'
			;*Extraer informacion de TFS
	       	CALL F.READ(FN.TFS, TFS.ID, R.TFS, F.TFS, Y.ERR.TFS)
	       	ID.TRX.LOG = FIELD(R.TFS<TFS.INPUTTER>,"_",2):"-":TFS.ID
	       	CALL F.READ(FN.TRX.LOG, ID.TRX.LOG, R.TRX.LOG, F.TRX.LOG, Y.ERR.TRX.LOG)
	       	Y.TFS.STAT = R.TFS<TFS.RECORD.STATUS>
			Y.TFS.REV  = R.TFS<TFS.REVERSAL.MARK>
			;*Evaluar el estado de los registros equivale a R.TELLER<TT.TE.RECORD.STATUS>
			IF Y.TFS.STAT EQ 'INAU' OR Y.TFS.STAT EQ 'INAO' OR Y.TFS.STAT EQ 'RNAU' OR Y.TFS.STAT EQ 'RNAO' OR Y.TFS.STAT EQ 'REVE' OR Y.TFS.REV EQ 'R' OR R.TRX.LOG EQ '' THEN
				S.REG.VALIDO = 'N'
			END
	
			IF S.REG.VALIDO EQ 'S' THEN
				Y.TFS.PAG.IMP = TRIM(FIELD(R.TFS<TFS.LOCAL.REF><1,POS.NOMP>,'-',1))
				IF LEFT(Y.TFS.PAG.IMP,3) EQ 'TFS' THEN
		       	;*Evaluar si es transaccion de pago de impuesto
		       		GOSUB PAGO.IMPUESTO
		       		CONTINUE
				END	       	
				IF Y.IMPUESTO EQ 0 THEN			
					Y.ID.VERSION = R.TRX.LOG<EB.SLV8.VERSION>
					;*Extraer informacion de TFS.TRANSACTION
			       	CALL F.READ(FN.TFS.TRA, FIELD(R.TFS<TFS.TRANSACTION>, VM, 1), R.TFS.TRA, F.TFS.TRA, Y.ERR.TFS.TRA)
					
					;*Extraer informacion de TELLER.TRANSACTION
			       	CALL F.READ(FN.TELL.TXN, R.TFS.TRA<TFS.TXN.INTERFACE.AS>, R.TELL.TXN, F.TELL.TXN, Y.ERR.TELL.TXN)
			
					;*Determinar numero de transacciones incluidas en TFS
					S.NO.REG.REF = DCOUNT(R.TFS<TFS.UNDERLYING>, VM)
								
					;*Obtener informacion de la cuenta del Deposito
					CALL F.READ(FN.ACC, R.TFS<TFS.PRIMARY.ACCOUNT>, R.ACC, F.ACC, Y.ERR.ACC)
					S.ARR.ID  = R.ACC<AC.ARRANGEMENT.ID>
					S.COD.CAT = R.ACC<AC.CATEGORY>
					S.TIPO.CATEG = SUBSTRINGS(R.TFS<TFS.PRIMARY.ACCOUNT>,9,4)
	
					;*Evaluar si la cuenta esta en historico
					IF DCOUNT(R.ACC, "?") EQ 0 THEN
						;*Extraer informacion de TELLER$HIS
			   			CALL F.READ(FN.ACC.HIS, R.TFS<TFS.PRIMARY.ACCOUNT>:";1", R.ACC.HIS, F.ACC.HIS, ERR.ACH)
			   			S.ARR.ID = R.ACC.HIS<AC.ARRANGEMENT.ID>
			   			S.COD.CAT = R.ACC.HIS<AC.CATEGORY>
					END
	
					;*Validar si es afectación de cuenta del cajero: efectivo o cheque
*					IF (SUBSTRINGS(FIELD(R.TFS<TFS.ACCOUNT.DR>, VM, 1), 4, 5) EQ S.CATEG.CAJA OR SUBSTRINGS(FIELD(R.TFS<TFS.ACCOUNT.DR>, VM, 1), 4, 5) EQ S.CATEG.CHQ) OR (S.COD.CAT EQ Y.CATEG.PLANILLA OR (S.COD.CAT EQ Y.CATEG.PRESTAMO AND S.TIPO.CATEG EQ Y.SUBCATEG.PREST)) THEN
			
						;*Obtener informacion de AA.ARRANGEMENT
						CALL F.READ(FN.ARR, S.ARR.ID, R.ARR, F.ARR, Y.ERR.ARR)				
			
						;*Obtener informacion de PRODUCT.LINE
						CALL F.READ(FN.PRD.L, R.ARR<AA.ARR.PRODUCT.LINE>, R.PRD.L, F.PRD.L, Y.ERR.PRDL)
						S.TIPO.CTA = LEFT(R.TFS<TFS.PRIMARY.ACCOUNT>,3)
						S.PRIM.ACC = R.TFS<TFS.PRIMARY.ACCOUNT>
						IF S.TIPO.CTA NE "USD" AND TRIM(S.TIPO.CTA) NE "" OR (S.COD.CAT EQ Y.CATEG.PLANILLA OR (S.COD.CAT EQ Y.CATEG.PRESTAMO AND S.TIPO.CATEG EQ Y.SUBCATEG.PREST))  OR S.PRIM.ACC EQ S.ACC.DEP OR S.PRIM.ACC EQ S.ACC.ACAP THEN
							S.DES.TRA = TFS.ID
							;*Obtener descripcion de transaccion principal
							CALL SLV.UTIL.TFS.GET.NOM.COMP(S.DES.TRA)
							S.DES.TRA = CHANGE(CHANGE(S.DES.TRA,'[',''),']','')
							S.DES.TRA = LEFT(S.DES.TRA,1):DOWNCASE(RIGHT(S.DES.TRA,LEN(S.DES.TRA)-1))
							;*Obtener parametro de versiones
							CALL F.READ(FN.GEN.PARAM, Y.GEN.PARAM, R.GEN.PARAM, F.GEN.PARAM, Y.ERR.GEN.PARAM)
							Y.VERSIONS   = R.GEN.PARAM<SLV.GEN.TX.DATA.PARAM>
							Y.TYPES      = R.GEN.PARAM<SLV.GEN.TX.TYPE.PARAM>
							Y.TIPO.TRANS = ''
							FINDSTR Y.ID.VERSION IN Y.VERSIONS SETTING Ap1, Vp1 THEN
*								Y.TIPO.TRANS = R.GEN.PARAM<SLV.GEN.TX.TYPE.PARAM><Vp1,1>
								Y.TIPO.TRANS = UPCASE(Y.TYPES<Ap1,Vp1>)
							END
							;*Determinar si es ingreso o egreso de caja
							FINDSTR Y.TIPO.TRANS IN Y.TRANS.SAL SETTING Ap1, Vp1 THEN
								S.TIPO.APP = "02"
							END ELSE
								S.TIPO.APP = "01"
							END	
							
							FOR J = 1 TO S.NO.REG.REF
								Y.MON.CTA.PRO = 0
								Y.MON.CTA.TER = 0
								S.COD.GRP = ''
								Y.MONTO.TOTAL = FIELD(R.TFS<TFS.AMOUNT.CR>, VM, J)
								Y.MONTO.CR = FIELD(R.TFS<TFS.AMOUNT.CR>, VM, J)						
								Y.MONTO.DR = FIELD(R.TFS<TFS.AMOUNT.DR>, VM, J)
								IF Y.MONTO.TOTAL GT 0 THEN
									;*Monto total del TFS
									S.MONTO.TFS += Y.MONTO.TOTAL
									
									;*Arreglo para validar TFS que ya fueron agregados
									STR.REGIS<-1> =  TFS.ID:"*":FIELD(R.TFS<TFS.UNDERLYING>, VM, J)
									Y.ID.TRANS = FIELD(R.TFS<TFS.TRANSACTION>, VM, J)
									S.CTA.OTR = FIELD(R.TFS<TFS.ACCOUNT.DR>,VM,J)
									IF S.PRIM.ACC EQ S.CTA.OTR THEN
										S.CTA.OTR = FIELD(R.TFS<TFS.ACCOUNT.CR>,VM,J)
									END
									
									CALL F.READ(FN.ACC, S.CTA.OTR, R.ACC.OTR, F.ACC, Y.ERR.ACC.OTR)
									S.COD.CAT.OTR = R.ACC.OTR<AC.CATEGORY>
									
									;*Para Acumular estas Transacciones en el Mismo Registro Sin Distincion de Tipo de Cuenta : OCORNEJO 17.10.2017
									IF R.TFS<TFS.TRANSACTION><1, J> EQ 'PagoSTM' THEN
										S.COD.GRP = 'COD01'
										S.TIPO.APP = '02'	;*Salida de Caja
									END
									ELSE
										FOR K = 1 TO NO.GRP
											IF S.COD.CAT.OTR GE FIELD(STR.GRP<K>,"*", 3) AND S.COD.CAT.OTR LE FIELD(STR.GRP<K>,"*", 4) THEN
												S.COD.GRP = FIELD(STR.GRP<K>,"*", 1)
											END
										NEXT K
									END
		
									IF S.TIPO.APP EQ '01' AND S.INT.CTA NE FIELD(R.TFS<TFS.ACCOUNT.DR>,VM,J) THEN
										FINDSTR Y.ID.TRANS IN Y.ID.CTA.PRO SETTING Ap1, Vp1 THEN
											GOSUB FORMA.TFS
											Y.MON.CTA.PRO += Y.MONTO.DR
											Y.DES.CTA.PRO = Y.DES.TRANS
											FINDSTR Y.ID.TRANS IN Y.ID.CHQ SETTING Vp1, Ap1 THEN 
												GOSUB FORMA.TFS
												S.COD.GRP = 'COD01'
												GOSUB TIPO.CHEQUE
												Y.DES.CTA.PRO = Y.DES.CTA.CHQ:" ":S.DESC.CHQ
											END
										END
										FINDSTR Y.ID.TRANS IN Y.ID.CTA.TER SETTING Ap1, Vp1 THEN
											GOSUB FORMA.TFS
											Y.MON.CTA.TER += Y.MONTO.CR
											Y.DES.CTA.TER = Y.DES.TRANS
											FINDSTR Y.ID.TRANS IN Y.ID.CHQ SETTING Vp1, Ap1 THEN 
												GOSUB FORMA.TFS
												S.COD.GRP = 'COD01'
												GOSUB TIPO.CHEQUE
												Y.DES.CTA.TER = Y.DES.CTA.CHQ:" ":S.DESC.CHQ
											END
										END
										S.TIPO.APP.CTA = '02'
									END		
									IF S.TIPO.APP EQ '02' AND S.INT.CTA NE FIELD(R.TFS<TFS.ACCOUNT.CR>,VM,J) THEN
										GOSUB FORMA.TFS
			
										FINDSTR Y.ID.TRANS IN Y.ID.CTA.PRO SETTING Ap1, Vp1 THEN
											Y.MON.CTA.PRO += Y.MONTO.TOTAL
											Y.DES.CTA.PRO = Y.DES.TRANS
										END
		
										FINDSTR Y.ID.TRANS IN Y.ID.CTA.TER SETTING Ap1, Vp1 THEN
											Y.MON.CTA.TER += Y.MONTO.TOTAL
											Y.DES.CTA.TER = Y.DES.TRANS
										END
										S.TIPO.APP.CTA = '01'
									END
									IF Y.MON.CTA.PRO GT 0 THEN
										S.KEY = S.TIPO.APP.CTA:S.COD.GRP:Y.DES.CTA.PRO:S.DES.GRP
										STR.TELL = S.KEY:"*":S.TIPO.APP.CTA:"*":S.COD.GRP:"*":Y.DES.CTA.PRO:" ":'':"*":1:"*":Y.MON.CTA.PRO
										Y.MON.REG = Y.MON.CTA.PRO
										STR.DATO = STR.TELL
										GOSUB REGISTRO.TRANS
									END
									IF Y.MON.CTA.TER GT 0 THEN
										S.KEY = S.TIPO.APP.CTA:S.COD.GRP:Y.DES.CTA.TER:S.DES.GRP
										STR.TELL = S.KEY:"*":S.TIPO.APP.CTA:"*":S.COD.GRP:"*":Y.DES.CTA.TER:" ":'':"*":1:"*":Y.MON.CTA.TER
										Y.MON.REG = Y.MON.CTA.TER
										STR.DATO = STR.TELL
										GOSUB REGISTRO.TRANS
									END
								END
							NEXT J
			
							IF S.MONTO.TFS EQ 0 THEN
								CONTINUE
							END
							
							;*Para Acumular estas Transacciones en el Mismo Registro Sin Distincion de Tipo de Cuenta : OCORNEJO 17.10.2017
							IF R.TFS<TFS.TRANSACTION><1, J> EQ 'PagoSTM' THEN
								S.COD.GRP = 'COD01'
								S.TIPO.APP = '02'	;*Salida de Caja
							END
							ELSE
								;*Para transaccion principal
								FOR K = 1 TO NO.GRP
									IF S.COD.CAT GE FIELD(STR.GRP<K>,"*", 3) AND S.COD.CAT LE FIELD(STR.GRP<K>,"*", 4) THEN
										S.COD.GRP = FIELD(STR.GRP<K>,"*", 1)
									END
								NEXT K
							END
		
							;*Evaluar si es pago masivo de prestamos o planilla
							IF (S.COD.CAT EQ Y.CATEG.PLANILLA OR (S.COD.CAT EQ Y.CATEG.PRESTAMO AND S.TIPO.CATEG EQ Y.SUBCATEG.PREST)) THEN
								IF S.COD.CAT EQ Y.CATEG.PRESTAMO THEN
									S.COD.GRP = 'COD04'
									S.DES.TRA = 'Pago Masivo de '
									S.DES.GRP = 'Prestamo'
								END ELSE
									S.COD.GRP = 'COD04'
									S.DES.TRA = 'Pago Masivo de '
									S.DES.GRP = 'Planilla'
								END
							END ELSE
								;*Evaluar grupo por categoria
								IF R.ARR<AA.ARR.PRODUCT.LINE> EQ 'LENDING' THEN
									S.COD.GRP = 'COD04'
								END ELSE
									IF R.ARR<AA.ARR.PRODUCT.LINE> EQ 'DEPOSITS' THEN
										S.COD.GRP = 'COD03'
									END
								END
								IF S.PRIM.ACC EQ S.ACC.ACAP THEN
									S.COD.GRP = 'COD01'
									S.DES.TRA = 'Compra de acciones'
									S.DES.GRP = ''
								END
								IF Y.ID.VERSION EQ Y.VER.RET.TAR THEN
									S.COD.GRP = ''
									S.DES.TRA = Y.TXT.RET.TAR
									S.DES.GRP = ''
								END
							END
							S.KEY = S.TIPO.APP:S.COD.GRP:S.DES.TRA:S.DES.GRP
							STR.TELL = S.KEY:"*":S.TIPO.APP:"*":S.COD.GRP:"*":TRIM(S.DES.TRA):" ":TRIM(S.DES.GRP):"*":1:"*":S.MONTO.TFS
						
							Y.MON.REG = S.MONTO.TFS
							STR.DATO = STR.TELL
							GOSUB REGISTRO.TRANS
						END	
					END
				END
			END
		REPEAT		
RETURN
*-----------------------------------------------------------------------------
TRANSCHARGES:
*-----------------------------------------------------------------------------		    
	SELECT.ACCHR  = "SELECT " : FN.ACCHR : " WITH CO.CODE EQ '": S.AGENCIA.ID :"' AND CHARGE.DATE EQ '":S.TODAY:"' AND STATUS EQ 'PAID'"

	CALL EB.READLIST(SELECT.ACCHR, LIST.ACCHR, '', NO.OF.RECS, ERR.ACCHR)

	STR.ACCHR  	= ''
	Y.POS 		= ''
	S.COD.GRP	= ''
	S.DES.TRA 	= ''		
	S.KEY 		= ''
	Y.TOT.MONTO   = 0.00
	S.MONTO.ACCHR = 0.00
	
	FOR I = 1 TO NO.OF.RECS
		S.REG.VALIDO = 'S'
		Ap1 = 0
		Vp1 = 0
		;*Extraer informacion de AC.CHARGE.REQUEST
        CALL F.READ(FN.ACCHR, LIST.ACCHR<I>, R.ACCHR, F.ACCHR, Y.ERR.ACCHR)

		;*Evaluar el estado de los registros equivale a R.TELLER<TT.TE.RECORD.STATUS> 
		IF R.ACCHR<CHG.RECORD.STATUS> EQ 'INAU' OR R.ACCHR<CHG.RECORD.STATUS> EQ 'INAO' OR R.ACCHR<CHG.RECORD.STATUS> EQ 'RNAU' OR R.ACCHR<CHG.RECORD.STATUS> EQ 'RNAO' OR R.ACCHR<CHG.RECORD.STATUS> EQ 'REVE' THEN
			S.REG.VALIDO = 'N'
		END
		
		IF S.REG.VALIDO EQ 'S' THEN
			;*Extraer informacion de AC.CHARGE.REQUEST
	        CALL F.READ(FN.FT.COMM, R.ACCHR<CHG.CHARGE.CODE>, R.FT.COMM, F.FT.COMM, Y.ERR.FTCOMM)
			
			S.TIPO.APP = "01"
			S.COD.GRP = R.ACCHR<CHG.CHARGE.CODE>
			S.DES.TRA = OCONV(R.ACCHR<CHG.RELATED.REF>, "MCT"):" ":R.FT.COMM<FT4.DESCRIPTION>
			S.MONTO.ACCHR = SUM(R.ACCHR<CHG.TOTAL.CHG.AMT>)

			S.KEY = S.TIPO.APP:S.COD.GRP:S.DES.TRA
			STR.ACCHR = S.KEY:"*":S.TIPO.APP:"*":S.COD.GRP:"*":TRIM(S.DES.TRA):"*":1:"*":S.MONTO.ACCHR
			Y.MON.REG = S.MONTO.ACCHR
			STR.DATO  = STR.ACCHR 
			GOSUB REGISTRO.TRANS
		END
	NEXT I
RETURN
*-----------------------------------------------------------------------------
TRANSTELL:
*-----------------------------------------------------------------------------		    
	SELECT.TELL  = "SELECT " : FN.TELLER : " WITH (CO.CODE EQ '": S.AGENCIA.ID :"') AND (VALUE.DATE.1 EQ '":S.TODAY:"')"

	CALL EB.READLIST(SELECT.TELL, LIST.TELL, '', NO.OF.RECS, ERR.TELL)

	STR.TELL  = ''
	Y.TXN.EFECTIVO = '10'	
	S.KEY = ''
	FOR I = 1 TO NO.OF.RECS
		S.REG.VALIDO = 'S'
		Y.TOT.MONTO = 0.00
		Y.POS = ''
		S.COD.GRP = ''
		S.DES.GRP = ''
		S.COD.CAT2 = ''
		S.TIPO.CATEG = ''
		Y.DESC.TT.TXN = ''
		
		;*Extraer informacion de TELLER
        CALL F.READ(FN.TELLER, LIST.TELL<I>, R.TELLER, F.TELLER, Y.ERR.TELL)

		;*Evaluar si la transaccion ya fue considerada como TFS
		FINDSTR FIELD(LIST.TELL<I>, ";", 1) IN STR.REGIS SETTING Ap1, Vp1 THEN
			Y.POS = 1
		END ELSE
			Y.POS = 0
		END

		;*Evaluar si el TFS de la transaccion ya fue agregado
		FINDSTR R.TELLER<TT.TE.THEIR.REFERENCE> IN STR.REGIS SETTING Ap1, Vp1 THEN
			Y.POS.TFS = 1
		END ELSE
			Y.POS.TFS = 0
		END
		
		Y.TRANS.CODE = R.TELLER<TT.TE.TRANSACTION.CODE>
		;*Obtener categoria de cuena de planilla o préstamos
		IF Y.TRANS.CODE NE Y.TXN.EFECTIVO THEN
			S.COD.CAT2   = SUBSTRINGS(CHANGE(R.TELLER<TT.TE.ACCOUNT.1>,"-",""),4,5)
			S.TIPO.CATEG = SUBSTRINGS(CHANGE(R.TELLER<TT.TE.ACCOUNT.1>,"-",""),9,4)
			
			IF S.COD.CAT2 NE Y.CATEG.PRESTAMO AND S.COD.CAT2 NE Y.CATEG.PLANILLA THEN
				S.COD.CAT2   = SUBSTRINGS(CHANGE(R.TELLER<TT.TE.ACCOUNT.2>,"-",""),4,5)
				S.TIPO.CATEG = SUBSTRINGS(CHANGE(R.TELLER<TT.TE.ACCOUNT.2>,"-",""),9,4)
			END
		END
						
*		IF Y.POS EQ 0 AND Y.POS.TFS EQ 0  OR (S.COD.CAT2 EQ Y.CATEG.PLANILLA OR (S.COD.CAT2 EQ Y.CATEG.PRESTAMO AND S.TIPO.CATEG EQ Y.SUBCATEG.PREST)) THEN
		IF Y.POS EQ 0 AND Y.POS.TFS EQ 0  THEN		
			IF FIELD(R.TELLER<TT.TE.NARRATIVE.1>, SM, 4) EQ '' THEN
				;*Descripcion de transaccion
				CALL F.READ(FN.TELL.TXN, R.TELLER<TT.TE.TRANSACTION.CODE>, R.TELL.TXN, F.TELL.TXN, Y.ERR.TELL.TXN)

				;*Evaluar el estado de los registros R.TELLER
				IF R.TELLER<TT.TE.RECORD.STATUS> EQ 'INAU' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'INAO' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'RNAU' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'RNAO' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'REVE' THEN
					S.REG.VALIDO = 'N'
				END
				
				IF S.REG.VALIDO EQ 'S' THEN
					;*Arreglo para validar TFS que ya fueron agregados
					STR.REGIS<-1> =  R.TELLER<TT.TE.THEIR.REFERENCE>:"*":LIST.TELL<I>
				
					IF R.TELLER<TT.TE.DR.CR.MARKER> EQ 'DEBIT' THEN
						IF SUBSTRINGS(CHANGE(R.TELLER<TT.TE.ACCOUNT.1>,"-",""), 4, 5) EQ S.CATEG.CAJA THEN
							S.TIPO.APP = "01"
						END ELSE
							S.TIPO.APP = "02"
						END					
					END
					IF R.TELLER<TT.TE.DR.CR.MARKER> EQ 'CREDIT' THEN
						IF SUBSTRINGS(CHANGE(R.TELLER<TT.TE.ACCOUNT.1>,"-",""), 4, 5) EQ S.CATEG.CAJA THEN
							S.TIPO.APP = "02"
						END ELSE
							S.TIPO.APP = "01"
						END					
					END

					;*Para transacciones donde no hay afectacion de saldo de caja
					IF SUBSTRINGS(CHANGE(R.TELLER<TT.TE.ACCOUNT.1>,"-",""), 4, 5) NE S.CATEG.CAJA AND SUBSTRINGS(CHANGE(R.TELLER<TT.TE.ACCOUNT.2>,"-",""), 4, 5) NE S.CATEG.CAJA THEN
						S.TIPO.APP = "03"
					END
					
					;*Consideración para transacción de cambio de moneda
					IF SUBSTRINGS(CHANGE(R.TELLER<TT.TE.ACCOUNT.1>,"-",""), 4, 5) EQ S.CATEG.CAJA AND SUBSTRINGS(CHANGE(R.TELLER<TT.TE.ACCOUNT.2>,"-",""), 4, 5) EQ S.CATEG.CAJA THEN
						S.TIPO.APP = "03"
					END 

					;*Bloque para separar registros por tipo de cuenta
					S.DES.GRP = ''
					IF R.TELLER<TT.TE.TRANSACTION.CODE> EQ '26' THEN
						;*Obtener informacion de la cuenta del Deposito
						CALL F.READ(FN.ACC, CHANGE(R.TELLER<TT.TE.ACCOUNT.2>,"-",""), R.ACC, F.ACC, Y.ERR.ACC)
						S.ARR.ID  = R.ACC<AC.ARRANGEMENT.ID>
						S.COD.CAT = R.ACC<AC.CATEGORY>
						
						;*Evaluar si la cuenta esta en historico
						IF DCOUNT(R.ACC, "?") EQ 0 THEN
							;*Extraer informacion de ACCOUNT$HIS
				   			CALL F.READ(FN.ACC.HIS, CHANGE(R.TELLER<TT.TE.ACCOUNT.2>,"-",""):";1", R.ACC.HIS, F.ACC.HIS, ERR.ACH)
				   			S.ARR.ID = R.ACC.HIS<AC.ARRANGEMENT.ID>
				   			S.COD.CAT = R.ACC.HIS<AC.CATEGORY>
						END
						
						;*Obtener informacion de AA.ARRANGEMENT
						CALL F.READ(FN.ARR, S.ARR.ID, R.ARR, F.ARR, Y.ERR.ARR)				
						
						;*Evaluar grupo por categoria
						IF R.ARR<AA.ARR.PRODUCT.LINE> EQ 'LENDING' THEN
							S.DES.GRP = 'Prestamo'
						END ELSE
							IF R.ARR<AA.ARR.PRODUCT.LINE> EQ 'DEPOSITS' THEN
								S.DES.GRP = 'Cuenta Depósito a Plazo'
							END ELSE	
								FOR K = 1 TO NO.GRP
									IF S.COD.CAT GE FIELD(STR.GRP<K>,"*", 3) AND S.COD.CAT LE FIELD(STR.GRP<K>,"*", 4) THEN
										S.DES.GRP = FIELD(STR.GRP<K>,"*", 2)
										BREAK
									END
								NEXT K
							END
						END
					END
					Y.DESC.TT.TXN = R.TELL.TXN<TT.TR.DESC>
					;*Evaluar descripcion
					GOSUB FORMA.TT

					S.KEY = S.TIPO.APP:R.TELLER<TT.TE.TRANSACTION.CODE>:S.DES.GRP
					STR.TELL = S.KEY:"*":S.TIPO.APP:"*":R.TELLER<TT.TE.TRANSACTION.CODE>:"*":Y.DESC.TT.TXN:" ":S.DES.GRP:"*":"1":"*":R.TELLER<TT.TE.AMOUNT.LOCAL.1>
					Y.MON.REG = R.TELLER<TT.TE.AMOUNT.LOCAL.1>
					STR.DATO  = STR.TELL 
					GOSUB REGISTRO.TRANS
				END			
			END
		END
	NEXT I
RETURN
*-----------------------------------------------------------------------------
TRANSFT:
*-----------------------------------------------------------------------------		    
	SELECT.FT  = "SELECT ": FN.FT:" WITH CO.CODE EQ '":S.AGENCIA.ID:"' AND PROCESSING.DATE EQ '": S.TODAY :"'" 
	 
	CALL EB.READLIST(SELECT.FT, LIST.FT, '', NO.OF.RECS, ERR.FT)
	
	STR.FT = ''
	STR.FT.CHQ = ''
	S.KEY 	   = ''
	
	FOR I = 1 TO NO.OF.RECS
		S.REG.VALIDO = 'S'
		Y.TOT.MONTO = 0.00
		Y.POS = ''
		S.COD.GRP = ''
		
		;*Extraer informacion de FUNDS.TRANSFER
        CALL F.READ(FN.FT, LIST.FT<I>, R.FT, F.FT, Y.ERR.FT)

		;*Evaluar si la transaccion ya fue considerada como TFS
		FINDSTR LIST.FT<I> IN STR.REGIS SETTING Ap1, Vp1 THEN
			Y.POS = 1
		END ELSE
			Y.POS = 0
		END

		;*Evaluar si el TFS de la transaccion ya fue agregado
		FINDSTR R.FT<FT.TFS.REFERENCE> IN STR.REGIS SETTING Ap1, Vp1 THEN
			Y.POS.TFS = 1
		END ELSE
			Y.POS.TFS = 0
		END

		IF Y.POS EQ 0 AND Y.POS.TFS EQ 0 THEN
			;*Descripcion de transaccion
			CALL F.READ(FN.FT.TXN, R.FT<FT.TRANSACTION.TYPE>, R.FT.TXN, F.FT.TXN, Y.ERR.FT.TXN)
	
			;*Evaluar el estado de los registros 
			IF R.FT<FT.RECORD.STATUS> EQ 'INAU' OR R.FT<FT.RECORD.STATUS> EQ 'INAO' OR R.FT<FT.RECORD.STATUS> EQ 'RNAU' OR R.FT<FT.RECORD.STATUS> EQ 'RNAO' OR R.FT<FT.RECORD.STATUS> EQ 'REVE' THEN
				S.REG.VALIDO = 'N'
			END
	
			IF S.REG.VALIDO EQ 'S' THEN	
				;*Arreglo para validar TFS que ya fueron agregados
				STR.REGIS<-1> =  LIST.FT<FT.TFS.REFERENCE>:"*":LIST.FT<I>
			
				Y.COUNT.DESC = DCOUNT(R.FT.TXN<FT6.DESCRIPTION>, VM)
				S.DES.TRA = R.FT.TXN<FT6.DESCRIPTION><1, Y.COUNT.DESC>
			
				S.COD.GRP = R.FT<FT.TRANSACTION.TYPE>
				
				;*Registros con afectacion de Caja
				IF SUBSTRINGS(CHANGE(R.FT<FT.DEBIT.ACCT.NO>,"-",""), 4, 5) EQ S.CATEG.CAJA OR SUBSTRINGS(CHANGE(R.FT<FT.CREDIT.ACCT.NO>,"-",""), 4, 5) EQ S.CATEG.CAJA THEN	
	
					N.MONTO	   = R.FT<FT.LOC.AMT.DEBITED>			
					
					IF SUBSTRINGS(CHANGE(R.FT<FT.DEBIT.ACCT.NO>,"-",""), 4, 5) EQ S.CATEG.CAJA THEN
						S.TIPO.APP = "01"
						N.MONTO	   = R.FT<FT.LOC.AMT.DEBITED>
					END
	
					IF SUBSTRINGS(CHANGE(R.FT<FT.CREDIT.ACCT.NO>,"-",""), 4, 5) EQ S.CATEG.CAJA THEN
						S.TIPO.APP = "02"
						N.MONTO	   = R.FT<FT.LOC.AMT.CREDITED>
					END

					S.KEY = S.TIPO.APP:S.COD.GRP:S.DES.TRA
					STR.FT = S.KEY:"*":S.TIPO.APP:"*":S.COD.GRP:"*":S.DES.TRA:"*":"1":"*":N.MONTO 
					Y.MON.REG = N.MONTO
					STR.DATO = STR.FT 
					GOSUB REGISTRO.TRANS
				END
			
				FINDSTR FIELD(R.FT<FT.INPUTTER>, "_",2) IN STR.CAJEROS SETTING Ap1, Vp1 THEN
					S.TRA.CAJA = 1
				END ELSE
					S.TRA.CAJA = 0
				END
	
				;*Registros sin afectacion de Caja
				IF SUBSTRINGS(CHANGE(R.FT<FT.DEBIT.ACCT.NO>,"-",""), 4, 5) NE S.CATEG.CAJA AND SUBSTRINGS(CHANGE(R.FT<FT.CREDIT.ACCT.NO>,"-",""), 4, 5) NE S.CATEG.CAJA AND S.TRA.CAJA = 1 THEN
					S.TIPO.APP = "03"
					N.MONTO	   = R.FT<FT.LOC.AMT.DEBITED>			
					
					S.KEY = S.TIPO.APP:S.COD.GRP:S.DES.TRA
					STR.FT.OTR = S.KEY:"*":S.TIPO.APP:"*":S.COD.GRP:"*":S.DES.TRA:"*":"1":"*":N.MONTO					
					Y.MON.REG = N.MONTO
					STR.DATO  = STR.FT.OTR 
					GOSUB REGISTRO.TRANS
				END
			END
		END
	NEXT I
RETURN
*-----------------------------------------------------------------------------
TRANSCHEQ:
*-----------------------------------------------------------------------------		    
	SELECT.TELL  = "SELECT " : FN.TELLER : " WITH (CO.CODE EQ '": S.AGENCIA.ID :"') AND (VALUE.DATE.1 EQ '":S.TODAY:"')"

	CALL EB.READLIST(SELECT.TELL, LIST.TELL, '', NO.OF.RECS, ERR.TELL)

	STR.TELL  = ''
	S.KEY 	  = ''
	
	FOR I = 1 TO NO.OF.RECS
		S.REG.VALIDO = 'S'
		Y.TOT.MONTO = 0.00
		Y.POS = ''
		S.COD.GRP = ''
		S.DES.GRP = ''
		
		;*Extraer informacion de TELLER
        CALL F.READ(FN.TELLER, LIST.TELL<I>, R.TELLER, F.TELLER, Y.ERR.TELL)

		;*Evaluar el estado de los registros R.TELLER<TT.TE.RECORD.STATUS> EQ '' OR 
		IF R.TELLER<TT.TE.TRANSACTION.CODE> EQ 99 OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'INAU' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'INAO' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'RNAU' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'RNAO' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'REVE' THEN
			S.REG.VALIDO = 'N'
		END

		IF S.REG.VALIDO EQ 'S' AND (R.TELLER<TT.TE.STOCK.NUMBER> GT 0 OR R.TELLER<TT.TE.CHEQUE.NUMBER> GT 0) THEN
			;*Para evitar duplicidad de registros
			STR.REGIS<-1> =  R.TELLER<TT.TE.THEIR.REFERENCE>:"*":LIST.TELL<I>

			;*Descripcion de transaccion TELLER.TRANSACTION
			CALL F.READ(FN.TELL.TXN, R.TELLER<TT.TE.TRANSACTION.CODE>, R.TELL.TXN, F.TELL.TXN, Y.ERR.TELL.TXN)

			;*Grupo de cheques
			S.TIPO.APP = "04"
			S.COD.GRP  = R.TELLER<TT.TE.TRANSACTION.CODE>
			S.DES.TRA  = TRIM(R.TELL.TXN<TT.TR.DESC>)
			S.KEY = S.TIPO.APP:S.COD.GRP:S.DES.TRA
			STR.TELL = S.KEY:"*":S.TIPO.APP:"*":S.COD.GRP:"*":S.DES.TRA:"*":"1":"*":R.TELLER<TT.TE.AMOUNT.LOCAL.1>						
			Y.MON.REG = R.TELLER<TT.TE.AMOUNT.LOCAL.1>
			STR.DATO  = STR.TELL 
			GOSUB REGISTRO.TRANS
		END
	NEXT I
RETURN

*-----------------------------------------------------------------------------
TIPO.CHEQUE:
*-----------------------------------------------------------------------------
	S.DES.GRP = ''
	S.DESC.CHQ = ''
	IF Y.ID.TRANS EQ S.TRA.CPR THEN
		 S.DESC.CHQ = 'cheque propio'
	END	 
	IF Y.ID.TRANS EQ S.TRA.CAJ THEN
		 S.DESC.CHQ = 'cheque de caja'
	END	 
	IF Y.ID.TRANS EQ S.TRA.AJE THEN
		 S.DESC.CHQ = 'cheque ajeno'
	END
	IF Y.ID.TRANS EQ S.TRA.CCE THEN
		 S.DESC.CHQ = 'cheque certificado'
	END	 
		 
RETURN
*-----------------------------------------------------------------------------
FORMA.TT:
*-----------------------------------------------------------------------------
	;*Evaluar descripciones de TELLER.TRANSACTION
	Y.TRA.CODE = '77' ;*TRANSACTION.CODE: 77	Emisión de Cheque + Comisión e IVA
	IF R.TELLER<TT.TE.TRANSACTION.CODE> EQ Y.TRA.CODE THEN
		;*Extraer informacion de TRANSACTION
       	CALL F.READ(FN.TXN, R.TELL.TXN<TT.TR.TRANSACTION.CODE.2>, R.TXN, F.TXN, Y.ERR.TXN)
		Y.DESC.TT.TXN = R.TXN<AC.TRA.NARRATIVE>
	END
RETURN

*-----------------------------------------------------------------------------
FORMA.TFS:
*-----------------------------------------------------------------------------
	Y.DES.CATEG = ''
	FOR K = 1 TO NO.GRP
		IF S.COD.CAT.OTR GE FIELD(STR.GRP<K>,"*", 3) AND S.COD.CAT.OTR LE FIELD(STR.GRP<K>,"*", 4) THEN
			IF FIELD(STR.GRP<K>,"*",1) EQ 'COD01' THEN
				Y.DES.CATEG = LOWCASE(TRIM(Y.DES.CTE))
			END
			IF FIELD(STR.GRP<K>,"*",1) EQ 'COD02' THEN
				Y.DES.CATEG = LOWCASE(TRIM(Y.DES.AHO))
			END
		END
	NEXT K
	
	;*Descripcion para contrapartida TFS de deposito
	IF S.TIPO.APP EQ '01' THEN
		IF S.COD.GRP EQ 'COD01' THEN
			Y.DES.TRANS = Y.TXT.CCTE.RET
		END
		IF S.COD.GRP EQ 'COD02' THEN
			Y.DES.TRANS = Y.TXT.CAHO.RET
		END
		Y.DES.CTA.CHQ = Y.TXT.CHQ.DEP
	END
	;*Descripcion para contrapartida TFS de retiro
	IF S.TIPO.APP EQ '02' THEN
		IF S.COD.GRP EQ 'COD01' THEN
			Y.DES.TRANS = Y.TXT.CCTE.DEP
		END
		IF S.COD.GRP EQ 'COD02' THEN
			Y.DES.TRANS = Y.TXT.CAHO.DEP
		END
		Y.DES.CTA.CHQ = Y.TXT.CHQ.RET
	END
	IF Y.DES.CATEG THEN
		Y.DES.TRANS = Y.DES.TRANS:' ':Y.DES.CATEG
	END
RETURN
*-----------------------------------------------------------------------------
REGISTRO.TRANS:
*-----------------------------------------------------------------------------
	IF Y.MON.REG GT 0 THEN
		FINDSTR S.KEY IN STR.ARR2 SETTING Ap1, Vp1 THEN
			Y.POS = 1
		END ELSE
			Y.POS = 0
		END
		
		IF Y.POS EQ 1 THEN
			Y.ARR.POS 	= A.INFO<Ap1, Vp1>
		  	Y.NO.REC 	= FIELD(Y.ARR.POS,"*", 5) + 1
		  	Y.MONTO 	= FIELD(Y.ARR.POS,"*", 6)
		  	Y.TOT.MONTO = Y.MONTO + Y.MON.REG
		  	A.INFO<Ap1, Vp1> = FIELD(Y.ARR.POS,"*", 1):"*":FIELD(Y.ARR.POS,"*", 2):"*":FIELD(Y.ARR.POS,"*", 3):"*":FIELD(Y.ARR.POS,"*", 4):"*":Y.NO.REC:"*":Y.TOT.MONTO
		END ELSE
			A.INFO<-1> 	 = STR.DATO
			STR.ARR2<-1> = S.KEY
		END
		Y.MON.REG = 0
		STR.DATO  = ''
	END
RETURN
*-----------------------------------------------------------------------------
PAGO.IMPUESTO:
*-----------------------------------------------------------------------------
	OUT.LIOF.TAX = ''
	OUT.CLIQ.TAX = ''
	OUT.SURR.TAX = ''
	S.TIPO.APP.CTA = '01'
	S.TIPO.APP = '01'
	S.COD.GRP      = 'COD03'
	Y.DES.CTA.PRO  = 'Cobro Imp. Control Liquidez'
	S.DES.GRP	   = 'DAP'

	CALL SLV.UTIL.TFS.GET.LIOF.TAX(Y.TFS.PAG.IMP, OUT.LIOF.TAX, OUT.CLIQ.TAX, OUT.SURR.TAX)
	Y.MON.REG = OUT.CLIQ.TAX
	IF Y.MON.REG GT 0 THEN
		Y.IMPUESTO = 1
		S.KEY = S.TIPO.APP.CTA:S.COD.GRP:Y.DES.CTA.PRO ;*:S.DES.GRP
		STR.DATO = S.KEY:"*":S.TIPO.APP.CTA:"*":S.COD.GRP:"*":Y.DES.CTA.PRO:" ":S.DES.GRP:"*":1:"*":Y.MON.REG
		GOSUB REGISTRO.TRANS
		
		;*Determinar numero de transacciones incluidas en TFS
		S.NO.REG.REF = DCOUNT(R.TFS<TFS.UNDERLYING>, VM)
		FOR J = 1 TO S.NO.REG.REF
			Y.MON.CTA.PRO = 0
			Y.MON.CTA.TER = 0
			S.DES.GRP	  = ''
			STR.DATO	  = ''
		    Y.ID.TRANS = FIELD(R.TFS<TFS.TRANSACTION>, VM, J)
			S.CTA.OTR = FIELD(R.TFS<TFS.ACCOUNT.DR>,VM,J)
			CALL F.READ(FN.ACC, S.CTA.OTR, R.ACC.OTR, F.ACC, Y.ERR.ACC.OTR)
			S.COD.CAT.OTR = R.ACC.OTR<AC.CATEGORY>
			;*Arreglo para validar TFS que ya fueron agregados
			STR.REGIS<-1> =  Y.TFS.PAG.IMP:"*":FIELD(R.TFS<TFS.UNDERLYING>, VM, J)
			
			FOR K = 1 TO NO.GRP
				IF S.COD.CAT.OTR GE FIELD(STR.GRP<K>,"*", 3) AND S.COD.CAT.OTR LE FIELD(STR.GRP<K>,"*", 4) THEN
					S.COD.GRP = FIELD(STR.GRP<K>,"*", 1)
					S.DES.GRP = FIELD(STR.GRP<K>,"*", 2)
					BREAK
				END
			NEXT K
		     
			IF S.INT.CTA NE FIELD(R.TFS<TFS.ACCOUNT.DR>,VM,J) THEN
				Y.MONTO.DR = FIELD(R.TFS<TFS.AMOUNT.DR>, VM, J)
			
				FINDSTR Y.ID.TRANS IN Y.ID.CTA.PRO SETTING Ap1, Vp1 THEN
					GOSUB FORMA.TFS
					GOSUB TIPO.CHEQUE
					Y.MON.CTA.PRO = Y.MONTO.DR
					Y.DES.CTA.PRO = Y.DES.TRANS					
					FINDSTR Y.ID.TRANS IN Y.ID.CHQ SETTING Vp1, Ap1 THEN 
						FINDSTR Y.ID.TRANS IN Y.PARAM.ID SETTING Vp1, Ap1 THEN
							GOSUB FORMA.TFS
							GOSUB TIPO.CHEQUE
							Y.DESC.PARAM = FIELD(FIELD(Y.PARAM.VAL, VM, Ap1), SM,5)
							Y.DESC.PARAM = CHANGE(Y.DESC.PARAM,".","")
							Y.DES.CTA.PRO = Y.DES.CTA.CHQ:" ":S.DESC.CHQ
						END
					END
				END
				FINDSTR Y.ID.TRANS IN Y.ID.CTA.TER SETTING Ap1, Vp1 THEN
					GOSUB FORMA.TFS
					Y.MON.CTA.TER = Y.MONTO.CR
					Y.DES.CTA.TER = Y.DES.TRANS
				END

				S.TIPO.APP.CTA = '02'
				IF Y.MON.CTA.PRO GT 0 THEN
					S.KEY = S.TIPO.APP.CTA:S.COD.GRP:Y.DES.CTA.PRO
					STR.DATO = S.KEY:"*":S.TIPO.APP.CTA:"*":S.COD.GRP:"*":Y.DES.CTA.PRO:"*":1:"*":Y.MON.CTA.PRO
					Y.MON.REG = Y.MON.CTA.PRO
					GOSUB REGISTRO.TRANS
				END
				IF Y.MON.CTA.TER GT 0 THEN
					S.KEY = S.TIPO.APP.CTA:S.COD.GRP:Y.DES.CTA.TER
					STR.DATO = S.KEY:"*":S.TIPO.APP.CTA:"*":S.COD.GRP:"*":Y.DES.CTA.TER:"*":1:"*":Y.MON.CTA.TER
					Y.MON.REG = Y.MON.CTA.TER
					GOSUB REGISTRO.TRANS
				END
			END
		NEXT J
	END
RETURN	
*-----------------------------------------------------------------------------
DET.FORMA.PAGO:
*-----------------------------------------------------------------------------
	;*Validar si es afectación de cuenta del cajero: efectivo o cheque
	CALL F.READ(FN.TFS, Y.TFS.REF, R.TFS, F.TFS, Y.ERR.TFS.REF)
	;*Determinar numero de transacciones incluidas en TFS
	S.NO.REG.REF = DCOUNT(R.TFS<TFS.UNDERLYING>, VM)
	
	S.TIPO.CTA = LEFT(R.TFS<TFS.PRIMARY.ACCOUNT>,3)
	S.PRIM.ACC = R.TFS<TFS.PRIMARY.ACCOUNT>
	S.DES.TRA = ''
	CTA.INT.VAL = 0
	FINDSTR S.PRIM.ACC IN  STR.CTAS.INT SETTING Ap1, Vp1 THEN
		CTA.INT.VAL = 1
	END

	IF S.TIPO.CTA NE "USD" AND TRIM(S.TIPO.CTA) NE "" OR (S.COD.CAT EQ Y.CATEG.PLANILLA OR (S.COD.CAT EQ Y.CATEG.PRESTAMO AND S.TIPO.CATEG EQ Y.SUBCATEG.PREST)) OR  CTA.INT.VAL EQ 1 THEN						
		FOR J = 1 TO S.NO.REG.REF
			Y.MON.CTA.PRO = 0
			Y.MON.CTA.TER = 0
			Y.MONTO.CR = FIELD(R.TFS<TFS.AMOUNT.CR>, VM, J)						
			Y.MONTO.DR = FIELD(R.TFS<TFS.AMOUNT.DR>, VM, J)
			S.COD.GRP = ''
			;*Arreglo para validar TFS que ya fueron agregados
			STR.REGIS<-1> =  Y.TFS.REF:"*":FIELD(R.TFS<TFS.UNDERLYING>, VM, J)
			Y.ID.TRANS = FIELD(R.TFS<TFS.TRANSACTION>, VM, J)
			S.CTA.OTR = FIELD(R.TFS<TFS.ACCOUNT.DR>,VM,J)
			CALL F.READ(FN.ACC, S.CTA.OTR, R.ACC.OTR, F.ACC, Y.ERR.ACC.OTR)
			S.COD.CAT.OTR = R.ACC.OTR<AC.CATEGORY>
			FOR K = 1 TO NO.GRP
				IF S.COD.CAT.OTR GE FIELD(STR.GRP<K>,"*", 3) AND S.COD.CAT.OTR LE FIELD(STR.GRP<K>,"*", 4) THEN
					S.COD.GRP = FIELD(STR.GRP<K>,"*", 1)
				END
			NEXT K

			IF S.TIPO.APP EQ '01' AND S.INT.CTA NE FIELD(R.TFS<TFS.ACCOUNT.DR>,VM,J) THEN
				FINDSTR Y.ID.TRANS IN Y.ID.CTA.PRO SETTING Ap1, Vp1 THEN
					GOSUB FORMA.TFS
					Y.MON.CTA.PRO += Y.MONTO.DR
					Y.DES.CTA.PRO = Y.DES.TRANS
					FINDSTR Y.ID.TRANS IN Y.ID.CHQ SETTING Vp1, Ap1 THEN
						S.COD.GRP = 'COD01' 
						FINDSTR Y.ID.TRANS IN Y.PARAM.ID SETTING Vp1, Ap1 THEN
							GOSUB FORMA.TFS
							GOSUB TIPO.CHEQUE
							Y.DESC.PARAM = FIELD(FIELD(Y.PARAM.VAL, VM, Ap1), SM,5)
							Y.DESC.PARAM = CHANGE(Y.DESC.PARAM,".","")
							Y.DES.CTA.PRO = Y.DES.CTA.CHQ:" ":S.DESC.CHQ
						END
					END
				END
				FINDSTR Y.ID.TRANS IN Y.ID.CTA.TER SETTING Ap1, Vp1 THEN
					GOSUB FORMA.TFS
					GOSUB TIPO.CHEQUE
					Y.MON.CTA.TER += Y.MONTO.CR
					Y.DES.CTA.TER = Y.DES.TRANS
					FINDSTR Y.ID.TRANS IN Y.ID.CHQ SETTING Vp1, Ap1 THEN 
						S.COD.GRP = 'COD01'
						FINDSTR Y.ID.TRANS IN Y.PARAM.ID SETTING Vp1, Ap1 THEN
							GOSUB FORMA.TFS
							GOSUB TIPO.CHEQUE
							Y.DESC.PARAM = FIELD(FIELD(Y.PARAM.VAL, VM, Ap1), SM,5)
							Y.DESC.PARAM = CHANGE(Y.DESC.PARAM,".","")
							Y.DES.CTA.TER = Y.DES.CTA.CHQ:" ":S.DESC.CHQ
						END
					END
				END
				S.TIPO.APP.CTA = '02'
				IF Y.MON.CTA.PRO GT 0 THEN
					S.KEY = S.TIPO.APP.CTA:S.COD.GRP:Y.DES.CTA.PRO:S.DES.GRP
					STR.DATO = S.KEY:"*":S.TIPO.APP.CTA:"*":S.COD.GRP:"*":Y.DES.CTA.PRO:"*":1:"*":Y.MON.CTA.PRO
					Y.MON.REG = Y.MON.CTA.PRO
					GOSUB REGISTRO.TRANS
				END
				IF Y.MON.CTA.TER GT 0 THEN
					S.KEY = S.TIPO.APP.CTA:S.COD.GRP:Y.DES.CTA.TER:S.DES.GRP
					STR.DATO = S.KEY:"*":S.TIPO.APP.CTA:"*":S.COD.GRP:"*":Y.DES.CTA.TER:"*":1:"*":Y.MON.CTA.TER
					Y.MON.REG = Y.MON.CTA.TER
					GOSUB REGISTRO.TRANS
				END
			END		
		NEXT J
	END
RETURN	

;*-----------------------------------------------------------------------------
TFS.COLECTOR.VEN:
;*----------------------------------------------------------------------------    

	;*Y.ID.COMPANY = S.AGENCIA.ID	
    
    ;*Id Colector Punto Express
    Y.SLV.COLECTOR.SELECT = "SELECT " : FN.SLV.COLECTOR : " WITH NOMBRE.COLECTOR LIKE %PuntoExpress%"    
    
    CALL EB.READLIST(Y.SLV.COLECTOR.SELECT, Y.SLV.COLECTOR.LIST.IDS, Y.SLV.COLECTOR.SELECTED, Y.SLV.COLECTOR.NUM.REGS, Y.SLV.COLECTOR.SYS.RESP)       
    Y.ID.COLECTOR.PEX = Y.SLV.COLECTOR.LIST.IDS                   
    
    ;*Id Busqueda Account Params
    Y.ID.ACCOUNT.PARAM = S.AGENCIA.ID : '.' : Y.ID.COLECTOR.PEX : '.VEN'       
    
    ;*Obtenemos la cuenta de agencia del registro Y.ID.ACCOUNT.PARAM        
    CALL F.READ(FN.ACCOUNT.PARAM, Y.ID.ACCOUNT.PARAM, REG.ACCOUNT.PARAM, F.ACCOUNT.PARAM, ERR.ACCOUNT.PARAM)                
    S.CTA.COLECTOR = REG.ACCOUNT.PARAM<EB.SLV47.RESERVADO5>
    
    ;*STR.CTAS.INT<-1> = S.CTA.COLECTOR
         
    IF S.CTA.COLECTOR NE '' THEN    
        Y.TFS.SELECT   = "SELECT " : FN.TFS
        Y.TFS.SELECT  := "  WITH CO.CODE         EQ " : "'" : S.AGENCIA.ID   : "'"    
        Y.TFS.SELECT  := "   AND PRIMARY.ACCOUNT EQ " : "'" : S.CTA.COLECTOR : "'"
        Y.TFS.SELECT  := "   AND BOOKING.DATE    EQ " : "'" : S.TODAY        : "'"
        
        CALL EB.READLIST(Y.TFS.SELECT, Y.TFS.LIST.IDS, Y.TFS.SELECTED, Y.TFS.NUM.REGS, Y.TFS.SYS.RESP)
      
        LOOP
        REMOVE Y.TFS.ITEM FROM Y.TFS.LIST.IDS SETTING Y.TFS.DELIM
        WHILE Y.TFS.ITEM  NE ''
            S.TIPO.APP   = '01'
	        S.COD.GRP    = 'CVPX'
	        S.DES.TRA    = 'Recepcion Colectores Ventanilla'
            S.MONTO.COLECTORES = 0.00
		    Y.POS 	           = ''
		    STR.COLECTORES     = '' 
		    S.KEY	           = '' 
		    S.REG.VALIDO       = 'S'		                  
            Y.ID.TFS           = Y.TFS.ITEM
         
            ;*Obtenemos los campos del registro Y.ID.TFS 
            CALL F.READ(FN.TFS, Y.ID.TFS, REG.TFS, F.TFS, ERR.TFS)
		    Y.TFS.STAT = REG.TFS<TFS.RECORD.STATUS>
		    Y.TFS.REV  = REG.TFS<TFS.REVERSAL.MARK>
		    
		    Y.ID.TELLER    = FIELD(REG.TFS<TFS.UNDERLYING>, VM, 1)		    
            
            ;*Obtenemos los campos del registro Y.ID.TELLER 
            CALL F.READ(FN.TELLER, Y.ID.TELLER, REG.TELLER, F.TELLER, ERR.TELLER)		    
		    
		    S.COD.CAJERO = REG.TELLER<TT.TE.TELLER.ID.1>
		    
		    ;*Evaluar el estado de los registros equivale a R.TELLER<TT.TE.RECORD.STATUS> 
		    IF Y.TFS.STAT EQ 'INAU' OR Y.TFS.STAT EQ 'INAO' OR Y.TFS.STAT EQ 'RNAU' OR Y.TFS.STAT EQ 'RNAO' OR Y.TFS.STAT EQ 'REVE' OR Y.TFS.REV EQ 'R' THEN
			    S.REG.VALIDO = 'N'
		    END
		    
		    ;*Evaluar si el registro ya fue agregado
		    FINDSTR Y.ID.TFS IN STR.REGIS SETTING Ap1, Vp1 THEN
			    Y.POS = 1
		    END ELSE
			    Y.POS = 0
		    END    		 
		    
		    IF S.REG.VALIDO EQ "S" AND Y.POS EQ 0 THEN
			    S.NO.REG.REF = DCOUNT(REG.TFS<TFS.UNDERLYING>, VM)
			
			    FOR J = 1 TO S.NO.REG.REF
				    IF FIELD(REG.TFS<TFS.RUNNING.TOTAL>, VM, J) GT 0 THEN				   
					    ;*Monto total del TFS
					    S.MONTO.TFS = FIELD(REG.TFS<TFS.RUNNING.TOTAL>, VM, J)
					    S.MONTO.COLECTORES += FIELD(REG.TFS<TFS.RUNNING.TOTAL>, VM, J)

					    ;*Arreglo para validar TFS que ya fueron agregados
					    STR.REGIS<-1> =  Y.ID.TFS:"*":FIELD(REG.TFS<TFS.UNDERLYING>, VM, J)
				    END
			    NEXT J			    			    
			    
			    IF S.MONTO.COLECTORES GT 0 THEN
				    S.KEY          = S.TIPO.APP:S.COD.GRP:S.DES.GRP
				    STR.COLECTORES = S.KEY:"*":S.TIPO.APP:"*":S.COD.GRP:"*":TRIM(S.DES.TRA):"*":1:"*":S.MONTO.COLECTORES
				    Y.MON.REG      = S.MONTO.COLECTORES
				    STR.DATO       = STR.COLECTORES 
				    GOSUB REGISTRO.TRANS
				    ;*Agregar detalle de forma de pago
				    S.COD.GRP = ''
				    Y.TFS.REF = Y.ID.TFS
				    GOSUB DET.FORMA.PAGO
			    END						
		    END
        REPEAT
    END   
  
RETURN    
*-----------------------------------------------------------------------------
* FIN - TFS.COLECTOR.VEN
*-----------------------------------------------------------------------------

END

