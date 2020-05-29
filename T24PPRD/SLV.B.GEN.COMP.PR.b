*-----------------------------------------------------------------------------
* <Rating>-19</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.GEN.COMP.PR
*-----------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		eurias 					comprobantes pago prestamos
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* 2.0 		Jonas		04.01.17	Se agrega llamado a subrutina SLV.B.GEN.COMP.PR.DET
*									 para obtener detalle de la distribucion del pago.
* 2.1 		Jonas		09.05.17	Agregar Ref. de actividad de pago.
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.STMT.ENTRY
$INSERT I_F.TRANSACTION
$INSERT I_F.DATES
$INSERT I_F.CUSTOMER
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.FUNDS.TRANSFER
*----------------------------------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS
    RETURN
RETURN
*----------------------------------------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------------------------------------
	FN.STMT = 'F.STMT.ENTRY'
	F.STMT  = ''
	FN.DTS 	= 'F.DATES'
	F.DTS  	= ''
	FN.CUS  = 'F.CUSTOMER'
	F.CUS	= ''
	FN.ACCE = 'F.ACCT.ENT.TODAY'
	F.ACCE  = ''
	FN.ARR	= 'F.AA.ARRANGEMENT'
	F.ARR	= ''
	FN.FT   = 'F.FUNDS.TRANSFER'
	F.FT    = ''
	SELECT.ACCE  = "SELECT ":FN.ACCE
	Y.FECHA.INF  = ''
	Y.FECHA.SUP	 = ''
	Y.COD.DTS 	 = 'SV0010001-COB'
	ID.LIST      = ''
	
	Y.FECHA.TODAY = ''
	Y.FECHA.ANT   = ''
	Y.PRODUCT 	  = 'LENDING'
	Y.SALDO.ANT   = 0
	Y.SALDO.ACT   = 0
	Y.STR.ACTIVITY= ''
	Y.TRANS.TYPE = 'ACRP'	
RETURN
*----------------------------------------------------------------------------------------------------
OPENFILE:
*----------------------------------------------------------------------------------------------------
	CALL OPF (FN.STMT, F.STMT)
	CALL OPF (FN.DTS, F.DTS)
	CALL OPF (FN.CUS, F.CUS)
	CALL OPF (FN.ARR, F.ARR)
RETURN
*----------------------------------------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------------------------------------
	;*Obtener fechas de COB
    GOSUB OBTDATECOBB

    ;*Generar ID para STMT.ENTRY
    GOSUB GENERAID
    ;*Prueba
*    R.STM.IDS = '180376911958946.010001':FM:'180376911958948.000001':FM:'180376911958978.000001':FM:'180376911959249.000001'
    Y.COUNT = DCOUNT(R.STM.IDS, FM)
    STR.FT = ''
	LOOP
    	REMOVE ST.ID FROM R.STM.IDS SETTING POS
    WHILE ST.ID NE ''
    	Y.ID = FIELD(ST.ID,".",1)
    	FINDSTR Y.ID IN STR.FT SETTING Ap, Vp THEN
    		CONTINUE
    	END
        GOSUB INFO_GEN
        IF STR.HEAD THEN
	        FINDSTR NUMTTFT IN STR.FT SETTING Ap, Vp THEN
			END ELSE
				IF NUMTTFT THEN
		        	STR.FT<-1> = NUMTTFT:"*":Y.ID
			        ;*Obtener distribucion del pago
			        CALL SLV.B.GEN.COMP.PR.DET(STR.HEAD, Y.FECHA.TODAY, Y.FECHA.ANT, Y.SALDO.ANT, Y.SALDO.ACT, Y.STR.ACTIVITY, STR.SAL.ANT)
		        END
	        END
        END
    REPEAT
RETURN
*----------------------------------------------------------------------------------------------------
OBTDATECOBB:
*----------------------------------------------------------------------------------------------------
	CALL F.READ (FN.DTS, Y.COD.DTS, R.DTS, F.DTS, ER)
	Y.FECHA.TODAY = R.DTS<EB.DAT.TODAY>
	Y.FECHA.ANT   = R.DTS<EB.DAT.LAST.PERIOD.END>
RETURN
*----------------------------------------------------------------------------------------------------
GENERAID:
*----------------------------------------------------------------------------------------------------
	CALL EB.READLIST(SELECT.ACCE, ID.LIST, '', NO.OF.RECS, ERR)
	LOOP
        REMOVE STACCE.ID FROM ID.LIST SETTING POS
    WHILE STACCE.ID NE ''
    	Y.ARR = STACCE.ID
    	CALL SLV.UTIL.GET.ARR.X.ACC(Y.ARR)
    	CALL F.READ (FN.ARR, Y.ARR, R.ARR, F.ARR, ERROR.ARR)
    	IF R.ARR<AA.ARR.PRODUCT.LINE> EQ Y.PRODUCT THEN
			CALL F.READ (FN.ACCE, STACCE.ID, R.ACCE, F.ACCE,ER)
			LOOP
				REMOVE STE.ID FROM R.ACCE SETTING POSE
			WHILE STE.ID NE ''
				R.STM.IDS<-1> = STE.ID
			REPEAT
		END				
	REPEAT
RETURN
*-----------------------------------------------------------------------------
INFO_GEN:
*-----------------------------------------------------------------------------
	STR.HEAD = ''
	R.STMT   = ''
    CALL F.READ(FN.STMT, ST.ID, R.STMT, F.STMT, ST.ERR)
	Y.BULK = FIELD(R.STMT<AC.STE.INPUTTER>,'_',8)
	Y.TRANSACTION.CODE = R.STMT<AC.STE.TRANSACTION.CODE>
	Y.ID.FT = FIELD(R.STMT<AC.STE.INPUTTER>,'_',9)	
	CALL F.READ(FN.FT, Y.ID.FT, R.FT, F.FT, ERR.FT)
	Y.TRANSACTION.TYPE = R.FT<FT.TRANSACTION.TYPE>
	;*Para pagos bulkpaymen y pagos en ventanilla
	;*Se modifica para tomar los pagos en ventanilla para actualizar saldo
    IF "FT.BULK.PROCESS" EQ Y.BULK OR Y.TRANSACTION.TYPE EQ Y.TRANS.TYPE THEN
        AGENCIA 	= R.STMT<AC.STE.COMPANY.CODE>
	    NUMTTFT     = FIELD(R.STMT<AC.STE.INPUTTER>,'_',9)
	    IF NUMTTFT EQ '' THEN
	    	NUMTTFT     = R.STMT<AC.STE.TRANS.REFERENCE>
	    END
	    CLIENTE		= R.STMT<AC.STE.CUSTOMER.ID>
	    NumPrestamo = R.STMT<AC.STE.ACCOUNT.NUMBER>
	    USUARIO     = FIELD(R.STMT<AC.STE.INPUTTER>,"_",2)
	    FECHAHORA  	= R.STMT<AC.STE.VALUE.DATE>
	 	TASAINTERES = ''
	 	TASAMORA	= ''
	 	MontoPrestamo = NumPrestamo
	 	CALL SLV.UTIL.IMP.P.GET.MON(MontoPrestamo)
	    Y.TX.MONT   = R.STMT<AC.STE.AMOUNT.LCY>
	    CALL F.READ(FN.CUS, CLIENTE, R.CUSTOMER, F.CUS, F.CUS.ERR)
	 	primerNombre   = R.CUSTOMER<EB.CUS.NAME.1>
	 	primerApellido = R.CUSTOMER<EB.CUS.TEXT>
	 	NOMBRECLIENTE  = primerNombre:" ":primerApellido
	 	Y.ID.ACTIVITY = R.STMT<AC.STE.TRANS.REFERENCE>
	 	STR.HEAD = AGENCIA:"*":NUMTTFT:"*":CLIENTE:"*":NumPrestamo:"*":USUARIO:"*":FECHAHORA:"*":TASAINTERES:"*":TASAMORA:"*":MontoPrestamo:"*":NOMBRECLIENTE:"*":Y.BULK:"*":Y.ID.ACTIVITY
	END
RETURN

END
