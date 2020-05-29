*-----------------------------------------------------------------------------
* <Rating>-58</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.AMT.ACUMULADO.DJ
*-----------------------------------------------------------------------------
*
* Nombre: SLV.I.AMT.ACUMULADO.DJ
* Descripción: 	Rutina que obtiene el monto acumulado por mes del cliente y le suma la actual, se envia a la rutina SLV.I.COUNT.WRT.AMT.DJA
* 				Para que realize el WRITE
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor			Fecha		Comentario
*-----------------------------------------------------------------------------
* jhenriquez	5.07.2019	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.TELLER
$INSERT I_F.ACCOUNT
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.EB.SLV.KEYS.PARAMS
$INSERT I_F.EB.SLV.CONTADOR.MAZUL
*-----------------------------------------------------------------------------

Y.STATUS  = R.NEW(TT.TE.RECORD.STATUS)
	IF Y.STATUS EQ 'INAU' OR  Y.STATUS EQ '' THEN
		IF (V$FUNCTION EQ 'I') OR (V$FUNCTION EQ 'A')  THEN
            GOSUB INI
			GOSUB OPENFILE
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
	 
	V.NOSTRO  = ''
	V.ACC.USD = '' 
RETURN

OPENFILE:
	CALL OPF(FN.CONTA, F.CONTA)
	CALL OPF(FN.KP, F.KP)
	CALL OPF(FN.ACC, F.ACC)
	CALL OPF(FN.FT, F.FT)
	CALL OPF(FN.TT, F.TT)
RETURN

PROCESS:
	Y.APP    = APPLICATION
	Y.TXN.ID = ID.NEW
    
    BEGIN CASE
    	CASE Y.APP EQ 'FUNDS.TRANSFER'
    	GOSUB WRITE.FT
    	CASE Y.APP EQ 'TELLER'
    	GOSUB WRITE.TT
    END CASE
RETURN

;*Se realiza actualizacion de las txn FT para la declaracion Jurada Acumulada
;*=======================================================
WRITE.FT:
	Y.TRANS.ID    = ID.NEW
	Y.TRANS.TYPE  = R.NEW(FT.TRANSACTION.TYPE)
	Y.TXN.AMT     = R.NEW(FT.DEBIT.AMOUNT)
	
;*Se valida para verificar que el campo debit tiene valor
;*=======================================================
    IF Y.TXN.AMT EQ 0 OR Y.TXN.AMT EQ '' THEN
	   	Y.TXN.AMT = R.NEW(FT.CREDIT.AMOUNT)
    END
    
    Y.FET.DATE  = R.NEW(FT.DEBIT.VALUE.DATE)
	Y.DEB.CUS   = R.NEW(FT.DEBIT.CUSTOMER)
	;*Y.CRE.CUS   = R.NEW(FT.CREDIT.CUSTOMER)
	Y.DEB.ACC   = R.NEW(FT.DEBIT.ACCT.NO)
	Y.CRE.ACC	= R.NEW(FT.CREDIT.ACCT.NO)

;*Se validan las cuentas para no tomar cuentas NOSTRO o ACC.INTERNAS
;*=======================================================

;*DEBIT.ACC
;*----------
	V.NOSTRO	= ''
	V.ACC.USD 	= ''
	
	CALL F.READ(FN.ACC, Y.DEB.ACC, R.ACC.DEB, F.ACC, ERR.ACC.DEB)
	V.NOSTRO 	= R.ACC.DEB<AC.LIMIT.REF>
	V.ACC.USD 	= SUBSTRINGS(Y.DEB.ACC, 1, 3) 
	
	IF V.NOSTRO NE 'NOSTRO' THEN
		IF V.ACC.USD NE 'USD' THEN
			Y.CUS.DEB	= R.ACC.DEB<AC.CUSTOMER>
			CALL SLV.I.COUNT.WRT.AMT.DJA(Y.CUS.DEB, Y.TXN.AMT)
		END
	END

;*CREDIT.ACC
;*-------------------------
	V.NOSTRO  = ''
	V.ACC.USD = ''	
	
	CALL F.READ(FN.ACC, Y.CRE.ACC, R.ACC.CRE, F.ACC, ERR.ACC.CRE)
	V.NOSTRO 	= R.ACC.CRE<AC.LIMIT.REF>
	V.ACC.USD 	= SUBSTRINGS(Y.CRE.ACC, 1, 3) 
	
	IF V.NOSTRO NE 'NOSTRO' THEN
		IF V.ACC.USD NE 'USD' THEN
			Y.CUS.CRE	= R.ACC.CRE<AC.CUSTOMER>
			CALL SLV.I.COUNT.WRT.AMT.DJA(Y.CUS.CRE, Y.TXN.AMT)
		END
	END
	
RETURN

;*Se realiza actualizacion de las txn TT para la declaracion Jurada Acumulada
;*======================================================
WRITE.TT:
	Y.TRANS.AMT	= R.NEW(TT.TE.AMOUNT.LOCAL.2)
    Y.TT.CODE   = R.NEW(TT.TE.TRANSACTION.CODE)
    Y.ACC.2	    = R.NEW(TT.TE.ACCOUNT.2)
    Y.ACC.1     = R.NEW(TT.TE.ACCOUNT.1)
    ;*V.CUS.1 	= R.NEW(TT.TE.CUSTOMER.1)
    ;*V.CUS.2 	= R.NEW(TT.TE.CUSTOMER.2)
    
;*Se validan las cuentas para no tomar cuentas NOSTRO o ACC.INTERNAS
;*=======================================================

;*DEBITO
;*------	
   	V.NOSTRO  = ''
	V.ACC.USD = ''	
	
	CALL F.READ(FN.ACC, Y.ACC.1, R.ACC.1, F.ACC, ERR.ACC.1)
	V.NOSTRO 	= R.ACC.1<AC.LIMIT.REF>
	V.ACC.USD 	= SUBSTRINGS(Y.ACC.1, 1, 3) 
	
	IF V.NOSTRO NE 'NOSTRO' THEN
		IF V.ACC.USD NE 'USD' THEN
			Y.CUS.1	= R.ACC.1<AC.CUSTOMER>
			CALL SLV.I.COUNT.WRT.AMT.DJA(Y.CUS.1, Y.TRANS.AMT)
		END
	END

;*CREDITO
;*------
	V.NOSTRO  = ''
	V.ACC.USD = ''	
	
	CALL F.READ(FN.ACC, Y.ACC.2, R.ACC.2, F.ACC, ERR.ACC.2)
	V.NOSTRO 	= R.ACC.2<AC.LIMIT.REF>
	V.ACC.USD 	= SUBSTRINGS(Y.ACC.2, 1, 3) 
	
	IF V.NOSTRO NE 'NOSTRO' THEN
		IF V.ACC.USD NE 'USD' THEN
			Y.CUS.2	= R.ACC.2<AC.CUSTOMER>
			IF Y.CUS.1 NE Y.CUS.2 THEN
				CALL SLV.I.COUNT.WRT.AMT.DJA(Y.CUS.2, Y.TRANS.AMT)
			END
		END
	END
RETURN

END
