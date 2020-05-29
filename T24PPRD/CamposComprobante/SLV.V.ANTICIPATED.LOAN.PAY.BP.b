*-----------------------------------------------------------------------------
* <Rating>-51</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.ANTICIPATED.LOAN.PAY.BP
*-----------------------------------------------------------------------------
*
* Nombre: SLV.V.ANTICIPATED.LOAN.PAY.BP
* Descripción: Rutina para Validar si el Pago es Anticipado y si las Condiciones 
*				del Credito Aplican a usar UNC en BulkPayments
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* ocornejo	08.12.2017	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_GTS.COMMON
$INSERT I_F.EB.SLV.KEYS.PARAMS
$INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB VALIDATE.TXN
IF Y.CONTINUE EQ 'Y' THEN
	IF V$FUNCTION EQ 'I' AND OFS$OPERATION EQ 'PROCESS' THEN
		GOSUB PROCESS
	END
END
RETURN

	INIT:
		FN.KEYS = 'F.EB.SLV.KEYS.PARAMS'
		F.KEYS  = ''
		CALL OPF(FN.KEYS, F.KEYS)
		
		FN.GBL       = 'F.EB.SLV.GLOBAL.PARAM'
		F.GBL        = ''
		CALL OPF(FN.GBL, F.GBL)
		
		;*Extraer Parametrizacion de Log
		CALL F.READ(FN.GBL, 'LOG.ALP', R.GBL, F.GBL, E.GBL)
		DIR.NAME   = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',1)	;*Ruta de Salida del Archivo
		R.ID       = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',3) : '.' : TODAY : '.log'	;*Nombre de Archivo Log
		LOG.ACTIVO = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',2)	;*Bandera de Log Activo/Inactivo
	RETURN
	
	VALIDATE.TXN:
		;*Validar si la Txn Es Pago de Prestamos
		Y.CONTINUE = 'N'
		CALL CACHE.READ(FN.KEYS, 'SLV.TXN.PAGO.PRESTAMOS', R.KEYS, E.KEYS)
		FIND 'PAGO.X.FT' IN R.KEYS<EB.SLV18.PARAM.ID> SETTING Ap, Vp THEN
			Y.VALUES = CHANGE(R.KEYS<EB.SLV18.VALOR>, SM, VM)
			FIND R.NEW(FT.TRANSACTION.TYPE) IN Y.VALUES SETTING Xp, Yp THEN
				Y.CONTINUE = 'Y'
			END			
		END
	RETURN

	PROCESS:		
		;*Obtener Campo Local
		CALL GET.LOC.REF ("FUNDS.TRANSFER", "LF.AMOUNT", LF.AMOUNT)
		
		;*Obteniendo Datos de Aplicacion
		Y.PAY = COMI ;*R.NEW(FT.DEBIT.AMOUNT)
		IF Y.PAY NE '' THEN
			COMI = ''	;*Limpiado DEBIT.AMOUNT
			R.NEW(FT.CREDIT.AMOUNT) = Y.PAY
		END
		ELSE
			IF R.NEW(FT.LOCAL.REF)<1, LF.AMOUNT> NE '' THEN
				Y.PAY = R.NEW(FT.LOCAL.REF)<1, LF.AMOUNT>
			END
			ELSE
				Y.PAY = R.NEW(FT.CREDIT.AMOUNT)
			END
		END
		
		;*Obtener Posicion de Campo Local y Setear Monto de Pago
		;*Para que la Validation Routine SLV.V.ANTICIPATED.LOAN.PAY 
		;*lo tome y haga todas sus evaluaciones
		R.NEW(FT.LOCAL.REF)<1, LF.AMOUNT> = Y.PAY
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
 