*-----------------------------------------------------------------------------
* <Rating>-39</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.COMPROBANTES.UTEC
*
* Nombre: SLV.V.COMPROBANTES.UTEC
* Descripción: 	Rutina que llena los campos que se utilizan para la impresion de comprobantes
*-----------------------------------------------------------------------------
* Version	Autor			Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		jhenriquez		27.03.19	Versión inicial.

$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.EB.SLV.MULTI.PAGO.COLECTOR
*-----------------------------------------------------------------------------

GOSUB INI
GOSUB OPENFILE
GOSUB PROCESS
RETURN

INI:
	FN.TFS 	= 'F.TELLER.FINANCIAL.SERVICES'
	F.TFS 	= ''
	
	FN.MULTI.PAGO 	= 'F.EB.SLV.MULTI.PAGO.COLECTOR'
	F.MULTI.PAGO 	= ''	
	
	V.TT 	= ''
RETURN

OPENFILE:
	CALL OPF(FN.TFS, F.TFS)
	CALL OPF(FN.MULTI.PAGO, F.MULTI.PAGO)
RETURN

PROCESS:
;*Se obtiene la posicion de los campos que contendran la data para la generacion de los comprobantes
;*==================================================================================================
	CALL GET.LOC.REF ('TELLER.FINANCIAL.SERVICES','LF.CM.VALOR.1',PLfCmValor1)
	CALL GET.LOC.REF ('TELLER.FINANCIAL.SERVICES','LF.CM.VALOR.3',PLfCmValor3)
	CALL GET.LOC.REF ('TELLER.FINANCIAL.SERVICES','LF.CM.VALOR.4',PLfCmValor4)
	CALL GET.LOC.REF ('TELLER.FINANCIAL.SERVICES','LF.CM.VALOR.5',PLfCmValor5)
	
		
;*Se actualiza campo que define que la transaccion es de colector
;*================================================================	
	CALL GET.LOC.REF ('TELLER.FINANCIAL.SERVICES','LF.EST.COLECTOR',PEstColector)
	R.NEW(TFS.LOCAL.REF)<1,PEstColector> = 1
	
;*Se consulta la informacion del pago
;*===================================
	CALL GET.LOC.REF ('TELLER.FINANCIAL.SERVICES','LF.NUM.REF',PIdCintex)
	V.ID.APP.LOCAL = R.NEW(TFS.LOCAL.REF)<1,PIdCintex>
	
	CALL F.READ(FN.MULTI.PAGO, V.ID.APP.LOCAL, RECORD.MULTI.PAGO, F.MULTI.PAGO, ERROR.MULTI.PAGO)
	V.ID.COLECTOR =  FIELD(V.ID.APP.LOCAL, '-', 1)
	V.NO.CARNET = DCOUNT(RECORD.MULTI.PAGO<EB.SLV55.INPUT.VALUE>,VM)

;*Se agregar Numero del colector a campo local para RPT Detallado por Cajero mas la suma de la txn en total
;*==========================================================================
	CALL GET.LOC.REF ('TELLER.FINANCIAL.SERVICES','LF.CM.CODCOLEC',PLfCmCodColec)
	R.NEW(TFS.LOCAL.REF)<1,PLfCmCodColec> = V.ID.COLECTOR
	
	CALL GET.LOC.REF ('TELLER.FINANCIAL.SERVICES','LF.CM.MONTO',PLfCmMonto)
	R.NEW(TFS.LOCAL.REF)<1,PLfCmMonto> = RECORD.MULTI.PAGO<EB.SLV55.AMT.TXN>
;*Se valida si el pago es por un carnet o varios
;*============================================== 	
	IF V.NO.CARNET EQ 1 THEN
		V.NUM.CARNET 	= RECORD.MULTI.PAGO<EB.SLV55.INPUT.VALUE>
		V.ALUMNO 		= RECORD.MULTI.PAGO<EB.SLV55.BENEFICIARIO>
		V.CARRERA       = RECORD.MULTI.PAGO<EB.SLV55.SERVICE.TYPE>
		V.CUOTA 		= RECORD.MULTI.PAGO<EB.SLV55.DESCRIPCION>
		
		R.NEW(TFS.LOCAL.REF)<1,PLfCmValor1> = V.NUM.CARNET 
		R.NEW(TFS.LOCAL.REF)<1,PLfCmValor3> = V.ALUMNO 
		R.NEW(TFS.LOCAL.REF)<1,PLfCmValor4> = V.CARRERA
		R.NEW(TFS.LOCAL.REF)<1,PLfCmValor5> = V.CUOTA
	END
	ELSE
		V.NUM.CARNET 	= RECORD.MULTI.PAGO<EB.SLV55.INPUT.VALUE>
		V.ALUMNO 		= RECORD.MULTI.PAGO<EB.SLV55.BENEFICIARIO>
		V.CARRERA       = RECORD.MULTI.PAGO<EB.SLV55.SERVICE.TYPE>
		V.CUOTA 		= RECORD.MULTI.PAGO<EB.SLV55.DESCRIPCION>
		
		R.NEW(TFS.LOCAL.REF)<1,PLfCmValor1> = DCOUNT(V.NUM.CARNET,VM) : ' Pagos'
		R.NEW(TFS.LOCAL.REF)<1,PLfCmValor3> = 'Universidad Tecnologica de El Salvador' 
		R.NEW(TFS.LOCAL.REF)<1,PLfCmValor4> = ''
		R.NEW(TFS.LOCAL.REF)<1,PLfCmValor5> = 'Pago Masivo de Mensualidades'
	END
RETURN

;*ESCRIBIR.ARCHIVO:
;*	DIR.NAME= 'LogColectorUTEC'
;*	R.ID   = 'LogComprobantes -':TODAY:'.txt'
;*	OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
;*		WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
;*	END
 ;* CLOSESEQ SEQ.PTR
;*RETURN

END
