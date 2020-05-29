*-----------------------------------------------------------------------------
* <Rating>-32</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.GET.CHNL.TXN.PARAM(TXN.ID, TXN.CODE, TXN.PARAMS)
**-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
*
* Nombre: SLV.GET.CHNL.TXN.PARAM
* Descripción: Rutina para Extraer Paramatros de Transaccion
*
*-----------------------------------------------------------------------------
* Modification History :
* Autor		Fecha		Comentario
* OCornejo	09.03.2016	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.UIF.CHNL.TRX.PARAM
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
RETURN
	
	INIT:
		FN.TXN.PARAM = 'F.EB.SLV.UIF.CHNL.TRX.PARAM'
		F.TXN.PARAM  = ''
	RETURN
	
	OPENFILE:
		CALL OPF(FN.TXN.PARAM, F.TXN.PARAM)
	RETURN
	
	PROCESS:
		;*Leer Registro de Transacciones Parametrizadas
		;*---------------------------------------------
		CALL F.READ (FN.TXN.PARAM, TXN.ID, R.TXN.PARAM, F.TXN.PARAM, ERR.TXN.PARAM)
		
		;*Recorrer Multivalor
		;*-------------------
		NO.REC = DCOUNT(R.TXN.PARAM, FM)
		FOR I = 1 TO NO.REC
		
			;*Buscar Coincidencia en Codigos de Transaccion
			;*---------------------------------------------
			IF R.TXN.PARAM<EB.SLV73.TXN.TYPE, I> EQ TXN.CODE THEN
				ARR = ''
				ARR := R.TXN.PARAM<EB.SLV73.TXN.TYPE, I>   : "*"
				ARR := R.TXN.PARAM<EB.SLV73.AMOUNT, I>     : "*"
				ARR := R.TXN.PARAM<EB.SLV73.ALERT, I>      : "*"
				ARR := R.TXN.PARAM<EB.SLV73.FREQ.ALERT, I> : "*"				
				TXN.PARAMS<-1> = ARR
			END
		NEXT I
	RETURN
END
