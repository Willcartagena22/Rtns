*-----------------------------------------------------------------------------
* <Rating>-34</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.STM.RULES
*-----------------------------------------------------------------------------
*
* Nombre: SLV.V.STM.RULES
* Descripción: Rutina para Validacion de Reglas de Servicio de Transferencia Movil
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* OCornejo	06.10.2017	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.STM.RULES
$INSERT I_F.EB.SLV.GLOBAL.PARAM
$INSERT I_F.EB.SLV.KEYS.PARAMS
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
RETURN

	INIT:
		FN.GBL   = 'F.EB.SLV.GLOBAL.PARAM'
		F.GBL    = ''
		FN.KEYS = 'F.EB.SLV.KEYS.PARAMS'
		F.KEYS  = ''
		EQU STM.ID TO 'STM'
	RETURN
	
	OPENFILE:
		CALL OPF(FN.GBL, F.GBL)
		CALL OPF(FN.KEYS, F.KEYS)
	RETURN
	
	PROCESS:
		CALL F.READ(FN.KEYS, 'SLV.TOKEN', R.KEYS, F.KEYS, E.KEYS)
		FIND STM.ID IN R.KEYS<EB.SLV18.PARAM.ID> SETTING Ap, Vp THEN
			Y.DAYS.TOKEN = FIELD(R.KEYS<EB.SLV18.VALOR><Ap, Vp>, SM, 3) ;*Duracion en Dias
		END
		
		;*Validar que los Dias de Bloqueo no Superen los de Validez del Token
		IF R.NEW(EB.SLV40.DAYS.LOCKOUT) GT Y.DAYS.TOKEN THEN
			E = 'EB-SLV.DAYS.GT.TOKEN' : @FM : Y.DAYS.TOKEN
			CALL ERR
		END
		
		;*Obtener Monto Minimo Antes de Aplicar LIOF
		CALL SLV.GET.LIOF.TRX.PARAM('ACuentaTercero', '', Y.TAX, '', '', '')
		CALL SLV.GET.TAX.PARAM(Y.TAX, Y.UPTO.AMT, '')
		IF R.NEW(EB.SLV40.LIMIT.AMT) GT Y.UPTO.AMT THEN
			E = 'EB-SLV.AMT.GT.LIOF' : @FM : Y.UPTO.AMT
			CALL ERR
		END
		
		;*Validar Transacciones Diarias Mayor a Cero
		Y.MIN = 1
		IF R.NEW(EB.SLV40.DAILY.TXN) LT Y.MIN THEN
			E = 'EB-CANNOT.BE.LESS.THAN' : @FM : Y.MIN
			CALL ERR
		END
	RETURN
END
