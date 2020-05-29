*-----------------------------------------------------------------------------
* <Rating>-51</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.STM.ACLK
*-----------------------------------------------------------------------------
*
* Nombre: SLV.V.STM.ACLK
* Descripción: Rutina para Validacion de Reglas de Servicio de Transferencia Movil
*			en AC.LOCKED.EVENTS
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
$INSERT I_F.AC.LOCKED.EVENTS
$INSERT I_F.EB.SLV.STM.RULES
$INSERT I_F.EB.SLV.GLOBAL.PARAM
$INSERT I_F.ACCOUNT
$INSERT I_GTS.COMMON 
*-----------------------------------------------------------------------------

;*Obteniendo Posiciones de Campos Locales
Y.APPL = "AC.LOCKED.EVENTS"
Y.FIELD = "LF.USER.BANCA" : VM : "LF.MOTIVO.CHQ"
Y.POS = ""
CALL MULTI.GET.LOC.REF(Y.APPL,Y.FIELD,Y.POS)
LF.USER.BANCA = Y.POS<1,1>
LF.MOTIVO     = Y.POS<1,2>
	    
;*Ejecutar Rutina solo Cuando sea para el Servicio de Transferencia Movil
IF R.NEW(AC.LCK.LOCAL.REF)<1,LF.MOTIVO> NE 'STM' THEN
	RETURN
END

GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
RETURN

	INIT:
		FN.GLOBAL = 'F.EB.SLV.GLOBAL.PARAM'
		F.GLOBAL  = ''
		FN.RULES  = 'F.EB.SLV.STM.RULES'
		F.RULES   = ''
		FN.ACC    = 'F.ACCOUNT'
		F.ACC     = ''
		FN.LOCK   = 'F.AC.LOCKED.EVENTS'
		F.LOCK    = ''
		
		Y.ID = 'SYSTEM'
	RETURN
	
	OPENFILE:
		CALL OPF(FN.GLOBAL, F.GLOBAL)
		CALL OPF(FN.RULES, F.RULES)
		CALL OPF(FN.ACC, F.ACC)
		CALL OPF(FN.LOCK, F.LOCK)
	RETURN
	
	PROCESS:
		;*Obtener Parametro de Global Param
		CALL CACHE.READ(FN.GLOBAL, 'STM.RULES', R.GLOBAL, E.GLOBAL)
		Y.PARAM = R.GLOBAL<EB.SLV39.VALOR.PARAM>
		
		;*Si el Parametro esta para Todos usar por Defecto de lo Contrario Buscar ID de Cliente
		IF Y.PARAM EQ 'CUSTOM' THEN
			;*Leer Cuenta para Obtener Cliente
			Y.ACC = R.NEW(AC.LCK.ACCOUNT.NUMBER)
			CALL CACHE.READ(FN.ACC, Y.ACC, R.ACC, E.ACC)
			Y.ID = R.ACC<AC.CUSTOMER>
		END
		
		;*Obtener Reglas de Negociacion
		CALL F.READ(FN.RULES, Y.ID, R.RULES, F.RULES, E.RULES)
		
		;*Evaluar Reglas contra Valores Ingresados
		IF R.RULES EQ '' THEN
			E = 'EB-SLV.STM.RULE.EXISTS' : @FM : Y.ID
			CALL ERR
			RETURN
		END
		
		;*Validar Cupo Activo
		IF R.RULES<EB.SLV40.STATUS> EQ 'INACTIVO' THEN
			E = 'EB-SLV.STM.RULE.INA' : @FM : Y.ID
			CALL ERR
			RETURN
		END
		
		;*Validar Monto de Transaccion
		IF R.RULES<EB.SLV40.LIMIT.AMT> LT R.NEW(AC.LCK.LOCKED.AMOUNT) THEN
			E = 'EB-SLV.STM.RULE.AMT'
			CALL ERR
		END
		
		;*Validar Numero de Transacciones Parametrizadas
		GOSUB VALIDAR.TXN.DIARIAS
		
		;*Validar que la cuenta tenga fondos suficientes
		CALL SLV.V.FONDOS.CTA(R.NEW(AC.LCK.ACCOUNT.NUMBER), R.NEW(AC.LCK.LOCKED.AMOUNT), Y.MSG)
		IF Y.MSG NE '' THEN
			Y.MSG = 'La transacción no puede ser efectuada por insuficiencia de fondos'
			Y.MSG = CHANGE(Y.MSG, CHAR(162), 'O') 	
		
			E = Y.MSG
			CALL ERR
			RETURN 
	 	END
		
		;*Calcular Fecha hasta la que existira el Bloqueo
		PROCESS.DATE = TODAY
	    DAY.COUNT = R.RULES<EB.SLV40.DAYS.LOCKOUT> : "W"
	    CALL CDT('SV05' : PROCESS.DATE[1,4], PROCESS.DATE, DAY.COUNT)
	    R.NEW(AC.LCK.TO.DATE) = PROCESS.DATE
	RETURN
	
	VALIDAR.TXN.DIARIAS:
		SELECT.STMT = "SELECT " : FN.LOCK : " WITH FROM.DATE EQ " : TODAY
		SELECT.STMT := " AND LF.MOTIVO.CHQ EQ STM"
		SELECT.STMT := " AND LF.USER.BANCA EQ " : R.NEW(AC.LCK.LOCAL.REF)<1, LF.USER.BANCA>
		SELECT.STMT := " AND RECORD.STATUS NE REVE"
		CALL EB.READLIST(SELECT.STMT, Y.LIST, '', NO.REC, SYSTEM.RETURN.CODE)
		
		IF NO.REC GT R.RULES<EB.SLV40.DAILY.TXN> THEN
			E = 'EB-SLV.STM.RULE.TXN' : @FM : R.RULES<EB.SLV40.DAILY.TXN> 
			CALL ERR
		END
	RETURN

END