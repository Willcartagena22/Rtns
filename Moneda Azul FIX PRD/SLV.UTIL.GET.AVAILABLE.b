*-----------------------------------------------------------------------------
* <Rating>-42</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.GET.AVAILABLE(CTA,AVAILABLE)
*-----------------------------------------------------------------------------
* Nombre: SLV.UTIL.GET.AVAILABLE(CTA,AVAILABLE)
* Descripcion: Rutina Encargada de devolver saldo disponible 
* Parametros: IN>CTA: Cuenta a validar.
*			  OUT>AVAILABLE: Saldo Disponible.
*---------------------------------------------------------------------------------------------------- 
* Version	Autor		Fecha		Comentario  
*----------------------------------------------------------------------------------------------------
* 1.0		iTurcios	02.04.2019	Version inicial 
*----------------------------------------------------------------------------------------------------
$INSERT I_COMMON 
$INSERT I_EQUATE
$INSERT I_F.ACCOUNT 
$INSERT I_F.AC.LOCKED.EVENTS
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN

INIT: 
	FN.ACC	 = 'F.ACCOUNT'
	F.ACC	 = ''
	CALL OPF(FN.ACC, F.ACC) 
	
	FN.AC.LOCKED.EVENTS = 'F.AC.LOCKED.EVENTS'
	F.AC.LOCKED.EVENTS = ''
	CALL OPF(FN.AC.LOCKED.EVENTS, F.AC.LOCKED.EVENTS)
	
	;*Debug:
	CTA = '10000000023197'
	
	IF CTA[1,2] EQ 'AA' THEN
		CALL SLV.UTIL.GET.ACC.X.ARR(CTA)
	END
RETURN

PROCESS:
	AVAILABLE = ""
	GOSUB GET.WORKING.BALANCE ;*Salida en Y.WORKING.BALANCE 
	
	GOSUB GET.BLOCK.AMOUNT	;*Salida en Y.SUM.LCK
	
	AVAILABLE = Y.WORKING.BALANCE - Y.SUM.LCK
RETURN

GET.WORKING.BALANCE:
	CALL F.READ(FN.ACC, CTA, REC.ACC, F.ACC, ERR.ACC)
	Y.WORKING.BALANCE = REC.ACC<AC.WORKING.BALANCE>
RETURN

GET.BLOCK.AMOUNT:
	SELECT.LOCKED.AMT = "SELECT " : FN.AC.LOCKED.EVENTS : " WITH ACCOUNT.NUMBER EQ " : "'" : CTA : "'"
	CALL EB.READLIST(SELECT.LOCKED.AMT, ID.LOCK, '', NO.OF.AC.LCK, ERR.LCK)
	
	Y.SUM.LCK = 0
	FOR LCK = 1 TO NO.OF.AC.LCK
		CALL F.READ(FN.AC.LOCKED.EVENTS, ID.LOCK<LCK>, RECORD.LCK, F.AC.LOCKED.EVENTS, ERROR.LCK)
		Y.SUM.LCK = Y.SUM.LCK + RECORD.LCK<AC.LCK.LOCKED.AMOUNT>
	NEXT LCK 
RETURN
END
