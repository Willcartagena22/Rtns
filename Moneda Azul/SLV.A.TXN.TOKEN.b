*-----------------------------------------------------------------------------
* <Rating>-33</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.A.TXN.TOKEN
*-----------------------------------------------------------------------------
*
* Nombre: SLV.A.TXN.TOKEN
* Descripción: Rutina para Inactivacion de Condiciones de Token
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* OCornejo	13.10.2017	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.AC.LOCKED.EVENTS
$INSERT I_F.EB.SLV.STM.FAVORITES
$INSERT I_F.EB.SLV.TOKEN.VERIFY
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
RETURN

	INIT:
		FN.LOCK = 'F.AC.LOCKED.EVENTS$HIS'
		F.LOCK  = ''
		
		Y.APPL   = "AC.LOCKED.EVENTS" : FM : "TELLER.FINANCIAL.SERVICES"
		Y.FIELD  = "LF.CHQ.BENEFICI" : VM : "LF.TRAN.TIME" : FM
		Y.FIELD := "LF.NUM.REF"
		Y.POS = ""
		CALL MULTI.GET.LOC.REF(Y.APPL,Y.FIELD,Y.POS)
		LF.BENEFICIARIO = Y.POS<1,1>
		LF.TRAN.TIME    = Y.POS<1,2>
		LF.NUM.REF      = Y.POS<2,1>
	RETURN
	
	OPENFILE:
		CALL OPF(FN.LOCK, F.LOCK)
	RETURN
	
	PROCESS:
		;*Leer Registro de Bloqueo
		CALL CACHE.READ(FN.LOCK, R.NEW(TFS.LOCAL.REF)<1, LF.NUM.REF>, R.LOCK, E.LOCK)
		
		;*Registrar Condiciones de Token para que ya no se pueda Utilizar
		FN.TOKEN = 'F.EB.SLV.TOKEN.VERIFY'
		F.TOKEN  = ''
		CALL OPF(FN.TOKEN, F.TOKEN)
		
		Y.ID = 'STM.' : R.LOCK<AC.LCK.FROM.DATE> : '.' : R.LOCK<AC.LCK.LOCAL.REF><1, LF.TRAN.TIME>
		CALL CACHE.READ(FN.TOKEN, Y.ID, R.TOKEN, E.TOKEN)
		IF R.TOKEN EQ '' THEN
			R.TOKEN<EB.SLV7.DATE.APPL> = TODAY
			R.TOKEN<EB.SLV7.TIME.APPL> = TIMEDATE()[1,8]
			
			;*Datos de Auditoria
			R.TOKEN<EB.SLV7.INPUTTER> = FIELD(R.NEW(TFS.INPUTTER), '_', 2)
			R.TOKEN<EB.SLV7.AUTHORISER> = OPERATOR
			R.TOKEN<EB.SLV7.CURR.NO>		 = 1 
			X = OCONV(DATE(),"D-")
	        V$TIMEDATE = TIMEDATE()
	        V$TIMEDATE = V$TIMEDATE[1,5]
	        CONVERT ":" TO "" IN V$TIMEDATE
	        X = X[9,2] : X[1,2] : X[4,2] : V$TIMEDATE
			R.TOKEN<EB.SLV7.DATE.TIME,1>	 = X
			R.TOKEN<EB.SLV7.CO.CODE>	     = R.NEW(TFS.CO.CODE)
			R.TOKEN<EB.SLV7.DEPT.CODE>	 = R.NEW(TFS.DEPT.CODE)
			CALL F.WRITE(FN.TOKEN, Y.ID, R.TOKEN)
		END	
	RETURN
END
