*-----------------------------------------------------------------------------
* <Rating>-46</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.TFS.STM.INFO
*-----------------------------------------------------------------------------
*
* Nombre: SLV.I.TFS.STM.INFO
* Descripción: Rutina para extraccion de informacion para el Pago STM
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* OCornejo	12.10.2017	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AC.LOCKED.EVENTS
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.EB.SLV.STM.FAVORITES
$INSERT I_F.TELLER.ID
$INSERT I_F.USER
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB GETTER
GOSUB PROCESS
RETURN

	INIT:
		FN.LOCK = 'F.AC.LOCKED.EVENTS$HIS'
		F.LOCK  = ''
		FN.FAVO = 'F.EB.SLV.STM.FAVORITES'
		F.FAVO  = ''
		FN.TELLER.ID = 'F.TELLER.ID'
		F.TELLER.ID  = ''
		FN.USER = 'F.USER'
		F.USER  = ''
	RETURN
	
	OPENFILE:
		CALL OPF(FN.LOCK, F.LOCK)
		CALL OPF(FN.FAVO, F.FAVO)
		CALL OPF(FN.TELLER.ID, F.TELLER.ID)
		CALL OPF(FN.USER, F.USER)
	RETURN
	
	GETTER:		
		;*Extraer Campos Locales
		Y.APPL   = "AC.LOCKED.EVENTS" : FM : "TELLER.FINANCIAL.SERVICES"
		Y.FIELD  = "LF.CHQ.BENEFICI" : FM
		Y.FIELD := "LF.DOC.CLIEN.EX" : VM : "LF.NOM.PER"  : VM : "LF.NUM.REF"
		Y.POS = ""
		CALL MULTI.GET.LOC.REF(Y.APPL,Y.FIELD,Y.POS)
		LF.BENEFICIARIO     = Y.POS<1,1>
		LF.DUI              = Y.POS<2,1>
		LF.BENEFICIARIO.TFS = Y.POS<2,2>
		LF.NUM.REF 			= Y.POS<2,3>
		
		Y.LOCK = R.NEW(TFS.LOCAL.REF)<1,LF.NUM.REF>
	RETURN
	
	PROCESS:
		;*Obtener Registro de Bloqueo
		CALL CACHE.READ(FN.LOCK, Y.LOCK, R.LOCK, E.LOCK)
		R.NEW(TFS.ACCOUNT.DR) = R.LOCK<AC.LCK.ACCOUNT.NUMBER>
		R.NEW(TFS.AMOUNT.CR)  = R.LOCK<AC.LCK.LOCKED.AMOUNT>
		R.NEW(TFS.AMOUNT.DR)  = R.LOCK<AC.LCK.LOCKED.AMOUNT>
		
		;*Buscar Info del Favorito
		CALL CACHE.READ(FN.FAVO, R.LOCK<AC.LCK.LOCAL.REF><1, LF.BENEFICIARIO>, R.FAVO, E.FAVO)
		;*R.NEW(TFS.LOCAL.REF)<1,LF.DUI>              = R.FAVO<EB.SLV83.DOCUMENT>
		R.NEW(TFS.LOCAL.REF)<1,LF.BENEFICIARIO.TFS> = "Pago a " : R.FAVO<EB.SLV83.NAME>
		
		;*Obtener ID del Cajero
		Y.TELLER.ID = ''
		SELECT.STMT = "SELECT " : FN.TELLER.ID : " WITH USER EQ " : OPERATOR : " AND STATUS EQ OPEN"
		CALL EB.READLIST (SELECT.STMT, Y.TELLER.ID, '', NO.REC, SYSTEM.RETURN.CODE)
		
		;*Obtener Agencia del Cajero
		CALL CACHE.READ(FN.USER, OPERATOR, R.USUARIO, E.USER)
		R.NEW(TFS.SURROGATE.AC) = LCCY : "10001" : Y.TELLER.ID : RIGHT(R.USUARIO<EB.USE.COMPANY.CODE><1,1>, 4)
		R.NEW(TFS.ACCOUNT.CR)   = LCCY : "10001" : Y.TELLER.ID : RIGHT(R.USUARIO<EB.USE.COMPANY.CODE><1,1>, 4)
	RETURN
END
