*-----------------------------------------------------------------------------
* <Rating>-98</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.VALIDA.FIELD.MONEDAZUL
* Nombre:	SLV.VALIDA.FIELD.MONEDAZUL  
* Referencia:
* Descripcion: Rutina encargada de validar que los campos sean ingresados correctamente
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		wrivas		18-07-2018	Version Inicial
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.LOCAL.REF.TABLE
$INSERT I_GTS.COMMON
$INSERT I_TSS.COMMON 
$INSERT I_F.EB.SLV.CLIENT.EXT
$INSERT I_F.EB.SLV.STM.FAVORITES
$INSERT I_F.AC.LOCKED.EVENTS
*-----------------------------------------------------------------------------

	GOSUB INIT 
	GOSUB POS.LOCAL.FIELD
	GOSUB PROCESS
	
*-----------------------------------------------------------------------------
INIT:

	FN.TFS  	= 'F.TELLER.FINANCIAL.SERVICES'
	F.TFS   	= ''
	CALL OPF(FN.TFS, F.TFS)
	
	FN.LOCK = 'F.AC.LOCKED.EVENTS$HIS'
	F.LOCK  = ''
	CALL OPF(FN.LOCK, F.LOCK)
		
	FN.L.R.TABLE = 'F.LOCAL.REF.TABLE'
	F.L.R.TABLE  = ''
	CALL OPF(FN.L.R.TABLE, F.L.R.TABLE) 
	
	FN.CLIENT.EXT 		= 'F.EB.SLV.CLIENT.EXT'
	F.CLIENT.EXT 		= ''
	CALL OPF(FN.CLIENT.EXT, F.CLIENT.EXT)
	
	FN.FAVO = 'F.EB.SLV.STM.FAVORITES'
	F.FAVO  = '' 
	CALL OPF(FN.FAVO, F.FAVO)
	
	EQU RTN TO 'SLV.VALIDA.FIELD.MONEDAZUL'
	
RETURN 
POS.LOCAL.FIELD:	
;*validar que los campos sean completados
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
		;*Y.LOCK = 'ACLK1522124031'
			Y.TXT = 'AC_:  = ' : Y.LOCK
			GOSUB WRITE_LOG_FILE
RETURN	
*-------------------------------------------------------------------------------------------
 
PROCESS:

		IF OFS$OPERATION EQ 'VALIDATE' OR OFS$OPERATION EQ 'PROCESS' THEN
				GOSUB REGISTROS.EXIST
				GOSUB VALIDAR.CAMPO
				GOSUB LIMIT.CAJERO
				GOSUB DUI.DIFERENTE
		END 
RETURN 
*----------------------------------------------------------------------------------------------
*Registro de cliente externo no existe
REGISTROS.EXIST:	
		Y.DUI	= R.NEW(TFS.LOCAL.REF)<1,LF.DUI>
**		Y.TXT = 'DUI  = ' : Y.DUI
**		GOSUB WRITE_LOG_FILE
*Y.DUI = '04567845124'
*		
		IF Y.DUI THEN
		  SCE = "SELECT " : FN.CLIENT.EXT : " WITH NUMERO.DOCUMENTO EQ '" : Y.DUI : "'"
			CALL EB.READLIST(SCE, LIST.RG, '' , NO.OF.RECS.RG, ERR.RG)
			 
			IF	NO.OF.RECS.RG EQ 0 THEN
*				AF     = TFS.LOCAL.REF
*		    	AV     = 
				E = 'AA-VAL.FIELD.N'
			 	CALL ERR
		 	END 
		END		
RETURN
*----------------------------------------------------------------------------------------------
*Compo dui esta vacio 
VALIDAR.CAMPO:	
Y.DUI	= R.NEW(TFS.LOCAL.REF)<1,LF.DUI>
		IF  Y.DUI  EQ '' THEN 
			AF     = TFS.LOCAL.REF
    		AV     = Y.DOC.CLIENT

			ETEXT = 'AA-VAL.FIELD'
	 		CALL STORE.END.ERROR 
		END
RETURN		
*----------------------------------------------------------------------------------------------
*Monto es mayor al permitido por transaccion 
LIMIT.CAJERO:
	 IF Y.AMOUN GT '1000' THEN
	 	TEXT = 'AA.AMOUNT.MAX'
        CURR.NO =V_NAME_FIELD
       CALL STORE.OVERRIDE(CURR.NO)
	 END 
RETURN
*---------------------------------------------------------------------------------------------
*Dui es diferente al que registro el cliente
DUI.DIFERENTE:
	Y.DUIR.NEW	= R.NEW(TFS.LOCAL.REF)<1,LF.DUI>
	    Y.TXT = 'DUI_RNEW:  = ' :  Y.DUIR.NEW
	    GOSUB WRITE_LOG_FILE
	;*Y.DUI = '465465354'
		CALL CACHE.READ(FN.LOCK, Y.LOCK, R.LOCK, E.LOCK)
		Y.BENEFIC=  R.LOCK<AC.LCK.LOCAL.REF><1, LF.BENEFICIARIO>
		 ;*AC.LOCKED.ITEM.REC<AC.LCK.LOCAL.REF,Y.POS.BENEFIC>
		Y.TXT = 'ID.BENE:  = ' : Y.BENEFIC
		GOSUB WRITE_LOG_FILE		
		
		CALL CACHE.READ(FN.FAVO, R.LOCK<AC.LCK.LOCAL.REF><1, LF.BENEFICIARIO>, R.FAVO, E.FAVO)
		Y.DUI.FAV = R.FAVO<EB.SLV83.DOCUMENT>
	        Y.TXT = 'DUI_FAV:  = ' :  Y.DUI.FAV
	       GOSUB WRITE_LOG_FILE
*	
*	DF = "SELECT " : FN.FAVO : " WITH DOCUMENT EQ '" : Y.DUI : "'"
*	DF = "SELECT " : FN.STF : " WITH TARJETA EQ '" : Y.TARJETA : "' AND OWNING.CUSTOMER EQ " : Y.CUSTOMER 
*			CALL EB.READLIST(DF, D.RG, '' , NO.OF.DUI.RG, ERR.RG)
**		Y.TXT = 'REGISTRO_FAVORITO :  = ' :  D.RG  : NO.OF.DUI.RG
**		GOSUB WRITE_LOG_FILE
**	 SCE = "SELECT " : FN.CLIENT.EXT : " WITH NUMERO.DOCUMENTO EQ '" : Y.DUI : "'"
**			CALL EB.READLIST(SCE, LIST.RG, '' , NO.OF.RECS.RG, ERR.RG)
*	
	IF	Y.DUIR.NEW NE Y.DUI.FAV THEN 
			E = 'AA-DOC.DIFERENT'
			CALL ERR
	END 	 							 
RETURN
*---------------------------------------------------------------------------------------------
**;* Archivo para Revisión de Errores   
WRITE_LOG_FILE:
*	DIR.NAME = 'CHQ.OUT'  
*	R.ID = RTN : '_' : TODAY : '.txt'     
*  
*	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR 
*		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN 
*		END
*	CLOSESEQ SEQ.PTR  
RETURN
*-----------------------------------------------------------------------------		
			
			
			
			