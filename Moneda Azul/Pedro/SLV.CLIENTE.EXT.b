*-----------------------------------------------------------------------------
* <Rating>27</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.CLIENTE.EXT
*-----------------------------------------------------------------------------
* Nombre: SLV.CLIENTE.EXT
* Descripcion: Rutina Encargada de guardar informacion de un cliente externo en caso de no tenga registro.
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		wrivas  	13.07.18	Version inicial    
*----------------------------------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_System
$INSERT I_ENQUIRY.COMMON 
$INSERT I_F.EB.SLV.CLIENT.EXT
$INSERT I_F.ACCOUNT
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.LOCAL.REF.TABLE
$INSERT I_GTS.COMMON
$INSERT I_TSS.COMMON 
*----------------------------------------------------------------------------------------------------
	RTN = 'SLV.CLIENTE.EXT'
	
*	Y.FUNCTION = ''
*	;*Y.FUNCTION =  OFS$OPERATION
*	Y.TXT = 'VF:_ = ' :  Y.FUNCTION 
**	CRT Y.TXT
*	GOSUB WRITE_LOG_FILE
	
	IF OFS$OPERATION EQ 'BUILD' THEN  
			GOSUB INIT
			GOSUB POS.LOCAL.FIELD
			GOSUB OPEN
			GOSUB PROCESS
   	END
RETURN
*-----------------------------------------------------------------------------
INIT:
	FN.CLIENT.EXT 		= 'F.EB.SLV.CLIENT.EXT'
	F.CLIENT.EXT 		= ''	
	
	FN.L.R.TABLE = 'F.LOCAL.REF.TABLE'
	F.L.R.TABLE  = ''
	
*    Y.TXT = 'AQUI: = ' : 'estuvo'
**	CRT Y.TXT
*	GOSUB WRITE_LOG_FILE 
*	
*	Y.FUNCTION = ''
*	Y.FUNCTION =  OFS$OPERATION
*	Y.TXT = 'VF:_ = ' :  Y.FUNCTION 
**	CRT Y.TXT
*	GOSUB WRITE_LOG_FILE 
	
RETURN
*-------------------------------------------------------------------------------
POS.LOCAL.FIELD:	
;*validar que los campos sean completados
		Y.APPLI     = "TELLER.FINANCIAL.SERVICES"
		Y.FIELD     = "LF.DOC.CLIEN.EX":VM:"LF.AMOUNT"
		Y.POS.FIELD = ""
		;*Obtener posicion de campos locales
		CALL MULTI.GET.LOC.REF(Y.APPLI, Y.FIELD, Y.POS.FIELD)  
		Y.DOC.CLIENT    = Y.POS.FIELD<1,1> ;*LF.DOC.CLIEN.EX
		Y.AMOUNTTFS    	= Y.POS.FIELD<1,2> ;*LF.AMOUNT
		
		;*Obtener informacion de campos locales de R.NEW
        Y.DUI	= R.NEW(TFS.LOCAL.REF)<1,Y.DOC.CLIENT>
        ;*Y.AMOUNT = R.NEW(TFS.LOCAL.REF)<1,Y.AMOUNTTFS>
         Y.TXT = 'DUI_R.NEW: = ' : Y.DUI 
        GOSUB WRITE_LOG_FILE
RETURN	 
*-------------------------------------------------------------------------------
WRITE_LOG_FILE:
*	DIR.NAME = 'CHQ.OUT'
*	R.ID = RTN : '_' : TODAY : '.txt'
*
*	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
*		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
*		END
*	CLOSESEQ SEQ.PTR 
RETURN
*-------------------------------------------------------------------------------
OPEN:
	CALL OPF(FN.CLIENT.EXT, F.CLIENT.EXT)
	CALL OPF(FN.L.R.TABLE, F.L.R.TABLE) 
RETURN
*--------------------------------------------------------------------------------
PROCESS:	
	Y.DUI = COMI
	
    Y.TXT = 'DUI_COMI: = ' : Y.DUI 
*	CRT Y.TXT
	GOSUB WRITE_LOG_FILE 	
*========================	
*;DEBUG: 
  ;*Y.DUI = '04248893-5'
 ;* Y.DUI = '04742240-5' 
*=========================
	
	CALL System.setVariable('CURRENT.MONEDAZUL', Y.DUI)
	
	IF Y.DUI THEN 
		SCE = "SELECT " : FN.CLIENT.EXT : " WITH NUMERO.DOCUMENTO EQ '" : Y.DUI : "'"
			CALL EB.READLIST(SCE, LIST.RG, '' , NO.OF.RECS.RG, ERR.RG)
			;*CRT LIST.RG
			Y.TXT = 'NUMERO DE: = ' : NO.OF.RECS.RG :'_ ' : LIST.RG
		    ;*	CRT Y.TXT
			GOSUB WRITE_LOG_FILE 
			
		IF	NO.OF.RECS.RG EQ 1 THEN 
			;*CALL EB.SET.NEXT.TASK('EB.SLV.CLIENT.EXT,SLV.INPUT.CLIENT.EXT I 04248893-5');*: LIST.RG)
			CALL EB.SET.NEW.TASK('EB.SLV.CLIENT.EXT,SLV.INPUT.CLIENT.EXT I ':LIST.RG) 
		
		END 
		ELSE IF	NO.OF.RECS.RG EQ 0 THEN
			Y.ID = 'C.EXT' : CHANGE(TIMESTAMP(),'.','')
			;*CALL EB.SET.NEW.TASK('EB.SLV.CLIENT.EXT,SLV.INPUT.CLIENT.EXT I F3 ')
			CALL EB.SET.NEW.TASK('EB.SLV.CLIENT.EXT,SLV.INPUT.CLIENT.EXT I ':Y.ID)
		END	 					

	END		
RETURN
*----------------------------------------------------------------------------------------------------------------
*	System.setVariable(variableName,"")
*	System.getVariable(variableName)
*----------------------------------------------------------------------------------------------------------------
END
 