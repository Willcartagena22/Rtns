*-----------------------------------------------------------------------------
* <Rating>-49</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.M.TCIB.BLUE.COINS.PRODS
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
* 1.0		psanchez	27.07.18	Rutina encargada de realizar una carga masiva que anade los productos los parametros de moneda azul para TCIB.

*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.EXTERNAL.USER 
$INSERT I_F.AA.PRODUCT.ACCESS
$INSERT I_F.CUSTOMER.ACCOUNT
$INSERT I_F.AA.ARRANGEMENT 
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY  
$INSERT I_F.AA.CUSTOMER
$INSERT I_F.AA.PROTECTION.LIMIT

*-----------------------------------------------------------------------------
GOSUB INIT 
GOSUB PROCESS 
RETURN


INIT:
	FN.EXT.USER	 = 'F.EB.EXTERNAL.USER'
	F.EXT.USER	 = ''
	CALL OPF(FN.EXT.USER, F.EXT.USER)

	FN.CUS.ACC	 = 'F.CUSTOMER.ACCOUNT'
	F.CUS.ACC	 = ''
	CALL OPF(FN.CUS.ACC,F.CUS.ACC)
	
	FN.ARR	 = 'F.AA.ARRANGEMENT'
	F.ARR	 = ''
	CALL OPF(FN.ARR,F.ARR)
	
	FN.AA.CUS	 = 'F.AA.ARR.CUSTOMER'
	F.AA.CUS	 = ''
	CALL OPF(FN.AA.CUS, F.AA.CUS)
	
	FN.PRODUCT = 'F.AA.PRODUCT'
	F.PRODUCT = ''
	CALL OPF(FN.PRODUCT, F.PRODUCT)
	
	EQU PERSONAL  TO 'PERSONAL'
	EQU ACTIVE    TO 'ACTIVE'
	EQU OFS.SOURCE TO 'SLVOFSPS'
	
	EQU Y.ID.PARAM.OFS TO 'OFS.TCIB.ADD.PRD'
	
	APPL.ARR ='AA.PRD.DES.PRODUCT.ACCESS' 
	FIELDNAME.ARR	='LF.MONEDA.AZUL':VM:'LF.MINIMUM.AZUL':VM:'LF.MAXIMUM.AZUL':VM:'LF.PERCENT.AZUL':VM:'LF.DL.AZUL'
	CALL MULTI.GET.LOC.REF(APPL.ARR,FIELDNAME.ARR,POS.ARR)
RETURN
PROCESS:
;*Realizar adicion si y solo si ya cuenta con un usuario de banca en linea
	SELECT.EXTERNAL	 = "SELECT " : FN.EXT.USER : " WITH USER.TYPE EQ " :  "'" : PERSONAL : "'"	  
	SELECT.EXTERNAL := " AND STATUS EQ " : "'" : ACTIVE : "'" 
	CALL EB.READLIST (SELECT.EXTERNAL, KEYS.EXT.USERS, '', NO.OF.EXT.USERS, ERR.EXT.USERS)
	
	IF KEYS.EXT.USERS THEN
		FOR I = 1 TO NO.OF.EXT.USERS 
			CALL F.READ(FN.EXT.USER, KEYS.EXT.USERS<I>, REC.EXTERNAL.USER, F.EXT.USER, ERR.EXTERNAL.USER)
			Y.ID.ISA = REC.EXTERNAL.USER<EB.XU.ARRANGEMENT>
			Y.ID.CUS = REC.EXTERNAL.USER<EB.XU.CUSTOMER>
			
			
			;*Variables que contendran los arreglos con los valores a enviar via OFS.
			Y.LF.MONEDA.AZUL	  = ""
			Y.LF.MINIMUM.AZUL	  = ""
			Y.LF.MAXIMUM.AZUL	  = ""
			Y.LF.PERCENT.AZUL	  = ""
			Y.LF.DL.AZUL		  = ""
			
			;*Enviar OFS  
			GOSUB SEND.OFS			
		NEXT I
	END
RETURN

SEND.OFS:	
	
	Y.STRING.OFS.AUX	= ""
		Y.LF.MONEDA.AZUL="SI"
		Y.LF.MINIMUM.AZUL=0.20
		Y.LF.MAXIMUM.AZUL=3.00
		Y.LF.PERCENT.AZUL='0.00'
		Y.LF.DL.AZUL=300

	
 
	;*Completar string para envio de OFS
	STRING.OFS.ADD.PRD	 = "" 
	STRING.OFS.ADD.PRD	:= "AA.ARRANGEMENT.ACTIVITY,SLV.TCIB.BLUE.PRDS/I/PROCESS//0,"
	STRING.OFS.ADD.PRD	:= ",,ARRANGEMENT:1:1=" : Y.ID.ISA : ",ACTIVITY:1:1=INTERNET.SERVICES-UPDATE-PRODACCESS,CURRENCY:1:1=USD,PROPERTY:1:1=PRODACCESS,"	
	STRING.OFS.ADD.PRD	:= "FIELD.NAME:1:1=LF.MONEDA.AZUL,FIELD.VALUE:1:1=": Y.LF.MONEDA.AZUL:",FIELD.NAME:1:2=LF.MINIMUM.AZUL,FIELD.VALUE:1:2=": Y.LF.MINIMUM.AZUL :","
	STRING.OFS.ADD.PRD	:= "FIELD.NAME:1:3=LF.MAXIMUM.AZUL,FIELD.VALUE:1:3=": Y.LF.MAXIMUM.AZUL:",FIELD.NAME:1:4=LF.PERCENT.AZUL,FIELD.VALUE:1:4=":Y.LF.PERCENT.AZUL:","
	STRING.OFS.ADD.PRD	:= "FIELD.NAME:1:5=LF.DL.AZUL,FIELD.VALUE:1:5=":Y.LF.DL.AZUL:","
*	STRING.OFS.ADD.PRD	:= Y.STRING.OFS.AUX


	
*	IF KEYS.EXT.USERS<I> EQ 'PSANCHEZBI' OR KEYS.EXT.USERS<I> EQ 'RGARAYBI' OR KEYS.EXT.USERS<I> EQ 'ALRODRIGUEZ' THEN
*		CRT KEYS.EXT.USERS<I>: "/" : Y.ID.CUS  : "->" : STRING.OFS.ADD.PRD
*		CALL OFS.POST.MESSAGE(STRING.OFS.ADD.PRD,'', OFS.SOURCE,'')
*			
*		Y.TXT = KEYS.EXT.USERS<I>: "/" : Y.ID.CUS  : "->" : STRING.OFS.ADD.PRD 
*		GOSUB WRITE_LOG_FILE		 
*	END 	 
	CRT "OFS>" : I : " DE " : NO.OF.EXT.USERS:" STRING ":STRING.OFS.ADD.PRD
		 
	;*Envio de OFS  
	CALL OFS.POST.MESSAGE(STRING.OFS.ADD.PRD,'', OFS.SOURCE,'')
    ;*Para aplicar cambios.
	CALL JOURNAL.UPDATE ('')
	;*Debug 
	Y.TXT = KEYS.EXT.USERS<I>: "/" : Y.ID.CUS  : "->" : STRING.OFS.ADD.PRD 
  	GOSUB WRITE_LOG_FILE
RETURN




WRITE_LOG_FILE:
	DIR.NAME = 'SIF.OUT' 
	R.ID = 'LOG_OFS_ADD_COINS' : '_' : TODAY : '.txt'
	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
		END
	CLOSESEQ SEQ.PTR 
RETURN
END