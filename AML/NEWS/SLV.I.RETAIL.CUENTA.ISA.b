*-----------------------------------------------------------------------------
* <Rating>-51</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.RETAIL.CUENTA.ISA
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE 
$INSERT I_AA.APP.COMMON 
$INSERT I_F.EB.EXTERNAL.USER 
$INSERT I_F.AA.PRODUCT.ACCESS  
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY 
$INSERT I_F.AA.CUSTOMER
*-----------------------------------------------------------------------------
;*Variables necesarias para validar y correr rutina solo en la creacion de los arrangement.
EQU ACTIVITY.ACC TO 'ACCOUNTS-NEW-ARRANGEMENT'
EQU ACTIVITY.DAP TO 'DEPOSITS-NEW-ARRANGEMENT'
EQU ACTIVITY.LEN TO 'LENDING-NEW-ARRANGEMENT'
;*Nombre de la actividad en ejecucion
Y.ACTIVITY = R.NEW(AA.ARR.ACT.ACTIVITY)
;*Debug
*Y.ACTIVITY =  'ACCOUNTS-NEW-ARRANGEMENT'
IF Y.ACTIVITY EQ ACTIVITY.ACC OR Y.ACTIVITY EQ ACTIVITY.DAP OR Y.ACTIVITY EQ ACTIVITY.LEN THEN
	GOSUB INIT 
	GOSUB PROCESS 
END
RETURN    

INIT: 
	FN.EXT.USER	 = 'F.EB.EXTERNAL.USER'
	F.EXT.USER	 = ''
	CALL OPF(FN.EXT.USER, F.EXT.USER)

	FN.AA.CUS	 = 'F.AA.ARR.CUSTOMER$NAU'
	F.AA.CUS	 = ''
	CALL OPF(FN.AA.CUS, F.AA.CUS)
	
	EQU PERSONAL  TO 'PERSONAL'
	EQU ACTIVE    TO 'ACTIVE'
	EQU OFS.SOURCE TO 'SLVOFSPS'
	EQU ACCOUNTS TO 'ACCOUNTS'
	EQU DEPOSITS TO 'DEPOSITS'
	EQU LENDING TO 'LENDING'
	
	;*Leyendo variables necesarias para la adicion automatica de cuenta.
	Y.CUSTOMER		 = AA$R.ARRANGEMENT<AA.ARR.CUSTOMER>
	Y.NEW.ACC		 = AA$R.ARRANGEMENT<AA.ARR.LINKED.APPL.ID>
	Y.NEW.ARR.ID 	 = R.NEW(AA.ARR.ACT.ARRANGEMENT)
	Y.PRODUCT.LINE	 = AA$R.ARRANGEMENT<AA.ARR.PRODUCT.LINE>
	
	;*Debug
*	Y.CUSTOMER	= '139796'
*	Y.NEW.ACC	= '10000000556171'		
*	Y.NEW.ARR.ID = 'AA1800409KDW'
*	Y.PRODUCT.LINE	 = 'ACCOUNTS'
*	Y.PRODUCT.LINE	 = 'DEPOSITS'
*	Y.PRODUCT.LINE	 = 'LENDING'
RETURN 

PROCESS: 
	;*Realizar adicion si y solo si ya cuenta con un usuario de banca en linea
	SELECT.EXTERNAL	 = "SELECT " : FN.EXT.USER : " WITH CUSTOMER EQ " : "'" : Y.CUSTOMER : "'" 
	SELECT.EXTERNAL := " AND USER.TYPE EQ " :  "'"  : PERSONAL : "'" 
	SELECT.EXTERNAL := " AND STATUS EQ " : "'" : ACTIVE : "'" 
	CALL EB.READLIST (SELECT.EXTERNAL, KEYS.EXT.USERS, '', NO.OF.EXT.USERS, ERR.EXT.USERS)
	
	IF KEYS.EXT.USERS THEN
		CALL F.READ(FN.EXT.USER, KEYS.EXT.USERS<1>, REC.EXTERNAL.USER, F.EXT.USER, ERR.EXTERNAL.USER)
		Y.ID.ISA = REC.EXTERNAL.USER<EB.XU.ARRANGEMENT>
		
		;*Lectura de cuentas configurado actualmente en el product access 
		CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ID.ISA, 'PRODUCT.ACCESS', '', TODAY, R.ID.PROD.ACC, R.PRODUCT.ACCESS , ERR.PRODUCT.ACCESS)
    	REC.PRODUCT.ACCESS = RAISE(R.PRODUCT.ACCESS)
		
		;*Se anade la nueva cuenta. 
		Y.CAMPO.OFS	= ""
		IF Y.PRODUCT.LINE EQ ACCOUNTS THEN ;*Cuenta se anade para txn si no es mancomunada. Si es mancomunada, si es tipo Y: a visualizar, O: a txn			
			;*Validar si la cuenta que esta creando es mancomunada
			Y.FLAG.MANCOMUNADA = 0
			
			IF Y.FLAG.MANCOMUNADA EQ 0 THEN ;*No es mancomunada, sigue proceso natural.
				Y.CAMPO.ISA	= "ACCT.TRANS:"
				Y.CANT.ACCT.TRANS = DCOUNT(REC.PRODUCT.ACCESS<AA.PRODA.ACCT.TRANS>, @VM)		
				Y.NEW.POS.ISA = Y.CANT.ACCT.TRANS + 1 
				Y.NEW.VAL.ISA = ":1,FIELD.VALUE:1:1=" : Y.NEW.ACC

				;*Enviar OFS  
				GOSUB SEND.OFS
			END		
		END			
	END
RETURN

SEND.OFS:
  	Y.CAMPO.OFS	= Y.CAMPO.ISA : Y.NEW.POS.ISA : Y.NEW.VAL.ISA
	
	STRING.OFS.ADD.PRD	 = "" 
	STRING.OFS.ADD.PRD	:= ""
	STRING.OFS.ADD.PRD	:= ""	
	STRING.OFS.ADD.PRD	:= "" : Y.CAMPO.OFS 
	;*Envio de OFS 
	CALL OFS.POST.MESSAGE(STRING.OFS.ADD.PRD,'', OFS.SOURCE,'')
     
	;*Debug 
*	Y.TXT = "OFS FINAL -> " : STRING.OFS.ADD.PRD
*  	GOSUB WRITE_LOG_FILE
RETURN


WRITE_LOG_FILE:
	DIR.NAME = 'SIMPLIFICADA' 
	R.ID = 'ISA' : '_' : TODAY :' ':Y.NEW.ARR.ID: '.txt'
	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
		END
	CLOSESEQ SEQ.PTR 
RETURN
END
