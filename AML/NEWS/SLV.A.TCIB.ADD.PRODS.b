*-----------------------------------------------------------------------------
* <Rating>-72</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.A.TCIB.ADD.PRODS  
*-----------------------------------------------------------------------------
* Modification History
* 1.0		iTurcios	27.06.18	Rutina encargada de anadir automaticamente al ISA los productos ACCOUNTS, LENDING, DEPOSITS 
*								    se coloco en version autorizacion de creacion de producto AA.ARRANGEMENT.ACTIVITY,AA
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
			GOSUB VALIDA.ACC.MANCOMUNADA			
			IF Y.FLAG.MANCOMUNADA EQ 0 THEN ;*No es mancomunada, sigue proceso natural.
				Y.CAMPO.ISA	= "ACCT.TRANS:"
				Y.CANT.ACCT.TRANS = DCOUNT(REC.PRODUCT.ACCESS<AA.PRODA.ACCT.TRANS>, @VM)		
				Y.NEW.POS.ISA = Y.CANT.ACCT.TRANS + 1 
				Y.NEW.VAL.ISA = ":1,FIELD.VALUE:1:1=" : Y.NEW.ACC

				;*Enviar OFS  
				GOSUB SEND.OFS
			END		
		END
		IF Y.PRODUCT.LINE EQ DEPOSITS THEN ;*Cuenta se anade para visualizar
			Y.CAMPO.ISA	= "ARRGT.SEE:"
			Y.CANT.ARRGT.SEE = DCOUNT(REC.PRODUCT.ACCESS<AA.PRODA.ARRGT.SEE>, @VM)		
			Y.NEW.POS.ISA = Y.CANT.ARRGT.SEE + 1
			Y.NEW.VAL.ISA = ":1,FIELD.VALUE:1:1=" : Y.NEW.ARR.ID

			;*Enviar OFS  
			GOSUB SEND.OFS
		END  				
		IF Y.PRODUCT.LINE EQ LENDING THEN ;*Para cuenta se anade para transaccionar
			Y.CAMPO.ISA	= "ARRGT.TRANS:"
			Y.CANT.ARRGT.TRANS	= DCOUNT(REC.PRODUCT.ACCESS<AA.PRODA.ARRGT.TRANS>, @VM)		
			Y.NEW.POS.ISA = Y.CANT.ARRGT.TRANS + 1 
			Y.NEW.VAL.ISA = ":1,FIELD.VALUE:1:1=" : Y.NEW.ARR.ID

			;*Enviar OFS  
			GOSUB SEND.OFS	
		END		
	END
RETURN

SEND.OFS:
  	Y.CAMPO.OFS	= Y.CAMPO.ISA : Y.NEW.POS.ISA : Y.NEW.VAL.ISA
	
	STRING.OFS.ADD.PRD	 = "" 
	STRING.OFS.ADD.PRD	:= "AA.ARRANGEMENT.ACTIVITY,SLV.TCIB.ADD.PRDS/I/PROCESS//0,/"
	STRING.OFS.ADD.PRD	:= ",,ARRANGEMENT:1:1=" : Y.ID.ISA : ",ACTIVITY:1:1=INTERNET.SERVICES-UPDATE-PRODACCESS,CURRENCY:1:1=USD,PROPERTY:1:1=PRODACCESS,"	
	STRING.OFS.ADD.PRD	:= "FIELD.NAME:1:1=" : Y.CAMPO.OFS 
	;*Envio de OFS 
	CALL OFS.POST.MESSAGE(STRING.OFS.ADD.PRD,'', OFS.SOURCE,'')
     
	;*Debug 
*	Y.TXT = "OFS FINAL -> " : STRING.OFS.ADD.PRD
*  	GOSUB WRITE_LOG_FILE
RETURN

VALIDA.ACC.MANCOMUNADA:
	;*Se lee la propiedad AA.ARR.CUSTOMER$NAU para conocer el tipo de cuenta Mancomunada o no.
	SELECT.AA.ARR.CUS = "SELECT " : FN.AA.CUS : " WITH @ID LIKE ..." : Y.NEW.ARR.ID : "-CUSTOMER..."
	CALL EB.READLIST (SELECT.AA.ARR.CUS, ID.ARR.CUS, '', NO.OF.ARR.CUS, ARR.CUS.ERR)
	;*Leyendo registro en INAU
	CALL F.READ(FN.AA.CUS, ID.ARR.CUS, REC.ARR.CUS, F.AA.CUS, ERR.ARR.CUS)
	
	;*Other Party: Contiene unicos de los clientes que son parte de la cuenta mancomunada.
	Y.AA.CUS.OTHER.PARTY = REC.ARR.CUS<AA.CUS.OTHER.PARTY>	
	
	IF Y.AA.CUS.OTHER.PARTY THEN ;*Es una cuenta mancomunada
		Y.FLAG.MANCOMUNADA = 1		
		;*Con Indis: Bandera que determina el tipo de Cuenta Mancomunaada (Y/O)
		CALL GET.LOC.REF("AA.ARR.CUSTOMER", "LF.CON.INDIS", POS.CON.INDIS)
		Y.AA.CON.INDIS = REC.ARR.CUS<AA.CUS.LOCAL.REF><1,POS.CON.INDIS>
		
		;*Tipo Y: Se coloca solo al Primario a visualizar										
		IF Y.AA.CON.INDIS EQ 'Y' THEN ;*Se coloca a ver solamente al primario a visualizar
			Y.CAMPO.ISA	= "ACCT.SEE:"
			Y.CANT.ACCT.SEE = DCOUNT(REC.PRODUCT.ACCESS<AA.PRODA.ACCT.SEE>, @VM)		
			Y.NEW.POS.ISA = Y.CANT.ACCT.SEE + 1 
			Y.NEW.VAL.ISA = ":1,FIELD.VALUE:1:1=" : Y.NEW.ACC
   			
			;*Enviar OFS  
			GOSUB SEND.OFS
		END
		;*Tipo O: Se coloca al primario a transaccionar.
		IF Y.AA.CON.INDIS EQ 'O' THEN ;*Se coloca solamente al primario a transaccionar
			Y.CAMPO.ISA	= "ACCT.TRANS:"
			Y.CANT.ACCT.TRANS = DCOUNT(REC.PRODUCT.ACCESS<AA.PRODA.ACCT.TRANS>, @VM)		
			Y.NEW.POS.ISA = Y.CANT.ACCT.TRANS + 1 
			Y.NEW.VAL.ISA = ":1,FIELD.VALUE:1:1=" : Y.NEW.ACC
   			
			;*Enviar OFS  
			GOSUB SEND.OFS 
		END						
	END		
RETURN

WRITE_LOG_FILE:
	DIR.NAME = 'SIF.OUT' 
	R.ID = 'LOG_ADICION_CTAS' : '_' : TODAY : '.txt'
	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
		END
	CLOSESEQ SEQ.PTR 
RETURN
END
