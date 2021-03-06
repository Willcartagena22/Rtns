*-----------------------------------------------------------------------------
* <Rating>-71</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.M.TCIB.ADD.MASIV.PRODS
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History
* 1.0		iTurcios	11.07.18	Rutina encargada de realizar una carga masiva que a�ade los productos al ISA para TCIB.					 				 
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.EXTERNAL.USER 
$INSERT I_F.AA.PRODUCT.ACCESS
$INSERT I_F.CUSTOMER.ACCOUNT
$INSERT I_F.AA.ARRANGEMENT 
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY 
$INSERT I_F.AA.CUSTOMER
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
	
	EQU PERSONAL  TO 'PERSONAL'
	EQU ACTIVE    TO 'ACTIVE'
	EQU OFS.SOURCE TO 'SLVOFSPS'
	EQU ACCOUNTS TO 'ACCOUNTS'
	EQU DEPOSITS TO 'DEPOSITS'
	EQU LENDING TO 'LENDING'
	EQU Y.ID.PARAM.OFS TO 'OFS.TCIB.ADD.PRD'		
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
			
			;*Lectura de los productos creados hasta la fecha para cliente.
			CALL F.READ (FN.CUS.ACC,Y.ID.CUS,REC.CUS.ACC,F.CUS.ACC,ERR.CUS.ACC)			
			Y.CANT.PRODS.CUSTOMER	= DCOUNT(REC.CUS.ACC, @FM)

			;*Variables que contendran los arreglos con las cuentas a enviar via OFS.
			Y.ARRAY.ACC.TRANS	  = ""
			Y.ARRAY.ACC.SEE 	  = ""
			Y.ARRAY.LENDING.TRANS = ""
			Y.ARRAY.DAP.SEE		  = ""
			
			FOR Y.PRODS.CUSTOMER = 1 TO Y.CANT.PRODS.CUSTOMER
				Y.CURR.ACC	= REC.CUS.ACC<Y.PRODS.CUSTOMER>
				GOSUB VALIDAR.TIPO.PRODUCTO	
			NEXT Y.PRODS.CUSTOMER
									
			;*Enviar OFS  
			GOSUB SEND.OFS			
		NEXT I
	END
RETURN

VALIDAR.TIPO.PRODUCTO: 
	;*Lectura
	Y.CURR.ACC.AUX	= Y.CURR.ACC
	;*Debug
	CALL SLV.UTIL.GET.ARR.X.ACC(Y.CURR.ACC.AUX)
	Y.CURR.ARR	= Y.CURR.ACC.AUX
	;*Lectura del Arrangement de la cuenta.
	CALL F.READ(FN.ARR, Y.CURR.ARR, REC.ARR, F.ARR, ERR.ARR)		
	Y.PRODUCT.LINE	= REC.ARR<AA.ARR.PRODUCT.LINE>
	
	IF Y.PRODUCT.LINE EQ ACCOUNTS THEN						
		;*Leyendo propiedad CUSTOMER para saber si es mancomunada la cuenta. 
		CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.CURR.ARR, 'CUSTOMER', '', TODAY, R.ID.ARR.CUS, R.ARR.CUS , ERR.ARR.CUS)
	    REC.ARR.CUS = RAISE(R.ARR.CUS)
	
		;*Other Party: Contiene unicos de los clientes que son parte de la cuenta mancomunada.
		Y.AA.CUS.OTHER.PARTY = REC.ARR.CUS<AA.CUS.OTHER.PARTY>
		;*Con Indis: Bandera que determina el tipo de Cuenta Mancomunada (Y/O)
		CALL GET.LOC.REF("AA.ARR.CUSTOMER", "LF.CON.INDIS", POS.CON.INDIS)
		Y.AA.CON.INDIS = REC.ARR.CUS<AA.CUS.LOCAL.REF><1,POS.CON.INDIS>
		
		IF Y.AA.CUS.OTHER.PARTY THEN ;*Es mancomunada
			IF Y.AA.CON.INDIS EQ 'Y' THEN
				Y.ARRAY.ACC.SEE<-1>	= Y.CURR.ACC
			END
			IF Y.AA.CON.INDIS EQ 'O' THEN
				Y.ARRAY.ACC.TRANS<-1> = Y.CURR.ACC				
			END
		END ELSE ;*Crearla a transaccionar		
			Y.ARRAY.ACC.TRANS<-1> = Y.CURR.ACC
		END		
	END
	IF Y.PRODUCT.LINE EQ DEPOSITS THEN ;*Cuenta se anade para visualizar
		Y.ARRAY.DAP.SEE<-1>	= Y.CURR.ARR
	END  				
	IF Y.PRODUCT.LINE EQ LENDING THEN ;*Para cuenta se anade para transaccionar
		Y.ARRAY.LENDING.TRANS<-1>	= Y.CURR.ARR
	END	 
RETURN

SEND.OFS:	
	Y.COUNT.ARRAY.ACC.TRANS		= DCOUNT(Y.ARRAY.ACC.TRANS, @FM)
	Y.COUNT.ARRAY.ACC.SEE		= DCOUNT(Y.ARRAY.ACC.SEE, @FM)
	Y.COUNT.ARRAY.DAP.SEE		= DCOUNT(Y.ARRAY.DAP.SEE, @FM) 
	Y.COUNT.ARRAY.LENDING.TRANS	= DCOUNT(Y.ARRAY.LENDING.TRANS, @FM)
	
	Y.STRING.OFS.AUX	= ""
	Y.ITERADOR.AUX		= 0
	;*Creando String del OFS para cuentas a Transaccionar
	IF Y.COUNT.ARRAY.ACC.TRANS GT 0 THEN
		Y.STRING.OFS.AUX = "FIELD.NAME:1:" : Y.ITERADOR.AUX + 1 : "=ACCT.TRANS:1-,"
		Y.ITERADOR.AUX +=1
		FOR I.ACC.TRANS = 1 TO Y.COUNT.ARRAY.ACC.TRANS								
			Y.STRING.OFS.AUX := "FIELD.NAME:1:" : Y.ITERADOR.AUX + 1 : "=ACCT.TRANS:": I.ACC.TRANS : ":1,FIELD.VALUE:1:": Y.ITERADOR.AUX + 1 : "=" : Y.ARRAY.ACC.TRANS<I.ACC.TRANS> : ","
			Y.ITERADOR.AUX +=1
		NEXT I.ACC.TRANS 
	END	
	;*Creando String del OFS para cuentas a Visualizar 
	IF Y.COUNT.ARRAY.ACC.SEE GT 0 THEN
		Y.STRING.OFS.AUX := "FIELD.NAME:1:" : Y.ITERADOR.AUX + 1 : "=ACCT.SEE:1-,"
		Y.ITERADOR.AUX +=1
		FOR I.ACC.SEE = 1 TO Y.COUNT.ARRAY.ACC.SEE 								
			Y.STRING.OFS.AUX := "FIELD.NAME:1:" : Y.ITERADOR.AUX + 1 : "=ACCT.SEE:": I.ACC.SEE : ":1,FIELD.VALUE:1:": Y.ITERADOR.AUX + 1 : "=" : Y.ARRAY.ACC.SEE<I.ACC.SEE> : ","
			Y.ITERADOR.AUX +=1
		NEXT I.ACC.SEE 
	END
	 ;*Creando String del OFS para Prestamos (a transaccionar) 
	IF Y.COUNT.ARRAY.LENDING.TRANS GT 0 THEN
		Y.STRING.OFS.AUX := "FIELD.NAME:1:" : Y.ITERADOR.AUX + 1 : "=ARRGT.TRANS:1-,"
		Y.ITERADOR.AUX +=1
		FOR I.LEN.SEE = 1 TO Y.COUNT.ARRAY.LENDING.TRANS 								
			Y.STRING.OFS.AUX := "FIELD.NAME:1:" : Y.ITERADOR.AUX + 1 : "=ARRGT.TRANS:": I.LEN.SEE : ":1,FIELD.VALUE:1:": Y.ITERADOR.AUX + 1 : "=" : Y.ARRAY.LENDING.TRANS<I.LEN.SEE> : ","
			Y.ITERADOR.AUX +=1
		NEXT I.LEN.SEE 
	END		
	;*Creando String del OFS para Daps (a visualizar) 
	IF Y.COUNT.ARRAY.DAP.SEE GT 0 THEN
		Y.STRING.OFS.AUX := "FIELD.NAME:1:" : Y.ITERADOR.AUX + 1 : "=ARRGT.SEE:1-,"
		Y.ITERADOR.AUX +=1
		FOR I.DAP.SEE = 1 TO Y.COUNT.ARRAY.DAP.SEE 								
			Y.STRING.OFS.AUX := "FIELD.NAME:1:" : Y.ITERADOR.AUX + 1 : "=ARRGT.SEE:": I.DAP.SEE : ":1,FIELD.VALUE:1:": Y.ITERADOR.AUX + 1 : "=" : Y.ARRAY.DAP.SEE<I.DAP.SEE> : ","
			Y.ITERADOR.AUX +=1
		NEXT I.DAP.SEE 
	END		
	;*Completar string para envio de OFS
	STRING.OFS.ADD.PRD	 = "" 
	STRING.OFS.ADD.PRD	:= "AA.ARRANGEMENT.ACTIVITY,SLV.TCIB.ADD.PRDS/I/PROCESS//0,/"
	STRING.OFS.ADD.PRD	:= ",,ARRANGEMENT:1:1=" : Y.ID.ISA : ",ACTIVITY:1:1=INTERNET.SERVICES-UPDATE-PRODACCESS,CURRENCY:1:1=USD,PROPERTY:1:1=PRODACCESS,"	
	STRING.OFS.ADD.PRD	:= Y.STRING.OFS.AUX 
	
*	IF KEYS.EXT.USERS<I> EQ 'PSANCHEZBI' OR KEYS.EXT.USERS<I> EQ 'RGARAYBI' OR KEYS.EXT.USERS<I> EQ 'ALRODRIGUEZ' THEN
*		CRT KEYS.EXT.USERS<I>: "/" : Y.ID.CUS  : "->" : STRING.OFS.ADD.PRD
*		CALL OFS.POST.MESSAGE(STRING.OFS.ADD.PRD,'', OFS.SOURCE,'')
*			
*		Y.TXT = KEYS.EXT.USERS<I>: "/" : Y.ID.CUS  : "->" : STRING.OFS.ADD.PRD 
*		GOSUB WRITE_LOG_FILE		 
*	END 
	 
	CRT "OFS>" : I : " DE " : NO.OF.EXT.USERS 
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
	R.ID = 'LOG_OFS_ADD_PRODUCTOS' : '_' : TODAY : '.txt'
	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
		END
	CLOSESEQ SEQ.PTR 
RETURN
END
