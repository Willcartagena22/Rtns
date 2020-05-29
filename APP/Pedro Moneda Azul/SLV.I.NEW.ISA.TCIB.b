*-----------------------------------------------------------------------------
* <Rating>-72</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.NEW.ISA.TCIB
*-----------------------------------------------------------------------------
* Rutina que crea el acuerdo ISA desde banca en linea
*-----------------------------------------------------------------------------
* Modification History :
* Date              who          Reference          description
*07-FEB-19       PSANCHEZ          25674			initial version
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------
$INSERT I_F.CUSTOMER
$INSERT I_F.AA.PRODUCT.ACCESS
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY 
$INSERT I_F.CUSTOMER.ACCOUNT
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.AA.CUSTOMER
$INSERT I_AA.APP.COMMON
$INSERT I_AA.LOCAL.COMMON

GOSUB INIT
GOSUB PROCESS
RETURN
INIT:
	FN.EXT.USER = 'F.EB.EXTERNAL.USER'
	F.EXT.USER	= ''	
	CALL OPF(FN.EXT.USER,F.EXT.USER)
		
    FN.ARR	 = 'F.AA.ARRANGEMENT'
	F.ARR	 = ''
	CALL OPF(FN.ARR,F.ARR)
	
	FN.CUS.ACC	 = 'F.CUSTOMER.ACCOUNT'
	F.CUS.ACC	 = ''
	CALL OPF(FN.CUS.ACC,F.CUS.ACC)
	
	FN.AA.CUS	 = 'F.AA.ARR.CUSTOMER'
	F.AA.CUS	 = ''
	CALL OPF(FN.AA.CUS, F.AA.CUS)
	
	 FN.CUS = 'F.CUSTOMER'
    F.CUS = ''
    CALL OPF(FN.CUS,F.CUS)
	
	
    EQU PERSONAL  TO 'PERSONAL'
	EQU ACCOUNTS TO 'ACCOUNTS'
	EQU DEPOSITS TO 'DEPOSITS'
	EQU LENDING TO 'LENDING'
	EQU CUST TO 'CUSTOMER'
	
		APPL.ARR ='AA.PRD.DES.PRODUCT.ACCESS'
		FIELDNAME.ARR	='LF.MONEDA.AZUL':VM:'LF.MINIMUM.AZUL':VM:'LF.MAXIMUM.AZUL':VM:'LF.PERCENT.AZUL':VM:'LF.DL.AZUL'
		CALL MULTI.GET.LOC.REF(APPL.ARR,FIELDNAME.ARR,POS.ARR)
		R.NEW(AA.PRODA.LOCAL.REF)<1,POS.ARR<1,1>>='SI'
		R.NEW(AA.PRODA.LOCAL.REF)<1,POS.ARR<1,2>>='0.20'
		R.NEW(AA.PRODA.LOCAL.REF)<1,POS.ARR<1,3>>='3.0'
		R.NEW(AA.PRODA.LOCAL.REF)<1,POS.ARR<1,4>>='0.00'
;*************************************************************
*DEBUG
Y.ID.CUS		 = AA$R.ARRANGEMENT<AA.ARR.CUSTOMER>
;*Y.TXT = 'Y.ID.CUS: ': Y.ID.CUS
;*GOSUB WRITE_LOG_FILE
*Y.ID.CUS = '122935'
;*************************************************************	
RETURN

PROCESS:
******************************************************************************************************    
    OPT.TYPE = ''
    OPTION = "SAVE"
    CALL AA.COMMON.MANAGER(OPTION, OPT.TYPE)
****************************************************************************************************** 
			;*Lectura de los productos creados hasta la fecha para cliente.
			CALL F.READ (FN.CUS.ACC,Y.ID.CUS,REC.CUS.ACC,F.CUS.ACC,ERR.CUS.ACC)			
			;*Y.TXT = 'REC.CUS.ACC: ': REC.CUS.ACC
			;*GOSUB WRITE_LOG_FILE
			Y.CANT.PRODS.CUSTOMER	= DCOUNT(REC.CUS.ACC, @FM)
			;*Y.TXT = 'Y.CANT.PRODS.CUSTOMER: ': Y.CANT.PRODS.CUSTOMER
			;*GOSUB WRITE_LOG_FILE
			;*Variables que contendran los arreglos con las cuentas a enviar via OFS.
			Y.ARRAY.ACC.TRANS	  = ""
			Y.ARRAY.ACC.SEE 	  = ""
			Y.ARRAY.LENDING.TRANS = ""
			Y.ARRAY.DAP.SEE		  = ""
						
			FOR Y.PRODS.CUSTOMER = 1 TO Y.CANT.PRODS.CUSTOMER
				Y.CURR.ACC	= REC.CUS.ACC<Y.PRODS.CUSTOMER>
				;*Y.TXT = 'INICIO FOR Y.CURR.ACC: ': Y.CURR.ACC
				;*GOSUB WRITE_LOG_FILE
				GOSUB VALIDAR.TIPO.PRODUCTO
			NEXT Y.PRODS.CUSTOMER
			;* Limite Diario para moneda azul
			CALL F.READ(FN.CUS, Y.ID.CUS, ARR.CUS, F.CUS, ERR)
			
	    	IF ARR.CUS THEN
				CALL GET.LOC.REF(CUST, 'LF.AML.AMT.TCIB', POS)
				;* Monto aproximado (Customer)
	    			Y.CUS.LIM.AMOU = ARR.CUS<EB.CUS.LOCAL.REF, POS>
	    			Y.TXT = 'Y.CUS.LIM.AMOU: ': Y.CUS.LIM.AMOU
	    	  		GOSUB WRITE_LOG_FILE
	    			R.NEW(AA.PRODA.LOCAL.REF)<1,POS.ARR<1,5>>=Y.CUS.LIM.AMOU
	    	  		
	    	END
	    	
*****************************************************************************************************    
    OPT.TYPE = ''
    OPTION = "RESTORE"
    CALL AA.COMMON.MANAGER(OPTION, OPT.TYPE)
***************************************************************************************************** 			
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
	    ;*Y.TXT = 'Y.PRODUCT.LINE: ': Y.PRODUCT.LINE
		;*GOSUB WRITE_LOG_FILE
		;*Y.TXT = 'R.ARR.CUS: ': R.ARR.CUS
		;*GOSUB WRITE_LOG_FILE
		;*Y.TXT = 'REC.ARR.CUS: ': REC.ARR.CUS
		;*GOSUB WRITE_LOG_FILE
		;*Other Party: Contiene unicos de los clientes que son parte de la cuenta mancomunada.
		Y.AA.CUS.OTHER.PARTY = REC.ARR.CUS<AA.CUS.OTHER.PARTY>
		;*Con Indis: Bandera que determina el tipo de Cuenta Mancomunada (Y/O)
		CALL GET.LOC.REF("AA.ARR.CUSTOMER", "LF.CON.INDIS", POS.CON.INDIS)
		Y.AA.CON.INDIS = REC.ARR.CUS<AA.CUS.LOCAL.REF><1,POS.CON.INDIS>
		;*Y.TXT = 'Y.AA.CON.INDIS: ': Y.AA.CON.INDIS
		;*GOSUB WRITE_LOG_FILE
		IF Y.AA.CUS.OTHER.PARTY THEN ;*Es mancomunada
			IF Y.AA.CON.INDIS EQ 'Y' THEN
				Y.ARRAY.ACC.SEE	= Y.CURR.ACC
				R.NEW(AA.PRODA.ACCT.SEE)<-1> = Y.ARRAY.ACC.SEE
*				R.NEW(AA.PRODA.ACCT.SEE)<2,1> = Y.ARRAY.ACC.SEE
				;*AA$R.ARRANGEMENT<AA.PRODA.ACCT.SEE> = Y.ARRAY.ACC.SEE<-1>
			END
			IF Y.AA.CON.INDIS EQ 'O' THEN
				Y.ARRAY.ACC.TRANS = Y.CURR.ACC		
				R.NEW(AA.PRODA.ACCT.TRANS)<-1> = Y.ARRAY.ACC.TRANS
*				R.NEW(AA.PRODA.ACCT.TRANS)<2,1> = Y.ARRAY.ACC.TRANS
				;*AA$R.ARRANGEMENT<AA.PRODA.ACCT.TRANS> = Y.ARRAY.ACC.TRANS<-1>
			END
		END ELSE ;*Crearla a transaccionar
			Y.ARRAY.ACC.TRANS = Y.CURR.ACC
			;*Y.TXT = 'Y.CURR.ACC ': Y.CURR.ACC
			;*GOSUB WRITE_LOG_FILE
			;*Y.TXT = 'ANTES DE R NEW: Y.ARRAY.ACC.TRANS ': Y.ARRAY.ACC.TRANS
			;*GOSUB WRITE_LOG_FILE
			R.NEW(AA.PRODA.ACCT.TRANS)<-1> = Y.ARRAY.ACC.TRANS
			;*Y.TXT = 'DESPUES DE R NEW: R.NEW(AA.PRODA.ACCT.TRANS)<1,1>': R.NEW(AA.PRODA.ACCT.TRANS)
			GOSUB WRITE_LOG_FILE
*			R.NEW(AA.PRODA.ACCT.TRANS)<2,1> = Y.ARRAY.ACC.TRANS
			;*AA$R.ARRANGEMENT<AA.PRODA.ACCT.TRANS> = Y.ARRAY.ACC.TRANS<-1>
		END		
	END
	IF Y.PRODUCT.LINE EQ DEPOSITS THEN ;*Cuenta se anade para visualizar
		Y.ARRAY.DAP.SEE	= Y.CURR.ARR
		;*Y.TXT = 'ANTES DE R NEW: Y.ARRAY.DAP.SEE ': Y.ARRAY.DAP.SEE
			;*GOSUB WRITE_LOG_FILE
		R.NEW(AA.PRODA.ARRGT.SEE)<-1> = Y.ARRAY.DAP.SEE
		;*AA$R.ARRANGEMENT<AA.PRODA.ARRGT.SEE> = Y.ARRAY.DAP.SEE<-1>
		;*Y.TXT = 'DESPUES DE R NEW: Y.ARRAY.DAP.SEE ': R.NEW(AA.PRODA.ARRGT.SEE)
			;*GOSUB WRITE_LOG_FILE
	END  				
	IF Y.PRODUCT.LINE EQ LENDING THEN ;*Para cuenta se anade para transaccionar
		Y.ARRAY.LENDING.TRANS	= Y.CURR.ARR
		R.NEW(AA.PRODA.ARRGT.TRANS)<-1> = Y.ARRAY.LENDING.TRANS
		;*AA$R.ARRANGEMENT<AA.PRODA.ARRGT.TRANS> = Y.ARRAY.LENDING.TRANS<-1>

	END	 
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
