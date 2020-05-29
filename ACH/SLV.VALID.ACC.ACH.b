*-----------------------------------------------------------------------------
* <Rating>1102</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.VALID.ACC.ACH
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_AA.LOCAL.COMMON
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.ACCOUNT
$INSERT I_F.CUSTOMER
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.EB.LOOKUP
$INSERT I_F.EB.SLV.COD.R.ACH
$INSERT I_F.EB.SLV.RANG.PARAM
$INSERT I_F.EB.SLV.VAL.ACC.ACH
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB PROCESS

RETURN


INIT:
    FN.ACCOUNT 	        = 'F.ACCOUNT'
    F.ACCOUNT       	= ''
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)
     
    FN.CUSTOMER 		= 'F.CUSTOMER'
    F.CUSTOMER 			= ''
    CALL OPF(FN.CUSTOMER, F.CUSTOMER) 
    
  	FN.ARR = 'FBNK.AA.ARRANGEMENT'
    F.ARR = ''
    CALL OPF(FN.ARR, F.ARR)
    
    FN.EBLOOKUP		= 'F.EB.LOOKUP'
 	F.EBLOOKUP		= ''
 	CALL OPF(FN.EBLOOKUP, F.EBLOOKUP)
    
    FN.COD.R.ACH = 'F.EB.SLV.COD.R.ACH'
    F.COD.R.ACH  = ''
    CALL OPF(FN.COD.R.ACH,F.COD.R.ACH)
    
    FN.RANG.PARAM = 'F.EB.SLV.RANG.PARAM'
	F.RANG.PARAM = ''
    CALL OPF(FN.RANG.PARAM, F.RANG.PARAM)
    
    ;*Campos Locales
	CALL GET.LOC.REF('ACCOUNT', 'LF.ESTADO.CTA', POS.ESTADO.CTA) 	;*Estado de la Cuenta
    
        ;* Cargar los criterios de seleccion
    ;*----------------------------------
*	LOCATE "PROPOSITO" IN D.FIELDS<1> SETTING PAR.POS.1 THEN
*		Y.PROPOSITO = D.RANGE.AND.VALUE<PAR.POS.1>
*	END


*    LOCATE "NO.ACCOUNT" IN D.FIELDS<1> SETTING PAR.POS.1 THEN
*		Y.NO.ACCOUNT = D.RANGE.AND.VALUE<PAR.POS.1>
*	END



*    LOCATE "ID.TXN" IN D.FIELDS<1> SETTING PAR.POS.1 THEN
*		Y.TXN.ACH = D.RANGE.AND.VALUE<PAR.POS.1>
*	END
 

    
    EQU LENDING 		TO 'LENDING'
    EQU DEPOSITS 		TO 'DEPOSITS'
    EQU ACCOUNTS 		TO 'ACCOUNTS'
    EQU AUTH 			TO 'AUTH'
    EQU CURRENT 		TO 'CURRENT'
    EQU Y.CLOSE			TO 'CLOSE'
    EQU Y.P.CLOSURE		TO 'PENDING.CLOSURE'
    EQU Y.ACTIVA 		TO 'Activa'
    EQU Y.CANCELADA		TO 'Cancelada'
    EQU Y.EMBARGO		TO 'Embargo'
    EQU Y.STR.ID.ACC 	TO 'SLV.AC.ESTADO.CUENTA*'
    EQU RANGO.ACC 		TO 'SLV.CAT.CUENTAS'
   	EQU ID.BANCO.AZUL TO 'BA119020W6LF5'
	EQU RANGO.ACC 		TO 'SLV.CAT.CUENTAS'
	EQU CTA.COR TO 'CTA.COR'
	EQU CTA.AHO TO 'CTA.AHO'
   
    EQU COD.INACTIVA 	TO '16'
    EQU COD.INVALIDO 	TO '04'
    EQU COD.CLOSE 		TO '02'
    EQU COD.NOT.FOUND	TO '03'
    
    
    ;*DEbug
    ;*Y.NO.ACCOUNT = '10000000867649'
    ;*Y.TXN.ACH 	= 'TRXCR19092009114582000B92E7CEE647C4'
RETURN

PROCESS:
	
	FOR I = 1 TO DCOUNT(R.NEW(EB.SLV56.CUENTA), VM) 
		
		INFO = ''
		STR.ARR = ''
		R.COD.R.ACH = ''
		Y.NO.ACCOUNT = R.NEW(EB.SLV56.CUENTA)<1,I>

		 ;*R.NEW(EB.SLV56.CUENTA)<1,I>	
		Y.TXN.ACH	= R.NEW(EB.SLV56.ID.TRX.ACH)<1,I>
	    
	    TEXTO.ARCHIVO = 'NO.CUENTA ':Y.NO.ACCOUNT
	    GOSUB ESCRIBIR.ARCHIVO
	    
	    ;*Leer registro Account		
		CALL F.READ(FN.ACCOUNT, Y.NO.ACCOUNT, REC.ACC, F.ACCOUNT, E.ACCOUNT)	
		
		IF NOT(REC.ACC) THEN
		
			FN.ACCOUNT 	        = 'F.ACCOUNT$HIS'
	    	F.ACCOUNT       	= ''
	   	 	CALL OPF(FN.ACCOUNT, F.ACCOUNT)
		
			CALL F.READ(FN.ACCOUNT, Y.NO.ACCOUNT:';1', REC.ACC, F.ACCOUNT, E.ACCOUNT)	
		
			IF NOT(REC.ACC) THEN
				STR.ARR = Y.NO.ACCOUNT		:"*"
				STR.ARR := 'N':"*"
				CALL F.READ(FN.COD.R.ACH,COD.NOT.FOUND ,R.COD.R.ACH,F.COD.R.ACH,E.COD.R.ACH)
				
				STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:'*':'':'*':Y.TXN.ACH
				INFO = STR.ARR
				
				R.NEW(EB.SLV56.IS.VALID)<1,I> = FIELD(INFO,"*",2)
				R.NEW(EB.SLV56.COD.ERROR)<1,I> = FIELD(INFO,"*",3)
				R.NEW(EB.SLV56.DES.ERROR)<1,I> = R.COD.R.ACH<EB.SLV41.DESCRIPTION>
				
				;*CRT 'INFO ':INFO ;******************** CRT
				CONTINUE
			END
				
		END
		
		;***************************************************
		TEXTO.ARCHIVO = 'REC.ACC-> ':REC.ACC		      ;*
		GOSUB ESCRIBIR.ARCHIVO							  ;*	
		;***************************************************
		
			;*Leer Registro de Arr
		CALL F.READ(FN.ARR, REC.ACC<AC.ARRANGEMENT.ID>, REC.ARR, F.ARR, ARR.ERR)
	      
	    Y.ESTADO.ARR   = REC.ARR<AA.ARR.ARR.STATUS>   ;*Estado Arrangement	
		Y.PRODUCT.LINE = REC.ARR<AA.ARR.PRODUCT.LINE> ;*Product Line
		
		IF Y.PRODUCT.LINE EQ LENDING AND Y.ESTADO.ARR EQ CURRENT THEN
			STR.ARR = Y.NO.ACCOUNT		:"*"
			STR.ARR := 'Y':'*':'':'*':'':'*':Y.TXN.ACH
			INFO = STR.ARR
				R.NEW(EB.SLV56.IS.VALID)<1,I> = FIELD(INFO,"*",2)
				R.NEW(EB.SLV56.COD.ERROR)<1,I> = FIELD(INFO,"*",3)
				
				;*CRT 'INFO ':INFO ;******************** CRT
				CONTINUE
		END
		 
		IF Y.PRODUCT.LINE EQ DEPOSITS THEN
				STR.ARR = Y.NO.ACCOUNT		:"*"
				STR.ARR := 'N':"*"
				CALL F.READ(FN.COD.R.ACH,COD.INVALIDO ,R.COD.R.ACH,F.COD.R.ACH,E.COD.R.ACH)
				STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:'*':'':'*':Y.TXN.ACH
				INFO = STR.ARR
				R.NEW(EB.SLV56.IS.VALID)<1,I> = FIELD(INFO,"*",2)
				R.NEW(EB.SLV56.COD.ERROR)<1,I> = FIELD(INFO,"*",3)
				R.NEW(EB.SLV56.DES.ERROR)<1,I> = R.COD.R.ACH<EB.SLV41.DESCRIPTION>
				;*CRT 'INFO ':INFO ;******************** CRT
				CONTINUE
		END 
		  
	    ;***************************************************
		TEXTO.ARCHIVO = 'Y.ESTADO.ARR-> ':Y.ESTADO.ARR	  ;*
		GOSUB ESCRIBIR.ARCHIVO							  ;*	
		;***************************************************
	    
		;*EB.LOOKUP para Estatus de Cuenta
		ID.ESTATUS.ACC	= Y.STR.ID.ACC : REC.ACC<AC.LOCAL.REF><1 , POS.ESTADO.CTA>		 
		CALL F.READ(FN.EBLOOKUP, ID.ESTATUS.ACC, REC.LOOKUP, F.EBLOOKUP, ERR.LOOKUP)		
		
		ESTADO.CTA	= REC.LOOKUP<EB.LU.DESCRIPTION><1,1>
		
		
		;***************************************************
		TEXTO.ARCHIVO = 'ESTADO.CTA-> ':ESTADO.CTA		  ;*
		GOSUB ESCRIBIR.ARCHIVO							  ;*	
		;***************************************************
		
		IF (ESTADO.CTA EQ Y.ACTIVA AND Y.ESTADO.ARR EQ AUTH ) OR (ESTADO.CTA EQ '' AND Y.ESTADO.ARR EQ AUTH) THEN ;* la Cuenta esta activa (para cuentas AHO el campo esta vacio al crearlas)
																											 ;*para cuentas AHO el campo esta vacio al momento de crearlas sin embargo estan activas si el estado de ARR es AUTH
			ESTADO.CTA = Y.ACTIVA
					
			;*La cuenta esta activa.. Se debe evaluar si posee alguna restriccion	
			POSTING.RESTRICT = REC.ACC<AC.POSTING.RESTRICT>	
			
			;************************************************************
			TEXTO.ARCHIVO = 'POSTING.RESTRICT-> ':POSTING.RESTRICT	   ;*
			GOSUB ESCRIBIR.ARCHIVO							  		   ;*	
			;************************************************************
				
			;*Evaluar restricciones de tipo Total 24/Debitos 25/Embargo 20/Orden Judicial 21
			IF POSTING.RESTRICT EQ 24 OR POSTING.RESTRICT EQ 26 OR POSTING.RESTRICT EQ 20 OR POSTING.RESTRICT EQ 21 THEN
				RES.VALIDACION	= 0	;*Se retorna 0 simulando FALSE	(Cuenta esta activa pero posee alguna restriccion de tipo Total 24/Debitos 25/Embargo 20/Orden Judicial 21) 				
			END ELSE ;* La cuenta no posee restricciones de tipo Total 24/Debitos 25/Embargo 20/Orden Judicial 21 y esta activa			
				RES.VALIDACION	= 1 ;*Se retorna 1 simulando TRUE							 			
			END		 
		END ELSE ;* La cuenta esta Inactiva/Emargo/Cancelada y ya no se evalua si tiene restriccion	
			RES.VALIDACION = 0 ;*Se retorna 0 simulando FALSE									 			
		END		
		
		;*----------------------- ARMANDO ARRAY RESPUESTA -----------------------
		
		STR.ARR = Y.NO.ACCOUNT		:"*"
		
		IF ESTADO.CTA NE Y.ACTIVA OR RES.VALIDACION EQ 0 THEN
			STR.ARR := 'N':"*"
		END
		ELSE 
			STR.ARR := 'Y':"*":'':'*':'':'*':Y.TXN.ACH
		END
		
		BEGIN CASE
			;* Cuenta cerrada
			CASE Y.ESTADO.ARR EQ Y.CLOSE OR Y.ESTADO.ARR EQ Y.P.CLOSURE 
			 	CALL F.READ(FN.COD.R.ACH,COD.CLOSE ,R.COD.R.ACH,F.COD.R.ACH,E.COD.R.ACH)	 	
				STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:"*":'':'*':Y.TXN.ACH
				
			;*Cuenta cancelada 
			CASE ESTADO.CTA EQ Y.CANCELADA AND RES.VALIDACION EQ 0 
			 	CALL F.READ(FN.COD.R.ACH,COD.CLOSE ,R.COD.R.ACH,F.COD.R.ACH,E.COD.R.ACH)	 	
				STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:"*":'':'*':Y.TXN.ACH
				
			;*embargo
			CASE ESTADO.CTA EQ Y.EMBARGO AND RES.VALIDACION EQ 0 
			 	CALL F.READ(FN.COD.R.ACH,COD.INVALIDO,R.COD.R.ACH,F.COD.R.ACH,E.COD.R.ACH)	 	
				STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:"*":'':'*':Y.TXN.ACH
				
			;*Cuenta inactiva y con restricciones
			CASE ESTADO.CTA NE Y.ACTIVA AND RES.VALIDACION EQ 0 
			 	CALL F.READ(FN.COD.R.ACH,COD.INACTIVA,R.COD.R.ACH,F.COD.R.ACH,E.COD.R.ACH)	 	
				STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:"*":'':'*':Y.TXN.ACH
				
			;*Cuenta activa pero restricciones	
			CASE ESTADO.CTA EQ Y.ACTIVA AND RES.VALIDACION EQ 0 	
				CALL F.READ(FN.COD.R.ACH,COD.INVALIDO,R.COD.R.ACH,F.COD.R.ACH,E.COD.R.ACH)	 	
				STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:"*":'':'*':Y.TXN.ACH
		END CASE
	
		Y.VALID = FIELD(STR.ARR,"*",2)
		IF Y.VALID EQ 'Y' THEN
				GOSUB GET.ACC.INFO
		END
		
		INFO = STR.ARR
		
		R.NEW(EB.SLV56.IS.VALID)<1,I> = FIELD(INFO,"*",2)
		R.NEW(EB.SLV56.COD.ERROR)<1,I> = FIELD(INFO,"*",3)
		R.NEW(EB.SLV56.DES.ERROR)<1,I> = R.COD.R.ACH<EB.SLV41.DESCRIPTION>						
		;*------------------------------------------------------------------------
	
	NEXT I 
	
RETURN


GET.ACC.INFO:

	FT.AZUL.ACC.TYPE = '0'
	;*Account
	FN.ACCOUNT 	        = 'F.ACCOUNT'
	CALL F.READ(FN.ACCOUNT, Y.NO.ACCOUNT, REC.ACC, F.ACCOUNT, E.ACCOUNT)	

	Y.CATEGORY =  REC.ACC<AC.CATEGORY>
	;*Obtiene Parametria de Categories para Cuentas de ahorro y corriente
	CALL F.READ(FN.RANG.PARAM, RANGO.ACC, R.PARAM, F.RANG.PARAM, ERR.RANG.PARAM)

	;*Cuentas Ahorro
    FIND CTA.AHO IN R.PARAM<EB.SLV56.ID.RANGO> SETTING Ap, Vp THEN
    	;*Validar si el Category de la cuenta se encuentra en el Rango
       	IF Y.CATEGORY GE R.PARAM<EB.SLV56.RANGO.INF><Ap, Vp> AND Y.CATEGORY LE R.PARAM<EB.SLV56.RANGO.SUP><Ap, Vp> THEN
			FT.AZUL.ACC.TYPE 		= '1'
		END
 	END 
 	;*Cuentas Corrientes
 	FIND CTA.COR IN R.PARAM<EB.SLV56.ID.RANGO> SETTING Ap, Vp THEN
    	;*Validar si el Category de la cuenta se encuentra en el Rango
       	IF Y.CATEGORY GE R.PARAM<EB.SLV56.RANGO.INF><Ap, Vp> AND Y.CATEGORY LE R.PARAM<EB.SLV56.RANGO.SUP><Ap, Vp> THEN
			FT.AZUL.ACC.TYPE 		= '1'
		END
 	END

 	IF FT.AZUL.ACC.TYPE NE '1' THEN
 	
 			STR.ARR = Y.NO.ACCOUNT		:"*"
			STR.ARR := 'N':"*"
			CALL F.READ(FN.COD.R.ACH,COD.INVALIDO,R.COD.R.ACH,F.COD.R.ACH,E.COD.R.ACH)
			STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:'*':'':'*':Y.TXN.ACH
			INFO = STR.ARR	 	
 	END
 	
RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'ACH'
    R.ID   = 'PAGO.STATUS.ACCOUNT.':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN

END
