*-----------------------------------------------------------------------------
* <Rating>76</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.NOF.V.ACC.STATUS.ACH(INFO)
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
    
    ;*Campos Locales
	CALL GET.LOC.REF('ACCOUNT', 'LF.ESTADO.CTA', POS.ESTADO.CTA) 	;*Estado de la Cuenta
    
        ;* Cargar los criterios de seleccion
    ;*----------------------------------
	LOCATE "PROPOSITO" IN D.FIELDS<1> SETTING PAR.POS.1 THEN
		Y.PROPOSITO = D.RANGE.AND.VALUE<PAR.POS.1>
	END
    
    LOCATE "NO.ACCOUNT" IN D.FIELDS<1> SETTING PAR.POS.1 THEN
		Y.NO.ACCOUNT = D.RANGE.AND.VALUE<PAR.POS.1>
	END
    
    EQU LENDING 		TO 'LENDING'
    EQU DEPOSITS 		TO 'DEPOSITS'
    EQU ACCOUNTS 		TO 'ACCOUNTS'
    EQU AUTH 			TO 'AUTH'
    EQU CURRENT 		TO 'CURRENT'
    EQU Y.CLOSE			TO 'CLOSE'
    EQU Y.P.CLOSURE		TO 'PENDING.CLOSURE'
    EQU Y.ACTIVA 		TO 'Activa'
    EQU Y.STR.ID.ACC 	TO 'SLV.AC.ESTADO.CUENTA*'
   
    EQU COD.INACTIVA 	TO '16'
    EQU COD.INVALIDO 	TO '04'
    EQU COD.CLOSE 		TO '02'
    EQU COD.NOT.FOUND	TO '03'
    

    
    ;*DEbug
    ;*Y.NO.ACCOUNT = '10000000812224'
RETURN

PROCESS:

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
			STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:'*'
			INFO = STR.ARR
			CRT 'INFO ':INFO ;******************** CRT
			RETURN
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
		STR.ARR := 'Y':"*":'*'
		INFO = STR.ARR
		CRT 'INFO ':INFO ;******************** CRT
		RETURN
	END
	 
	IF Y.PRODUCT.LINE EQ DEPOSITS THEN
			STR.ARR = Y.NO.ACCOUNT		:"*"
			STR.ARR := 'N':"*"
			CALL F.READ(FN.COD.R.ACH,COD.INVALIDO ,R.COD.R.ACH,F.COD.R.ACH,E.COD.R.ACH)
			STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:'*'
			INFO = STR.ARR
			CRT 'INFO ':INFO ;******************** CRT
			RETURN
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
		STR.ARR := 'Y':"*"
	END
	
	BEGIN CASE
		;* Cuenta cerrada
		CASE Y.ESTADO.ARR EQ Y.CLOSE OR Y.ESTADO.ARR EQ Y.P.CLOSURE 
		 	CALL F.READ(FN.COD.R.ACH,COD.CLOSE ,R.COD.R.ACH,F.COD.R.ACH,E.COD.R.ACH)	 	
			STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:"*"
			
		;*Cuenta inactiva y con restricciones
		CASE ESTADO.CTA NE Y.ACTIVA AND RES.VALIDACION EQ 0 
		 	CALL F.READ(FN.COD.R.ACH,COD.INACTIVA,R.COD.R.ACH,F.COD.R.ACH,E.COD.R.ACH)	 	
			STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:"*"
			
		;*Cuenta activa pero restricciones	
		CASE ESTADO.CTA EQ Y.ACTIVA AND RES.VALIDACION EQ 0 	
			CALL F.READ(FN.COD.R.ACH,COD.INVALIDO,R.COD.R.ACH,F.COD.R.ACH,E.COD.R.ACH)	 	
			STR.ARR := R.COD.R.ACH<EB.SLV41.ID.COD>:"*"
	END CASE

	
	INFO = STR.ARR
	
	CRT 'INFO ':INFO ;******************** CRT
	;*------------------------------------------------------------------------
	
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
