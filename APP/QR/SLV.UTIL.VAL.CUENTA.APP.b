*-----------------------------------------------------------------------------
* <Rating>-74</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.VAL.CUENTA.APP(CTA,FLAG)
*-----------------------------------------------------------------------------
* Nombre: SLV.UTIL.VAL.CUENTA(CTA)
* Descripcion: Rutina Encargada de validar estados de la cuenta ahorro corriente, depositos y prestamos.
*			   In: Cta - Out: Flag(1 correcto, 0 fallo)
*---------------------------------------------------------------------------------------------------- 
* Version	Autor		Fecha		Comentario  
*----------------------------------------------------------------------------------------------------
* 1.0		iTurcios	03.01.2019	Version inicial 
*----------------------------------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.ACCOUNT
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.EB.LOOKUP
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN

INIT:
 	FN.ARR	= 'FBNK.AA.ARRANGEMENT'
    F.ARR 	= ''
    CALL OPF(FN.ARR, F.ARR)
    
	FN.ACC		= 'F.ACCOUNT' 
	F.ACC 		= ''
	CALL OPF(FN.ACC, F.ACC)
	
	FN.EBLOOKUP		= 'F.EB.LOOKUP'
 	F.EBLOOKUP		= ''
 	CALL OPF(FN.EBLOOKUP, F.EBLOOKUP)
 	
	;*Campos Locales
	CALL GET.LOC.REF('ACCOUNT', 'LF.ESTADO.CTA', POS.ESTADO.CTA) 	;*Estado de la Cuenta
	;*Constantes
	EQU LENDING 		TO 'LENDING'
    EQU DEPOSITS 		TO 'DEPOSITS'
    EQU ACCOUNTS 		TO 'ACCOUNTS'
    EQU AUTH 			TO 'AUTH'
    EQU CURRENT 		TO 'CURRENT'
    EQU Y.ACTIVA 		TO 'Activa'
    EQU Y.STR.ID.ACC 	TO 'SLV.AC.ESTADO.CUENTA*'
        	
    ;*Parametro de Entrada.
	Y.ACC	= CTA		
	;*Debug
*	Y.ACC 	= "10000000001528"	
*	Y.ACC	= "AA16238CXZYK"
RETURN
 
PROCESS:
	;*Leer registro Account 
	IF Y.ACC[1,2] EQ 'AA' THEN		
		CALL SLV.UTIL.GET.ACC.X.ARR(Y.ACC)		 
	END
	;*Leer registro Account		
	CALL F.READ(FN.ACC, Y.ACC, REC.ACC, F.ACC, ERR.ACC)		
	;*Leer Registro de Arr
	CALL F.READ(FN.ARR, REC.ACC<AC.ARRANGEMENT.ID>, REC.ARR, F.ARR, ARR.ERR)
	
	Y.PRODUCT.LINE = REC.ARR<AA.ARR.PRODUCT.LINE> ;*Product Line		
	Y.ESTADO.ARR   = REC.ARR<AA.ARR.ARR.STATUS>   ;*Estado Arrangement

	BEGIN CASE

		CASE Y.PRODUCT.LINE EQ ACCOUNTS
			GOSUB VAL.ACC
			
		CASE Y.PRODUCT.LINE EQ DEPOSITS
			GOSUB VAL.DAPS			
		CASE Y.PRODUCT.LINE EQ LENDING
			GOSUB VAL.LOANS
	END CASE
RETURN

VAL.ACC:
	;*EB.LOOKUP para Estatus de Cuenta
	ID.ESTATUS.ACC	= Y.STR.ID.ACC : REC.ACC<AC.LOCAL.REF><1 , POS.ESTADO.CTA>		 
	CALL F.READ(FN.EBLOOKUP, ID.ESTATUS.ACC, REC.LOOKUP, F.EBLOOKUP, ERR.LOOKUP)		
	
	ESTADO.CTA	= REC.LOOKUP<EB.LU.DESCRIPTION><1,1>
	IF ESTADO.CTA EQ Y.ACTIVA OR ESTADO.CTA EQ '' THEN ;* la Cuenta esta activa (para cuentas AHO el campo esta vacio al crearlas)
		IF ESTADO.CTA EQ '' AND Y.ESTADO.ARR EQ AUTH THEN ;*para cuentas AHO el campo esta vacio al momento de crearlas sin embargo estan activas si el estado de ARR es AUTH
			ESTADO.CTA = Y.ACTIVA
		END		
		
		;*La cuenta esta activa.. Se debe evaluar si posee alguna restriccion	
		POSTING.RESTRICT = REC.ACC<AC.POSTING.RESTRICT>	

		;*Evaluar restricciones de tipo Total 24/Debitos 25/Embargo 20/Orden Judicial 21
		IF POSTING.RESTRICT EQ 24 OR POSTING.RESTRICT EQ 25 OR POSTING.RESTRICT EQ 20 OR POSTING.RESTRICT EQ 21 OR POSTING.RESTRICT EQ 26 OR POSTING.RESTRICT EQ 28 THEN

		RES.VALIDACION	= 0	;*Se retorna 0 simulando FALSE	(Cuenta esta activa pero posee alguna restriccion de tipo Total 24/Debitos 25/Embargo 20/Orden Judicial 21) 				
		END ELSE ;* La cuenta no posee restricciones de tipo Total 24/Debitos 25/Embargo 20/Orden Judicial 21 y esta activa			
			RES.VALIDACION	= 1 ;*Se retorna 1 simulando TRUE							 			
		END		 
	END ELSE ;* La cuenta esta Inactiva/Emargo/Cancelada y ya no se evalua si tiene restriccion	
		RES.VALIDACION = 0 ;*Se retorna 0 simulando FALSE				
		BREAK			 			
	END		
	
	;*Resultado de validacion
	IF RES.VALIDACION EQ 1 THEN
    	FLAG = 1 ;*Paso validaciones de Cuenta
	END ELSE 
		FLAG = 0 ;*No paso validaciones de Cuenta
	END
RETURN
 
VAL.DAPS:
	;*Si estado del Arrangement es CURRENT debe presentarse en Banca en Linea
	IF Y.ESTADO.ARR EQ CURRENT THEN
		FLAG = 1 ;*Paso validaciones de Cuenta
	END ELSE
		FLAG = 0 ;*No paso validaciones de Cuenta
	END
RETURN

VAL.LOANS:
	;*Si estado del Arrangement es CURRENT debe presentarse en Banca en Linea
	IF Y.ESTADO.ARR EQ CURRENT THEN
		FLAG = 1 ;*Paso validaciones de Cuenta
	END ELSE
		FLAG = 0 ;*No paso validaciones de Cuenta
	END
RETURN



END
