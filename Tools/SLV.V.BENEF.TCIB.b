*-----------------------------------------------------------------------------
* <Rating>288</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.BENEF.TCIB
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Nombre: SLV.V.BENEF.TCIB
* Descripción: Valida los datos basicos para relacionar un
*				Beneficiary y adicionalmente obtiene nombre
*				asociado con la cuenta de tercero del registro
*-----------------------------------------------------------------------------
* Version	Paquete								Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		Initial								R.Garay		25.02.16	Version Inicial
* 1.1		Modify								R.Cortes	10.03.16	Validacion, para evitar la adicion de cuentas propias en 
*																		administracion de cuentas favoritos				
*-----------------------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------
$INSERT I_F.BENEFICIARY 
$INSERT I_F.ACCOUNT
$INSERT I_F.CUSTOMER
$INSERT I_F.VERSION


GOSUB INIT
GOSUB OPENFILE
GOSUB LOAD
GOSUB PROCESS 

*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
FN.TABLE.BEN = 'F.BENEFICIARY' 
F.TABLE.BEN = '' 
FN.TABLE.ACC = 'F.ACCOUNT' 
F.TABLE.ACC = '' 
FN.TABLE.CUS = 'F.CUSTOMER' 
F.TABLE.CUS = '' 
RETURN 
*----------------------------------------------------------------------------- 


*-----------------------------------------------------------------------------
OPENFILE:
*-----------------------------------------------------------------------------
CALL OPF(FN.TABLE.ACC, F.TABLE.ACC)
CALL OPF(FN.TABLE.CUS, F.TABLE.CUS)
CALL OPF(FN.TABLE.BEN, F.TABLE.BEN) 
RETURN 
*----------------------------------------------------------------------------- 

*-----------------------------------------------------------------------------
LOAD:
*-----------------------------------------------------------------------------
BEN.ID = ID.NEW 
R.BEN = R.NEW 
;* debug 
*-----------------------------
*BEN.ID = 'BEN1521286135' 
*CALL F.READ(FN.TABLE.BEN,BEN.ID,R.BEN,F.TABLE.BEN,ERR)   
*-----------------------------
RETURN 
*----------------------------------------------------------------------------- 


*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

;* colocar el nombre del titular de la cuenta de tercero
;* en el campo 
Y.ACC 		= R.NEW(ARC.BEN.BEN.ACCT.NO) ;*obtener numero de cuenta 
Y.CUST.OWN 	= R.NEW(ARC.BEN.OWNING.CUSTOMER) ;* Obtener Id cliente (propia)
Y.FUN		= V$FUNCTION

;* debug 
;* Y.ACC 		=   '10000000008654' ;* '12234567890987'
;* Y.CUST.OWN 	= '101519' ;* 101519

	IF Y.FUN NE 'R' THEN	
		CALL F.READ(FN.TABLE.ACC,Y.ACC,R.ACC,F.TABLE.ACC,ERR.ACC) ;* obtener Id cliente (tercero)
		Y.CUST = R.ACC<AC.CUSTOMER> 
		Y.CATEG = R.ACC<AC.CATEGORY> ;* obtener category 
		CATEG.INITIAL = SUBSTRINGS(Y.CATEG,0,2)
		
		;*TEXTO.ARCHIVO = R.BEN + ' | ' + BEN.ID 
		;*GOSUB ESCRIBIR.ARCHIVO
		;*TEXTO.ARCHIVO = Y.CATEG + ' | ' + CATEG.INITIAL 
		;*GOSUB ESCRIBIR.ARCHIVO
		
		IF R.ACC EQ '' THEN 
			;* error si la cuenta no fue encontrada 
			E = 'EB-SLV.I.BENEF.NO.VALIDA' 
			CALL ERR 
			RETURN 
		END
		
		IF Y.CUST.OWN EQ Y.CUST THEN
			;* error si la cuenta es propia
			E = 'EB-SLV.I.BENEF.NO.VALIDA' 
			CALL ERR 
			RETURN 
		END
		
		IF CATEG.INITIAL NE 60 AND CATEG.INITIAL NE 10 THEN 
			;* error de cuenta no es ahorro o corriente 
			E = 'EB-SLV.I.BENEF.CATEG.FAIL' 
			CALL ERR 
			RETURN 
		END
		 
		CALL GET.LOC.REF('ACCOUNT', 'LF.ESTADO.CTA', POS.ESTADO)
		Y.ESTADO = R.ACC<AC.LOCAL.REF, POS.ESTADO> 
		
		Y.POST.REST = R.ACC<AC.POSTING.RESTRICT> 
		
		;*IF Y.ESTADO EQ 'OTR' THEN 
			IF Y.POST.REST EQ 24 OR Y.POST.REST EQ 26 OR Y.POST.REST EQ 20 OR Y.POST.REST EQ 23 OR Y.POST.REST EQ 21 THEN 
				;* error porque tiene restricciones 
				;* totales o restric de credito 
				E = 'EB-SLV.I.BENEF.RESTRICT' 
				CALL ERR 
				RETURN 
			END
		;*END 
		
		IF Y.ESTADO EQ 'CAN' OR Y.ESTADO EQ 'CER' OR Y.ESTADO EQ 'EMB' OR Y.ESTADO EQ 'INA' THEN
			;* el estado de la cuenta no es apropiado 
			E = 'EB-SLV.I.BENEF.ESTADO.FAIL'
			CALL ERR 
			RETURN 
		END
		
		;* este punto indica que la cuenta es correcta en su estado OTR 
		
		CALL F.READ(FN.TABLE.CUS,Y.CUST,R.CUS,F.TABLE.CUS,ERR.CUS) ;* obtener short.name cliente
		Y.TITU.NAME = R.CUS<EB.CUS.SHORT.NAME> 
		
		Y.NOMB.PROD = R.ACC<AC.ACCOUNT.TITLE.1> 
		
		
		;*CRT BEN.ID
		;*CRT R.BEN 
		;*CRT Y.CUST 
		;*CRT Y.TITU.NAME 
		;*CRT CATEG.INITIAL 
		;*CRT Y.ESTADO 
		
		R.NEW(ARC.BEN.BEN.SHORT.NAME)<1,1> = Y.TITU.NAME ;* asignar valor recuperado 
		
		R.NEW(ARC.BEN.LOCAL.REF)<1,POS.BP> = Y.NOMB.PROD ;* asignar nombre de producto beneficiario 
	END

RETURN 
*----------------------------------------------------------------------------- 


ESCRIBIR.ARCHIVO: 
	DIR.NAME= 'CHQ.OUT'
	R.ID   = 'BENEF_I_':TODAY:'.txt'
	;* hacer que escriba un archivo 
	
	OPENSEQ DIR.NAME,R.ID TO SEQ.PTR 
		WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
		END 
	CLOSESEQ SEQ.PTR 
	
RETURN 


END