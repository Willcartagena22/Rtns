*-----------------------------------------------------------------------------
* <Rating>-62</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.TCIB.GET.ACC(ENQ.DATA)
*-----------------------------------------------------------------------------
* Nombre: SLV.E.NOF.TCIB.GET.ACC(ENQ.DATA)
* Descripcion: Rutina Encargada de devolver cuentas de T24 parametrizadas en ISA para Banca Personas.
*---------------------------------------------------------------------------------------------------- 
* Version	Autor		Fecha		Comentario   
*----------------------------------------------------------------------------------------------------
* 1.0		iTurcios	28.01.19	Version inicial 
*----------------------------------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_System
$INSERT I_F.AA.PRODUCT.ACCESS
$INSERT I_F.ACCOUNT  
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.AC.LOCKED.EVENTS
$INSERT I_F.AA.PRODUCT
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN 

INIT:
	FN.ACC	= 'F.ACCOUNT'
	F.ACC	= ''
	CALL OPF(FN.ACC, F.ACC) 
	
 	FN.ARR	= 'FBNK.AA.ARRANGEMENT'
    F.ARR	= ''
    CALL OPF(FN.ARR, F.ARR)
    
	FN.LCK	= 'F.AC.LOCKED.EVENTS'
	F.LCK 	= ''
	CALL OPF(FN.LCK, F.LCK)
	
	FN.PROD = 'F.AA.PRODUCT'
	F.PROD	= ''
	CALL OPF(FN.PROD, F.PROD)
		
	;*Constantes    
	EQU EXT.ARRANGEMENT	TO 'EXT.ARRANGEMENT'
	EQU CURRENCY.FORMAT TO 'L2,$'
		
	;*Isa del Usuario Logueado en TCIB
    ID.ISA = System.getVariable(EXT.ARRANGEMENT)
    ;*Debug Isa
*   ID.ISA  = "AA16214KYMQJ"
*  	ID.ISA  = "AA16302R6TT2"
RETURN

PROCESS:
	;*Leyendo Cuentas asociadas al ISA. 
    CALL AA.GET.ARRANGEMENT.CONDITIONS(ID.ISA, 'PRODUCT.ACCESS', '', TODAY, R.ID.PROD.ACC, R.PRODUCT.ACCESS ,ERR.PRODUCT.ACCESS)
    REC.PRODUCT.ACCESS	=	RAISE(R.PRODUCT.ACCESS)

    IF R.PRODUCT.ACCESS THEN
    	;*Cuentas a Transaccionar 
    	CANT.PA.ACCT.TRANS = DCOUNT(REC.PRODUCT.ACCESS<AA.PRODA.ACCT.TRANS>, @VM)
    	FOR PA.AT = 1 TO CANT.PA.ACCT.TRANS 
    		Y.ACC.T.S	= 'T'
    		Y.ACC.CURR	= REC.PRODUCT.ACCESS<AA.PRODA.ACCT.TRANS><1,PA.AT>
    		GOSUB VAL.ACC
    	NEXT PA.AT
    	;*Cuentas a visualizar
    	CANT.PA.ACCT.SEE = DCOUNT(REC.PRODUCT.ACCESS<AA.PRODA.ACCT.SEE>, @VM)
    	FOR PA.AS = 1 TO CANT.PA.ACCT.SEE
    		Y.ACC.T.S	= 'S'
    		Y.ACC.CURR  = REC.PRODUCT.ACCESS<AA.PRODA.ACCT.SEE><1,PA.AS>			
			GOSUB VAL.ACC		
    	NEXT PA.AS
    END
RETURN 

VAL.ACC:
	FLAG = ""
	CALL SLV.UTIL.VAL.CUENTA(Y.ACC.CURR,FLAG)
	IF FLAG EQ 1 THEN
		GOSUB SEND.INFO
	END    		
RETURN 

SEND.INFO:	
	IF Y.ACC.CURR[1,2] EQ 'AA' THEN
		CALL SLV.UTIL.GET.ACC.X.ARR(Y.ACC.CURR)
	END
	;*Account
	CALL F.READ(FN.ACC, Y.ACC.CURR, REC.ACC.CURR, F.ACC, ERR.ACC.CURR)
	
	;*Arrangement
	CALL F.READ(FN.ARR, REC.ACC.CURR<AC.ARRANGEMENT.ID>, REC.ARR, F.ARR, ARR.ERR)
	
	;*Nombre del producto
	CALL F.READ(FN.PROD, REC.ARR<AA.ARR.PRODUCT>, REC.PRD, F.PROD, PRD.ERR)
		
	;*Obteniendo Informacion adicional para productos T24
	GOSUB GET.ACC.INFO 

	;*Llenando informacion	 
	STR.ACCOUNT	 = ""
	STR.ACCOUNT	:= REC.ACC.CURR<AC.CATEGORY> 			  		: '*' ;*1>Category		
	STR.ACCOUNT	:= TRIM(REC.PRD<AA.PDT.DESCRIPTION>) 	  		: '*' ;*2>Nombre Producto
	STR.ACCOUNT	:= ''								 	  		: '*' ;*3>Nickname cuenta
	STR.ACCOUNT	:= Y.ACC.CURR							  		: '*' ;*4>Numero de cuenta		
	STR.ACCOUNT	:= FMT(Y.SALDO.TOTAL,CURRENCY.FORMAT)			: '*' ;*5>Saldo Total
	STR.ACCOUNT	:= FMT(Y.SALDO.DISPONIBLE,CURRENCY.FORMAT)		: '*' ;*6>Saldo Disponible	
	STR.ACCOUNT	:= Y.ACC.T.S							  			  ;*7>Tipo de Cuenta: T:Transaccionar S:Ver	
		 	
	;*Se agrega salida con informacion.
	ENQ.DATA<-1> = STR.ACCOUNT  	
RETURN

GET.ACC.INFO:	
	;*Working Balance
	Y.WORKING.BAL = TRIM(REC.ACC.CURR<AC.WORKING.BALANCE>)
	
	;* Calculando el Monto Bloqueado en la Cuenta
	SMT = "SELECT " : FN.LCK : " WITH ACCOUNT.NUMBER EQ '" : Y.ACC.CURR : "'"
	CALL EB.READLIST(SMT, IDS.LCK, '', NUM.REG.LCK, ERR.LCK)

	Y.SUM.LCK = 0
	FOR LCK = 1 TO NUM.REG.LCK
		CALL F.READ(FN.LCK, IDS.LCK<LCK>, RECORD.LCK, F.LCK, ERROR.LCK)
		Y.SUM.LCK += RECORD.LCK<AC.LCK.LOCKED.AMOUNT>
	NEXT LCK
	
	;*Saldo Disponible
	Y.SALDO.DISPONIBLE = Y.WORKING.BAL - Y.SUM.LCK
	
	Y.SUM.SALDO.DISPONIBLE += Y.SALDO.DISPONIBLE
	;*Saldo Total
	Y.SALDO.TOTAL = TRIM(REC.ACC.CURR<AC.ONLINE.ACTUAL.BAL>)
	Y.SUM.SALDO.TOTAL += Y.SALDO.TOTAL
RETURN

END
