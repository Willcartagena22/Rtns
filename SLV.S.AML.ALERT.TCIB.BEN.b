*-----------------------------------------------------------------------------
* <Rating>-54</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.S.AML.ALERT.TCIB.BEN(GROUP.TXN,ACCT.NO,CUSTOMER.NO,AMOUNT,ADDIT.INFO,RESULT,ADDIT.MSG)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
*
* Nombre: SLV.S.AML.ALERT.TCIB.BEN
* Descripción: Rutina para Lanzar Alerta TCIB para Beneficiarios
*
*-----------------------------------------------------------------------------
* Modification History :
* Autor		Fecha		Comentario
* OCornejo	06.04.2016	Initial Code
* eurias    11.10.2016  cambio por bi empresas, tratamiento doble aut
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.AML.TCIB.COUNTER
$INSERT I_F.EB.SLV.UIF.CHNL.TRX
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
RETURN

	INIT:
		FN.COUNTER  = 'F.EB.SLV.AML.TCIB.COUNTER'
		F.COUNTER   = ''
		FN.TXN  	= 'F.EB.SLV.UIF.CHNL.TRX'
		F.TXN  		= ''
		FN.CUSTOMER = 'F.CUSTOMER'
		F.CUSTOMER = ''
		POS.SEGMENT = ''
		Y.CUSTOMER = 'CUSTOMER'
		FIELDNAME.ARR = 'SEGMENT'
		CALL MULTI.GET.LOC.REF(Y.CUSTOMER,FIELDNAME.ARR,POS.SEGMENT)
		
		;*Constante
		EQU ALERT.ID TO 'AL.BEN'
		
		;*Variables
		Y.FT.TYPE   = R.NEW(FT.TRANSACTION.TYPE)
		Y.ORIG.CUST = R.NEW(FT.DEBIT.CUSTOMER)
		Y.DEST.CUST = R.NEW(FT.CREDIT.CUSTOMER)
	RETURN
	
	OPENFILE:
		CALL OPF(FN.COUNTER, F.COUNTER)
		CALL OPF(FN.TXN, F.TXN)
		CALL OPF(FN.CUSTOMER,F.CUSTOMER)
	RETURN
	
	PROCESS:    	
    	;*Buscar Contador
    	;*---------------
    	CALL F.READ(FN.COUNTER, CUSTOMER.NO, R.COUNTER, F.COUNTER, ERR.COUNTER)
    	
    	;*Buscar Parametro de Alerta
    	;*--------------------------
    	CALL SLV.GET.AML.TCIB.PARAM(ALERT.ID, CUSTOMER.NO, Y.LIMITE)
    	
    	;*Validar Si Debe Dispararse la Alerta
    	;*------------------------------------
    	Y.CANT = R.COUNTER<EB.SLV63.NUM.BEN>
    	IF Y.CANT EQ '' THEN
    		Y.CANT = 0
    	END
    	
    	;*Sumar 1 ya que a este punto aun no ha dado Commit la Tabla de Contadores y esto Simula que ya lo hizo
    	;*-----------------------------------------------------------------------------------------------------
    	Y.CANT += 1
    	
    	;*Validar que estos Clientes No hayan Tenido Operaciones Entre Ellos en el Mes
    	;*----------------------------------------------------------------------------
    	SELECT.STMT =  "SELECT " : FN.TXN : " WITH TXN.DATE GE '" : TODAY[0,6] : "01'"
		SELECT.STMT	:= " AND TXN.DATE LE '" : TODAY : "'"
		SELECT.STMT	:= " AND ORIGIN.CUST EQ '" : Y.ORIG.CUST : "'"
		SELECT.STMT	:= " AND DESTIN.CUST EQ '" : Y.DEST.CUST : "'"		
		CALL EB.READLIST(SELECT.STMT, Y.LIST, '', NO.REC, ERR.LIST)
			;*TEST
			;*Y.LIST = 'FT16244RPMRH.20160831'
			;*NO.REC = 1
		;*Verificar si ya se hizo Transferencias a este Beneficiario/Remitente en el Mes
		;*------------------------------------------------------------------------------
		FLG.PROCEDER = 0
		ID.CUSTOMER = Y.ORIG.CUST
		GOSUB EVAL.SEGMENT ; * 
		IF NO.REC EQ 0 THEN
			FLG.PROCEDER = 1
		END
    	
    	;*De momento queda quemado ACAT porque debe Analizarse la Parametria para Evitar Lanzar la Alerta Cuando
    	;*la transaccion se ACIB por ejemplo o se dispare junto con otras alertas ya que en ese escenario no le
    	;*corresponderia : OCORNEJO - 08.04.2016
    	;*------------------------------------------------------------------------------------------------------
    	IF Y.CANT GE Y.LIMITE AND Y.FT.TYPE EQ 'ACAT' AND Y.ORIG.CUST EQ CUSTOMER.NO AND FLG.PROCEDER EQ 1 THEN
    		RESULT = 'S'
    	END
	RETURN

*-----------------------------------------------------------------------------

*** <region name= EVAL.SEGMENT>
EVAL.SEGMENT:
*** <desc>Evaluar segmento, si empresa y el resultado es 1 se debe agregar 1 al contador, esto por la doble autorizacion de bi empresas </desc>
CUST.SEGMENTO = POS.SEGMENT<1,1>
CALL F.READ(FN.CUSTOMER,ID.CUSTOMER,R.CUSTOMER,F.CUSTOMER,ERR.CUST)
CUST.SEGMENTO = R.CUSTOMER<EB.CUS.LOCAL.REF><1,CUST.SEGMENTO>
IF (CUST.SEGMENTO EQ 2) AND (NO.REC EQ 1)THEN;*si es empresa y el numero de rec devueltos establecer en uno para que el counter tome 1
	;*EVALUAR SI ES EL MISMO FT DE ESTE MOMENTO EL QUE SE ESTA EVALUANDO, SI ES EL MISMO SIGNIFICA QUE NO SE DEBE CONTABILIZAR, SINO NO ES EL MISMO DEBE DE TOMARLO EN CUENTA PARA LANZAR ALERTA
	FT.ID = ID.NEW
	FINDSTR FT.ID IN Y.LIST SETTING Ap,Vp THEN
		;*si es el mismo id q r.new dentro del flujo autorizacion bi empresas entonces significa q se encuentra en el paso aut, por tanto se debe lanzar la alerta nuevamente
		NO.REC = 0		
	END
END
RETURN
*** </region>

END

