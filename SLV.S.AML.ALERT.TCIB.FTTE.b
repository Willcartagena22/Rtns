*-----------------------------------------------------------------------------
* <Rating>-36</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.S.AML.ALERT.TCIB.FTTE(GROUP.TXN,ACCT.NO,CUSTOMER.NO,AMOUNT,ADDIT.INFO,RESULT,ADDIT.MSG)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
*
* Nombre: SLV.S.AML.ALERT.TCIB.FTTE
* Descripción: Rutina para Lanzar Alerta TCIB Transferencias a Terceros Entrantes
*
*-----------------------------------------------------------------------------
* Modification History :
* Autor		Fecha		Comentario
* OCornejo	06.04.2016	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.AML.TCIB.COUNTER
$INSERT I_F.FUNDS.TRANSFER
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
RETURN

	INIT:
		FN.COUNTER    = 'F.EB.SLV.AML.TCIB.COUNTER'
		F.COUNTER     = ''
		
		;*Constante
		EQU ALERT.ID TO 'AL.FTTE'
		
		;*Variables
		Y.FT.TYPE     = R.NEW(FT.TRANSACTION.TYPE)
		Y.CUST.FT.ENT = R.NEW(FT.CREDIT.CUSTOMER)
	RETURN
	
	OPENFILE:
		CALL OPF(FN.COUNTER, F.COUNTER)
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
    	Y.CANT = R.COUNTER<EB.SLV63.NUM.FT.ENT>
    	IF Y.CANT EQ '' THEN
    		Y.CANT = 0
    	END
    	
    	;*Sumar 1 ya que a este punto aun no ha dado Commit la Tabla de Contadores y esto Simula que ya lo hizo
    	;*-----------------------------------------------------------------------------------------------------
    	Y.CANT += 1
    	
    	;*De momento queda quemado ACAT porque debe Analizarse la Parametria para Evitar Lanzar la Alerta Cuando
    	;*la transaccion se ACIB por ejemplo o se dispare junto con otras alertas ya que en ese escenario no le
    	;*corresponderia : OCORNEJO - 08.04.2016
    	;*------------------------------------------------------------------------------------------------------
    	IF Y.CANT GE Y.LIMITE AND Y.FT.TYPE EQ 'ACAT' AND Y.CUST.FT.ENT EQ CUSTOMER.NO THEN
    		RESULT = 'S'
    	END
	RETURN
END
