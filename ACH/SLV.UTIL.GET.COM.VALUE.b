*-----------------------------------------------------------------------------
* <Rating>-65</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.GET.COM.VALUE(IN.REF, OUT.VAL.COM,OUT.VAL.IVA)
*-----------------------------------------------------------------------------
* 
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE 
$INSERT I_F.TAX
$INSERT I_F.EB.SLV.COMM.TRANS.CUS
$INSERT I_F.CUSTOMER
*-------------------------------------------------------------
	GOSUB INIT
	GOSUB OPENFILE
	GOSUB PROCESS
	GOSUB OUTPUT
	RETURN
	
*-------------------------------------------------------------
INIT:
    FN.CUSTOMER = 'F.CUSTOMER' 
    F.CUSTOMER = ''
	FN.TAX = 'F.TAX'
	F.TAX  = ''
	
	FN.COMM.TRANS.CUS   = 'F.EB.SLV.COMM.TRANS.CUS'
    F.COMM.TRANS.CUS    = ''
	
    ;*Definicion de variables
    Y.REF   	= 'SLV.COMM.TRANS.CUS'
	Y.PARAM 	= 'COMM.TRA.ACH'
	Y.VAL.COM   = 0
	IN.TAX 		= '11'
	Y.EXENTO.IVA = 'NO'	
	Y.COM.DIF 	= 'NO'      
RETURN
*-----------------------------------------------------------------------------
OPENFILE:
	CALL OPF(FN.CUSTOMER,F.CUSTOMER)
	CALL OPF(FN.TAX, F.TAX)
	CALL OPF(FN.COMM.TRANS.CUS, F.COMM.TRANS.CUS)
RETURN
*-----------------------------------------------------------------------------
PROCESS:
	Y.ID.CUS = IN.REF
*	Y.ID.CUS = '117221'
	;*comision diferenciada
	CALL F.READ (FN.COMM.TRANS.CUS, Y.ID.CUS, R.COMM.TRANS.CUS, F.COMM.TRANS.CUS, ERR.COMM.TRANS.CUS)

	;*Obtener ID de tipo de comision
	ARR.PARAM = R.COMM.TRANS.CUS<EB.SLV.CTC.PARAM.ID>
	;*Buscar posicion del parametro de entrada
	FIND Y.PARAM IN ARR.PARAM SETTING Ap,Vp THEN
	END
	
	Y.COM.DIF = R.COMM.TRANS.CUS<EB.SLV.CTC.COMISION.DIF><Ap,Vp>
	
	IF Y.COM.DIF EQ 'SI' THEN
		Y.VAL.COM = R.COMM.TRANS.CUS<EB.SLV.CTC.MONTO.COMISION><Ap,Vp>
	END
	
	ELSE
		;*comision publicada
		CALL SLV.GET.KEYS.PARAM(Y.REF,Y.PARAM,'','',Y.VAL.COM)
	END	

	;*iva	
	GOSUB TAX.IVA	
RETURN
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
OUTPUT:
	OUT.VAL.COM = Y.VAL.COM
	
	IF TAX.IVA NE 0 THEN
		Y.VAL.IVA = DROUND(Y.VAL.COM * TAX.IVA,2)
		OUT.VAL.IVA	= Y.VAL.IVA
	END	
RETURN

*-----------------------------------------------------------------------------
TAX.IVA:
*-----------------------------------------------------------------------------
    CALL SLV.V.EXENTO.IVA.ER(Y.ID.CUS,Y.EXENTO.IVA)
    
    IF Y.EXENTO.IVA EQ 'NO' THEN
		;*Obtener tasa de impuesto al valor agregado (IVA)
		Y.RATE.IVA = 0
		GOSUB GET.TAX
		TAX.IVA = Y.RATE.IVA / 100
	END
RETURN

*-----------------------------------------------------------------------------
GET.TAX:
*-----------------------------------------------------------------------------	
	SELECT.STMT = "SELECT " : FN.TAX : " WITH @ID LIKE '" : IN.TAX : "...'"

	CALL EB.READLIST(SELECT.STMT, Y.LIST, '', NO.REC, SYSTEM.RETURN.CODE)
	Y.LAST = ''
	FOR I = 1 TO NO.REC 											;*Buscar Registro de Tax mas Actualizado
		IF Y.LAST EQ '' OR Y.LAST LT FIELD(Y.LIST<I>,'.',2) THEN
			Y.LAST = FIELD(Y.LIST<I>,'.',2)
		END
	NEXT I
	
	CALL F.READ (FN.TAX, IN.TAX : '.' : Y.LAST, R.TAX, F.TAX, ERR.TAX)
	IF R.TAX THEN	
	   Y.RATE.IVA  = R.TAX <EB.TAX.RATE>
	END
RETURN


END
