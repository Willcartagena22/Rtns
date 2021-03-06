*-----------------------------------------------------------------------------
* <Rating>-52</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.FT.ACH.LIMIT
*-----------------------------------------------------------------------------
*jnramirez    validando monto limite en las ft de banca persona
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.ACCOUNT
$INSERT I_F.EB.SLV.KEYS.PARAMS
$INSERT I_F.EB.SLV.ACUMULADOR.ACH
$INSERT I_F.EB.SLV.TRANSACTION.ACH

*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS

INIT:
	FN.EB.SLV.KEYS.PARAMS    = 'F.EB.SLV.KEYS.PARAMS'
	F.EB.SLV.KEYS.PARAMS     = ''
	FN.EB.SLV.ACUMULADOR.ACH = 'F.EB.SLV.ACUMULADOR.ACH'
	F.EB.SLV.ACUMULADOR.ACH	 = ''	
    FN.EB.SLV.TRANSACTION.ACH = 'F.EB.SLV.TRANSACTION.ACH'
	F.EB.SLV.TRANSACTION.ACH  = ''
	FN.EB.ACCOUNT			  = 'F.ACCOUNT' 
	F.ACCOUNT				  =  ''	
	
	Y.VER.AMOUNT = R.NEW(FT.DEBIT.AMOUNT)
    Y.DEBIT.ACC     = R.NEW(FT.DEBIT.ACCT.NO)
*	Y.DEBIT.ACC  = '10000000108993'
*	Y.CUSTOMER     = R.NEW(FT.DEBIT.CUSTOMER) 
	Y.CUSTOMER   = '' 
*	R.NEW<FT.PROFIT.CENTRE.CUST>
	
*	Y.COSTUMER   = '107223'
	Y.FIELD      = 'SLV.LIMITES.ACH'
	EQU Y.PARAM.ID  TO 'BP'
	STRERR = 'EB-ERROR.ACH.LIMIT'
	
	Y.LIMIT.DAY   = ''
	Y.LIMIT.MONTH = '' 
	Y.DATE  = TODAY[1,6]
	
	TEXTO.ARCHIVO = 'TEXTO ARCHIVO VALOR': Y.VER.AMOUNT
	GOSUB ESCRIBIR.ARCHIVO
	TEXTO.ARCHIVO = 'DATE':Y.DATE
	GOSUB ESCRIBIR.ARCHIVO
	TEXTO.ARCHIVO = 'FT.DEBIT.ACCT.NO':Y.DEBIT.ACC
	GOSUB ESCRIBIR.ARCHIVO
*	Y.DATE  = '201901'  
	
	
RETURN 


OPENFILE:
	CALL OPF(FN.EB.SLV.KEYS.PARAMS, F.EB.SLV.KEYS.PARAMS)
	CALL OPF(FN.EB.SLV.TRANSACTION.ACH, F.EB.SLV.TRANSACTION.ACH)
	CALL OPF(FN.EB.ACCOUNT, F.ACCOUNT)
RETURN

PROCESS:
	CALL F.READ (FN.EB.ACCOUNT, Y.DEBIT.ACC, RECORDS.ACC, F.ACCOUNT, ERR)
	IF RECORDS.ACC NE '' THEN 
		Y.CUSTOMER = RECORDS.ACC<AC.CUSTOMER>
		TEXTO.ARCHIVO = 'Y.CUSTOMER':Y.CUSTOMER
    	GOSUB ESCRIBIR.ARCHIVO
	END 

 	CALL F.READ (FN.EBSLV.KEYS.PARAMS, Y.FIELD, REC, F.EB.SLV.KEYS.PARAMS, ER.KEY)
 		FIND Y.PARAM.ID IN REC<EB.SLV18.PARAM.ID> SETTING Ap, Vp THEN	
	 		Y.LIMIT.DAY   = FIELD(REC<EB.SLV18.VALOR><Ap, Vp>, SM, 1)
	 		Y.LIMIT.DAY   = FIELD(Y.LIMIT.DAY,'~',2)
	 		TEXTO.ARCHIVO = 'Y.Y.LIMIT.DAY ':Y.LIMIT.DAY 
    		GOSUB ESCRIBIR.ARCHIVO
	 		
	 		Y.LIMIT.MONTH = FIELD(REC<EB.SLV18.VALOR><Ap, Vp>, SM, 2)
	 		Y.LIMIT.MONTH = FIELD(Y.LIMIT.MONTH,'~',2)
	 		
	 		TEXTO.ARCHIVO = 'Y.LIMIT.MONTH':Y.LIMIT.MONTH
    		GOSUB ESCRIBIR.ARCHIVO
 		END
 		
 		
    TEXTO.ARCHIVO = 'ANTES DE GET ACUMULADOR':Y.CUSTOMER
    GOSUB ESCRIBIR.ARCHIVO
    CALL F.READ(FN.EB.SLV.ACUMULADOR.ACH, Y.CUSTOMER, REC.ACUM, F.EB.SLV.ACUMULADOR.ACH, ER.ACU)
    	FIND Y.DATE IN REC.ACUM<EB.SLV96.FECHA.MES> SETTING Ap, Vp THEN
    		Y.VP = Vp
    	END
    	Y.END = DCOUNT (REC.ACUM<EB.SLV96.ID.FT,Y.VP>,SM)
    	
    	TEXTO.ARCHIVO = 'Y.END ':Y.END 
    	GOSUB ESCRIBIR.ARCHIVO
    	
    	FOR I = 1 TO Y.END 
    		ID.FT = REC.ACUM<EB.SLV96.ID.FT,Y.VP,I>
    		CALL F.READ(FN.EB.SLV.TRANSACTION.ACH,ID.FT,RESULT, F.EB.SLV.TRANSACTION.ACH, ERR)
    			Y.FECHAS = RESULT<EB.SLV98.FECHA.UPD>[1,10]
    			
    			Y.MONTH.TXN = FIELD(Y.FECHAS, '/',2) 
    			Y.DAY.TXN   = FIELD(Y.FECHAS, '/',1)
    			Y.YEAR.TXN  = FIELD(Y.FECHAS, '/',3)
    			Y.DATE.COMPARE = Y.YEAR.TXN:Y.MONTH.TXN:Y.DAY.TXN
    			
    			
    		CRT Y.DATE.COMPARE
    		CRT RESULT<EB.SLV98.STATUS>
    		CRT RESULT<EB.SLV98.STATUS>
    		CRT TODAY
    		
    		IF (RESULT<EB.SLV98.STATUS> EQ '1' OR RESULT<EB.SLV98.STATUS> EQ '2') AND  Y.DATE.COMPARE EQ TODAY THEN 
    		Y.MONTO = Y.MONTO + RESULT<EB.SLV98.MONTO>
    		CRT 'MONTO ':Y.MONTO
    		
    		
    		TEXTO.ARCHIVO = 'MONTO':Y.MONTO
    		GOSUB ESCRIBIR.ARCHIVO
    		END
    		 
    	NEXT I 
    	
    	Y.MONTO  = Y.MONTO + Y.VER.AMOUNT
    	
    	IF Y.MONTO GT Y.LIMIT.DAY  THEN
    		GOSUB CRT_ERROR		
    	END 
RETURN

CRT_ERROR:
	  	E = 'EB-ERROR.ACH.LIMIT'
		CALL ERR 

RETURN 

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'FATCA'
    R.ID   = 'SLV.V.LIMITE.TRX.ACH':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN




END
