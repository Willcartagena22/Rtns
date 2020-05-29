*-----------------------------------------------------------------------------
* <Rating>69</Rating>
*-----------------------------------------------------------------------------
* Version 1.0 24/09/2015  FBatres Initial
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.VALIDA.AA.INTERNET
*-----------------------------------------------------------------------------
* Program Description: Valida si un CLIENTE ya posee arreglo de banca personas
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.CUSTOMER.ARRANGEMENT
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPEN
    GOSUB PROCESS
    RETURN

INIT:
    FN.AA.ARR.ACT	= 'F.AA.ARRANGEMENT.ACTIVITY'
    F.AA.ARR.ACT	= ''
    FN.ARR			= 'F.AA.ARRANGEMENT'
    F.ARR			= ''
    FN.CUS.ARR		= 'F.AA.CUSTOMER.ARRANGEMENT'
    F.CUS.ARR		= ''
    Y.CUS = R.NEW(AA.ARR.ACT.CUSTOMER)
    Y.ARR = R.NEW(AA.ARR.ACT.ARRANGEMENT)
    Y.PERSONAL = 'PERSONAL'
    
*-----------------------------------------------------------------------------
*------DEBUG------------------------------------------------------------------
*	Y.CUS  = '100909'
*	Y.ARR = 'AA15205B419J'
*	Y.ARR = 'AA15205B419K'
*-----------------------------------------------------------------------------
    
    RETURN

OPEN:
    CALL OPF(FN.AA.ARR.ACT,F.AA.ARR.ACT)
    CALL OPF(FN.ARR,F.ARR)
    CALL OPF(FN.CUS.ARR,F.CUS.ARR)
    RETURN

PROCESS:
	CALL F.READ(FN.CUS.ARR,Y.CUS,R.CUS.ARR,F.CUS.ARR,CUS.ARR.ERR)
	Y.PRD.LN = R.CUS.ARR<AA.CUSARR.PRODUCT.LINE,Y.POS.PL>
	Y.ARR.ID = R.CUS.ARR<AA.CUSARR.ARRANGEMENT,Y.POS.AR>
	CONVERT FM TO SM IN Y.PRD.LN
    CONVERT FM TO SM IN Y.ARR.ID
    LOOP
    	REMOVE ARR.ID FROM Y.ARR.ID SETTING POS.ARR
    WHILE ARR.ID
		CALL F.READ(FN.ARR,ARR.ID,R.AA.ARR,F.ARR,ARR.ERR)
        Y.PRODUCT.LINE = R.AA.ARR<AA.ARR.PRODUCT.LINE>
        Y.PRODUCT = R.AA.ARR<AA.ARR.PRODUCT>
        Y.STATUS = R.AA.ARR<AA.ARR.ARR.STATUS>
        IF Y.PRODUCT.LINE EQ 'INTERNET.SERVICES' THEN
        	IF Y.PRODUCT EQ Y.PERSONAL THEN
        		IF Y.STATUS EQ 'AUTH' THEN
		        	IF Y.ARR NE ARR.ID THEN
		        		ETEXT = "Cliente ya posee un arreglo de este tipo con numero :":ARR.ID
			    		CALL STORE.END.ERROR
			    		BREAK
		        	END
		        END
	        END
        END 
    REPEAT
    RETURN

    END
