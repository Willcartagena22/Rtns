*-----------------------------------------------------------------------------
* <Rating>-46</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.STMT.NARR.SEG(APPLIC.ID,APPLIC.REC,STMT.ID,STMT.REC,OUT.TEXT)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
	$INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------

    GOSUB INIT 
    GOSUB PROCESS 

*-----------------------------------------------------------------------------

INIT:

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

	FN.PRM 	= 'F.EB.SLV.GLOBAL.PARAM'
    F.PRM 	= ''
    CALL OPF(FN.PRM, F.PRM)

*	;* Para obtener la cta. transitoria de MH NPE
*	Y.PRM.ID = 'ACC.NPE.MH'
	
    RETURN


PROCESS:
    
    ;* *****************
    ;* APPLIC.ID	= 'FT15216WN7NX'
    ;* *****************

    ID.FT = FIELD(APPLIC.ID, ";", 1)
    ;*ID.FT='FT152214J9VT'
    
    GOSUB READ.FT 

*    CALL F.READ(FN.PRM, Y.PRM.ID, R.PRM, F.PRM, ERR.PRM)
*    IF R.PRM THEN
		
	APPL.NAME = 'FUNDS.TRANSFER'
	FLD.NAME  = 'LF.COD.CL':VM:'LF.CINTEX'
	FLD.POS   = ''
		    
	CALL MULTI.GET.LOC.REF(APPL.NAME,FLD.NAME,FLD.POS)
		
	LOCPOSCL = FLD.POS<1,1>
	LOCPOSCINTEX = FLD.POS<1,2>
		
        ORD.BANK = R.FT<FT.ORDERING.BANK>
        VAR.COL = R.FT<FT.LOCAL.REF><1,LOCPOSCL>
        VAR.DEBIT.ACC= R.FT<FT.DEBIT.ACCT.NO>        
        VAR.CREDIT.ACC = R.FT<FT.CREDIT.ACCT.NO>
        VAR.COL=SUBSTRINGS(VAR.COL,1,2)
    	
    	BEGIN CASE
    	
	        CASE VAR.COL EQ '03'
	        VAR.DEBIT.ACCOUNT = TRIM(STMT.REC)
	        VAR.DEBIT.ACC=SUBSTRINGS(VAR.DEBIT.ACCOUNT,1,3)
	      
	    	 	BEGIN CASE
	    	 	
			    	CASE ORD.BANK EQ 'PRIMA SEGUROS' AND VAR.COL EQ '03' 
			    		OUT.TEXT = 'Prima Seguros Azul'
			    	
			    	CASE ORD.BANK EQ 'LIQUIDACION COMISION' AND VAR.COL EQ '03'	
			    		OUT.TEXT = 'comisi':CHAR(243):'n Seguros Azul'
		
			    	CASE ORD.BANK EQ 'REINTEGRO PRIMA' AND VAR.COL EQ '03'	AND VAR.DEBIT.ACC NE 'USD'
			    		OUT.TEXT = 'reintegro de prima Seguros Azul'
			    		
			    	CASE ORD.BANK EQ 'REINTEGRO PRIMA' AND VAR.COL EQ '03'	AND VAR.DEBIT.ACC EQ 'USD'
			    		OUT.TEXT = 'reintegro de prima Seguros Azul'
			    					    		
			    	CASE 1
			    	OUT.TEXT=' A Cuenta No.- ':VAR.CREDIT.ACC	
			    	
	    		END CASE
    		
    		CASE 1
    		
    		OUT.TEXT=' A Cuenta No.- ':VAR.CREDIT.ACC
    		
    	END CASE
    	
    	
*	END
	TEXTO.ARCHIVO ='STMT.REC': VAR.DEBIT.ACC:'-----'
    GOSUB ESCRIBIR.ARCHIVO
	

	
	
    RETURN
*** </region>
ESCRIBIR.ARCHIVO:
    DIR.NAME= 'SegPruebasEstadoCuenta'
    R.ID   = 'SegPruebasEstadoCuenta':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN
*-----------------------------------------------------------------------------

*** <region name= READ.FT>
READ.FT:
*** <desc> </desc>
R.FT = ''
ERR.FT = ''
Y.ERR= ''
 CALL F.READ(FN.FUNDS.TRANSFER,ID.FT,R.FT,F.FUNDS.TRANSFER,ERR.FT)
    IF NOT(R.FT) THEN
        FN.FUNDS.TRANSFER$HIS='F.FUNDS.TRANSFER$HIS'
        F.FUNDS.TRANSFER$HIS=''
        Y.ERR = ''
        CALL OPF(FN.FUNDS.TRANSFER$HIS,F.FUNDS.TRANSFER$HIS)
        CALL EB.READ.HISTORY.REC (F.FUNDS.TRANSFER$HIS,ID.FT,R.FT,Y.ERR)
    END
    
RETURN
*** </region>

END





