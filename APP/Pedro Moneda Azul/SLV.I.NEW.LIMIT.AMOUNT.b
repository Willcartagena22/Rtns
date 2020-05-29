*-----------------------------------------------------------------------------
* <Rating>-33</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.NEW.LIMIT.AMOUNT
*-----------------------------------------------------------------------------
* Rutina que extrae el monto limite del acuerdo de banca en linea para anadirselo al ISA
*-----------------------------------------------------------------------------
* Modification History :
* Date              who          Reference          description
*26-NOV-18       PSANCHEZ          25674			initial version
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
    $INSERT I_AA.ACTION.CONTEXT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.PROTECTION.LIMIT
    $INSERT I_AA.APP.COMMON
    $INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.AA.CUSTOMER
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN
	INIT:
	    FN.CUS = 'F.CUSTOMER'
	    F.CUS = ''
		CALL OPF(FN.CUS,F.CUS)
		
	    EQU CUST TO 'CUSTOMER'
		;* Customer ID
		Y.ID.CUS =  AA$R.ARRANGEMENT<AA.ARR.CUSTOMER>
		Y.TXT = 'Y.ID.CUS ': Y.ID.CUS
	     GOSUB WRITE_LOG_FILE
		;*Y.ID.CUS = '122935'
	RETURN
	
	PROCESS:
	CALL F.READ(FN.CUS, Y.ID.CUS, ARR.CUS, F.CUS, ERR)
				Y.TXT = 'ARR.CUS: ': ARR.CUS
	    	  GOSUB WRITE_LOG_FILE
	    IF ARR.CUS THEN
			CALL GET.LOC.REF(CUST, 'LF.AML.AMT.TCIB', POS)
			;* Monto aproximado (Customer)
	    	Y.CUS.LIM.AMOU = ARR.CUS<EB.CUS.LOCAL.REF, POS>
	    	  Y.TXT = 'Y.CUS.LIM.AMOU: ': Y.CUS.LIM.AMOU
	    	  GOSUB WRITE_LOG_FILE
	    END
	    
	    ;* Monto Limite (ISA)
	    R.NEW(AA.PRCT.LIMIT.AMOUNT) = Y.CUS.LIM.AMOU
	    Y.TXT = 'R.NEW(AA.PRCT.LIMIT.AMOUNT): ': Y.CUS.LIM.AMOU
	    GOSUB WRITE_LOG_FILE
	    ;*CRT Y.CUS.LIM.AMOU
	    R.NEW(AA.PRCT.CURRENCY) = 'USD'
	    Y.TXT = 'R.NEW(AA.PRCT.CURRENCY): ': 'USD'
	    GOSUB WRITE_LOG_FILE
	RETURN
	WRITE_LOG_FILE:
		DIR.NAME = 'SIF.OUT' 
		R.ID = 'LOG_LIMIT' : '_' : TODAY : '.txt'
		OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
			WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
			END
		CLOSESEQ SEQ.PTR 
RETURN
END
