*-----------------------------------------------------------------------------
* <Rating>-44</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.A.MANDATES.COLECTORES
*-----------------------------------------------------------------------------
*  
*-----------------------------------------------------------------------------
* Modification History :
* Version 1.0 			  RGaray 		Initial
* Version 1.1 2017.06.06  RCortes 		Se modifica para que los pagos autorizados para CINTEX no sean enviados a PEX 
* Version 1.2 
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER 
$INSERT I_TSS.COMMON
$INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------

	GOSUB INIT 
	GOSUB OPENFILE
	GOSUB PROCESS 
	
	RETURN 
	
INIT: 

    FN.FT   	= 'F.FUNDS.TRANSFER'
    F.FT   		= ''	

	FN.FT.NAU	= 'F.FUNDS.TRANSFER$NAU'
	F.FT.NAU 	= ''
	
	FN.FT.HIS	= 'F.FUNDS.TRANSFER$HIS'
	F.FT.HIS 	= ''
	
 	;* Obteniendo ID de PEX
 	CALL GET.LOC.REF ('FUNDS.TRANSFER', 'LF.ID.COL.MOD', ID.COL.MOD.POS)

	S.APP.ID = ID.NEW
	
	;********************
	;* S.APP.ID = 'FT15221G5M6R'
	;********************
	
	CRT OFS$OPERATION 
	CRT MESSAGE 
	CRT V.TRANSAC.TYPE
RETURN 

OPENFILE:
	CALL OPF(FN.FT, F.FT)
	CALL OPF(FN.FT.NAU, F.FT.NAU)
	CALL OPF(FN.FT.HIS, F.FT.HIS)
RETURN

PROCESS: 

    CALL F.READ(FN.FT, S.APP.ID, A.RECORD, F.FT, ERR.FT)
    IF ERR.FT NE '' THEN
		
		Y.FT.NAU.ERR = ""
		CALL F.READ(FN.FT.NAU, S.APP.ID, A.RECORD, F.FT.NAU, Y.FT.NAU.ERR)
		IF Y.FT.NAU.ERR THEN	
			CALL F.READ.HISTORY(FN.FT.HIS, S.APP.ID, A.RECORD, F.FT.HIS, ERR.HIS)
		END
	
    END
    
	LF.ID.COL.MOD 	= A.RECORD<FT.LOCAL.REF, ID.COL.MOD.POS> 
	V.TRANSAC.TYPE 	= A.RECORD<FT.TRANSACTION.TYPE> 

	;* Si es pago de colector y este NO proviene de CINTEX entonces
	IF V.TRANSAC.TYPE EQ 'AC64' AND LF.ID.COL.MOD EQ ''  THEN 
		CALL SLV.V.PAGO.CL.NPE
	END 
	
RETURN 

*-----------------------------------------------------------------------------
ESCRIBIR.ARCHIVO: 
*-----------------------------------------------------------------------------
	DIR.NAME= 'TCIB'
	R.ID   = 'TCE_AUTH_': TODAY[1,4] : '-' : TODAY[5,2] :'.txt'
	;* hacer que escriba un archivo 
	
	OPENSEQ DIR.NAME,R.ID TO SEQ.PTR 
		WRITESEQ TXT.ARCH APPEND TO SEQ.PTR THEN
		END 
	CLOSESEQ SEQ.PTR 
	
RETURN 

END
