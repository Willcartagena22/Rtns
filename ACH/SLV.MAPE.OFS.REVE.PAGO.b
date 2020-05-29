*-----------------------------------------------------------------------------
* <Rating>-42</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.MAPE.OFS.REVE.PAGO
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.GLOBAL.PARAM
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.AC.LOCKED.EVENTS
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE

	CALL GET.LOC.REF('FUNDS.TRANSFER','LF.TCIB.FUT', Y.TCIB.FUT)
    MARCA.ACH = R.NEW(FT.LOCAL.REF)<1,Y.TCIB.FUT>
    IF MARCA.ACH NE 'ACHS' THEN
    	GOSUB PROCESS
    END
RETURN

INIT:
    FN.LCK 				= 'F.AC.LOCKED.EVENTS$HIS'
	F.LCK 				= ''
	ID.PARAM.OFS 		= 'OFS.BLOQUEO.VEN.MH'
	ID.OFS 				= ''
RETURN

OPENFILE:
	CALL OPF(FN.LCK,F.LCK)
RETURN

PROCESS:
	ID.LCK = R.NEW(FT.ORDERING.CUST)<1,1>
	CALL GET.LOC.REF('AC.LOCKED.EVENTS','LF.ID.COL.MOD', LF.ID.FT) 
	CALL GET.LOC.REF ('AC.LOCKED.EVENTS', 'LF.CHQ.BENEFICI', CHQ.BENEFICI)
	CALL GET.LOC.REF ('AC.LOCKED.EVENTS', 'LF.MOTIVO.CHQ', MOTIVO.CHQ)
	CALL GET.LOC.REF ('AC.LOCKED.EVENTS', 'LF.TRAN.TIME', TRAN.TIME)
	CALL GET.LOC.REF ('AC.LOCKED.EVENTS', 'LF.USER.BANCA', USER.BANCA)
	CALL GET.LOC.REF ('AC.LOCKED.EVENTS', 'LF.MOTIVO.AZUL', MOTIVO.AZUL)
	
	CALL F.READ(FN.LCK, ID.LCK:';1', RECORD.LCK, F.LCK, ERROR.LCK)
	
	RECORD.LCK<AC.LCK.RECORD.STATUS> = ''
	RECORD.LCK<AC.LCK.LOCAL.REF,LF.ID.FT> = ID.NEW
	
	;*Envio de OFS en Linea
    CALL SLV.OFS.UTIL.OL.TRX(ID.OFS, RECORD.LCK, ID.PARAM.OFS, Y.OUT)
	
RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'COLECTORES'
    ;*DIR.NAME='C:\Users\rramos\Documents\Temp'
    R.ID   = 'PAGO.MONEDA.REVE.PE.':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
 END
    CLOSESEQ SEQ.PTR
    
RETURN

END
