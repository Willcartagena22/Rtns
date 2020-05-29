*-----------------------------------------------------------------------------
* <Rating>-31</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.PAGO.ACH.REVERSA
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.EB.SLV.TRANSACTION.ACH
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB PROCESS
RETURN

INIT:
	FN.TXN.ACH = 'F.EB.SLV.TRANSACTION.ACH'
	F.TXN.ACH = ''
	CALL OPF(FN.TXN.ACH,F.TXN.ACH)
RETURN

PROCESS:
	ID.DETALLE.ACH	= R.NEW(FT.ORDERING.CUST)<1,1>
	
	TEXTO.ARCHIVO = 'ID.DETALLE.ACH -> ':ID.DETALLE.ACH
	GOSUB ESCRIBIR.ARCHIVO
	
	CALL F.READ(FN.TXN.ACH,ID.DETALLE.ACH,R.TXN.ACH,F.TXN.ACH,E.TXN.ACH)
	
	TEXTO.ARCHIVO = 'R.TXN.ACH -> ':R.TXN.ACH:' E.TXN.ACH -> ':E.TXN.ACH
	GOSUB ESCRIBIR.ARCHIVO
		
	IF R.TXN.ACH THEN
		R.TXN.ACH<EB.SLV98.STATUS> = '3'
		R.TXN.ACH<EB.SLV98.ID.FT.REV> = ID.NEW
		CALL F.WRITE(FN.TXN.ACH,ID.DETALLE.ACH,R.TXN.ACH) 
	END
	
RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'ACH'
    R.ID   = 'ACH.PAGO.REVERSA.':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN

END
