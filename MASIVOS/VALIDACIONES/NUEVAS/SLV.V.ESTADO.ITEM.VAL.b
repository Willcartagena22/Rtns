*-----------------------------------------------------------------------------
* <Rating>-40</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.ESTADO.ITEM.VAL
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.MASTER.MASIVOS
    $INSERT I_F.EB.SLV.ITEMS.MASIVOS
*-----------------------------------------------------------------------------
INIT:
    FN.EB.SLV.ITEMS.MASIVOS='F.EB.SLV.ITEMS.MASIVOS'
    F.EB.SLV.ITEMS.MASIVOS=''
RETURN

ABRIR:
    CALL OPF(FN.EB.SLV.ITEMS.MASIVOS, F.EB.SLV.ITEMS.MASIVOS)
RETURN

PROCESAR:
IDV=ID.NEW
		CALL F.WRITE(FN.EB.SLV.ITEMS.MASIVOS, IDV, REC.VAL)

RETURN

ESCRIBIR.ARCHIVO:
	    DIR.NAME= 'MASIVOS'
	    R.ID   = 'Validaciones.txt'
	    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
	    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
	    END
	    CLOSESEQ SEQ.PTR
    RETURN



END
