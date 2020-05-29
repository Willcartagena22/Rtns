*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.OFS.IM.UPLOAD.SIM2
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.IM.DOCUMENT.IMAGE
*-----------------------------------------------------------------------------
GOSUB PROCESAR
GOSUB OFS.IMAGE


PROCESAR:
	DESCRIPT=R.NEW(IM.DOC.DESCRIPTION)
	UPLOAD.ID=ID.NEW
	DESCRIPT=DESCRIPT:'B.jpg'
	

RETURN

OFS.IMAGE:

    OFS.CLI=OFS.CLI :'IM.DOCUMENT.UPLOAD,SLV.SET.DUI.SIMP/I/PROCESS//0,/,':UPLOAD.ID:',UPLOAD.ID:1:1=':UPLOAD.ID:',FILE.UPLOAD:1:1=':DESCRIPT:','
    Y.OPTIONS='SLVOFSPS'

    CALL OFS.POST.MESSAGE(OFS.CLI,Y.RESPONSE,Y.OPTIONS,TXN.RESULT)

    RETURN



END
