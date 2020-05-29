*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE POST.MESSAGE.CW
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------

OFS.STR=''

OFS.SRC='SLVOFSRP'
OFS.MSG.ID='SECTOR,/I//PROCESS///,WALVARADO/Bazul20164,5005/NOMBREOFSDETAILA,/DUPLICATE,DESCRIPTION=PRUEBA ESCTOR POR OFS,SHORT.NAME=P.RUTINA'
OPTIONS=''
CALL OFS.POST.MESSAGE(OFS.REC, OFS.MSG.ID, OFS.SOURCE.ID, OPTIONS)
CALL JOURNAL.UPDATE("")

RETURN



END
