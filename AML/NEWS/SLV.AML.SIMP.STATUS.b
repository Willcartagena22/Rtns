*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.AML.SIMP.STATUS
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.DISPO.ITEMS
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB ABRIR
GOSUB PROCESS

INIT:
    FN.DISPO.ITEMS = "F.DISPO.ITEMS$NAU"
    F.DISPO.ITEMS = ""
RETURN

ABRIR:
CALL OPF(FN.DISPO.ITEMS,F.DISPO.ITEMS)
RETURN

PROCESS:
CALL F.READ(FN.DISPO.ITEMS, TRANSACTION.ID,R.DISPO.ITEMS,F.DISPO.ITEMS,YERR)



RETURN


END
