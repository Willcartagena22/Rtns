*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.OFS.IMAGE.SIMP
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

GOSUB OFS.IMAGE
RETURN

OFS.IMAGE:
CUSTOMER=ID.NEW
DUI=R.NEW(EB.CUS.LEGAL.ID)
DUI=DUI

    OFS.CLI=OFS.CLI :'IM.DOCUMENT.IMAGE,SLV.SET.DUI.SIM/I/PROCESS//0,/,,IMAGE.APPLICATION:1:1=CUSTOMER,IMAGE.REFERENCE:1:1=':CUSTOMER:',SHORT.DESCRIPTION:1:1=DUI,DESCRIPTION:1:1=':DUI:','
    Y.OPTIONS='SLVOFSPS'

    CALL OFS.POST.MESSAGE(OFS.CLI,Y.RESPONSE,Y.OPTIONS,TXN.RESULT)

    RETURN

END