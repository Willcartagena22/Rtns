*-----------------------------------------------------------------------------
* <Rating>-11</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.NOINPUT.FIELDS.REC
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.LEND.REC.SSF
*-----------------------------------------------------------------------------
GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
	;*Definir NOINPUT campos de nombres
	IF R.NEW(EB.SLV76.CTA.ESTADO) NE 5 THEN
		T(EB.SLV76.MNT.RMES.CPROC)<3>='NOINPUT'
	END
	
RETURN
END
