*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.LIMPIAR.VAL.MAS

*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.VALIDACIONES.MASIVOS

*-----------------------------------------------------------------------------


GOSUB INIT
GOSUB OPENED
GOSUB LIMPIAR

INIT:
FN.EB.SLV.VALIDACIONES.MASIVOS='F.EB.SLV.VALIDACIONES.MASIVOS'
F.EB.SLV.VALIDACIONES.MASIVOS=''
RETURN

OPENED:
CALL OPF(FN.EB.SLV.VALIDACIONES.MASIVOS, F.EB.SLV.VALIDACIONES.MASIVOS)
RETURN

LIMPIAR:
IDV='BKML1550784584349.18680170105578401'
CALL F.READ(FN.EB.SLV.VALIDACIONES.MASIVOS, IDV, REC.VAL, F.EB.SLV.VALIDACIONES.MASIVOS, ERR.VAL.MAS)
IF REC.VAL THEN
		REC.VAL<EB.SLV30.ESTADO> = ''
		REC.VAL<EB.SLV30.FECHA> =''
		REC.VAL<EB.SLV30.NOMBRE.CAMPO>=''
		REC.VAL<EB.SLV30.VALIDACION>=''
		CALL F.WRITE(FN.EB.SLV.VALIDACIONES.MASIVOS, IDV, REC.VAL)
		CALL JOURNAL.UPDATE(FN.EB.SLV.VALIDACIONES.MASIVOS)	

END

RETURN
END
