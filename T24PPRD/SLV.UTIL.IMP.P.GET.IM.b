*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.IMP.P.GET.IM(ARRANGEMENT)
*-----------------------------------------------------------------------------
*@AUTOR EURIAS TASA INTERES MORATORIO
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.AA.INTEREST

*-----------------------------------------------------------------------------
IF(PUTENV("OFS_SOURCE=GCS")) THEN	
    CRT "OK"
END
CALL SLV.UTIL.GET.ARR.X.ACC(ARRANGEMENT)
*-----------------------------------------------------------------------------
;*obtener fecha de inicio de acuerdo
	idArr = ARRANGEMENT
    nombreApliacion ="F.AA.ARRANGEMENT"
	aplicacionFull = ''
    aplicacionDataAA=''
    Error=''
    CALL OPF(nombreApliacion, aplicacionFull)
    CALL F.READ(nombreApliacion,idArr,aplicacionDataAA,aplicacionFull,Error)
    fechaInicio = aplicacionDataAA<AA.ARR.START.DATE>
*-----------------------------------------------------------------------------
;*obtener monto de prestamo
    nombreApliacion ="F.AA.ARR.INTEREST"
    idTermAmount = ARRANGEMENT:'-PENALTYINT-':fechaInicio:'.1'
	aplicacionFull = ''
    aplicacionDataTermAmount=''
    Error=''
    CALL OPF(nombreApliacion, aplicacionFull)
    CALL F.READ(nombreApliacion,idTermAmount,aplicacionDataTermAmount,aplicacionFull,Error)
    interesMora = aplicacionDataTermAmount<AA.INT.EFFECTIVE.RATE>
*-----------------------------------------------------------------------------
	ARRANGEMENT = interesMora:"%"
	

END
