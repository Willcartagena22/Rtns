*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.IMP.P.GET.MON(ARRANGEMENT)
*-----------------------------------------------------------------------------
*@AUTOR EURIAS OBTENER EL MONTO DEL PRESTAMO
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.AA.TERM.AMOUNT
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
    nombreApliacion ="F.AA.ARR.TERM.AMOUNT"
    idTermAmount = ARRANGEMENT:'-COMMITMENT-':fechaInicio:'.1'
	aplicacionFull = ''
    aplicacionDataTermAmount=''
    Error=''
    CALL OPF(nombreApliacion, aplicacionFull)
    CALL F.READ(nombreApliacion,idTermAmount,aplicacionDataTermAmount,aplicacionFull,Error)
    monto = aplicacionDataTermAmount<AA.AMT.AMOUNT>
*-----------------------------------------------------------------------------
	ARRANGEMENT = monto
	CRT monto
END
