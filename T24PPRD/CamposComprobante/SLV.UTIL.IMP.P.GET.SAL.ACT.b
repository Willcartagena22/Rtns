*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.IMP.P.GET.SAL.ACT(CUENTA.PRESTAMO)
*-----------------------------------------------------------------------------
*@AUTOR EURIAS 
*@UTIL OBTIENE EL SALDO ACTUAL DEL PRESTAMO
*-----------------------------------------------------------------------------
* Modification History :.
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.CONTRACT.BALANCES
*-----------------------------------------------------------------------------
IF(PUTENV("OFS_SOURCE=GCS")) THEN	
    CRT "OK"
END
*-----------------------------------------------------------------------------

	idArr = CUENTA.PRESTAMO
    nombreApliacion ="F.EB.CONTRACT.BALANCES"
	aplicacionFull = ''
    aplicacionDataAA=''
    Error=''
    CALL OPF(nombreApliacion, aplicacionFull)
    CALL F.READ(nombreApliacion,idArr,aplicacionDataAA,aplicacionFull,Error)
    saldoActual= aplicacionDataAA<ECB.WORKING.BALANCE>
    CUENTA.PRESTAMO = (saldoActual * (-1));* cuando son remanentes se mostrara saldo negativo representando saldo a favor del cliente
END
