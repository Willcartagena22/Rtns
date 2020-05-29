*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.IMP.P.GET.SAL.ANT(CUENTA.PRESTAMO)
*-----------------------------------------------------------------------------
*@AUTOR EURIAS 
*@UTIL OBTIENE EL SALDO ANTERIOR AL PAGO DE PRESTAMO
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.CONTRACT.BALANCES
$INSERT I_F.AA.ACTIVITY.BALANCES
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
    saldoActual = ABS(saldoActual)
*-----------------------------------------------------------------------------
	;*OBTENER EL CAPITAL PAGADO PARA DESCONTARLO DEL SALDO ACTUAL
	capitalpagado = CUENTA.PRESTAMO
	CALL SLV.UTIL.IMP.P.GET.CAP(capitalpagado);*retorna el capital pagado.
    CUENTA.PRESTAMO = saldoActual + capitalpagado    
    
END
