*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
PROGRAM TEST.RTN
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.BATCH
	$INSERT I_F.SLV.RET.ISR.AVERAGE.BALANCE
*-----------------------------------------------------------------------------
	PUTENV("OFS_SOURCE=GCS")
    CALL JF.INITIALISE.CONNECTION 
	
	FN.SALDOS = 'F.SLV.RET.ISR.AVERAGE.BALANCE'
	F.SALDOS = ''
	

	CALL OPF(FN.SALDOS, F.SALDOS)

	;* Recuperar el promedio de saldos
	CALL F.READ(FN.SALDOS, '100909-2017', R.SALDOS, F.SALDOS, ERROR.READ)
	PERIODOS = R.SALDOS<SLV.AVG.ISR.PERIOD>
	CUENTAS = R.SALDOS<SLV.AVG.ISR.ACCOUNT>
	SALDOS = R.SALDOS<SLV.AVG.ISR.AVERAGE.BALANCE>
	
	CRT R.SALDOS	
	CRT PERIODOS
	CRT CUENTAS
	CRT SALDOS 
	
END