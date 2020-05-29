*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.UPD.PARAM.MAZUL.MAS
*-----------------------------------------------------------------------------
* Modification History :
* vburgos 	10.09.2019  Version Inicial
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.UPD.PARAMS.MAZUL
$INSERT I_F.EB.SLV.KEYS.PARAMS
*-----------------------------------------------------------------------------

GOSUB INI
GOSUB OPF
GOSUB PROCESS

INI:
	FN.KEYS='F.EB.SLV.KEYS.PARAMS'
	F.KEYS=''
	
	Y.KEYS.ID='SLV.TOKEN'
	R.KEYS=''
RETURN


OPF:
	CALL OPF(FN.KEYS, F.KEYS)
RETURN

PROCESS:
	
	CALL F.READ(FN.KEYS, Y.KEYS.ID, R.KEYS, F.KEYS, ERR.KEYS)
	
	R.KEYS<EB.SLV18.VALOR,2,10> =  R.NEW(EB.SLV79.MONTO.DIARIO)
	R.KEYS<EB.SLV18.VALOR,2,5> = R.NEW(EB.SLV79.MONTO.TXN)
	R.KEYS<EB.SLV18.VALOR,2,8> = R.NEW(EB.SLV79.MONTO.MES)
	R.KEYS<EB.SLV18.VALOR,2,9> = R.NEW(EB.SLV79.CANT.DIA)
	R.KEYS<EB.SLV18.VALOR,2,7> = R.NEW(EB.SLV79.CANT.MES)
	R.KEYS<EB.SLV18.VALOR,2,3> = R.NEW(EB.SLV79.VIG.TOKEN)
	
	
		
	CALL F.WRITE(FN.KEYS, Y.KEYS.ID, R.KEYS)
*	CALL JOURNAL.UPDATE(FN.KEYS)
RETURN




END
