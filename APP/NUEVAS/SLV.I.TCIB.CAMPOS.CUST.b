*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.TCIB.CAMPOS.CUST
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB ABRIR
GOSUB COMPLETAR

INIT:
    FN_CUS 			= 'F.CUSTOMER'
    F_CUS 			= ''
    FN_SLVPA		= 'F.EB.SLV.GLOBAL.PARAM'
    F_SLVPA			= ''
    
RETURN

ABRIR:
    CALL OPF(FN_CUS, F_CUS)
    CALL OPF(FN_SLVPA,F_SLVPA)
    CALL GET.LOC.REF('CUSTOMER', 'LF.AML.AMT.TCIB', POSCusAMT)
	CALL GET.LOC.REF('CUSTOMER', 'LF.AML.MOT.TCIB', POSCusMOT)
	

RETURN



COMPLETAR:
CALL F.READ(FN_SLVPA,'CTA.SIMPLIFICADA.MONTO.MONTH', R.SLVPA, F_SLVPA, E.SLVPA)
    MONTO.MES = R.SLVPA<EB.SLV39.VALOR.PARAM>

R.NEW(EB.CUS.LOCAL.REF)<1,POSCusMOT>='CUENTA SIMPLIFICADA'
R.NEW(EB.CUS.LOCAL.REF)<1,POSCusAMT>=MONTO.MES

RETURN

END
