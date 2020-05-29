*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.VAL.FECH.INI.FIN
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.CLS.FECH.REC.SSF
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS

INIT:

    FN.FECH.SSF  = 'F.EB.SLV.CLS.FECH.REC.SSF'
    F.FECH.SSF	 = ''
    CALL OPF(FN.FECH.SSF,F.FECH.SSF)
RETURN

PROCESS:
    FECHA.INI = R.NEW(EB.SLV42.FECHA.INICIO)
	FECHA.FIN = R.NEW(EB.SLV42.FECHA.FIN)

    IF FECHA.FIN LT FECHA.INI THEN
        STRERR = 'Fecha Final debe de ser mayor o igual a la Fecha Inicial'
        GOSUB CRT_ERROR
    END
RETURN
    
CRT_ERROR:
    ETEXT = STRERR
    AS = 1
    CALL STORE.END.ERROR
RETURN

END