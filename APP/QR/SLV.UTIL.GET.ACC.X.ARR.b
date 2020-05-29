*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.GET.ACC.X.ARR(arrangenment)
*-----------------------------------------------------------------------------
*@AUTOR EURIAS
*@20141117
*@UTIL OBTENER EL NUMERO DE CUENTA INGRESANDO COMO PARAMETRO EL NUMERO DE ARRANGEMENT
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AA.ARRANGEMENT
*-----------------------------------------------------------------------------
	idArr = arrangenment
    nombreApliacionArr ="F.AA.ARRANGEMENT"
	aplicacionFullArr = ''
    aplicacionDataArr=''
    ErrorArr=''
    CALL OPF(nombreApliacionArr, aplicacionFullArr)
    CALL F.READ(nombreApliacionArr,idArr,aplicacionDataArr,aplicacionFullArr,ErrorArr)
    arrangenment = aplicacionDataArr<AA.ARR.LINKED.APPL.ID>
END
