*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.GET.ARR.X.ACC(ID.CUENTA)
*-----------------------------------------------------------------------------
*@EURIAS OBTENER EL ID DEL ARRANGEMENT POR EL NUMERO DE CUENTA PRESTAMO
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.ACCOUNT 
*-----------------------------------------------------------------------------
	idArr = ID.CUENTA
    nombreApliacionArr ="F.ACCOUNT"
	aplicacionFullArr = ''
    aplicacionDataArr=''
    ErrorArr=''
    CALL OPF(nombreApliacionArr, aplicacionFullArr)
    CALL F.READ(nombreApliacionArr,idArr,aplicacionDataArr,aplicacionFullArr,ErrorArr)
    ID.CUENTA = aplicacionDataArr<AC.ARRANGEMENT.ID>
    
    
END
