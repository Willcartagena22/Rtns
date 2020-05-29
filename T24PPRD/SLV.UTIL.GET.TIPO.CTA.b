*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.GET.TIPO.CTA(cuenta)
*-----------------------------------------------------------------------------
*@autor eurias
*@date20141117
*@util obtener el tipo de cuenta dependiendo de la categoria devuelve 0 -> si es ahorro 1 -> si es corriente
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.ACCOUNT
*-----------------------------------------------------------------------------
	idCuenta = cuenta
	nombreApliacion ="F.ACCOUNT"
	aplicacionFull = ''
    aplicacionData=''
    categoria = ''
    Error=''
    
    CALL OPF(nombreApliacion, aplicacionFull)
    CALL F.READ(nombreApliacion,idCuenta,aplicacionData,aplicacionFull,Error)
    categoria  = aplicacionData<AC.CATEGORY>
    IF categoria  GT 6000 AND categoria LT 6499 THEN
    	cuenta=0 ;*ahorro
    END 
    IF categoria  GT 1000 AND categoria LT 1999 THEN
    	cuenta=1 ;*corriente
    END
END
