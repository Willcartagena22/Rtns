*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.GET.TITLE.PROD.ACCOUNT(ACCOUNT)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------

CALL SLV.UTIL.GET.TIPO.CTA(CUENTA)

NOMBRE.CUENTA = ''

IF CUENTA EQ '0' THEN
NOMBRE.CUENTA = 'CUENTA DE AHORRO'
END
ELSE
NOMBRE.CUENTA = 'CUENTA CORRIENTE'
END

CUENTA = NOMBRE.CUENTA

END
