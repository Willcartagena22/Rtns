*-----------------------------------------------------------------------------
* <Rating>-11</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.ACCOUNT.TYPE(CUENTA)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------

GOSUB PROCESS

PROCESS:

;*OBTENEMOS EL TIPO DE CUENTA
ACC = CUENTA
CALL SLV.UTIL.GET.TIPO.CTA(ACC)

IF ACC EQ '0' THEN
CUENTA = 'CUENTA DE AHORRO'
END
ELSE
CUENTA = 'CUENTA CORRIENTE'
END

RETURN

END
