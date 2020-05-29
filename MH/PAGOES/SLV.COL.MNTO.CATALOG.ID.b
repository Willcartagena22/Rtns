*-----------------------------------------------------------------------------
* <Rating>170</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.COL.MNTO.CATALOG.ID
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------

GOSUB INICIALIZAR
GOSUB OPENFILE
GOSUB PROCESS

RETURN

INICIALIZAR:
    FN.MNTO.CATALOG	= 'F.EB.SLV.COL.MNTO.CATALOG'
    F.MNTO.CATALOG	= ''

    RETURN

OPENFILE:
    CALL OPF(FN.MNTO.CATALOG, F.MNTO.CATALOG)
    
    RETURN

PROCESS:
	NOMBRE.VERSION = PGM.VERSION
	
	IF NOMBRE.VERSION EQ ',INPUT' THEN
		COMI = TODAY:'-1'
	END
	ELSE IF NOMBRE.VERSION EQ ',INPUT2' THEN
		COMI = TODAY:'-2'
	END
	ELSE IF NOMBRE.VERSION EQ ',INPUT3' THEN
		COMI = TODAY:'-3'
	END
	
	RETURN
END
