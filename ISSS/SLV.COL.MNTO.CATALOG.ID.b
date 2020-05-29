*-----------------------------------------------------------------------------
* <Rating>346</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.COL.MNTO.CATALOG.ID
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.2		CSurio		15.08.17	Consulta Id en APP Catalogo Colector Gral
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------

GOSUB INICIALIZAR
GOSUB OPENFILE
GOSUB PROCESS

RETURN

INICIALIZAR:
    FN.MNTO.CATALOG	     = 'F.EB.SLV.COL.MNTO.CATALOG'
    F.MNTO.CATALOG	     = ''
    FN.MNTO.CATALOG.GNRL = 'F.EB.SLV.MNTO.CATALOG.COL.GNRL'
    F.MNTO.CATALOG.GNRL  = ''

    RETURN

OPENFILE:
    CALL OPF(FN.MNTO.CATALOG, F.MNTO.CATALOG)
    CALL OPF(FN.MNTO.CATALOG.GNRL, F.MNTO.CATALOG.GNRL)
    
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
	;*@Author  : Ronald Ortiz
	;*@Date    : 20170420
	;*@Descrip : Se agrega la versión para el catálogo de calendario
	ELSE IF NOMBRE.VERSION EQ ',INPUT4' THEN
	    COMI = TODAY:'-4'
	END
	ELSE IF NOMBRE.VERSION EQ ',CALENDARIO' THEN
	    
	    CALL F.READ(FN.MNTO.CATALOG.GNRL,COMI,RESPONSE.CALENDAR,F.MNTO.CATALOG.GNRL,ERR.CALENDAR)
	    
	    IF RESPONSE.CALENDAR EQ '' THEN
	       GOSUB GET.CORRELATIVE
	       COMI = ID.CATALOG.GNRL:'-5'
	    END
	END
RETURN

GET.CORRELATIVE:
RANDOM.NUMBER = RND(10000000000)
ID.CATALOG.GNRL = SUBSTRINGS(RANDOM.NUMBER,0,7)
RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'MHLogs'
    R.ID = 'MNTO.CATALOGID.':TODAY:'.txt'
	;* hacer que escriba un archivo
    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    
    CLOSESEQ SEQ.PTR
RETURN

END
