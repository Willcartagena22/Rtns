*-----------------------------------------------------------------------------
* <Rating>-42</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.D.NEW.CUS.TCIB
*-----------------------------------------------------------------------------
* Rutina que limpia la informacion de la aplicacion local
*-----------------------------------------------------------------------------
* Modification History :
* Date              who          Reference          description
*01-FEB-19       PSANCHEZ          25674			initial version
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.NEW.TCIB.USER
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.EXTERNAL.USER
*-----------------------------------------------------------------------------
 GOSUB INIT
 GOSUB PROCESS
 INIT:
 	FN.TABLE.CUS 		= 'F.CUSTOMER'
    F.TABLE.CUS 		= ''
    CALL OPF(FN.TABLE.CUS, F.TABLE.CUS)

    FN.SLV.NEW.TCIB.USER	= 'F.EB.SLV.NEW.TCIB.USER'
    F.SLV.NEW		= ''
    CALL OPF(FN.SLV.NEW.TCIB.USER, F.SLV.NEW)

    FN.ERR  = 'F.EB.ERROR'
    F.ERR   = ''
    CALL OPF(FN.ERR, F.ERR)

    FN.EXT.USER	 = 'F.EB.EXTERNAL.USER'
    F.EXT.USER	 = ''
    CALL OPF(FN.EXT.USER, F.EXT.USER)
RETURN

PROCESS:

	
		Y.CUSTOMER.ID = R.NEW(EB.XU.CUSTOMER)
		Y.TXT = 'Y.CUSTOMER.ID: ' : Y.CUSTOMER.ID
		GOSUB WRITE_LOG_FILE
		CALL F.READ(FN.TABLE.CUS, Y.CUSTOMER.ID, A.EXT, F.TABLE.CUS, ERR.EXT)
		Y.TXT = 'A.EXT: ' : A.EXT
		GOSUB WRITE_LOG_FILE
		IF A.EXT THEN
			 CALL GET.LOC.REF('CUSTOMER','LF.NIT', POS.NIT)
		    LF.NIT =  A.EXT<EB.CUS.LOCAL.REF, POS.NIT>				
		    GOSUB ACTUALIZAR.TABLA.LOCAL
		END	

	
	
RETURN
ACTUALIZAR.TABLA.LOCAL:
	;*Comparar arreglo entrante con lo que esta en la tabla, si es nuevo id empresa agregarlo, sino incluir la posicion especifica en arreglo para un "nuevo" F.WRITE.	
		Y.TXT = 'ACTUALIZAR TABLA LOCAL '
		GOSUB WRITE_LOG_FILE
		Y.TABLE.ID = LF.NIT
		Y.TXT = 'Y.TABLE.ID: ' : Y.TABLE.ID
		GOSUB WRITE_LOG_FILE	
		REC.SLV.NEW.TCIB	= ""
		CALL F.WRITE(FN.SLV.NEW.TCIB.USER,Y.TABLE.ID,REC.SLV.NEW.TCIB)
RETURN
;* Archivo para Revisión de Errores
WRITE_LOG_FILE:
    DIR.NAME = 'CHQ.OUT'
    R.ID =  'PSDELETE_' : TODAY : '.txt'

    OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
    WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN

END
