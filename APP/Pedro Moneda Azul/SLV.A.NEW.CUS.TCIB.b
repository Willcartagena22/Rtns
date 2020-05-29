*-----------------------------------------------------------------------------
* <Rating>-41</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.A.NEW.CUS.TCIB
*-----------------------------------------------------------------------------
* Nombre: SLV.A.NEW.CUS.TCIB
* Descripcion: Rutina Encargada de actualizar el usuario de External User a Activo
* Version	Autor		Fecha		Comentario
* 1.0		psanchez	28/01/2019 	initial
*-----------------------------------------------------------------------------
* Modification History :
* El campo Status = ACTIVE y el campo Change Reason = Usuario Activo
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.EXTERNAL.USER
$INSERT I_F.EB.SLV.NEW.TCIB.USER
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
INIT:
	FN.EXT.USER = 'F.EB.EXTERNAL.USER'
	F.EXT.USER	= ''	
	CALL OPF(FN.EXT.USER,F.EXT.USER)
		
	FN.SLV.NEW.TCIB.USER = 'F.EB.SLV.NEW.TCIB.USER'
    F.SLV.NEW = ''
    CALL OPF(FN.SLV.NEW.TCIB.USER, F.SLV.NEW)
    
    FN.TABLE.CUS 		= 'F.CUSTOMER'
    F.TABLE.CUS 		= ''
    CALL OPF(FN.TABLE.CUS, F.TABLE.CUS)
    
RETURN

PROCESS:
	R.NEW(EB.XU.STATUS) = 'ACTIVE'
	R.NEW(EB.XU.STATUS.CHANGE.REASON) = 'Usuario Activo'
	
	Y.CUSTOMER.ID='161949'
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
		Y.TABLE.ID = LF.NIT	
		REC.SLV.NEW.TCIB	= ""
		CALL F.READ(FN.SLV.NEW.TCIB.USER,Y.TABLE.ID,REC.SLV.NEW.TCIB,F.SLV.NEW,Y.ERROR.USER)
		REC.SLV.NEW.TCIB<EB.SLV10.DEVICE.STATUS> = 'ASIGNADO'
		REC.SLV.NEW.TCIB<EB.SLV10.RESERVADO3> ='AUTORIZADO'
		CALL F.WRITE(FN.SLV.NEW.TCIB.USER,Y.TABLE.ID,REC.SLV.NEW.TCIB)

RETURN

WRITE_LOG_FILE:
    DIR.NAME = 'CHQ.OUT'
    R.ID =  'PSAUTH_' : TODAY : '.txt'

    OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
    WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN

END
