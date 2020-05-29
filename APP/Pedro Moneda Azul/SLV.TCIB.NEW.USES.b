*-----------------------------------------------------------------------------
* <Rating>-76</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.TCIB.NEW.USES
*-----------------------------------------------------------------------------
* Rutina de validacion y creacion de usuario external user en HID
*-----------------------------------------------------------------------------
* Modification History :
* Date              who          Reference          description
*15-JAN-19       PSANCHEZ          25674			initial version 
*------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.EXTERNAL.USER
$INSERT I_TSS.COMMON
$INSERT I_GTS.COMMON
$INSERT I_F.EB.EXTERNAL.USER.DEVICE
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.NEW.TCIB.USER
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS

INIT:
	FN.EXT.US  = "F.EB.EXTERNAL.USER"
    F.EXT.US   = ""
    CALL OPF(FN.EXT.US,F.EXT.US)
    FN.CUS 	= "F.CUSTOMER"
    F.CUS  	= ""
    CALL OPF(FN.CUS,F.CUS)
    
   FN.EXT.US.DEV  = 'F.EB.EXTERNAL.USER.DEVICE'
    F.EXT.US.DEV   = ''
    CALL OPF (FN.EXT.US.DEV, F.EXT.US.DEV)
    
    FN.SLV.NEW.TCIB.USER = 'F.EB.SLV.NEW.TCIB.USER'
    F.SLV.NEW = ''
    CALL OPF(FN.SLV.NEW.TCIB.USER, F.SLV.NEW)
	
	;*********************************************
	Y.CUS.ID = R.NEW(EB.XU.CUSTOMER)
	Y.US.ID = R.NEW(EB.XU.NAME)
	;*********************************************
	Y.IP = TSS$CLIENTIP
 	Y.PORT=8000
    Y.COD.ERR = '2'
    Y.COD.SUC = '01'
    
RETURN

PROCESS:
;************************DEBUG********************
	Y.US.ID = 'BANCAZULMOVIL'
	Y.CUS.ID = '161949'
	Y.ARRANGEMENT = 'AA192336WC4C'
;************************DEBUG********************
	
	GOSUB EXTRACTION.CUSTOMER
	
	IF Y.EMAIL THEN
		IF Y.MOV THEN
			GOSUB CREATE.USER.HID
			IF Y.COD.SUC EQ Y.COD THEN

			END
			ELSE
				Y.MSG.ERROR = "EB-SLV.USER.CONTACT.BANK"
				GOSUB GET.MESSAGE.ERROR
			END
			
		END
		ELSE
			Y.MSG.ERROR = "EB-SLV.USER.CONTACT.BANK"
			GOSUB GET.MESSAGE.ERROR
		END
	END
	ELSE
		Y.MSG.ERROR = "EB-SLV.USER.CONTACT.BANK"
		GOSUB GET.MESSAGE.ERROR
	END
	IF Y.MSG.ERROR EQ '' THEN
		GOSUB ACTUALIZAR.TABLA.LOCAL
	END

RETURN
EXTRACTION.CUSTOMER:
	CALL F.READ (FN.CUS,Y.CUS.ID,R.CUS,F.CUS,ER)
	    IF R.CUS  THEN
	    	 ;*Parametros para crear usuario
	        Y.PRIMER.NOMBRE = R.CUS<EB.CUS.NAME.1>
	        Y.PRIMER.APELLIDO = R.CUS<EB.CUS.TEXT>
	        ;*TODO: Validar cuando email sea nulo y telefono movil       
	        Y.EMAIL = R.CUS<EB.CUS.EMAIL.1>
	        Y.MOV = R.CUS<EB.CUS.SMS.1>
	     ;*NIT DE usuario
	    CALL GET.LOC.REF('CUSTOMER','LF.NIT', POS.NIT)
	    LF.NIT = R.CUS<EB.CUS.LOCAL.REF, POS.NIT>
	        
	    END
	    ELSE
	    	Y.MSG.ERROR = "EB-SLV.USER.CONTACT.BANK"
		GOSUB GET.MESSAGE.ERROR
	    END
	    
RETURN

CREATE.USER.HID:
 	THIS.PACKAGE.CLASS = "com.mad.banco.azul.hid.t24.T24HID"
	THIS.METHOD.USU = "createUser"
	Y.PRIMER.NOMBRE='VNOMBRE'
	CALLJ.ARGUMENTS.USU = Y.US.ID:'%M':Y.PRIMER.NOMBRE:'%M':Y.PRIMER.APELLIDO:'%M':Y.EMAIL:'%M':Y.MOV
	CALLJ.ERROR.USU = ""
	CALLJ.RESPONSE.USU = ""
	;*Llamada para la creacion de usuario
	CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.USU,CALLJ.ARGUMENTS.USU,CALLJ.RESPONSE.USU,CALLJ.ERROR.USU)
	IF CALLJ.ERROR.USU THEN
*		ETEXT = "No Se puede crear el usuario - no se pudo invocar libreria: ":CALLJ.ERROR.USU:" ":Y.US.ID
		Y.MSG.ERROR = "EB-SLV.USER.CONTACT.BANK"
		GOSUB GET.MESSAGE.ERROR	
	END
	Y.COD = FIELD(CALLJ.RESPONSE.USU,'%M',1)
	Y.MSG = FIELD(CALLJ.RESPONSE.USU,'%M',2)
RETURN


WRITE_LOG_FILE:
	DIR.NAME = 'CHQ.OUT'
	R.ID =  'PS_' : TODAY : '.txt'

	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
		END
	CLOSESEQ SEQ.PTR 
RETURN
GET.MESSAGE.ERROR:
    ETEXT = Y.MSG.ERROR
    CALL STORE.END.ERROR
RETURN


ACTUALIZAR.TABLA.LOCAL:
	;*Comparar arreglo entrante con lo que esta en la tabla, si es nuevo id empresa agregarlo, sino incluir la posicion especifica en arreglo para un "nuevo" F.WRITE.
		Y.TABLE.ID = LF.NIT	
		REC.SLV.NEW.TCIB	= ""
		CALL F.READ(FN.SLV.NEW.TCIB.USER,Y.TABLE.ID,REC.SLV.NEW.TCIB,F.SLV.NEW,Y.ERROR.USER)
		REC.SLV.NEW.TCIB<EB.SLV10.EXTERNAL.USER> = Y.US.ID
		REC.SLV.NEW.TCIB<EB.SLV10.RESERVADO3> = 'INAU'
		CALL F.WRITE(FN.SLV.NEW.TCIB.USER,Y.TABLE.ID,REC.SLV.NEW.TCIB)
RETURN


END
