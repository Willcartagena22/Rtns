*-----------------------------------------------------------------------------
* <Rating>-167</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.NEW.SMS.TOKEN
*-----------------------------------------------------------------------------
* Rutina que permite crear y validar un SMS Token para cliente TCIB
*-----------------------------------------------------------------------------
* Modification History :
* Date              who          Reference          description
*26-NOV-18       PSANCHEZ          25674			initial version      
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------
$INSERT I_GTS.COMMON
$INSERT I_F.EB.EXTERNAL.USER
$INSERT I_TSS.COMMON
$INSERT I_F.EB.EXTERNAL.USER.DEVICE
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.GLOBAL.PARAM
$INSERT I_F.USER
$INSERT I_F.EB.SLV.NEW.TCIB.USER

GOSUB INIT
GOSUB GENERATE.ID.TOKEN
GOSUB PROCESS
INIT:
    
    FN.EXTERNAL.USER.NAU = "F.EB.EXTERNAL.USER$NAU"
    F.EXTERNAL.USER.NAU = ""
    CALL OPF(FN.EXTERNAL.USER.NAU,F.EXTERNAL.USER.NAU)
    
    FN.EXTERNAL.USER.DEVICE = "F.EB.EXTERNAL.USER.DEVICE"
	F.EXTERNAL.USER.DEVICE = ""	
	CALL OPF(FN.EXTERNAL.USER.DEVICE,F.EXTERNAL.USER.DEVICE)
    
    FN.EXTERNAL.USER.DEVICE.NAU = "F.EB.EXTERNAL.USER.DEVICE$NAU"
	F.EXTERNAL.USER.DEVICE.NAU = ""	
	CALL OPF(FN.EXTERNAL.USER.DEVICE.NAU,F.EXTERNAL.USER.DEVICE.NAU)
	
	FN.CUSTOMER = 'F.CUSTOMER'
	F.CUSTOMER =''
	CALL OPF(FN.CUSTOMER,F.CUSTOMER)
	
	FN.TABLE.PA = 'F.EB.SLV.GLOBAL.PARAM'
	F.TABLE.PA = ''
	CALL OPF(FN.TABLE.PA, F.TABLE.PA)

    FN.SLV.NEW.TCIB.USER = 'F.EB.SLV.NEW.TCIB.USER'
    F.SLV.NEW = ''
    CALL OPF(FN.SLV.NEW.TCIB.USER, F.SLV.NEW)
	
	
	R.NEW(EB.EXDEV.CO.CODE) = ID.COMPANY
	R.NEW(EB.EXDEV.COMPANY) = ID.COMPANY
		Y.USER.TYPE = R.NEW(EB.EXDEV.USER.TYPE)
		Y.TIPO.TOKEN = 'SMS'
		Y.DESCRIPCION.TOKEN = 'SMS Token'
		Y.TIPO.OPERACION = 'N'	
		Y.BUS.TOKEN = 'NUMBER.SMSTOKEN'
RETURN
GENERATE.ID.TOKEN:

		Y.USER.SOFT = R.NEW(EB.EXDEV.USER.ID)


;***********************DEBUG******************************
;*Y.USER.SOFT = 'USER100198'
;***********************DEBUG******************************

		ERR.EXTERNAL.USER = "" ; R.EXTERNAL.USER = ""
		CALL F.READ(FN.EXTERNAL.USER.NAU,Y.USER.SOFT,R.EXTERNAL.USER.NAU,F.EXTERNAL.USER.NAU,ERR.EXTERNAL.USER)
		IF R.EXTERNAL.USER.NAU THEN
			Y.CUSTOMER.ID = R.EXTERNAL.USER.NAU<EB.XU.CUSTOMER>
			Y.CUSTOMER.IDTYPE = R.EXTERNAL.USER.NAU<EB.XU.USER.TYPE>
			Y.USER.ID =	R.EXTERNAL.USER.NAU<EB.XU.NAME> 
			R.NEW(EB.EXDEV.USER.CUST.ID) = Y.CUSTOMER.ID
			R.NEW(EB.EXDEV.USER.ID) = Y.USER.ID
		;*Obtiene cantidad de Token de un tipo especifico en los registros INAU
			Y.SELECT = 'SELECT ' :FN.EXTERNAL.USER.DEVICE.NAU: ' WITH USER.ID EQ ' : Y.USER.ID  : ' AND DEVICE.TYPE EQ ' : "'": Y.DESCRIPCION.TOKEN:"'"	
		   	CALL EB.READLIST(Y.SELECT , LIST.ARR, '', NO.OF.RECS, Y.ERR)
		   	Y.TOKEN.ID = NO.OF.RECS + 1
		   	
		   	GOSUB GET.CORRELATIVO.TOKEN
		   	Y.NUM.SERIE = R.NEW(EB.EXDEV.CODE.SERIE)
			Y.IP = TSS$CLIENTIP
			Y.ID =FIELD(ID.NEW,'-',1)
		END
		ELSE
			AF = 2
			Y.TXT = 'Usuario de banca en linea no existe.'
*			E = Y.MSG
			GOSUB CRT_ERROR
			
		END
RETURN
GET.CORRELATIVO.TOKEN:
	
	Y.CANTIDAD.TOKEN = ''	
	;* Busca en tabla global.param la cantidad maxima de token por cada tipo
	CALL F.READ(FN.TABLE.PA, Y.BUS.TOKEN, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    Y.CANTIDAD.TOKEN = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
    
    Y.VOU.FEC = TIMESTAMP()
	GOSUB PARSE.DATETIME
    
    IF Y.TOKEN.ID LE Y.CANTIDAD.TOKEN THEN
		Y.USER.SOFT = Y.USER.ID : "-" : Y.TIPO.TOKEN : "-": Y.VOU.FEC : "-" :  Y.TOKEN.ID
		ID.NEW = Y.USER.SOFT
*		COMI = Y.USER.SOFT
	END
	ELSE
		ERR.NUM.TOK = 'TRUE'
		AF=EB.EXDEV.DEVICE.TYPE
        ETEXT='La cantidad de tokens de este tipo ha sido excedido'
        CALL STORE.END.ERROR 	
	END
   
RETURN
PARSE.DATETIME:

		utcDateTime =  Y.VOU.FEC
		UTC.FLAG = ''
		;*Evaluar UTC Time or Standard Time
		
		FINDSTR "." IN utcDateTime SETTING Ap, Vp THEN
			UTC.FLAG = '1'
		END
		
		IF UTC.FLAG EQ '1' THEN
			localZoneDate1 = OCONV(LOCALDATE(utcDateTime,localZone),'D4-E')
			localZoneTime1= OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
			Y.VOU.FEC = localZoneDate1:'-':localZoneTime1
		END
		ELSE
			Y.DAY.BC = utcDateTime[3,2]
			Y.MONTH.BC = utcDateTime[5,2]
			Y.YEAR.BC = utcDateTime[1,2]
			Y.DATE.BC = OCONV(ICONV(utcDateTime[1,6],'D'),'D4-E')
			;*Y.DATE.BCs = OCONV(ICONV(utcDateTime,'D'),'D4/E')
*			Y.DATE.BC = Y.DAY.BC:'/':Y.MONTH.BC:'/20':Y.YEAR.BC
			Y.TIME.BC = utcDateTime[7,2]:':':utcDateTime[9,2]:':':'00'
			Y.VOU.FEC = Y.DATE.BC: '-': Y.TIME.BC
		END
RETURN
PROCESS:
	
	IF Y.TIPO.TOKEN EQ 'SMS' AND ERR.NUM.TOK NE 'TRUE' THEN

		GOSUB VALIDAR.SMS.TOKEN
	END

RETURN
VALIDAR.SMS.TOKEN:
	CALL F.READ(FN.CUSTOMER, Y.CUSTOMER.ID, A.EXT, F.CUSTOMER, ERR.EXT)
	IF A.EXT THEN
		 CALL GET.LOC.REF('CUSTOMER','LF.NIT', POS.NIT)
	    LF.NIT =  A.EXT<EB.CUS.LOCAL.REF, POS.NIT>
		Y.NUMBER.TEL = A.EXT<EB.CUS.SMS.1>
		IF Y.NUMBER.TEL NE '' THEN
				R.NEW(EB.EXDEV.SOFT.NUMBER) = Y.NUMBER.TEL
				;*GOSUB CREATE.SMSTOKEN
					GOSUB UPDATE.SMS.HID
					GOSUB CREATE.SMSTOKEN
					GOSUB BLOCKED.CHANNEL.SMS
					GOSUB ACTUALIZAR.EXTERNAL.USER
					GOSUB ACTUALIZAR.TABLA.LOCAL
					;*GOSUB CREATE.SMSTOKEN
			END
			ELSE
				;*Error
				;*E = 'Cliente ' : Y.CUSTOMER.ID : ' no posee Telefono Movil. No se puede asociar SMS Token'
				;*CALL ERR
				STRERR = ' Cliente ' : Y.CUSTOMER.ID : ' no posee Telefono Movil. No se puede asociar SMS Token'
                GOSUB CRT_ERROR_CEL
			END	
	END 

RETURN
CREATE.SMSTOKEN:
	Y.METODO= 'requestOTPUsingSMS_V2'
	Y.IN.PARAMETER =  Y.USER.ID
	R.OUT.DATA = ''
	
	CALL SLV.GET.APIHID(Y.METODO, Y.IN.PARAMETER, R.OUT.DATA)
		
	IF R.OUT.DATA NE '' THEN
		R.NEW(EB.EXDEV.CODE.ACTIVATION) = R.OUT.DATA
	END
	ELSE
		E = "Error no ha sido posible asociar el SMS Token"
	   	CALL ERR
	END
	
	
RETURN
UPDATE.SMS.HID:

	Y.METODO = 'updatePhone'
*	Y.IN.PARAMETER =  Y.IP:'%M':Y.ID:'%M':Y.NUM.SERIE
	Y.IN.PARAMETER =  Y.USER.ID:'%M':Y.NUMBER.TEL
	R.OUT.DATA = ''
	CALL SLV.GET.APIHID(Y.METODO, Y.IN.PARAMETER, R.OUT.DATA)
	
	Y.COD = FIELD(R.OUT.DATA,'%M',1)
   	Y.MSG = FIELD(R.OUT.DATA,'%M',2)
    IF Y.COD EQ '01' THEN

    END
    ELSE
       	E = Y.MSG
       	CALL ERR
    END

RETURN
BLOCKED.CHANNEL.SMS:
	Y.METODO= 'blockedChannelSms'
	Y.IN.PARAMETER = Y.USER.ID:"%M":"CH_WEB"
	R.OUT.DATA = ''
	CALL SLV.GET.APIHID(Y.METODO, Y.IN.PARAMETER, R.OUT.DATA)
	Y.COD = FIELD(R.OUT.DATA,'%M',1)
	Y.MSG = FIELD(R.OUT.DATA,'%M',2)
    IF Y.COD EQ '01' THEN

    END
    ELSE
	    E = "Error al Asignar Dispositivo- ":Y.MSG
	    CALL ERR
    END
RETURN
*-----------------------------------------------------------------------------
ACTUALIZAR.EXTERNAL.USER:
*-----------------------------------------------------------------------------
*;Deshabilita el Canal TCIB para que los usuarios no puedan logearse estando 
*;no autorizado el registro.
;*Cambiar Credencial de Usuario en HID
    Y.PAQUETE 			= "com.mad.banco.azul.hid.t24.T24HID"
    Y.METODO  			= "updateBloqueoCanal"
    Y.IN.PARAMETER 		= Y.USER.ID:"%M":"CH_WEB"
    Y.OUT.PARAMETER		= ''
    Y.ERROR.RESPONSE 	= ''
    
    ;*LLamada updateBloqueoCanal en HIDBancoAzulAPI
    CALL EB.CALLJ(Y.PAQUETE,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)

    IF Y.ERROR.RESPONSE THEN
        E = "Error al Crear Usuario - no se pudo invocar libreria: ":Y.ERROR.RESPONSE
        CALL ERR
    END
    ELSE
	    Y.COD = FIELD(Y.OUT.PARAMETER,'%M',1)
	    Y.MSG = FIELD(Y.OUT.PARAMETER,'%M',2)
	    IF Y.COD EQ '01' THEN
	    END
	    ELSE
		    E = "Error al Crear Usuario - ":Y.MSG
		    CALL ERR
	    END
END

VALID_CONTENT:

    BEGIN CASE
        CASE V_ERR_CODE EQ 1
            ;*numeric
            IF V_CONTENT NE '' AND NUM( V_CONTENT) EQ 0 THEN
                STRERR =V_NAME_FIELD :' Tipo de dato incorrecto, Númericos con decimales'
                GOSUB CRT_ERROR_CEL
                BREAK
            END

        CASE V_ERR_CODE EQ 2
            ;* numeric int
            IF  V_CONTENT NE '' AND ISDIGIT( V_CONTENT) EQ 0 THEN
                STRERR =' Tipo de dato incorrecto, Solo Enteros'
                GOSUB CRT_ERROR_CEL
                BREAK
            END

       
            ;*valida longitud de valor ingresado TELEFONO
        CASE V_ERR_CODE EQ 5
            IF  V_CONTENT NE '' AND LEN(TRIM(V_CONTENT))<>8 THEN
                STRERR ='Numero de telefono no valido ':V_CONTENT
                GOSUB CRT_ERROR_CEL
                BREAK
            END
        
        CASE V_ERR_CODE EQ 10
            ;* numeric int
            IF  V_CONTENT NE '' AND ISDIGIT( V_CONTENT) EQ 0 THEN
                STRERR =' Invalido. Favor ingrese 8 numeros enteros.'
                GOSUB CRT_ERROR_CEL
                BREAK
            END
            
        CASE V_ERR_CODE EQ 11
            ;* numeric int
           IF  V_CONTENT NE '' AND LEN(TRIM(V_CONTENT))<>8 THEN
                STRERR =' Invalido. Favor ingrese 8 numeros enteros.' 
                GOSUB CRT_ERROR_CEL
                BREAK
            END
                         
    END CASE

RETURN
CRT_ERROR:
	ETEXT = Y.TXT

	CALL STORE.END.ERROR

RETURN
CRT_ERROR_CEL:
	Y.ERR.NUMBER = 'Y'
    AF  = V_NAME_FIELD
    AV 	= POS_VALUE
    ETEXT = STRERR
    CALL STORE.END.ERROR
RETURN
;* Archivo para Revisión de Errores
WRITE_LOG_FILE:
    DIR.NAME = 'CHQ.OUT'
    R.ID =  'PS_CREATETOKEN' : TODAY : '.txt'

    OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
    WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN

ACTUALIZAR.TABLA.LOCAL:
	;*Comparar arreglo entrante con lo que esta en la tabla, si es nuevo id empresa agregarlo, sino incluir la posicion especifica en arreglo para un "nuevo" F.WRITE.	
		Y.TABLE.ID = LF.NIT	
		REC.SLV.NEW.TCIB	= ""
		CALL F.READ(FN.SLV.NEW.TCIB.USER,Y.TABLE.ID,REC.SLV.NEW.TCIB,F.SLV.NEW,Y.ERROR.USER)
		REC.SLV.NEW.TCIB<EB.SLV10.DEVICE.TYPE> = 'Sms Token'
		REC.SLV.NEW.TCIB<EB.SLV10.DEVICE.STATUS> = 'INAU'
		REC.SLV.NEW.TCIB<EB.SLV10.COD.ACTIVATION> =  R.NEW(EB.EXDEV.CODE.ACTIVATION)
		REC.SLV.NEW.TCIB<EB.SLV10.RESERVADO4> = Y.USER.SOFT
		CALL F.WRITE(FN.SLV.NEW.TCIB.USER,Y.TABLE.ID,REC.SLV.NEW.TCIB)
RETURN
END
