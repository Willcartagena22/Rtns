*-----------------------------------------------------------------------------
* <Rating>-45</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.NV.TOKEN (ID, SEEDS, TOKEN, ERROR)
*-----------------------------------------------------------------------------
*

* Nombre: SLV.UTIL.STM.TOKEN
* Descripción: Rutina para Generacion de Tocken
* Requerido: Para el funcionamiento es requerido que en la aplicacion
*			 que genera el token se almacene la fecha y la hora de creacion
*			 ya que de lo contrario no se podra generar para la verificacion
* Parametros:
*	ID = Id Keys Params para Catalogo SLV.TOKEN
*	SEEDS = Array de Semillas (4) para validar (2) para generar
*		S1 = Semilla Numerica de 8 Caracteres
*		S2 = Semilla Numerica de 8 Caracteres
*		S3 = Fecha de Generacion de Token
*		S4 = Hora de Generacion de Token
*	TOKEN = Codigo de Seguridad de N Digitos @VM Hora de Generacion del Token
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.KEYS.PARAMS
$INSERT I_F.EB.SLV.TOKEN.VERIFY
*-----------------------------------------------------------------------------
GOSUB INIT

GOSUB PROCESS
RETURN

INIT:		
		FN.KEYS = 'F.EB.SLV.KEYS.PARAMS'
		F.KEYS  = ''
		FN.TOKEN = 'F.EB.SLV.TOKEN.VERIFY'
		F.TOKEN  = ''
		
		Y.S1 = FIELD(SEEDS, @VM, 1)
		Y.S2 = FIELD(SEEDS, @VM, 2)
		Y.S3 = FIELD(SEEDS, @VM, 3)
		Y.S4 = FIELD(SEEDS, @VM, 4)
		Y.OP = 'VALIDATE'
		
*		;*Para Creacion de Token
*		IF Y.S3 EQ '' THEN
*			Y.S3 = TODAY
*			Y.OP = 'GENERATE'
*		END
*		IF Y.S4 EQ '' THEN
*			Y.S4 = CHANGE(TIMEDATE()[1,8],':','') : TODAY[7,2]
*			Y.OP = 'GENERATE'
*		END
*		TOKEN = ''
RETURN

PROCESS:
CALL F.READ(FN.KEYS, 'SLV.TOKEN', R.KEYS, F.KEYS, E.KEYS)
		FIND ID IN R.KEYS<EB.SLV18.PARAM.ID> SETTING Ap, Vp THEN
			Y.LEN.TOKEN    = FIELD(R.KEYS<EB.SLV18.VALOR><Ap, Vp>, SM, 1) ;*Longitud del Token
			Y.TYPE.TOKEN   = FIELD(R.KEYS<EB.SLV18.VALOR><Ap, Vp>, SM, 2) ;*Tipo de Token
			Y.TIME.TOKEN   = FIELD(R.KEYS<EB.SLV18.VALOR><Ap, Vp>, SM, 3) ;*Duracion en Dias
			Y.STATUS.TOKEN = FIELD(R.KEYS<EB.SLV18.VALOR><Ap, Vp>, SM, 4) ;*Activo o Inactivo
		END
		ELSE
			ERROR = 'Parametros para Llave Token ' : ID : ' No Existen'
			RETURN ;*Retornar si generar Token
		END
		IF Y.STATUS.TOKEN EQ 'INACTIVO' THEN
			ERROR = 'Parametros de Token Inactivos'
			RETURN ;*Retornar si generar Token
		END
		IF Y.OP EQ 'VALIDATE' THEN
			;*Calcular Fecha hasta la que existira el Bloqueo
			T1 = Y.S3
			COPIA = T1
			GOSUB PARSE.DATETIME
			T1=Y.FEC.FT
			T2 = Y.S4
			COPIA = T2
			GOSUB PARSE.DATETIME
			T2=Y.FEC.FT
			RESULT2 = TIMEDIFF(T1,T2,0)
			MIN = FIELD(RESULT2, @FM, 3)
			CRT "MINUTOS: " : FIELD(RESULT2, @FM, 3)
			CRT RESULT2

			IF MIN GT Y.TIME THEN
				ERROR = 'Token Expirado'
				RETURN
			END
		END

RETURN
PARSE.DATETIME:
      Y.FEC.FT    = ''
      utcDateTime = COPIA
      UTC.FLAG    = ''
      ;*Evaluar UTC Time or Standard Time
      
      FINDSTR "." IN utcDateTime SETTING Ap, Vp THEN
            UTC.FLAG = '1'
      END
      
      IF UTC.FLAG EQ '1' THEN
            localZoneDate1 = OCONV(LOCALDATE(utcDateTime,localZone),'D4/E')
            localZoneTime1= OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
            Y.FEC.FT = localZoneDate1:' ':localZoneTime1
      END
      ELSE
            Y.DAY.BC = utcDateTime[3,2]
            Y.MONTH.BC = utcDateTime[5,2]
            Y.YEAR.BC = utcDateTime[1,2]
            Y.DATE.BC = OCONV(ICONV(utcDateTime[1,6],'D'),'D4/E')
            ;*Y.DATE.BCs = OCONV(ICONV(utcDateTime,'D'),'D4/E')
*                 Y.DATE.BC = Y.DAY.BC:'/':Y.MONTH.BC:'/20':Y.YEAR.BC
            Y.TIME.BC = utcDateTime[7,2]:':':utcDateTime[9,2]:':':'00'
            Y.FEC.FT = Y.DATE.BC: ' ': Y.TIME.BC
      END
RETURN
END
