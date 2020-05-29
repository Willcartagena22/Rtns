*-----------------------------------------------------------------------------
* <Rating>-89</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.STM.TOKEN(ID, SEEDS, TOKEN, ERROR)
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
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* OCornejo	05.10.2017	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.KEYS.PARAMS
$INSERT I_F.EB.SLV.TOKEN.VERIFY
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
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
		
		;*Para Creacion de Token
		IF Y.S3 EQ '' THEN
			Y.S3 = TODAY
			Y.OP = 'GENERATE'
		END
		IF Y.S4 EQ '' THEN
			Y.S4 = CHANGE(TIMEDATE()[1,8],':','') : TODAY[7,2]
			Y.OP = 'GENERATE'
		END
		TOKEN = ''
	RETURN
	
	OPENFILE:
		CALL OPF(FN.KEYS, F.KEYS)
		CALL OPF(FN.TOKEN, F.TOKEN)
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
		
		GOSUB ALTER.SEED.1
		GOSUB ALTER.SEED.2
		
		BEGIN CASE
			CASE Y.TYPE.TOKEN EQ 'N'
				GOSUB SEED.CROSSING
				
			CASE Y.TYPE.TOKEN EQ 'A'
				GOSUB SEED.CROSSING.ALPHA
		END CASE
		
		IF Y.OP EQ 'VALIDATE' THEN
			;*Calcular Fecha hasta la que existira el Bloqueo
			PROCESS.DATE = Y.S3
		    DAY.COUNT = Y.TIME.TOKEN : "W"
		    CALL CDT('SV05' : PROCESS.DATE[1,4], PROCESS.DATE, DAY.COUNT)
		    CALL CDD('', PROCESS.DATE, TODAY, DAYS)
			IF DAYS GT 0 THEN
				ERROR = 'Token Expirado'
				RETURN
			END
		END
		
		GOSUB REGISTER.TOKEN
	RETURN
	
	ALTER.SEED.1:
		Y.SA1 = ''
		Y.SA1 = (((Y.S1 ^ 3) * Y.S4) - ((Y.S3 ^ 2) + (Y.S4 ^ 2)))
		Y.SA1 = SQRT(Y.SA1)
		Y.SA1 = CHANGE(Y.SA1,'.','')
	RETURN
	
	ALTER.SEED.2:
		Y.SA2 = ''		
		Y.SA2 = ((Y.S2 ^ 3) + (Y.S4 ^ 3)) - (Y.S4[3,4] ^ 2) - SQRT(Y.SA1 + Y.S4)
		Y.SA2 = SQRT(Y.SA2)
		Y.SA2 = CHANGE(Y.SA2,'.','')
	RETURN
	
	SEED.CROSSING:
		Y.TOKEN = ''
		Y.SA1 = Y.SA1[4,LEN(Y.SA1)]
		Y.SA2 = Y.SA2[4,LEN(Y.SA2)]
				
		NO.ITER = LEN(Y.SA1)
		IF NO.ITER LT LEN(Y.SA2) THEN
			NO.ITER = LEN(Y.SA2)
		END	
			
		IF NO.ITER GT Y.LEN.TOKEN THEN
			FOR I = 1 TO Y.LEN.TOKEN
				IF MOD(I,2) EQ 0 THEN
					Y.TOKEN := Y.SA1[I,1]
				END
				ELSE
					Y.TOKEN := Y.SA2[I,1]
				END
			NEXT I
		END
		ELSE
			FOR I = 1 TO NO.ITER
				IF MOD(I,2) EQ 0 THEN
					Y.TOKEN := Y.SA1[I,1]
				END
				ELSE
					Y.TOKEN := Y.SA2[I,1]
				END
			NEXT I
			
			Y.DIF = Y.LEN.TOKEN - LEN(Y.TOKEN)
			
			FOR I = 1 TO Y.DIF
				IF MOD(I,2) EQ 0 THEN
					Y.TOKEN := Y.SA2[I,1]
				END
				ELSE
					Y.TOKEN := Y.SA1[I,1]
				END
			NEXT I
		END		
		TOKEN = Y.TOKEN : @VM : Y.S4
	RETURN
	
	REGISTER.TOKEN:	
		IF Y.OP EQ 'VALIDATE' THEN
			Y.ID = ID : '.' : Y.S3 : '.' : Y.S4
			CALL CACHE.READ(FN.TOKEN, Y.ID, R.TOKEN, E.TOKEN)
			IF R.TOKEN EQ '' THEN
				TOKEN = Y.TOKEN
			END
			ELSE
				TOKEN = ''
				ERROR = 'Token Invalido' ;*Quiere decir que el Token ya fue utilizado
			END
		END
	RETURN
	
	SEED.CROSSING.ALPHA:
		ERROR = 'Funcionalidad Token Alfanumerico No Desarrollada'
	RETURN
END
