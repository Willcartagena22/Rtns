*-----------------------------------------------------------------------------
* <Rating>-87</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.VALIDATE.INFO(ENQ.DATA)
*-----------------------------------------------------------------------------
* Rutina que verifica que el token ingresado la informacion del cliente se encuentre guardada en T24
*-----------------------------------------------------------------------------
* Modification History :
* Date              who          Reference          description
*07-FEB-19       PSANCHEZ          25674			initial version
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.EB.ERROR
$INSERT I_F.EB.SLV.NEW.TCIB.USER
$INSERT I_F.EB.SLV.KEYS.PARAMS
$INSERT I_F.EB.SLV.TOKEN.VERIFY
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN

INIT:
	FN.SLV.NEW.TCIB.USER	= 'F.EB.SLV.NEW.TCIB.USER'
	F.SLV.NEW		= ''
	CALL OPF(FN.SLV.NEW.TCIB.USER, F.SLV.NEW)
	FN.EB.KEYS.PARAM = 'F.EB.SLV.KEYS.PARAMS'
	F.EB.KEYS.PARAM  = ''
	CALL OPF(FN.EB.KEYS.PARAM,F.EB.KEYS.PARAM)
	FN.ERR  = 'F.EB.ERROR'
	F.ERR   = ''
	CALL OPF(FN.ERR, F.ERR)
	FN.TOKEN = 'F.EB.SLV.TOKEN.VERIFY'
	F.TOKEN  = ''
	CALL OPF(FN.TOKEN, F.TOKEN)
	
*	;*Obtener Parametros desde el Enquiry
;******************************************************************
		LOCATE "DOCUMENT" IN D.FIELDS<1> SETTING LN.POS THEN
	    	Y.DOCUMENT = D.RANGE.AND.VALUE<LN.POS>
	    END
	    LOCATE "TOKEN" IN D.FIELDS<1> SETTING LN.POS THEN
	    	Y.TOKEN = D.RANGE.AND.VALUE<LN.POS>
	    END
	    LOCATE "TYPE" IN D.FIELDS<1> SETTING LN.POS THEN
	    	Y.TYPE = D.RANGE.AND.VALUE<LN.POS>
	    END
;******************************************************************	    
	    Y.OP = 'VALIDATE'
	    Y.ID = 'STM'
	    
RETURN

PROCESS:
;*******************************************************************
*DEBUG
*Y.DOCUMENT = '06143006861250'
*Y.TOKEN	= '28497394'
*Y.TYPE		= 'NIT'
;*******************************************************************
;* Verificar Informacion registrada a partir del NIT
	GOSUB VERIFY.REGISTER

	;*Validar que el documento exista y sea valido
	IF Y.MSG.ERROR EQ '' THEN
		GOSUB VALIDAR.LOCK.TOKEN
	END
	IF Y.MSG.ERROR THEN
		GOSUB GET.DESCRIPT.ERROR
	END
	
	GOSUB RESPONSE.TO.TCIB	
RETURN


VALIDAR.LOCK.TOKEN:
		;*Generar Token de Validacion
		IF Y.MSG.ERROR EQ '' THEN
			GOSUB VALIDATE.TIME
		END
		
		;*Validar si Token Ingresado Corresponde
		IF Y.MSG.ERROR EQ '' THEN
			IF Y.TOKEN EQ Y.TOKEN.VERIF THEN
				IF Y.MSG.ERROR EQ '' THEN
					Y.PROCESAR = 'S'	;*Token Correcto
					Y.CONTADOR = 0
				END
				ELSE
					Y.PROCESAR = 'N'	;*Token Incorrecto
					Y.CONTADOR = Y.CONTADOR + 1
					Y.MSG.ERROR ='EB-SLV.TOKEN.EXPIRE'
				END
			END
			ELSE
				Y.PROCESAR = 'N'	;*Token Incorrecto
				Y.CONTADOR = Y.CONTADOR + 1
				Y.MSG.ERROR ='EB-SLV.TOKEN.EXPIRE'
			END
		END
		ELSE
				Y.PROCESAR = 'N'	;*Token Incorrecto
				Y.CONTADOR = Y.CONTADOR + 1
				Y.MSG.ERROR ='EB-SLV.TOKEN.EXPIRE'
		END
RETURN
VERIFY.REGISTER:
    Y.TABLE.ID = Y.DOCUMENT
    CALL F.READ(FN.SLV.NEW.TCIB.USER,Y.TABLE.ID,R.SLV.NEW.TCIB,F.SLV.NEW,Y.ERROR.USER)
     IF R.SLV.NEW.TCIB THEN
		     Y.CONTADOR    	= R.SLV.NEW.TCIB<EB.SLV10.FAIL.COUNTER>
		     Y.USER.STATUS 	= R.SLV.NEW.TCIB<EB.SLV10.STATUS>
		     Y.FROM.DATE	= R.SLV.NEW.TCIB<EB.SLV10.DATE.TIME>
		     Y.X.DATE		= R.SLV.NEW.TCIB<EB.SLV10.TRAN.TIME>
		     Y.TOKEN.VERIF 	= R.SLV.NEW.TCIB<EB.SLV10.RESERVADO1> ;* Obtengo el token para poder validarlo
		     Y.START.DATE	= R.SLV.NEW.TCIB<EB.SLV10.START.DATE>
		     Y.ARRANGEMENT  = R.SLV.NEW.TCIB<EB.SLV10.USER.ISA.ID>
		     Y.USER.TYPE	= R.SLV.NEW.TCIB<EB.SLV10.USER.TYPE>
		     Y.CUSTOMER  	= R.SLV.NEW.TCIB<EB.SLV10.CUSTOMER.ID>
		     Y.SMS			= R.SLV.NEW.TCIB<EB.SLV10.SMS>
		     Y.CHANNEL		= R.SLV.NEW.TCIB<EB.SLV10.CHANNEL>
		     Y.CONTADOR		= R.SLV.NEW.TCIB<EB.SLV10.FAIL.COUNTER>
		   		IF Y.USER.STATUS EQ 'SI' THEN
		            Y.MSG.ERROR = 'EB.STATUS.BLOCK'
		        END
		        
		        GOSUB KEYS.PARAMS
				IF Y.MSG.ERROR EQ '' THEN
					IF Y.CONTADOR GE Y.LIMITE.CONTADOR THEN
		            	Y.MSG.ERROR = 'EB.STATUS.BLOCK'
		            	Y.USER.STATUS = 'SI'
		        	END

				END
				
     END
     ELSE
     	Y.MSG.ERROR = 'EB-SLV.USER.NOT.EXITS'
     END
     
RETURN
*F.LIVE.WRITE
KEYS.PARAMS:
    CALL F.READ(FN.EB.KEYS.PARAM, 'SLV.TCIB.NC.CONF', R.KEYS, F.EB.KEYS.PARAM, E.KEYS)
    FIND 'CONTADOR' IN R.KEYS<EB.SLV18.PARAM.ID> SETTING Ap, Vp THEN
			 Y.LIMITE.CONTADOR    = FIELD(R.KEYS<EB.SLV18.VALOR><Ap, Vp>, SM, 1) ;*Limite de contador
		END
		ELSE
			Y.MSG.ERROR = 'EB.SLV.NOT.KEY.PARAMS'
			RETURN ;*Retornar si generar Token
		END
RETURN

VALIDATE.TIME:
		CALL F.READ(FN.EB.KEYS.PARAM, 'SLV.TCIB.NC.CONF', R.KEYS, F.EB.KEYS.PARAM, E.KEYS)
		FIND 'TIME' IN R.KEYS<EB.SLV18.PARAM.ID> SETTING Ap, Vp THEN
			Y.TIME.TOKEN   = FIELD(R.KEYS<EB.SLV18.VALOR><Ap, Vp>, SM, 1) ;*Duracion en Dias
		END
		ELSE
			Y.MSG.ERROR = 'EB-SLV.NOT.KEY.PARAMS'
			RETURN ;*Retornar si generar Token
		END
		
		IF Y.OP EQ 'VALIDATE' THEN
			;*Calcular Fecha hasta la que existira el Bloqueo
			T1 = Y.FROM.DATE
			T2 = TIMESTAMP()
			RESULT2 = TIMEDIFF(T1,T2,0)
			HOUR = FIELD(RESULT2, @FM, 2)
			MIN = FIELD(RESULT2, @FM, 3)
			
			IF HOUR LE 0 THEN
				IF MIN LE Y.TIME.TOKEN THEN
					IF Y.MSG.ERROR EQ '' THEN
						GOSUB SEARCH.TOKEN
					END
					GOSUB REGISTER.TOKEN
				END
				ELSE
					Y.MSG.ERROR = 'EB-SLV.TOKEN.EXPIRE'
				END
			END
			ELSE
				Y.MSG.ERROR = 'EB-SLV.TOKEN.EXPIRE'
			END
		END

RETURN

SEARCH.TOKEN:	
		IF Y.OP EQ 'VALIDATE' THEN
			Y.ID = 'STM.' : Y.START.DATE:'.':Y.TOKEN
			CALL F.READ(FN.TOKEN, Y.ID, R.TOKEN,F.TOKEN, E.TOKEN)
			IF R.TOKEN EQ '' THEN
				TOKEN = Y.TOKEN
			END
			ELSE
				TOKEN = ''
				Y.MSG.ERROR = 'EB-SLV.TOKEN.EXPIRE' ;*Quiere decir que el Token ya fue utilizado
			END
		END
RETURN
REGISTER.TOKEN:
;*Registrar Condiciones de Token para que ya no se pueda Utilizar
	
		Y.ID = 'STM.' : Y.START.DATE:'.':Y.TOKEN
		CALL CACHE.READ(FN.TOKEN, Y.ID, R.TOKEN, E.TOKEN)
		IF R.TOKEN EQ '' THEN
			R.TOKEN<EB.SLV7.DATE.APPL> = TODAY
			R.TOKEN<EB.SLV7.TIME.APPL> = TIMEDATE()[1,8]
			
			;*Datos de Auditoria
			R.TOKEN<EB.SLV7.INPUTTER> = OPERATOR
			R.TOKEN<EB.SLV7.AUTHORISER> = OPERATOR
			R.TOKEN<EB.SLV7.CURR.NO>		 = 1 
			X = OCONV(DATE(),"D-")
	        V$TIMEDATE = TIMEDATE()
	        V$TIMEDATE = V$TIMEDATE[1,5]
	        CONVERT ":" TO "" IN V$TIMEDATE
	        X = X[9,2] : X[1,2] : X[4,2] : V$TIMEDATE
			R.TOKEN<EB.SLV7.DATE.TIME,1>	 = X
			R.TOKEN<EB.SLV7.CO.CODE>	     = 'SV0010001'
			CALL F.WRITE(FN.TOKEN, Y.ID, R.TOKEN)
			CALL JOURNAL.UPDATE ('')
		END	
RETURN
GET.DESCRIPT.ERROR:
		;*Obtener Error
		CALL F.READ(FN.ERR, Y.MSG.ERROR, R.ERR,F.ERR, E.ERR)
		IF R.ERR<EB.ERR.ERROR.MSG> EQ '' THEN
			R.ERR<EB.ERR.ERROR.MSG> = Y.MSG.ERROR
		END
		Y.MSG.ERROR = R.ERR<EB.ERR.ERROR.MSG>
		Y.PROCESAR = 'N'
;*		A.INFO = CHANGE(R.ERR<EB.ERR.ERROR.MSG>, '&', Y.REPLA) : '* * * * * * * *'
RETURN
RESPONSE.TO.TCIB:
;* Completar Datos de ENQ con la informacion necesaria para el siguiente paso
A.INFO  = Y.DOCUMENT				:'*';* 1. NIT
A.INFO := Y.CUSTOMER				:'*';* 2. CUSTOMER ID
A.INFO := Y.ARRANGEMENT			 	:'*';* 3. ACUERDO ISA
A.INFO := Y.USER.TYPE				:'*';* 4. TIPO DE USUARIO
A.INFO := Y.SMS						:'*';* 5. SMS
A.INFO := Y.CHANNEL					:'*';* 6. CHANNEL
A.INFO := Y.MSG.ERROR				:'*';* 7. MENSAJE DE ERROR
A.INFO := Y.PROCESAR				:'*';* 8. PROCESAR
A.INFO := Y.USER.STATUS					:'*';* 9. ESTADO
A.INFO := Y.CONTADOR				:'*';* 10. CONTADOR

ENQ.DATA<-1> = A.INFO
RETURN
;* Archivo para Revisión de Errores
WRITE_LOG_FILE:
	DIR.NAME = 'CHQ.OUT'
	R.ID =  'PS_' : TODAY : '.txt'

	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
		END
	CLOSESEQ SEQ.PTR 
RETURN
END

