*-----------------------------------------------------------------------------
* <Rating>800</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.I.NEW.CUS.TCIB
*-----------------------------------------------------------------------------
* Rutina que verifica que la informacion del cliente se encuentre guardada en T24
*-----------------------------------------------------------------------------
* Modification History :
* Date              who          Reference          description
*26-NOV-18       PSANCHEZ          25674			initial version
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SLV.NIT.CUSTOMER.CNT
    $INSERT I_DAS.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_DAS.EB.EXTERNAL.USER
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.SLV.DUI.CUSTOMER.CNT
    $INSERT I_F.EB.EXTERNAL.USER.DEVICE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_System
    $INSERT I_F.CUSTOMER
    $INSERT I_F.EB.SLV.NEW.TCIB.USER
    $INSERT I_F.EB.ERROR
    $INSERT I_F.EB.SLV.KEYS.PARAMS
    $INSERT I_F.EB.EXTERNAL.USER
    $INSERT I_F.AA.PRODUCT.ACCESS
    $INSERT I_F.CUSTOMER.ACCOUNT
    

*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB PROCESS
    RETURN
    
INIT:

    FN.SLV.NIT.CUSTOMER	= 'F.SLV.NIT.CUSTOMER.CNT'
    F.SLV.NIT		= ''
    CALL OPF(FN.SLV.NIT.CUSTOMER, F.SLV.NIT)

    FN.SLV.DUI.CUSTOMER	= 'F.SLV.DUI.CUSTOMER.CNT'
    F.SLV.DUI		= ''
    CALL OPF(FN.SLV.DUI.CUSTOMER, F.SLV.DUI)


    FN.ARR='F.AA.ARRANGEMENT.ACTIVITY'
    F.ARR=''
    CALL OPF(FN.ARR,F.ARR)

    FN.EXTERNAL.USER.DEVICE.NAU = "F.EB.EXTERNAL.USER.DEVICE$NAU"
    F.EXT.USER.DEVICE.NAU = ""
    CALL OPF(FN.EXTERNAL.USER.DEVICE.NAU,F.EXT.USER.DEVICE.NAU)

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
    
     FN.EXT.USER.NAU	 = 'F.EB.EXTERNAL.USER$NAU'
    F.EXT.USER.NAU	 = ''
    CALL OPF(FN.EXT.USER.NAU, F.EXT.USER.NAU)
    
    FN.EB.KEYS.PARAM = 'F.EB.SLV.KEYS.PARAMS'
    F.EB.KEYS.PARAM	 = ''
    CALL OPF(FN.EB.KEYS.PARAM,F.EB.KEYS.PARAM)
    
    FN.CUS.ACC	 = 'F.CUSTOMER.ACCOUNT'
	F.CUS.ACC	 = ''
	CALL OPF(FN.CUS.ACC,F.CUS.ACC)
	
	FN.EXTERNAL.USER.DEVICE = "F.EB.EXTERNAL.USER.DEVICE"
	F.EXT.USER.DEVICE = ""	
	CALL OPF(FN.EXTERNAL.USER.DEVICE,F.EXT.USER.DEVICE)
	Y.COUNT.ACC = 0
******************************VERSION*****************************
*    Y.DOCUMENT.ID = R.NEW(EB.SLV10.DOCUMENT)
Y.DOCUMENT.ID = '031768958'
*    Y.TXT = 'Y.TXT ':Y.DOCUMENT.ID
*    GOSUB WRITE_LOG_FILE
*    Y.TYPE = R.NEW(EB.SLV10.DOCUMENT.TYPE)
Y.TYPE = 'DUI'
*    Y.TXT = 'Y.TXT ': Y.TYPE
*    GOSUB WRITE_LOG_FILE
******************************************************************
    RETURN

PROCESS:

;*144502
;*DEBUG**********************************************
* 	Y.DOCUMENT.ID = '06160612941024'
*	Y.TYPE   = 'NIT'
;****************************************************
    GOSUB VERIFY.NIT
    IF Y.MSG.ERROR EQ '' THEN
        GOSUB VERIFY.DUI
    END
    IF Y.MSG.ERROR EQ '' THEN
        GOSUB VERIFY.REGISTER
        	  IF Y.BANDERA EQ 'N' THEN ;* N = Significa Usuario Nuevo
					IF Y.MSG.ERROR EQ '' THEN
		                ;*Extraer los metodos de notificacion
		               GOSUB METHOD.NOTIFICATION
		            END
		            IF Y.MSG.ERROR EQ '' THEN
		                GOSUB VERIFY.ARRANGEMENT
		            END
		            IF Y.MSG.ERROR EQ '' AND Y.FLAG NE  'NEWISA' THEN
		                GOSUB VERIFY.EXTERNAL.USER
		            END
		            IF Y.MSG.ERROR EQ '' AND Y.FLAG NE  'NEWISA' THEN
		                GOSUB VERIFY.DEVICE
		            END
		            IF Y.MSG.ERROR EQ '' THEN
		                ;*Extraer Token
		                GOSUB GENERATE.TOKEN
		            END
		            IF Y.MSG.ERROR EQ '' THEN
		                ;* Registrar Informacion en la aplicacion Local
		                GOSUB FULL.TABLE
		            END
            END;* N = Significa Usuario Nuevo
            ELSE ;*Es un usuario Registrado Anteriormente
            	IF Y.MSG.ERROR EQ '' THEN
		                ;*Extraer los metodos de notificacion
		               GOSUB METHOD.NOTIFICATION
		        END
            	IF Y.MSG.ERROR EQ '' THEN
            		;*Extraer Token
            		;*GOSUB GENERATE.TOKEN
					;*GOSUB GET.EXTERNAL.USER
					
				
					GOSUB VERIFY.EXTERNAL.USER
            	END
            	IF Y.MSG.ERROR EQ '' THEN
                	;* Registrar Informacion en la aplicacion Local
                		GOSUB FULL.TABLE.EXIST
            	END	
        	END;* Es un usuario registrado anteriormente

    END

    IF Y.MSG.ERROR THEN
        GOSUB GET.MESSAGE.ERROR
    END

;*GOSUB GET_ENQUIRY_DATA

RETURN
VERIFY.NIT:
    IF Y.TYPE EQ 'NIT' THEN
        CALL F.READ(FN.SLV.NIT.CUSTOMER,Y.DOCUMENT.ID,R.NIT,F.SLV.NIT,Y.NIT.ERR)
        IF R.NIT THEN
            CUS.NO = R.NIT<SLV.NIT.CUSTOMER.CODE>
        END
        ELSE
        Y.MSG.ERROR = 'EB-SLV.NIT.NOT.EXITS'
        RETURN
    END
    END
RETURN

VERIFY.DUI:
    IF Y.TYPE EQ 'DUI' THEN
        CALL SLV.S.VALIDATE.DUI(Y.DOCUMENT.ID,Y.RESULT,Y.ERROR.CODE)
        IF Y.RESULT THEN
            ;*Verificar si el número de DUI ingresado corresponde a un cliente existente en T24.
            CALL F.READ(FN.SLV.DUI.CUSTOMER,Y.DOCUMENT.ID,R.DUI,F.SLV.DUI,Y.DUI.ERR)
            IF R.DUI THEN
                CUS.NO = R.DUI<SLV.DUI.CUSTOMER.CODE>
            END
            ELSE
    	        Y.MSG.ERROR =  'EB-SLV.DUI.NOT.EXITS'
        	    RETURN
	        END
    	END
    	ELSE
   	 		Y.MSG.ERROR=  'EB-SLV.DUI.NOT.EXITS'
    		RETURN
    	END
    END

RETURN

VERIFY.REGISTER:
    Y.TABLE.ID = Y.DOCUMENT.ID
    CALL F.READ(FN.SLV.NEW.TCIB.USER,Y.TABLE.ID,R.SLV.NEW.TCIB,F.SLV.NEW,Y.ERROR.USER)
    IF R.SLV.NEW.TCIB NE '' AND R.SLV.NEW.TCIB<EB.SLV10.CUSTOMER.ID> NE '' THEN
        Y.CONTADOR    = R.SLV.NEW.TCIB<EB.SLV10.FAIL.COUNTER>
        Y.USER.STATUS = R.SLV.NEW.TCIB<EB.SLV10.STATUS>
        Y.SMS		  = R.SLV.NEW.TCIB<EB.SLV10.SMS>
        Y.EMAIL		  = R.SLV.NEW.TCIB<EB.SLV10.EMAIL>
        AA.ARR.ID	  = R.SLV.NEW.TCIB<EB.SLV10.USER.ISA.ID>
        Y.NEW.ISA	  = R.SLV.NEW.TCIB<EB.SLV10.RESERVADO5>
        IF Y.NEW.ISA EQ 'NEWISA' THEN
			GOSUB VERIFY.ARRANGEMENT
		END
		
		Y.TXT = 'Y.USER.STATUS: ': Y.USER.STATUS
		GOSUB WRITE_LOG_FILE

        IF Y.USER.STATUS EQ 'SI' THEN
            ;*Y.MSG.ERROR = 'EB-SLV.USER.BLOCK'
            Y.MSG.ERROR = 'EB.STATUS.BLOCK'
            Y.TXT = 'Y.MSG.ERROR: ' : Y.MSG.ERROR
            GOSUB WRITE_LOG_FILE
            RETURN
        END
        GOSUB KEYS.PARAMS
		IF Y.MSG.ERROR EQ '' THEN
			Y.BANDERA ='S'
			Y.CONTADOR	= 0
		END


    END

    ELSE
    Y.USER.STATUS = 'NO'
    Y.CONTADOR	= 0
    Y.BANDERA = 'N'
    END
    RETURN

VERIFY.ARRANGEMENT:
    THE.ARGS = CUS.NO
    SEL.LIST = DAS$CUSARRANGEMENT
    CALL DAS('AA.ARRANGEMENT.ACTIVITY',SEL.LIST,THE.ARGS,TABLE.SUFFIX)
    IF SEL.LIST THEN
        CALL F.READ(FN.ARR,SEL.LIST,REC.ARR,F.ARR,REC.ERROR)
        IF REC.ARR THEN
            ;*Id Isa
            AA.ARR.ID=REC.ARR<AA.ARR.ACT.ARRANGEMENT>
            ;*CLiente al que pertenece el ISA
            CUS.NO=REC.ARR<AA.ARR.ACT.CUSTOMER>
            ;*Estado del Acuerdo de banca en linea
            Y.STATUS.ARR = REC.ARR<AA.ARR.ACT.RECORD.STATUS>
            ;* Verificar el estado del Acuerdo de banca en línea para el cliente.
            IF Y.STATUS.ARR EQ 'INAU' THEN
                Y.MSG.ERROR = 'EB-SLV.ARR.STATUS'
                RETURN
            END
            Y.TYPE.USER = REC.ARR<AA.ARR.ACT.PRODUCT>
            IF Y.TYPE.USER NE 'PERSONAL' THEN
            	AA.ARR.ID =''
            	Y.FLAG = 'NEWISA'
            END
            ;*Verificar que el usuario posea cuentas en el acuerdo de banca en linea
            ID.PROPERTY 		= ''
            ID.PROPERTY.CLASS	= 'PRODUCT.ACCESS'
            EFFECTIVE.DATE		= TODAY
            CALL AA.GET.ARRANGEMENT.CONDITIONS(AA.ARR.ID, ID.PROPERTY.CLASS, ID.PROPERTY, EFFECTIVE.DATE, RETURN.IDS, RETURN.CONDITIONS, RETURN.ERROR)
            IF RETURN.CONDITIONS THEN
                R.CONDITION	= RAISE(RETURN.CONDITIONS)
                ACCT.TRANS 	= R.CONDITION<AA.PRODA.ACCT.TRANS>
                Y.COUNT.TRANS = DCOUNT(ACCT.TRANS,@VM)
                ACCT.SEE	= R.CONDITION<AA.PRODA.ACCT.SEE>
                Y.COUNT.SEE = DCOUNT(ACCT.SEE,@VM)
                ARRGT.TRANS	= R.CONDITION<AA.PRODA.ARRGT.TRANS>
                Y.COUNT.ARRGT.TRANS = DCOUNT(ARRGT.TRANS,@VM)
                ARRGT.SEE	= R.CONDITION<AA.PRODA.ARRGT.SEE>
                Y.COUNT.ARRGT.SEE = DCOUNT(ARRGT.SEE,@VM)

                IF Y.COUNT.TRANS EQ 0 AND Y.COUNT.SEE EQ 0 AND Y.COUNT.ARRGT.TRANS EQ 0 AND Y.COUNT.ARRGT.SEE EQ 0 THEN
                    Y.MSG.ERROR = 'EB-SLV.NOT.PROD'
                    RETURN
                END

            END
        END
        ELSE
        	Y.MSG.ERROR = 'EB-SLV.ARR.NOT.EXITS'
        END
    END
    ELSE
*;    Y.MSG.ERROR = 'EB-SLV.ARR.NOT.EXITS'
    	Y.FLAG = 'NEWISA'

    END

    RETURN

VERIFY.EXTERNAL.USER:
;*Verificar si ya existe un usuario de banca en línea para el cliente
    THE.LIST = DAS.EXT$ARRANGEMENT
    THE.ARGS = AA.ARR.ID
;*Verifica si external user existe
    CALL DAS('EB.EXTERNAL.USER',THE.LIST,THE.ARGS,TABLE.SUFFIX)
    EXT.USER.ID=THE.LIST
    IF EXT.USER.ID THEN
        CALL F.READ(FN.EXT.USER,EXT.USER.ID,R.EXT,F.EXT.USER,Y.ERROR.EXT)
        IF R.EXT THEN
            Y.EXT.STATUS = R.EXT<EB.XU.RECORD.STATUS>
            IF Y.EXT.STATUS EQ '' THEN
                Y.MSG.ERROR = 'EB-SLV.EXT.LIVE' ;*Ya posees un ID de usuario registrado
                RETURN
            END
        END
    END
    IF EXT.USER.ID EQ '' THEN
		GOSUB GET.EXTERNAL.USER
	END

    RETURN

VERIFY.DEVICE:
;*Verificar si ya tiene dispositivos asociados a banca en linea
    IF EXT.USER.ID THEN
    	Y.SELECT.EXTDEV = 'SELECT ' : FN.EXTERNAL.USER.DEVICE.NAU : ' WITH USER.ID EQ ' : EXT.USER.ID
	    CALL EB.READLIST(Y.SELECT.EXTDEV , LIST.ARR.EXT.DEV, '', NO.OF.RECS.DEV, Y.ERR)
	    IF NO.OF.RECS.DEV GT 0 THEN
	    		FOR K = 1 TO NO.OF.RECS.DEV
		        	CALL F.READ(FN.EXTERNAL.USER.DEVICE.NAU, LIST.ARR.EXT.DEV<K>, EXT.DEV.REC.NAU, F.EXT.USER.DEVICE.NAU, ERR.EXT.DEV)
				        IF EXT.DEV.REC.NAU THEN
					        Y.ID.EXT.DEV = LIST.ARR.EXT.DEV<K>
					        Y.NAME.USER  = EXT.DEV.REC.NAU<EB.EXDEV.USER.ID>
					        Y.TYPE.DEVICE = EXT.DEV.REC.NAU<EB.EXDEV.DEVICE.TYPE>
					        Y.COD.ACTIVATION = EXT.DEV.REC.NAU<EB.EXDEV.CODE.ACTIVATION>
					        Y.DEVICE.STATUS = EXT.DEV.REC.NAU<EB.EXDEV.DEVICE.STATUS>
					        Y.NUM.SERIE = EXT.DEV.REC.NAU<EB.EXDEV.CODE.SERIE>
					        Y.NO.OF.RECS.DEV =NO.OF.RECS.DEV
					        
					        IF Y.TYPE.DEVICE EQ 'Soft Token' OR Y.TYPE.DEVICE EQ 'Hard Token' THEN
		                		Y.MSG.ERROR = 'EB-SLV.EXT.LIVE' ;*Ya posees un ID de usuario registrado
		                		RETURN
		            		END
				        END
	    		NEXT K
	    END
	    ELSE
	    	;*Obtiene cantidad de Token de un tipo especifico en los registros LIVE
			Y.SELECT.EXTDEV = 'SELECT ' : FN.EXTERNAL.USER.DEVICE: ' WITH USER.ID EQ ' : EXT.USER.ID							   	
		   	CALL EB.READLIST(Y.SELECT.EXTDEV , LIST.ARR.EXT.DEV, '', NO.OF.RECS.DEV, Y.ERR)
		   	IF NO.OF.RECS.DEV GT 0 THEN
				FOR K = 1 TO NO.OF.RECS.DEV
		        	CALL F.READ(FN.EXTERNAL.USER.DEVICE, LIST.ARR.EXT.DEV<K>, EXT.DEV.REC, F.EXT.USER.DEVICE, ERR.EXT.DEV)
				        IF EXT.DEV.REC THEN
					        Y.ID.EXT.DEV = LIST.ARR.EXT.DEV<K>
					        Y.NAME.USER  = EXT.DEV.REC<EB.EXDEV.USER.ID>
					        Y.TYPE.DEVICE = EXT.DEV.REC<EB.EXDEV.DEVICE.TYPE>
					        Y.COD.ACTIVATION = EXT.DEV.REC<EB.EXDEV.CODE.ACTIVATION>
					        Y.DEVICE.STATUS = EXT.DEV.REC<EB.EXDEV.DEVICE.STATUS>
					        Y.NUM.SERIE = EXT.DEV.REC<EB.EXDEV.CODE.SERIE>
					        Y.NO.OF.RECS.DEV =NO.OF.RECS.DEV
					        
					        IF Y.TYPE.DEVICE EQ 'Soft Token' OR Y.TYPE.DEVICE EQ 'Hard Token' THEN
		                		Y.MSG.ERROR = 'EB-SLV.EXT.LIVE' ;*Ya posees un ID de usuario registrado
		                		RETURN
		            		END
				        END
	    		NEXT K
		   	END		   	
*		   	Y.TOKEN.ID = NO.OF.RECS + 1
	    END
	    
    END
    	
RETURN


GET.MESSAGE.ERROR:
    ETEXT = Y.MSG.ERROR
    CALL STORE.END.ERROR
RETURN


METHOD.NOTIFICATION:
	 
	;*Lecutra campos locales
	APPL.ARR		='CUSTOMER'
	FIELDNAME.ARR	='LF.AML.USA.TCIB':VM:'LF.AML.AMT.TCIB'
	CALL MULTI.GET.LOC.REF(APPL.ARR, FIELDNAME.ARR, POS.LF.CUSTOMER)
	
    Y.EMAIL		= ''
    Y.SMS		= ''
   		CALL F.READ(FN.TABLE.CUS,CUS.NO, REC.CUS,F.TABLE.CUS, ERR.CUS)
    		IF REC.CUS THEN
		    	Y.EMAIL		= REC.CUS<EB.CUS.EMAIL.1>
		    	Y.SMS		= REC.CUS<EB.CUS.SMS.1>
		    	Y.NAME		= REC.CUS<EB.CUS.SHORT.NAME>
		    	Y.LF.USA.TCIB	= REC.CUS<EB.CUS.LOCAL.REF><1, POS.LF.CUSTOMER<1,1>>
				Y.LF.AMT.TCIB = REC.CUS<EB.CUS.LOCAL.REF><1, POS.LF.CUSTOMER<1,2>>
		    	IF Y.LF.USA.TCIB NE 'SI' OR Y.LF.AMT.TCIB EQ '' THEN
		    		Y.MSG.ERROR = 'EB-SLV.NOT.TCIB' ;* Usuario no tiene activado el SI de usar banca en linea
		    	END
		    	IF Y.MSG.ERROR EQ '' THEN
		    		IF Y.EMAIL EQ '' OR Y.SMS EQ '' THEN
			    		Y.MSG.ERROR = 'EB-EMAIL.SMS'
		    		END
		    	END
		    ;*Lectura de los productos creados hasta la fecha para cliente.
		    	IF Y.BANDERA EQ 'N'THEN
					CALL F.READ (FN.CUS.ACC,CUS.NO,REC.CUS.ACC,F.CUS.ACC,ERR.CUS.ACC)			
					Y.TXT = 'REC.CUS.ACC: ': REC.CUS.ACC
					GOSUB WRITE_LOG_FILE
					Y.CANT.PRODS.CUSTOMER	= DCOUNT(REC.CUS.ACC, @FM)
		    		FOR I = 1 TO Y.CANT.PRODS.CUSTOMER
	    			  	Y.ACCOUNT = TRIM(REC.CUS.ACC<I,1>)
	    				CALL SLV.UTIL.VAL.CUENTA(Y.ACCOUNT,Y.FLAG.AC)
	    				IF Y.FLAG.AC EQ 1 THEN
	    					Y.COUNT.ACC += 1
	    					BREAK
	    				END
			    	NEXT I
			    	IF Y.COUNT.ACC EQ 0 OR Y.COUNT.ACC EQ '' THEN
			    		Y.MSG.ERROR = 'EB-SLV.NOT.PROD' ;* Usuario no tiene productos
			    	END
			    END
    		END
    RETURN

GENERATE.TOKEN:
;*Generar Token Unico
    Y.ID    = 'STM'
    Y.SEEDS  = Y.SMS : @VM : Y.DOCUMENT.ID
    Y.TOKEN = ''
    Y.ERROR = ''
    CALL SlV.UTIL.NC.TOKEN(Y.ID, Y.SEEDS, Y.TOKEN, Y.ERROR)
    Y.TOKEN=FIELD(Y.TOKEN, @VM, 1)
;*Si se produjo un error en la generacion del Token retornar error
		IF Y.ERROR NE '' THEN
			Y.MSG.ERROR = Y.ERROR
			RETURN
		END
RETURN

FULL.TABLE:
;*	Y.CANT.PROXIES	= DCOUNT(Y.PROXY.ARR,@VM)
    GOSUB GET.DATETIME ;*Obtiene Fecha en formato DD/MM/ANIO HORA:MINUTO:SEGUNDOS
;*    ID.NEW = CUS.NO
 	R.NEW(EB.SLV10.CUSTOMER.ID)    	= CUS.NO
    R.NEW(EB.SLV10.DOCUMENT)		= Y.DOCUMENT.ID
    R.NEW(EB.SLV10.DOCUMENT.TYPE)	= Y.TYPE
    R.NEW(EB.SLV10.START.DATE)		= TODAY
    R.NEW(EB.SLV10.USER.ISA.ID)		= AA.ARR.ID
    R.NEW(EB.SLV10.STATUS)			=  Y.USER.STATUS
    R.NEW(EB.SLV10.EXTERNAL.USER)	= EXT.USER.ID
	R.NEW(EB.SLV10.NUM.SERIE)		= Y.NUM.SERIE
    R.NEW(EB.SLV10.COD.ACTIVATION) 	= Y.COD.ACTIVATION
;*	R.NEW(EB.SLV10.COD.REGISTER)   	= Y.COD.REGISTER
    R.NEW(EB.SLV10.DEVICE.TYPE) 	= Y.TYPE.DEVICE
	R.NEW(EB.SLV10.DEVICE.STATUS) 	= Y.DEVICE.STATUS
	R.NEW(EB.SLV10.USER.TYPE) 		= 'PERSONAL'
	R.NEW(EB.SLV10.CHANNEL) 		= 'INTERNET'
	R.NEW(EB.SLV10.FAIL.COUNTER) 	= Y.CONTADOR
	R.NEW(EB.SLV10.TRAN.TIME)	 	= X
    R.NEW(EB.SLV10.EMAIL)			= Y.EMAIL
    R.NEW(EB.SLV10.SMS)				= Y.SMS
    R.NEW(EB.SLV10.RESERVADO1)		= Y.TOKEN
    R.NEW(EB.SLV10.DATE.TIME.UPD)	= X
    R.NEW(EB.SLV10.RESERVADO2)		= Y.NAME
    R.NEW(EB.SLV10.RESERVADO6)		= Y.LF.USA.TCIB
    R.NEW(EB.SLV10.RESERVADO5)		= Y.FLAG
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

*GET.DATETIME:
*    X = OCONV(DATE(),"D-")
*    V$TIMEDATE = TIMEDATE()
*    V$TIMEDATE = V$TIMEDATE[1,5]
*    CONVERT ":" TO "" IN V$TIMEDATE
*    X = X[9,2] : X[1,2] : X[4,2] : V$TIMEDATE
*RETURN
GET.DATETIME:
	HORA = TIMEDATE()[1,8]
	FECHA = OCONV(DATE(),"D/")
  	X = FECHA[4,2]:'/':FECHA[1,2]:'/':FECHA[7,4]:' ': HORA
RETURN

KEYS.PARAMS:
    CALL F.READ(FN.EB.KEYS.PARAM, 'SLV.TCIB.NC.CONF', R.KEYS, F.EB.KEYS.PARAM, E.KEYS)
    FIND 'CONTADOR' IN R.KEYS<EB.SLV18.PARAM.ID> SETTING Ap, Vp THEN
			 Y.LIMITE.CONTADOR    = FIELD(R.KEYS<EB.SLV18.VALOR><Ap, Vp>, SM, 1) ;*Limite de contador
	END
	ELSE
			Y.MSG.ERROR = 'EB.SLV.NOT.KEY.PARAMS'
	END
RETURN

FULL.TABLE.EXIST:
	GOSUB GET.DATETIME ;*Obtiene Fecha en formato DD/MM/ANIO HORA:MINUTO:SEGUNDOS
	R.NEW(EB.SLV10.STATUS)			= Y.USER.STATUS
    R.NEW(EB.SLV10.FAIL.COUNTER) 	= Y.CONTADOR
	R.NEW(EB.SLV10.TRAN.TIME)	 	= X
    R.NEW(EB.SLV10.RESERVADO1)		= Y.TOKEN
    R.NEW(EB.SLV10.DATE.TIME.UPD)	= X
    R.NEW(EB.SLV10.EXTERNAL.USER)	= EXT.USER.ID
    R.NEW(EB.SLV10.RESERVADO3)		= Y.EXT.STATUS
    R.NEW(EB.SLV10.DEVICE.TYPE)		= Y.TYPE.DEVICE
    R.NEW(EB.SLV10.COD.ACTIVATION)  = Y.COD.ACTIVATION
    R.NEW(EB.SLV10.DEVICE.STATUS)	= Y.EXT.STATUS
    R.NEW(EB.SLV10.RESERVADO4)		= Y.ID.EXT.DEV
    R.NEW(EB.SLV10.USER.ISA.ID)		= AA.ARR.ID
    R.NEW(EB.SLV10.EMAIL)			= Y.EMAIL
    R.NEW(EB.SLV10.SMS)				= Y.SMS
    R.NEW(EB.SLV10.RESERVADO6)		= Y.LF.USA.TCIB
RETURN

GET.EXTERNAL.USER:

	ERR.EXTERNAL.USER =''
	ERR.EXT.USERS=''
	REC.EXTERNAL.USER=''
	SELECT.EXTERNAL	 = "SELECT " : FN.EXT.USER.NAU : " WITH CUSTOMER EQ " :  CUS.NO
	CALL EB.READLIST (SELECT.EXTERNAL, KEYS.EXT.USERS, '', NO.OF.EXT.USERS, ERR.EXT.USERS)
    IF KEYS.EXT.USERS THEN
    	FOR I = 1 TO NO.OF.EXT.USERS
    	CALL F.READ(FN.EXT.USER.NAU, KEYS.EXT.USERS<I>, REC.EXTERNAL.USER, F.EXT.USER.NAU, ERR.EXTERNAL.USER)
    		EXT.USER.ID = REC.EXTERNAL.USER<EB.XU.NAME>
	    	Y.EXT.STATUS = REC.EXTERNAL.USER<EB.XU.RECORD.STATUS>
    	NEXT I
    END
    
    
    IF EXT.USER.ID NE '' AND Y.EXT.STATUS EQ 'INAU' THEN
		GOSUB VERIFY.DEVICE
	END
	ELSE
		
		GOSUB GENERATE.TOKEN
	END
RETURN	

END
