*-----------------------------------------------------------------------------
* <Rating>1622</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.TRG.SMS.ACH
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*   Fecha:                     Usuario:        Descripción:
*   29/Mayo/2015        btorres         Creación de Rutina para envió de mensajes
*	06/Nove/2015   		rortiz			Pago de Planilla
*   11/Marzo/2016       psanchez		Modificacion de Rutina Envio Mensaje
*   28/Septiembre/2018  wrivas			Se lee valor de campo LF.AMOUNT por desarrollo pago anticipado
										;*se ageegar validacion para determinar si es trasaccion programada
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.EB.SLV.TRX.NOTIFIC
    $INSERT I_F.EB.SLV.TRX.NOTIFIC.MAPS
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.EB.SLV.TRANSACTION.ACH
    $INSERT I_F.EB.SLV.EMAIL.CONFIGURATION
*-----------------------------------------------------------------------------
    GOSUB INIT
    
    ;*----------------------------------------------
    	TEXTO.ARCHIVO = 'ESTADO -> ': R.NEW(EB.SLV98.STATUS)
    	GOSUB ESCRIBIR.ARCHIVO
    ;***********************************************
    
    IF R.NEW(EB.SLV98.STATUS) EQ '1' AND V$FUNCTION EQ 'I' THEN
    	    GOSUB PROCESS
    END   
    RETURN   
INIT:
    FN.FT  = "F.FUNDS.TRANSFER"
    F.FT   = ""
    FN.CUS = "F.CUSTOMER"
    F.CUS  = ""
    FN.ACC = "F.ACCOUNT"
    F.ACC  = ""
    MSG    = ""
    Y.MEDIO= ""
    Y.FLAG.ADV = '0'
    Y.FLAG.ALE = '0'
    FN.TABLE.NOTIFIC = 'F.EB.SLV.TRX.NOTIFIC'
    F.TABLE.NOTIFIC  = ""
    FN.TABLE.NOTIFIC.MAPS = 'F.EB.SLV.TRX.NOTIFIC.MAPS'
    F.TABLE.NOTIFIC.MAPS  = ""
    FN.TXN.ACH = 'F.EB.SLV.TRANSACTION.ACH'
	F.TXN.ACH = ''
	FN.EMAIL.CONF = 'F.EB.SLV.EMAIL.CONFIGURATION'
	F.EMAIL.CONF = ''
	
*   Open Funds Transfer POS
    CALL OPF (FN.FT, F.FT)
*   Open Customer
    CALL OPF (FN.CUS, F.CUS)
*       Open Account
    CALL OPF (FN.ACC, F.ACC)
*   Open Notificaciones
    CALL OPF (FN.TABLE.NOTIFIC,  F.TABLE.NOTIFIC)
*   Open Mapeo Notificaciones
    CALL OPF (FN.TABLE.NOTIFIC.MAPS,  F.TABLE.NOTIFIC.MAPS)
	
	CALL OPF(FN.TXN.ACH, F.TXN.ACH)  
	
	CALL OPF (FN.EMAIL.CONF,F.EMAIL.CONF)
    
    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

	Y.TIPO.TRANS = 'ACHA'

;*	Y.STATUS.FUT = R.NEW(FT.STATUS)
RETURN 


PROCESS:
    PACKAGE.NAME = "com.bancoazul.sms.SmsT24Tigo"
    THIS.METHOD.NAME = ""
*-----------------------------------------------------------------------------
*------DEBUG------------------------------------------------------------------
;*Y.TIPO.TRANS = 'ACPS'
*-----------------------------------------------------------------------------
*   Y.FT  = 'FT15221FS85Q'
*    CALL F.READ(FN.FT, Y.FT, R.FT,F.FT,FT.ERR)
*
*    Y.TIPO.TRANS = R.FT<FT.TRANSACTION.TYPE>
*    Y.MONTO =  R.FT<FT.CREDIT.AMOUNT>
*    IF Y.MONTO EQ '' THEN
*        Y.MONTO= R.FT<FT.DEBIT.AMOUNT> 
*    END
    
* ;*FT15212WWRJY
* ;*FT152126SR2R   

*	IF Y.STATUS.FUT EQ "AFWD" THEN
*		RETURN
*	END 
*	ELSE
*  	   Y.TIPO.TRANS = R.NEW(FT.TRANSACTION.TYPE)
*  ;* Se lee valor de campo LF.AMOUNT por desarrollo pago anticipado
*  	   IF Y.LF.AMOUNT THEN
*  			Y.MONTO = Y.LF.AMOUNT 
*  	   END	
* 	   ELSE 
*	   	 Y.MONTO =  R.NEW(FT.CREDIT.AMOUNT)
*	     IF Y.MONTO EQ '' THEN
*	        Y.MONTO = R.NEW(FT.DEBIT.AMOUNT)
*	     END  
*   		END 
*   	END
   	
   	
   	CALL F.READ(FN.FUNDS.TRANSFER,R.NEW(EB.SLV98.ID.FT),R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,E.FUNDS.TRANSFER)
   	CALL GET.LOC.REF ('FUNDS.TRANSFER', 'LF.TCE.NARR', Y.LF.TCE.NARR)
   	Y.FT.NARR = R.FUNDS.TRANSFER<FT.LOCAL.REF><1, Y.LF.TCE.NARR>
   	
   	IF RIGHT(Y.FT.NARR,2) EQ 'BP' THEN
   	
	   	Y.MONTO = R.NEW(EB.SLV98.MONTO)
	   	
	   	    ;*----------------------------------------------
;*	    	TEXTO.ARCHIVO = 'FT -> ': R.FUNDS.TRANSFER
;*	    	GOSUB ESCRIBIR.ARCHIVO
	    ;***********************************************
	   	
	;*	Y.TIPO.TRANS = R.NEW(FT.TRANSACTION.TYPE)
	;* Validar que Tipo Transaccion si existe
	
	 ;*TEXTO.ARCHIVO = 'Y.TIPO.TRANS >': Y.TIPO.TRANS:', Y.MONTO >':Y.MONTO
	 ;*   GOSUB ESCRIBIR.ARCHIVO
	
	    CALL F.READ (FN.TABLE.NOTIFIC, Y.TIPO.TRANS , R.NOTI,  F.TABLE.NOTIFIC,ERR.NOTI)
	    IF R.NOTI THEN
	        ESTADO.NOTIFICACION = R.NOTI<EB.SLV.TN.STATUS.NOTIF>
	        IF ESTADO.NOTIFICACION EQ 'ACTIVE' THEN
	            ;* VALIDAR SI ES 1.SMS , 2.EMAIL O 3.AMBAS
	            TIPO.NOTIFICACION = R.NOTI<EB.SLV.TN.TYPE.NOTIF>
	            Y.CHANNEL = R.NOTI<EB.SLV.TN.CHANNEL>
	            LOCATE '11' IN R.NOTI<EB.SLV.TN.OPERATION.API,1> SETTING POS.CREDIT ELSE POS.CREDIT = ''
	            LOCATE '2' IN R.NOTI<EB.SLV.TN.OPERATION.API,1> SETTING POS.DEBIT ELSE POS.DEBIT = ''
	
	            IF POS.DEBIT NE '' OR POS.CREDIT NE '' THEN
	                Y.DEBIT.ACC = R.NOTI<EB.SLV.TN.OPERATION.API,POS.DEBIT>
	                Y.CREDIT.ACC = R.NOTI<EB.SLV.TN.OPERATION.API,POS.CREDIT>
	
	                GOSUB EXTRAER.DATOS.CLIENTE
	            END
	        END
	    END
	END     
RETURN
    

*ESCRIBIR.ARCHIVO:
*    DIR.NAME= 'CHQ.OUT'
*    R.ID   = 'SLV.TRG.SMS':TODAY:'.txt'
*;* hacer que escriba un archivo
*    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
*    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
*    END
*    CLOSESEQ SEQ.PTR
*RETURN


EXTRAER.DATOS.CLIENTE:

*    IF Y.CREDIT.ACC NE '' THEN
*        POS = Y.CREDIT.ACC
*        Y.ACC.CUENTA = R.FUNDS.TRANSFER<FT.CREDIT.ACCT.NO>
**        Y.ACC.CUENTA = R.FT<POS>
*        GOSUB DATOS.CLIENTE
*    END
    IF Y.DEBIT.ACC NE '' THEN
       POS = Y.DEBIT.ACC
        Y.ACC.CUENTA = R.NEW(EB.SLV98.CTA.CLIENTE.AZUL)
*        Y.ACC.CUENTA = R.FT<POS>
        GOSUB DATOS.CLIENTE
    END
    RETURN

DATOS.CLIENTE:
    Y.ACC = Y.ACC.CUENTA
    CALL F.READ(FN.ACC, Y.ACC,R.ACC,F.ACC,ERR.AC)
;*OBTIENE CLIENTE
    Y.CUS=R.ACC<AC.CUSTOMER>
    CALL F.READ(FN.CUS, Y.CUS, R.CUS, F.CUS, ERR.CUS)
    
    ;*----------------------------------------------
    	TEXTO.ARCHIVO = 'CLIENTE -> ': R.CUS
    	GOSUB ESCRIBIR.ARCHIVO
    ;***********************************************
    
    IF R.CUS THEN
        ;*NOMBRE PRINCIPAL
        Y.NOMBRE.PRINCIPAL = R.CUS<EB.CUS.NAME.1>:" ":R.CUS<EB.CUS.TEXT>
       ;* CRT 'Y.NOMBRE.PRINCIPAL ->' : Y.NOMBRE.PRINCIPAL
        ;*MULTIVALOR
        Y.CANAL.PREFERENCIA = R.CUS<EB.CUS.PREF.CHANNEL>
        ;*CRT 'Y.CANAL.PREFERENCIA ->' : Y.CANAL.PREFERENCIA
        ;*MULTIVALOR
        Y.TIPO.COMUNICACION = R.CUS<EB.CUS.COMM.TYPE>
        ;*CRT 'Y.TIPO.COMUNICACION ->' : Y.TIPO.COMUNICACION
        ;*GENERO
        Y.GENERO = R.CUS<EB.CUS.GENDER>
        IF Y.GENERO EQ 'MALE' THEN
            Y.GENERO= "1"
        END ELSE
            Y.GENERO="0"
        END
        ;*MOVIL
        Y.MOVIL = R.CUS<EB.CUS.SMS.1>
        ;*CRT 'Y.MOVIL ->' : Y.MOVIL
		Y.MAIL = R.CUS<EB.CUS.EMAIL.1>

        LOCATE 'SMS' IN R.CUS<EB.CUS.PREF.CHANNEL,1> SETTING POS.SMS ELSE POS.SMS = ''
        LOCATE 'ADVISES' IN R.CUS<EB.CUS.COMM.TYPE,1> SETTING POS.ADV ELSE POS.ADV = ''
        LOCATE 'ALERTS' IN R.CUS<EB.CUS.COMM.TYPE,1> SETTING POS.ALE ELSE POS.ALE = ''
        LOCATE 'EMAIL' IN R.CUS<EB.CUS.COMM.TYPE,1> SETTING POS.ALE ELSE POS.ALE = ''
        ;*Y.MOVIL='79729452'
        IF POS.ADV NE '' OR POS.ALE NE '' AND  Y.MOVIL NE '' THEN
            Y.CANAL.ADV = R.CUS<EB.CUS.PREF.CHANNEL>
            Y.CANAL.ALE = R.CUS<EB.CUS.PREF.CHANNEL>
            ;* Limpiar Variables
            Y.FLAG.ADV = ''
            Y.FLAG.ADVE = ''
            Y.FLAG.ALE = ''
            Y.FLAG.ALEE = ''
            LOOP
                REMOVE CANAL.ADV FROM Y.CANAL.ADV SETTING POS.CADV
            WHILE CANAL.ADV
                IF CANAL.ADV EQ 'SMS' THEN
                    Y.FLAG.ADV = '1'
                    ;*CRT 'CANAL.ADV -> ' : CANAL.ADV
                END
                IF CANAL.ADV EQ 'EMAIL' THEN
                    Y.FLAG.ADVE = '2'
                    ;*CRT 'CANAL.ADV Email -> ' : CANAL.ADV
                END
            REPEAT
            LOOP
                REMOVE CANAL.ALE FROM Y.CANAL.ALE SETTING POS.CALE
            WHILE CANAL.ALE
                IF CANAL.ALE EQ 'SMS' THEN
                    ;*CRT 'CANAL.ALE -> ' : CANAL.ALE
                    Y.FLAG.ALE = '1'
                END
                IF CANAL.ALE EQ 'EMAIL' THEN
                    ;*CRT 'CANAL.ALE Email -> ' : CANAL.ALE
                    Y.FLAG.ALEE = '2'
                END
            REPEAT
        END ;*Fin IF

		;*Validate type of customer 
		CALL GET.LOC.REF ('CUSTOMER', 'SEGMENT', Y.SEGMENT)
		TYPE.CUSTOMER = R.CUS<EB.CUS.LOCAL.REF><1,Y.SEGMENT>
		
	 ;*----------------------------------------------
    	TEXTO.ARCHIVO = 'TYPE.CUSTOMER -> ': TYPE.CUSTOMER
    	GOSUB ESCRIBIR.ARCHIVO
    ;***********************************************
		
			GOSUB ARMAR.MENSAJE   
			
    END
    RETURN

ARMAR.MENSAJE:
    CALL F.READ (FN.TABLE.NOTIFIC.MAPS, Y.TIPO.TRANS , R.NOTI.MAPS,  F.TABLE.NOTIFIC.MAPS,ERR.NOTI.MAP)
    IF R.NOTI.MAPS THEN
    	;* Llenando Header
        IF Y.GENERO EQ '1' THEN
            LOCATE 'M' IN R.NOTI.MAPS<EB.SLV.MPP.GENDER,1> SETTING POS.MALE ELSE POS.MALE = ''
            IF POS.MALE NE '' THEN
            	HEADER = R.NOTI.MAPS<EB.SLV.MPP.SECTION.HEADER,POS.MALE>
            END
        END
        ELSE
        	LOCATE 'F' IN R.NOTI.MAPS<EB.SLV.MPP.GENDER,1> SETTING POS.FEMALE ELSE POS.FEMALE = ''
        	IF POS.FEMALE NE '' THEN
            	HEADER = R.NOTI.MAPS<EB.SLV.MPP.SECTION.HEADER,POS.FEMALE>
            END      
    	END
    	
    	;*Llenando Body
    	IF R.NOTI.MAPS<EB.SLV.MPP.OPERATION.CREDIT> EQ POS THEN
    		BODY = SWAP(R.NOTI.MAPS<EB.SLV.MPP.MESSAGE.CREDIT>, @VM, ' ')
    	END
    	ELSE
    		BODY = SWAP(R.NOTI.MAPS<EB.SLV.MPP.MESSAGE.DEBIT>, @VM, ' ')
    	END
    	
    	;*Llenando Footer
    	FOOTER = SWAP(R.NOTI.MAPS<EB.SLV.MPP.FOOTER>, @VM, ' ')
    	
    	;*VALIDAR SI ES NECESARIO EL MONTO O NO
				AMT.FLAG  = R.NOTI<EB.SLV.TN.AMT.FLAG>
    			IF AMT.FLAG EQ 'INACTIVE' THEN
    				Y.ARGS = HEADER:" ":Y.NOMBRE.PRINCIPAL:BODY:FOOTER:"%S":Y.MOVIL
    			END
    			ELSE
    				Y.ARGS = HEADER:" ":Y.NOMBRE.PRINCIPAL:BODY:Y.MONTO:FOOTER:"%S":Y.MOVIL
    			END
*    			 TEXTO.ARCHIVO = 'AMT.FLAG >': AMT.FLAG:', Y.ARGS >':Y.ARGS
*    			GOSUB ESCRIBIR.ARCHIVO

		 ;*----------------------------------------------
    	TEXTO.ARCHIVO = 'Y.ARGS -> ': Y.ARGS
    	GOSUB ESCRIBIR.ARCHIVO
    ;***********************************************

	IF TYPE.CUSTOMER EQ '2' THEN

        IF (Y.FLAG.ADV EQ '1' OR Y.FLAG.ALE EQ '1') AND (TIPO.NOTIFICACION ='SMS' OR TIPO.NOTIFICACION = 'AMBAS') THEN
            METHOD.NAME= "enviarMSJ"
            GOSUB ENVIAR.MENSAJE
        END

        IF (Y.FLAG.ADVE EQ '2' OR Y.FLAG.ALEE EQ '2') AND (TIPO.NOTIFICACION ='EMAIL' OR TIPO.NOTIFICACION = 'AMBAS') THEN
            METHOD.NAME= "enviarEMAIL"
            ;*GOSUB ENVIAR.MENSAJE
        END
    END
    ELSE
    	IF TYPE.CUSTOMER EQ '1' THEN
    		Y.ID.CONF.MAIL = 'ACH.BP'
    		GOSUB SEND.MAIL
    	END
    END    
        
    END

RETURN

ENVIAR.MENSAJE:
    THIS.PACKAGE.CLASS = PACKAGE.NAME
    THIS.METHOD.SMS=  METHOD.NAME
    CALLJ.ARGUMENTS.SMS =  Y.ARGS
	CALLJ.ERROR.SMS = " "
	CALLJ.RESPONSE.SMS = " "
    ;*CRT 'THIS.METHOD.NAME -> '    : THIS.METHOD.SMS
    ;*CRT 'THIS.PACKAGE.CLASS -> ' : THIS.PACKAGE.CLASS
    ;*CRT 'Y.ARGS -> ' : Y.ARGS
;*Llamada el metodo.
;*-----------------------------------------------------------------------------
		CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.SMS,CALLJ.ARGUMENTS.SMS,CALLJ.RESPONSE.SMS,CALLJ.ERROR.SMS)
		IF CALLJ.ERROR.SMS EQ '0' AND CALLJ.ARGUMENTS.SMS[1,3]!='(-1' THEN
			 ;*ETEXT = "No se puede enviar el sms":CALLJ.RESPONSE.SMS:"ARGS:":CALLJ.ARGUMENTS.SMS
		;*----------------------------------------------
    	TEXTO.ARCHIVO = 'CALLJ.ERROR.SMS -> ': "No se puede enviar el sms":CALLJ.RESPONSE.SMS:"ARGS:":CALLJ.ARGUMENTS.SMS
    	GOSUB ESCRIBIR.ARCHIVO
    ;***********************************************
			 
		END ELSE
		
		END
RETURN

SEND.MAIL:

	CALL F.READ(FN.EMAIL.CONF,Y.ID.CONF.MAIL,R.EMAIL.CONF,F.EMAIL.CONF,E.EMAIL.CONF)
	BODY.EMAIL = R.EMAIL.CONF<EB.SLV49.EMAIL.BODY>
	
	TYPE.EMAIL = R.EMAIL.CONF<EB.SLV49.EMAIL.FORMAT>
	SUBJECT.EMAIL = R.EMAIL.CONF<EB.SLV49.EMAIL.HEAD>
	
	CHANGE '@monto'  TO FMT(Y.MONTO,'$L2,') IN BODY.EMAIL
	CHANGE '@cuenta'  TO 'XXXXXXX':RIGHT(Y.ACC.CUENTA,4)  IN BODY.EMAIL
	CHANGE '@beneficiario'  TO  R.NEW(EB.SLV98.NAME.BENEF) IN BODY.EMAIL
	
	 IF FIELD(OCONV(TIME(), "MTS"),":",1) GE 1 AND FIELD(OCONV(TIME(), "MTS"),":",1) LE 11 THEN
	 	CHANGE '@fechaHora' TO  R.NEW(EB.SLV98.FECHA.UPD):' am'  IN BODY.EMAIL
	 END 		
	 ELSE
	 	CHANGE '@fechaHora'  TO R.NEW(EB.SLV98.FECHA.UPD):' pm' IN BODY.EMAIL
	 END
	 
	CHANGE '@bancoBeneficiario'  TO R.NEW(EB.SLV98.BANK.NAME.BEN) IN BODY.EMAIL
	
	CHANGE '@FT'   TO R.NEW(EB.SLV98.ID.FT) IN BODY.EMAIL
	
	Y.MESSAGE.EMAIL = '<p>':HEADER:Y.NOMBRE.PRINCIPAL:BODY.EMAIL
	
			Y.MESSAGE.EMAIL  = CHANGE(Y.MESSAGE.EMAIL , FM, ' ')
			Y.MESSAGE.EMAIL  = CHANGE(Y.MESSAGE.EMAIL , SM, ' ')
			Y.MESSAGE.EMAIL  = CHANGE(Y.MESSAGE.EMAIL , VM, ' ')
	;*----------------------------------------------
    	TEXTO.ARCHIVO = 'SUBJECT.EMAIL -> ': SUBJECT.EMAIL :" Y.MESSAGE.EMAIL -> ": Y.MESSAGE.EMAIL: " Y.MAIL -> ":Y.MAIL:" TYPE.EMAIL-> ":TYPE.EMAIL
    	GOSUB ESCRIBIR.ARCHIVO
    ;***********************************************
	CALL SLV.SEND.EMAIL.T24(SUBJECT.EMAIL,Y.MESSAGE.EMAIL,Y.MAIL,'N/0',TYPE.EMAIL,R.EMAIL)
	TEXTO.ARCHIVO = " R.EMAIL-> ":R.EMAIL
	GOSUB ESCRIBIR.ARCHIVO
	
RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'ACH'
    R.ID   = 'ACH.PAGO.SMS.':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN  
    
 END   