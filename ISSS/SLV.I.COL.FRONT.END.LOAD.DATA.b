*-----------------------------------------------------------------------------
* <Rating>-111</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.I.COL.FRONT.END.LOAD.DATA
*-----------------------------------------------------------------------------
*
* Descripcion: RTN que permite obtener la DATA que se obtiene del GATEWAY del colector (en esta caso MH)
*              y realiza la distribucion de la DATA en la version
* 1.0 		*******		2017.**.**		Inicial
* 2.0		RCORTES		2017.09.04		- Actualizaci�n para pagos que provienen de PAGOES = TOKEN 
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.COL.FRONT.END
    $INSERT I_F.USER
    $INSERT I_F.DATES
    $INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------
    GOSUB INI
    GOSUB OPENFILE
    GOSUB PROCESS


INI:
    FN.COL.FRONT.END = 'F.EB.SLV.COL.FRONT.END'
    F.COL.FRONT.END = ''

    FN.DATES = 'F.DATES'
    F.DATES =  ''

    RETURN

OPENFILE:

    CALL OPF(FN.COL.FRONT.END, F.COL.FRONT.END)

    RETURN
PROCESS:

;*********** <DEBUG> ***********  
*FLAG 				= '0'
*CODIGO.COLECTOR 	= '01'
*CHANNEL 			= 'TCIB'
*SERVICIO 			= 'NPEMH'
*TOKEN.NPE 			= '0463000003558820170113051000004598'
*CODIGO.INSTITUCION = ''
*ID.APP 			= '01-946'
*TYPE.INPUT 		= 'NPE'
;*********** </DEBUG> **********    
    
    FLAG 				= R.NEW(EB.SLV8.RESERVADO.39)
    CODIGO.COLECTOR 	= R.NEW(EB.SLV8.RESERVADO.3)
    CHANNEL 			= R.NEW(EB.SLV8.RESERVADO.38)
    SERVICIO 			= R.NEW(EB.SLV8.RESERVADO.17)
    TOKEN.NPE 			= R.NEW(EB.SLV8.RESERVADO.1)
    CODIGO.INSTITUCION 	= R.NEW(EB.SLV8.RESERVADO.2)
    ID.APP 				= ID.NEW
    TYPE.INPUT 			= R.NEW(EB.SLV8.RESERVADO.16)

;*RECUPERAR DATE
    CALL F.READ(FN.DATES,'SV0010001',RECORD.DATES,F.DATES,ERR.DATES)
    R.NEW(EB.SLV8.RESERVADO.44) = RECORD.DATES<EB.DAT.TODAY>
    R.NEW(EB.SLV8.RESERVADO.45) = RECORD.DATES<EB.DAT.CO.BATCH.STATUS>
   
    IF FLAG EQ '0' THEN
						    TEXTO.ARCHIVO = '----------- CONSULTA DE PAGO -----------'
						    GOSUB ESCRIBIR.ARCHIVO
						    
        GOSUB CALL.J
        
        					TEXTO.ARCHIVO = '----------------------------------------'
						    GOSUB ESCRIBIR.ARCHIVO
    END
    
    RETURN

CALL.J:
;*OBTENIENDO AGENCIA 
    IF CHANNEL EQ 'TCIB' OR CHANNEL EQ 'TCE' THEN
        AGENCIA = 'SV0010001'
        R.NEW(EB.SLV8.RESERVADO.23) = AGENCIA
    END
    ELSE
    	AGENCIA = R.USER<EB.USE.COMPANY.CODE><1,1>
    	R.NEW(EB.SLV8.RESERVADO.23) = AGENCIA
    END
    
    IF SERVICIO EQ 'PAGOES' THEN
    	R.NEW(EB.SLV8.RESERVADO.16) = 'TOKEN'
       	TYPE.INPUT = 'TOKEN'
    END
		     				TEXTO.ARCHIVO = TIMEDATE() : ' | ID >> ' : ID.APP : ' | Servicio >> ' : SERVICIO : ' | ' : TYPE.INPUT : ' >> ' : TOKEN.NPE
						    GOSUB ESCRIBIR.ARCHIVO
						        
;*DEFINIENDO EL PARAMETRO PARA LA FUNCION EN EL JAR QUE SE CONECTARA A BD
    Y.PARAMETRO.ARGUMENT = CODIGO.COLECTOR:'~':CHANNEL:'~':SERVICIO:'~':ID.APP:'~':AGENCIA:'~':TYPE.INPUT:'~':TOKEN.NPE:'~':CODIGO.INSTITUCION

						    TEXTO.ARCHIVO = 'PASO 1: INVOCANDO METODO CINTEX <<getPaymentDetail>>'
						    GOSUB ESCRIBIR.ARCHIVO
						    
						    TEXTO.ARCHIVO = ' + Parametro de entrada >> ' : Y.PARAMETRO.ARGUMENT
						    GOSUB ESCRIBIR.ARCHIVO
						    
;*ASIGNACION DE PARAMETROS PARA EL CALLJ
    THIS.PACKAGE.CLASS 	= "com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT		= "getPaymentDetail2"
    CALLJ.ARGUMENTS.CLT = Y.PARAMETRO.ARGUMENT
    CALLJ.ERROR.SMS 	= " "
    CALLJ.RESPONSE.CLT 	= " "
;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    TRAMA.ORDER.PAY = CALLJ.RESPONSE.CLT
    CNN.TRAMA =  FIELD(TRAMA.ORDER.PAY,'|',2)
    
							    TEXTO.ARCHIVO = ' + Parametro de salida >> ' : TRAMA.ORDER.PAY
							    GOSUB ESCRIBIR.ARCHIVO
							    
	;* Manejo de respuesta del gateway (si ocurriese error)
    IF CNN.TRAMA EQ 'OK' THEN
        ;*CRT CALLJ.RESPONSE.CLT
        
        
	    IF SERVICIO EQ 'PAGOES' THEN
			GOSUB SEGMENTACION.TRAMA
	    END        
        ELSE 
        	GOSUB SEGMENTACION.TRAMA.NPE	
        END
        
*        IF TYPE.INPUT EQ 'NPE' THEN
*        	GOSUB SEGMENTACION.TRAMA.NPE	
*        END
*        ELSE
*        	;* SERVICIO = 'PAGOES'
*        	GOSUB SEGMENTACION.TRAMA
*        END
    END
    ELSE
    	SEGMENTO.ERROR = TRAMA.ORDER.PAY
    	ERROR = FIELD(SEGMENTO.ERROR,'|',3)

    	R.NEW(EB.SLV8.RESERVADO.15) = ERROR
    	R.NEW(EB.SLV8.RESERVADO.39) = "-2"

		;* E='EB-COLLECTOR.CALLJ.LOAD.TRAMA'
		;* CALL ERR
    END

	;* CRT TRAMA.ORDER.PAY
    RETURN

;************EJEMPLO DEL VALOR A RECUPERAR EN EL CALLJ
;*                   CAMPO  VALOR
;*                  |-----| |---|
;*  123456789|8552|@"campo 1:1.99|campo 2:2.99|campo 3:3.99@campo 4:4.99|campo 5:5.99|campo 6:6.99"
;*                  |------------------------------------| |-------------------------------------|
;*                                 BLOQUE 2                                  BLOQUE 3
;************FIN EJEMPLO
SEGMENTACION.TRAMA:

						    TEXTO.ARCHIVO = 'PASO 2: REALIZACION DE <<SEGMENTACION.TRAMA>>'
						    GOSUB ESCRIBIR.ARCHIVO


;*    CAMPOS.MANDATORIOS = "OK|NPE-TOKEN:0|COD.INSTITUCION:1|COD.COLECTOR:2|VALOR1:10000000005148|VALOR2:10000000005148|VALOR3:123456789|VALOR4:10000000005148|VALOR5:prueba4|VALOR6:20161111|VALOR7:11|VALOR8:20161111|VALOR9:PAGO DECLARACIONES|VALOR10:12|VALOR11:750.25"
    CAMPOS.MANDATORIOS 		= FIELD(TRAMA.ORDER.PAY,'@',1)
    CAMPOS.MULTIVALOR.X2 	= FIELD(TRAMA.ORDER.PAY,'@',2)
    CAMPOS.MULTIVALOR.X3 	= FIELD(TRAMA.ORDER.PAY,'@',3) 
    MONTO.PAGAR 			= FIELD(TRAMA.ORDER.PAY,'@',4)
    CUENTA.TRANSITORIA.SEG 	= FIELD(TRAMA.ORDER.PAY,'@',5)

    TRAMA.MANDATORIO 		= CHANGE(CAMPOS.MANDATORIOS,'|',VM)    
    TRAMA.MONTO.PAGAR 		= CHANGE(MONTO.PAGAR,'|',VM) 
    CUENTA.TRANSITORIA		= CHANGE(CUENTA.TRANSITORIA.SEG,'|',VM)

;*    CANTIDAD.REGISTROS = DCOUNT(TRAMA.MANDATORIO,VM)
;*------------------- CAMPOS MANDATORIOS
;*TRANSACCION GUBERNAMENTAL
    VAR.3 						= FIELD(TRAMA.MANDATORIO,VM,3)
    VALOR.RESER.3 				= FIELD(VAR.3,':',2)
    R.NEW(EB.SLV8.RESERVADO.6) 	=  VALOR.RESER.3

;*FECHA DE PAGO
    VAR.4 						= FIELD(TRAMA.MANDATORIO,VM,4)
    VALOR.RESER.4 				= FIELD(VAR.4,':',2)
    R.NEW(EB.SLV8.RESERVADO.9) 	=  VALOR.RESER.4

;*HORA DE PAGO
    VAR.5 						= FIELD(TRAMA.MANDATORIO,VM,5)
    VALOR.RESER.5 				= FIELD(VAR.5,':',2)
    R.NEW(EB.SLV8.RESERVADO.10) =  VALOR.RESER.5

;*FECHA DE VENCIMIENTO
    VAR.6 						= FIELD(TRAMA.MANDATORIO,VM,6)
    VALOR.RESER.6 				= FIELD(VAR.6,':',2)
    R.NEW(EB.SLV8.RESERVADO.11) =  VALOR.RESER.6

;*CODIGO DE INSTITUCION
    VAR.7 						= FIELD(TRAMA.MANDATORIO,VM,7)
    VALOR.RESER.7 				= FIELD(VAR.7,':',2)
    R.NEW(EB.SLV8.RESERVADO.2) 	=  VALOR.RESER.7

;*NOMBRE DE INSTITUCION
    VAR.8 						= FIELD(TRAMA.MANDATORIO,VM,8)
    VALOR.RESER.8 				= FIELD(VAR.8,':',2)
    R.NEW(EB.SLV8.RESERVADO.7) 	=  VALOR.RESER.8

;*Numero Documento Cliente
    VAR.9 						= FIELD(TRAMA.MANDATORIO,VM,9)
    VALOR.RESER.9 				= FIELD(VAR.9,':',2)
    R.NEW(EB.SLV8.RESERVADO.40) =  VALOR.RESER.9

;*NATURALEZA
    VAR.10 						= FIELD(TRAMA.MANDATORIO,VM,10)
    VALOR.RESER.10 				= FIELD(VAR.10,':',2)
    R.NEW(EB.SLV8.RESERVADO.41) =  VALOR.RESER.10

;*NUMERO DOCUMENTO
    VAR.11 						= FIELD(TRAMA.MANDATORIO,VM,11)
    VALOR.RESER.11 				= FIELD(VAR.11,':',2)
    R.NEW(EB.SLV8.RESERVADO.8) 	=  VALOR.RESER.11

;*REFERENCIA
    VAR.12 						= FIELD(TRAMA.MANDATORIO,VM,12)
    VALOR.RESER.12 				= FIELD(VAR.12,':',2)
    R.NEW(EB.SLV8.RESERVADO.12) =  VALOR.RESER.12
;*PERIODO
    VAR.13 						= FIELD(TRAMA.MANDATORIO,VM,13)
    VALOR.RESER.13 				= FIELD(VAR.13,':',2)
    R.NEW(EB.SLV8.RESERVADO.42) =  VALOR.RESER.13

;*MONTO TOTAL
    VAR.14 						= FIELD(TRAMA.MONTO.PAGAR,VM,1)
    R.NEW(EB.SLV8.RESERVADO.14) = VAR.14

;*CUENTA TRANSITORIA
    VAR.15 						= FIELD(CUENTA.TRANSITORIA,VM,1)
    R.NEW(EB.SLV8.RESERVADO.5) 	= VAR.15

;*-------------------FIN CAMPOS MANDATORIOS

;*------------------CAMPOS MULTIVALOR X2
;*    CAMPOS.MULTIVALOR.X2 = "9|DUI:04209645-7|NIT:123456789-7|ADUANA:852456"
    TRAMA.MULTIVALOR.X2 				= CHANGE(CAMPOS.MULTIVALOR.X2,'|',VM)
    CANTIDAD.REGISTROS.MULTIVALOR.X2 	= DCOUNT(TRAMA.MULTIVALOR.X2,VM)-1 ;* MENOS EL IDENTIFICADOR DEL TAMA�O DEL SEGMENTO
    
;*    CRT CANTIDAD.REGISTROS.MULTIVALOR.X3
    FOR J = 1 TO CANTIDAD.REGISTROS.MULTIVALOR.X2

        SEGMENTO.X2 	= FIELD(TRAMA.MULTIVALOR.X2,VM,J+1)
        MARKER.FIELD 	= FIELD(SEGMENTO.X2,':',1)
        MARKER.VALUE 	= FIELD(SEGMENTO.X2,':',2)

        IF J EQ 1 THEN
            R.NEW(EB.SLV8.RESERVADO.33) =  MARKER.FIELD
            R.NEW(EB.SLV8.RESERVADO.34) =  MARKER.VALUE
        END
        ELSE
        R.NEW(EB.SLV8.RESERVADO.33) := VM: MARKER.FIELD
        R.NEW(EB.SLV8.RESERVADO.34) := VM: MARKER.VALUE
    END
    NEXT J
;*-----------------FIN CAMPOR MULTIVALOR X2

;*------------------CAMPOS MULTIVALOR X3
;*    CAMPOS.MULTIVALOR.X3 = "9|11101:Esp1:5.0|15301:Esp 2:0.99|00000:Esp3:0.01|00000:Esp4:0|00000:Esp5:4|00000:Esp6:0|00000:Esp7:0|00000:Esp8:0|00000:Esp9:0|9999:Esp10:9.00"
    TRAMA.MULTIVALOR.X3 				= CHANGE(CAMPOS.MULTIVALOR.X3,'|',VM)
    CANTIDAD.REGISTROS.MULTIVALOR.X3 	= DCOUNT(TRAMA.MULTIVALOR.X3,VM)-1 ;* MENOS EL IDENTIFICADOR DEL TAMA�O DEL SEGMENTO
    
;*    CRT CANTIDAD.REGISTROS.MULTIVALOR.X3
    FOR H = 1 TO CANTIDAD.REGISTROS.MULTIVALOR.X3

        SEGMENTO.X3 		= FIELD(TRAMA.MULTIVALOR.X3,VM,H+1)
        VALOR.CODIGO 		= FIELD(SEGMENTO.X3,':',1)
        VALOR.DESCRIPCION 	= FIELD(SEGMENTO.X3,':',2)
        VALOR.TOTAL 		= FIELD(SEGMENTO.X3,':',3)

        IF H EQ 1 THEN
            R.NEW(EB.SLV8.RESERVADO.35) =  VALOR.CODIGO
            R.NEW(EB.SLV8.RESERVADO.36) =  VALOR.DESCRIPCION
            R.NEW(EB.SLV8.RESERVADO.37) =  VALOR.TOTAL
        END
        ELSE
	        R.NEW(EB.SLV8.RESERVADO.35) := VM: VALOR.CODIGO
	        R.NEW(EB.SLV8.RESERVADO.36) := VM: VALOR.DESCRIPCION
	        R.NEW(EB.SLV8.RESERVADO.37) := VM: VALOR.TOTAL
    	END
    NEXT H

;*-----------------FIN CAMPOR MULTIVALOR X3

RETURN

SEGMENTACION.TRAMA.NPE:

						    TEXTO.ARCHIVO = 'PASO 2: REALIZACION DE <<SEGMENTACION.TRAMA.NPE>>'
						    GOSUB ESCRIBIR.ARCHIVO


	CAMPOS.MANDATORIOS = TRAMA.ORDER.PAY
	TRAMA.MANDATORIO = CHANGE(CAMPOS.MANDATORIOS,'|',VM)
    

	;*FECHA DE VENCIMIENTO
    VAR.6 						= FIELD(TRAMA.MANDATORIO,VM,5)
    VALOR.RESER.6 				= FIELD(VAR.6,':',2)
    R.NEW(EB.SLV8.RESERVADO.11) =  VALOR.RESER.6
	
	;*REFERENCIA
    VAR.12 						= FIELD(TRAMA.MANDATORIO,VM,7)
    VALOR.RESER.12 				= FIELD(VAR.12,':',2)
    R.NEW(EB.SLV8.RESERVADO.12) =  VALOR.RESER.12
	
	;*MONTO TOTAL
    VAR.14 						= FIELD(TRAMA.MANDATORIO,VM,4)
    VALOR.RESER.14 				= FIELD(VAR.14,':',2)
    R.NEW(EB.SLV8.RESERVADO.14) = VALOR.RESER.14

	;*CUENTA TRANSITORIA
    ;*VAR.15 = FIELD(TRAMA.MANDATORIO,VM,9)
    ;*R.NEW(EB.SLV8.RESERVADO.5) = VAR.15
	;*CRT VAR.15
	
	;*@Author:Ronald Ortiz
	;*@Date: 20170505
	FIELD.COUNT 				= DCOUNT(TRAMA.MANDATORIO,VM)
	CTA.TRAN    				= FIELD(TRAMA.MANDATORIO,VM,FIELD.COUNT-1)
	R.NEW(EB.SLV8.RESERVADO.5) 	= CTA.TRAN

	
	;*@Author:Ronald Ortiz
	;*@Date: 20170511
    CAMPOS.MANDATORIOS 			= TRAMA.ORDER.PAY
	TRAMA.MULTIVALOR 			= CHANGE(CAMPOS.MANDATORIOS,'|',FM)
	POSICION.INICIAL.MULTIVALOR = 9
	POSICION.FINAL.MULTIVALOR   = DCOUNT(TRAMA.MULTIVALOR,FM) - 2
		
	FOR I = 9 TO DCOUNT(TRAMA.MULTIVALOR,FM)-2
	    CLAVE.VALOR.MTVR = TRAMA.MULTIVALOR<I,1>
		IF I EQ POSICION.INICIAL.MULTIVALOR THEN
		        R.NEW(EB.SLV8.RESERVADO.33) =  FIELD(CLAVE.VALOR.MTVR,':',1)
	            R.NEW(EB.SLV8.RESERVADO.34) =  FIELD(CLAVE.VALOR.MTVR,':',2)
		END
		ELSE
		         R.NEW(EB.SLV8.RESERVADO.33) := VM: FIELD(CLAVE.VALOR.MTVR,':',1)
	             R.NEW(EB.SLV8.RESERVADO.34) := VM: FIELD(CLAVE.VALOR.MTVR,':',2)
		END
	NEXT I
	
	RETURN

ESCRIBIR.ARCHIVO: 
    DIR.NAME= 'COLECTORES'
    R.ID   = 'CINTEX.CONSULTA.PAGO':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN


    END
