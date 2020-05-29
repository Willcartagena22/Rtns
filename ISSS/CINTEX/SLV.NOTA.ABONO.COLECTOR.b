*-----------------------------------------------------------------------------
* <Rating>-74</Rating>
*-----------------------------------------------------------------------------
    ;*SUBROUTINE  SLV.NOTA.ABONO.COLECTOR(MONTO, CD.CUENTA,NOMBRECOLECTOR,NOMBRE.CLIENTE.DEBITO,NOMBRE.CLIENTE.CREDITO)
    SUBROUTINE  SLV.NOTA.ABONO.COLECTOR(MONTO, NOMBRE.COLECTOR, CD.CUENTA, CD.CLIENTE, DB.CUENTA, DB.CLIENTE, DB.NOMBRE.CTA) 
*-----------------------------------------------------------------------------
* 1.0		XXXXX			XXXXX			Version inicial 
* 2.0		RCORTES			20171115		Actualización de Parametros recibidos y enviados para generacion de archivo txt 
*-----------------------------------------------------------------------------
* Modification History : 
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPEN.FILE
    GOSUB PROCESS
    RETURN

INIT:
	FN.ACCOUNT       = 'F.ACCOUNT'
	F.ACCOUNT        = ''
RETURN

OPEN.FILE:
	CALL OPF(FN.ACCOUNT,F.ACCOUNT)
RETURN

PROCESS:
    CRT 'INICIANDO PROCESO DE GENERACION NOTA DE ABONO'
    GOSUB GET.DATE.SYSTEM
    GOSUB GET.VALORES.NOTA.ABONO
    GOSUB GENERAR.SPOOL
    CRT 'FINALIZACION PROCESO DE GENERACION NOTA DE ABONO'
    RETURN

GET.VALORES.NOTA.ABONO:
    MONTO.ENVIADO = MONTO
    NO.CUENTA     = CD.CUENTA

;*PROCEDEMOS A FORMATEAR EL MONTO ENVIADO 
    CALL SLV.UTIL.FORMATO.MONEDA(MONTO)
    MONTO.FORMATEADO = MONTO

;*PROCEDEMOS A OBTENER EL MONTO EN LETRAS 
    CALL SLV.UTIL.COLECTOR.NUMBER.LETTER(MONTO.ENVIADO, '', MONTO.ENVIADO)
    ;*CALL SLV.UTIL.IMP.GET.NUMLETRAS(MONTO.ENVIADO)
    ;*MONTO.LETRAS     = MONTO.ENVIADO

;*OBTENEMOS EL TIPO DE CUENTA
    CALL SLV.UTIL.GET.TIPO.CTA(CD.CUENTA)
    
    IF CUENTA EQ '0' THEN
        TIPO.CUENTA = 'CUENTA DE AHORRO'
    END
    ELSE
    TIPO.CUENTA = 'CUENTA CORRIENTE'
    END

;*OBTENEMOS LA RAZON SOCIAL DEL CLIENTE
    Y.RAZON.SOCIAL = NO.CUENTA
    CALL SLV.GET.RAZON.SOCIAL(Y.RAZON.SOCIAL)
    RETURN
 
;*SUBPROCESO PARA OBTENER LA FECHA EN QUE SE GENERA EL ABONO A LA CUENTA DEL CLIENTE
GET.DATE.SYSTEM:
    FECHA = OCONV(DATE(),"D/")
    FECHA.TRX = FECHA[7,4]:FECHA[1,2]:FECHA[4,2]
    FECHA.PAGO    = FECHA[4,2]:'/':FECHA[1,2]:'/':FECHA[7,4]
    RETURN 

GENERAR.SPOOL:
	;*CRT CHANGE(NOMBRE.COLECTOR, ' ', '_')
	NOMBRE.COLECTOR		= UPCASE(NOMBRE.COLECTOR)
    MONTO.SPOOL 		= '$':MONTO.FORMATEADO
    FECHA.COMPROBANTE 	= OCONV(ICONV(TODAY, "D"),"D4/E"):' ':OCONV(TIME(), "MTS")
    stringData  		= ''
    stringData 			:= 'Fecha='			:OCONV(ICONV(TODAY, "D"),"D4/E"):';'
    stringData 			:= 'TipoCuenta='	:TIPO.CUENTA:';'
    stringData 			:= 'CuentaCredito='	:NO.CUENTA:';'
    stringData 			:= 'MontoTr='		:MONTO.SPOOL:';'
    stringData 			:= 'NameCtaCr='		:NOMBRE.COLECTOR:';'
    stringData 			:= 'CuentaDebito='	:DB.CUENTA:';'  
    stringData 			:= 'NameCtaDb='		:NOMBRE.COLECTOR:';'
    stringData 			:= 'MontoLetras='	:MONTO.ENVIADO:';'
    stringData 			:= 'FechaPago='		:FECHA.COMPROBANTE:';'
    stringData 			:= 'NombreCuentaCredito='	:CD.CLIENTE:';'
    ;*stringData 			:= 'NameCtaCr='		:Y.RAZON.SOCIAL:';'
    ;*stringData 			:= 'nombre='		:DB.CLIENTE:';'
    
    
    NOMBRE.COLECTOR = CHANGE(NOMBRE.COLECTOR, ' ', '_')
    idArchivo    		= 'NOTAABONOCOLECTOR-'		:NOMBRE.COLECTOR:'-':FECHA.TRX:'.txt'
;*    direcArchivo = 'C:\APDF\spool\'
    direcArchivo = 'SPOOL.FILES'
    OPENSEQ direcArchivo, idArchivo TO SEQ.PTR ELSE
        CREATE SEQ.PTR ELSE
        CRT 'No se puede crear el archivo.'
    END

    WRITESEQ stringData ON SEQ.PTR ELSE
    CRT 'No se pueden escribir los datos en el archivo.'

    END

    RETURN

    END
