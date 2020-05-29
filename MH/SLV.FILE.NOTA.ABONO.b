*-----------------------------------------------------------------------------
* <Rating>-55</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.FILE.NOTA.ABONO(MONTO,CUENTA,NOMBRECOLECTOR)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB PROCCESS
    RETURN

INIT:
    RETURN

PROCCESS:
      CRT 'INICIANDO PROCESO DE GENERACION NOTA DE ABONO'
      GOSUB GET.DATE.SYSTEM
      GOSUB GET.VALORES.NOTA.ABONO
      GOSUB GENERAR.SPOOL
      CRT 'FINALIZACION PROCESO DE GENERACION NOTA DE ABONO'
    RETURN

GET.VALORES.NOTA.ABONO:
    MONTO.ENVIADO = MONTO
    NO.CUENTA     = CUENTA

;*PROCEDEMOS A FORMATEAR EL MONTO ENVIADO
    CALL SLV.UTIL.FORMATO.MONEDA(MONTO)
    MONTO.FORMATEADO = MONTO

;*PROCEDEMOS A OBTENER EL MONTO EN LETRAS
    CALL SLV.UTIL.IMP.GET.NUMLETRAS(MONTO.ENVIADO)
    MONTO.LETRAS     = MONTO.ENVIADO

;*OBTENEMOS EL TIPO DE CUENTA
    CALL SLV.UTIL.GET.TIPO.CTA(CUENTA)

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
       MONTO.SPOOL = '$':MONTO.FORMATEADO
       FECHA.COMPROBANTE = OCONV(ICONV(TODAY, "D"),"D4/E"):' ':OCONV(TIME(), "MTS")
       stringData  = ''
       stringData := 'Fecha=':OCONV(ICONV(TODAY, "D"),"D4/E"):';'
       stringData := 'TipoCuenta=':TIPO.CUENTA:';'
       stringData := 'NumeroCuenta=':NO.CUENTA:';'
       stringData := 'MontoTr=':MONTO.SPOOL:';'
       stringData := 'MontoLetras=':MONTO.LETRAS:';'
       stringData := 'FechaPago=':FECHA.COMPROBANTE:';'
       stringData := 'Colector=':Y.RAZON.SOCIAL:';'
       
       idArchivo    = 'NOTAABONO-':FECHA.TRX:'.txt'
       ;*direcArchivo = 'C:\APDF\spool\'
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


