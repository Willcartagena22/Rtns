*-----------------------------------------------------------------------------
* <Rating>-56</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.AMT.BENEF.MAZUL(IN.DUI, IN.MONTO, OUT.MENSUAL, OUT.DIARIO)
*-----------------------------------------------------------------------------
* Nombre: SLV.V.AMT.BENEF.MAZUL
* Descripcion: Rutina encargada de validar Monto Limite de Moneda Azul Masiva para Beneficiario (Monto mensual diario y No. Transacciones mensuales y diario).
* 			   Entrada: IN.DUI: Dui beneficiario
*    					IN.MONTO: Monto del pago entrante							
* 			   Salida: OUT.MENSUAL: Flag 1/0. 1: permitir pago. 0: No permitir pago
*					   OUT.DIARIO: Flag 1/0. 1: permitir pago. 0: No permitir pago
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		iTurcios	29.08.19	Version inicial
* 1.1		iTurcios	12.09.19	Se anade validacion monto diario permitido por beneficiario.  
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.CONTADOR.MAZUL
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN

INIT:
    FN.CONT.MAZUL = 'F.EB.SLV.CONTADOR.MAZUL'
    F.CONT.MAZUL  = ''
    CALL OPF(FN.CONT.MAZUL, F.CONT.MAZUL)
    
    ;*Constantes
    EQU SLV.TOKEN TO 'SLV.TOKEN'
    EQU TMMA TO 'TMMA'
    
    OUT.MENSUAL = 1
    OUT.DIARIO = 1 
    
    ;*Variables 
    Y.CANT.DIARIA.MAZUL = ''
    Y.MONTO.DIARIO.MAZUL = '' 
    Y.CANT.MENSUAL.MAZUL = ''
    Y.MONTO.MENSUAL.MAZUL = ''    
    
    ;*Debug 
*   IN.DUI = '046728832'
*   IN.MONTO = 10
RETURN

PROCESS:	
	;*Obtiene limites.
	CALL F.READ(FN.CONT.MAZUL, IN.DUI, R.CONT.MAZUL, F.CONT.MAZUL, ERR.CONT)
	
	IF R.CONT.MAZUL THEN
		;*Obteniendo parametria
		CALL SLV.GET.KEYS.PARAM(SLV.TOKEN,TMMA,OUT.DESGEN,OUT.DES,OUT.VAL)
	    
	    Y.CANT.DIARIA.MAZUL   = FIELD(OUT.VAL, @SM, 9)
	    Y.MONTO.DIARIO.MAZUL  = FIELD(OUT.VAL, @SM, 10) 
    	Y.CANT.MENSUAL.MAZUL  = FIELD(OUT.VAL, @SM, 7)
    	Y.MONTO.MENSUAL.MAZUL = FIELD(OUT.VAL, @SM, 8)
    	    	
		GOSUB VALID.MONTHLY.LIMIT ;*OUT.MENSUAL con salida 0 si falla validacion
	    
		GOSUB VALID.DAILY.LIMIT ;*OUT.DIARIO con salida 0 si falla validacion
	END  
RETURN

VALID.MONTHLY.LIMIT:
	;*Mensual 
    Y.MONTHLY.NUM.MAZUL = R.CONT.MAZUL<EB.SLV.TMA.ACUM.MES.TRANSAC> 
    Y.MONTHLY.MON.MAZUL = R.CONT.MAZUL<EB.SLV.TMA.ACUM.MES.MONTO>
    
    Y.NEW.MONTHLY.NUM.MAZUL = Y.MONTHLY.NUM.MAZUL + 1
    Y.NEW.MONTHLY.MON.MAZUL = Y.MONTHLY.MON.MAZUL + IN.MONTO 
	
    IF Y.NEW.MONTHLY.NUM.MAZUL GT Y.CANT.MENSUAL.MAZUL OR Y.NEW.MONTHLY.MON.MAZUL GT Y.MONTO.MENSUAL.MAZUL THEN
    	OUT.MENSUAL = 0
    END 
RETURN

VALID.DAILY.LIMIT:
    ;*Diario.
	;*Se asignan los valores iniciales de forma manual. (Evitando creacion de TSA.SERVICE que sature COB diariamente para limpiar)
	IF R.CONT.MAZUL<EB.SLV.TMA.FECHA.ULT.UPDATE> EQ TODAY THEN
    	Y.DAILY.NUM.MAZUL = R.CONT.MAZUL<EB.SLV.TMA.ACUM.DIA.TRANSAC>
		Y.DAILY.MON.MAZUL = R.CONT.MAZUL<EB.SLV.TMA.ACUM.DIA.MONTO>    	    	
	END ELSE
    	Y.DAILY.NUM.MAZUL = 0
		Y.DAILY.MON.MAZUL = 0.0  
	END
	 
	Y.NEW.DAILY.NUM.MAZUL = Y.DAILY.NUM.MAZUL + 1
	Y.NEW.DAILY.MON.MAZUL = Y.DAILY.MON.MAZUL + IN.MONTO
	
	IF Y.NEW.DAILY.NUM.MAZUL GT Y.CANT.DIARIA.MAZUL OR Y.NEW.DAILY.MON.MAZUL GT Y.MONTO.DIARIO.MAZUL THEN
		OUT.DIARIO = 0
	END
RETURN

;*Archivo para revision de errores
WRITE_LOG_FILE:
	DIR.NAME = 'SIF.OUT'
	R.ID = 'LOG.VALIDACIONES.MONEDA.AZUL' : '.txt' 
 
	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
		END 
	CLOSESEQ SEQ.PTR 
RETURN

END
