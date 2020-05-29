*-----------------------------------------------------------------------------
* <Rating>-94</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.CONTROL.REMESA(REMESA.ID, REMESA.REC)
*-----------------------------------------------------------------------------
* Name: SLV.UTIL.CONTROL.REMESA
*-----------------------------------------------------------------------------
* Description: Rutina para Consulta/Insercion de Datos a Aplicacion de Control de Remesas 
*-----------------------------------------------------------------------------
* JAR: SLV.REMESA.FAM.jar
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* ocornejo	22.04.2018	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.CONTROL.REMESAS
$INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------

*GOSUB LOGGER
GOSUB INIT
GOSUB VALIDATE.ERROR
GOSUB PROCESS
RETURN

	INIT:
		FN.CTRL = 'F.EB.SLV.CONTROL.REMESAS'
		F.CTRL  = ''
		CALL OPF(FN.CTRL, F.CTRL)
		EQU CONSULTA TO 'EB.SLV.PAGO.REMESA,SLV.CONSULTA'
	RETURN
	
	VALIDATE.ERROR:
		IF APPLICATION:PGM.VERSION NE CONSULTA THEN
			RETURN
		END

		;*Buscar Remesa por Cajero, Fecha y Estado
		SELECT.STMT = "SELECT " : FN.CTRL : " WITH RESERVADO.1 EQ " : OPERATOR 
		SELECT.STMT := " AND FECHA EQ " : TODAY : " AND ESTADO EQ 'P'"
		
*		TEXTO.ARCHIVO = 'VALIDATE.ERROR --> SELECT.STMT = ' : SELECT.STMT
*		GOSUB ESCRIBIR.ARCHIVO
		
		CALL EB.READLIST (SELECT.STMT, Y.LIST, '', NO.REC, SYSTEM.RETURN.CODE)
		
*		TEXTO.ARCHIVO = 'VALIDATE.ERROR --> Y.LIST = ' : Y.LIST
*		GOSUB ESCRIBIR.ARCHIVO
		
		;*Iterar las Referencias y marcarlas como Error
		FOR I = 1 TO NO.REC
*			TEXTO.ARCHIVO = 'VALIDATE.ERROR --> Marcando como Error ' : Y.LIST<I>
*			GOSUB ESCRIBIR.ARCHIVO
			;*Leer Registro y marcarlo como Error ya que solo puede tener una como Pendiente
			CALL F.READ(FN.CTRL, Y.LIST<I>, R.CTRL, F.CTRL, E.CTRL)
			R.CTRL<EB.SLV51.ESTADO> = 'E'
			CALL F.WRITE(FN.CTRL, Y.LIST<I>, R.CTRL)
*			TEXTO.ARCHIVO = 'VALIDATE.ERROR --> Marcado R.CTRL = ' : R.CTRL
*			GOSUB ESCRIBIR.ARCHIVO
		NEXT I
	RETURN
	
	PROCESS:
*		TEXTO.ARCHIVO = 'PROCESS --> REMESA.ID = ' : REMESA.ID
*		GOSUB ESCRIBIR.ARCHIVO
		
		;*Si no vienen ID no hace nada
		IF REMESA.ID EQ '' THEN
*			TEXTO.ARCHIVO = 'PROCESS --> Falta REMESA.ID - Buscar por Cajero y Fecha'
*			GOSUB ESCRIBIR.ARCHIVO
*			TEXTO.ARCHIVO = 'PROCESS --> Cajero = ' : OPERATOR
*			GOSUB ESCRIBIR.ARCHIVO
*			TEXTO.ARCHIVO = 'PROCESS --> Fecha = ' : TODAY
*			GOSUB ESCRIBIR.ARCHIVO
			GOSUB FIND.REMESA
*			TEXTO.ARCHIVO = 'PROCESS --> Fin del Proceso'
*			GOSUB ESCRIBIR.ARCHIVO
			RETURN
		END
		
*		TEXTO.ARCHIVO = 'PROCESS --> REMESA.REC = ' : REMESA.REC
*		GOSUB ESCRIBIR.ARCHIVO
		
		;*Identificar si es Consulta o Ingreso de Informacion a la Aplicacion
		IF REMESA.REC EQ '' THEN
*			TEXTO.ARCHIVO = 'PROCESS --> Falta REMESA.REC Procesar Busqueda por REMESA.ID = ' : REMESA.ID
*			GOSUB ESCRIBIR.ARCHIVO
			
			;*Buscar por ID
			CALL F.READ(FN.CTRL, REMESA.ID, REMESA.REC, F.CTRL, E.CTRL)
			
*			TEXTO.ARCHIVO = 'PROCESS --> Resultado = ' : REMESA.REC
*			GOSUB ESCRIBIR.ARCHIVO
*			TEXTO.ARCHIVO = 'PROCESS --> Fin del Proceso'
*			GOSUB ESCRIBIR.ARCHIVO
			RETURN
		END
		
		;*Datos de Auditoria
		REMESA.REC<EB.SLV51.RESERVADO.1> = OPERATOR
		REMESA.REC<EB.SLV51.FECHA>       = TODAY
		
		;*Si es Ingreso o Actualizacion de Informacion
*		TEXTO.ARCHIVO = 'PROCESS --> Guardar Informacion de Remesa ' : REMESA.ID
*		GOSUB ESCRIBIR.ARCHIVO
		CALL F.WRITE(FN.CTRL, REMESA.ID, REMESA.REC)
*		TEXTO.ARCHIVO = 'PROCESS REMESA.ID, REMESA.REC--> ':REMESA.ID:'-':REMESA.REC
*		GOSUB ESCRIBIR.ARCHIVO

*		TEXTO.ARCHIVO = 'PROCESS --> Guardado - Fin del Proceso'
*		GOSUB ESCRIBIR.ARCHIVO
	RETURN
	
	FIND.REMESA:
		;*Buscar Remesa por Cajero, Fecha y Estado
		SELECT.STMT = "SELECT " : FN.CTRL : " WITH RESERVADO.1 EQ " : OPERATOR 
		SELECT.STMT := " AND FECHA EQ " : TODAY : " AND ESTADO EQ 'P'"
		
*		TEXTO.ARCHIVO = 'FIND.REMESA --> SELECT.STMT = ' : SELECT.STMT
*		GOSUB ESCRIBIR.ARCHIVO
		
		CALL EB.READLIST (SELECT.STMT, Y.LIST, '', NO.REC, SYSTEM.RETURN.CODE)
		
*		TEXTO.ARCHIVO = 'FIND.REMESA --> Y.LIST = ' : Y.LIST
*		GOSUB ESCRIBIR.ARCHIVO
		
		REMESA.ID = Y.LIST<NO.REC> ;*Tomar la Ultima
		CALL F.READ(FN.CTRL, REMESA.ID, REMESA.REC, F.CTRL, E.CTRL)
		
*		TEXTO.ARCHIVO = 'FIND.REMESA --> REMESA.ID = ' : REMESA.ID
*		GOSUB ESCRIBIR.ARCHIVO
*		TEXTO.ARCHIVO = 'FIND.REMESA --> REMESA.REC = ' : REMESA.REC
*		GOSUB ESCRIBIR.ARCHIVO
	RETURN
	
	LOGGER:
		FN.GBL       = 'F.EB.SLV.GLOBAL.PARAM'
		F.GBL        = ''
		CALL OPF(FN.GBL, F.GBL)
		
		;*Extraer Parametrizacion de Log
		CALL F.READ(FN.GBL, 'LOG.REMESA', R.GBL, F.GBL, E.GBL)
		DIR.NAME   = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',1)	;*Ruta de Salida del Archivo
		R.ID       = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',3) : '.' : TODAY : '.log'	;*Nombre de Archivo Log
		LOG.ACTIVO = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',2)	;*Bandera de Log Activo/Inactivo
	RETURN
	
	ESCRIBIR.ARCHIVO:
		;*Si el parametro de Log esta Activo Escribir Archivo
		IF LOG.ACTIVO EQ 'Y' THEN
		    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
		    WRITESEQ '[' : LEFT(TIMEDATE(),8) : ']  ' : TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
		    END
		    CLOSESEQ SEQ.PTR
		END
    RETURN
END
