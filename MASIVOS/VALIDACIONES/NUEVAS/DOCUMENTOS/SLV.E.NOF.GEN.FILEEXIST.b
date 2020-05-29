*-----------------------------------------------------------------------------
* <Rating>-38</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.GEN.FILEEXIST(PERFIL.ARCHIVO)
*-----------------------------------------------------------------------------
*
*----------------------------------------------------------------------------- 
* Modification History :
*-----------------------------------------------------------------------------
* Nombre: SLV.E.NOF.DPF.CERTIFICADO
* Descripción: Rutina NOFILE para enquiry de emisión de certificado DPF
*-----------------------------------------------------------------------------
* Version	Paquete								Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		20150220-AZUL001-CORR244-RELEASE1	RGaray		20.02.15	Verificar la existencia de un archivo o notificar fallo.
* 1.1											RGaray		24.02.15	Ahora se obtiene el tiempo limite de verificacion desde la aplicacion EB.SLV.GLOBAL.PARAM y el parametro MINUTOS.VALIDAR.PDF
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------
	
	GOSUB INIT
	GOSUB OPENFILE  
	GOSUB EXISTENCE
	
	RETURN 
	
*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
;******************************************************************    
;*	DEBUG
*   DIR.NAME = 'c:\temp\'
*   FILE.NAME = 'CON-10000000073267--1424120496364.txt'
*	FILE.NAME = 'ELARCHIVO'
*	FILE.PATH = DIR.NAME : FILE.NAME : @FM : 'ENQUIRYNAME'
;******************************************************************
* Identificar directorio de ubicacion y nombre del archivo
	
	FN.TABLE.PA = 'F.EB.SLV.GLOBAL.PARAM'
    F.TABLE.PA = '' 
	
	REF.TIEMPO.1 = TIME() 
	REF.TIEMPO.2 = TIME()
	FILE.PATH = FIELD(PERFIL.ARCHIVO, @FM, 1) ;*asignar path para ubicar archivo 
	GIVE.UP.VAR = 0 
CRT 'FILE.PATH -> ' : FILE.PATH 
CRT 'PERFIL.ARCHIVO -> ' : PERFIL.ARCHIVO
	RETURN 
	
*-----------------------------------------------------------------------------
OPENFILE:
*-----------------------------------------------------------------------------
;* Apertura de archivos a utilizar
	CALL OPF(FN.TABLE.PA, F.TABLE.PA)

    RETURN
	
*-----------------------------------------------------------------------------
EXISTENCE:
*-----------------------------------------------------------------------------
;* Limite de tiempo en minutos para verificacion
	MINUTOS.VALIDAR.ID = 'MINUTOS.VALIDAR.PDF'
	CALL F.READ(FN.TABLE.PA, MINUTOS.VALIDAR.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    MINUTOS.VALIDAR.PDF = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
CRT 'MINUTOS.VALIDAR.PDF -> ' : MINUTOS.VALIDAR.PDF
* Verificar existencia del archivo
    LOOP
    WHILE CANT.INTENTO LT MINUTOS.VALIDAR.PDF 
	    CRT 'CANT.INTENTO -> ' : CANT.INTENTO
	    
	    OPENPATH FILE.PATH TO PATH.FILE THEN
	    	GIVE.UP.VAR = 1
	    	BREAK 
	    END 
	    ELSE
	        CRT 'No se encontro el archivo.'
	        REF.TIEMPO.2 = TIME()
*	        ABORT
	    END
	  		    
		MULTI.DIFF = TIMEDIFF(REF.TIEMPO.1, REF.TIEMPO.2, 0)
		CRT 'MULTI.DIFF -> ' : MULTI.DIFF
		CRT 'FIELD(MULTI.DIFF, @FM, 3) -> ' : FIELD(MULTI.DIFF, @FM, 3)
		CANT.INTENTO = FIELD(MULTI.DIFF, @FM, 3)
		SLEEP 5
	REPEAT 
    
    CLOSESEQ PATH.FILE 
    IF GIVE.UP.VAR EQ 1 THEN 
    	CRT 'Se encontro el archivo.'
    	PERFIL.ARCHIVO = 1
	END ELSE  
		CRT 'Se acabo el tiempo.'
		PERFIL.ARCHIVO = 0
	END 
	
	RETURN 

	
END