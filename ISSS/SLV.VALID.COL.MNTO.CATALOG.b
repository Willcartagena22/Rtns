*-----------------------------------------------------------------------------
* <Rating>-85</Rating>
*-----------------------------------------------------------------------------
* Nombre:      SLV.VALID.COL.MNTO.CATALOG.b
* Descripcion: Rutina de Validacion de campos en Version calendario.
*              VERSION >> EB.SLV.MNTO.CATALOG.COL.GNRL,CALENDARIO
*-----------------------------------------------------------------------------
* Version		Autor			Fecha			Comentario
*-----------------------------------------------------------------------------
* 1.0			CMonterrosa		08092017		Version inicial
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.VALID.COL.MNTO.CATALOG
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.MNTO.CATALOG.COL.GNRL
    $INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------

    GOSUB INIT    
    GOSUB PROCESS
    RETURN


INIT:
*-----------------------------------------------------------------------------
    Y.VERSION   = PGM.VERSION
    Y.OPERACION = OFS$OPERATION
    
    ;*Variables para escritura de Log
    ;*-------------------------------
	Y.RUTINE               = 'SLV.VALID.COL.MNTO.CATALOG'	
	Y.LOG.NAME             = 'LOG.SLV.VALID.COL.MNTO.CATALOG'
	Y.STRING.TO.BE.WRITTEN = ''
		
RETURN
*-----------------------------------------------------------------------------	
*FIN - INIT
*-----------------------------------------------------------------------------


PROCESS:
*-----------------------------------------------------------------------------    
	;*Creacion de archivo LOG
	;*-----------------------
	;*GOSUB CREATELOG
	;*GOSUB WRITELOG
	;*-----------------------
    
    Y.STRING.TO.BE.WRITTEN = 'OFS$OPERATION = ' : OFS$OPERATION
    GOSUB WRITELOG
    
    BEGIN CASE
       CASE OFS$OPERATION EQ 'VALIDATE' OR OFS$OPERATION EQ 'PROCESS'
            GOSUB VALIDATE.FIELDS            
    END CASE
	
	;*Cerrar Archivo LOG
	;*------------------
	;*GOSUB CLOSELOG
RETURN
*-----------------------------------------------------------------------------
* FIN - PROCESS
*-----------------------------------------------------------------------------

VALIDATE.FIELDS:
;*---------------------------------------------------------------------------- 
   ;*Validacion de Campos para version Calendario AFP CRECER
   IF Y.VERSION EQ ',CALENDARIO' OR Y.VERSION EQ ',CALENDAR.CS' THEN
      Y.FIELDS       = R.NEW(EB.SLV59.CMP.2.CATALOG)
      Y.TOTAL.FIELDS = DCOUNT(Y.FIELDS,VM)
      
      FOR J=1 TO Y.TOTAL.FIELDS      
         ;*VALIDACION DE FECHA ULTIMA DE PAGO
         ;*----------------------------------
         Y.FECHA         = TRIM(FIELD(R.NEW(EB.SLV59.CMP.2.CATALOG),VM,J))
         Y.FORMATO.FECHA = 'yyyymmdd'
         CALL SLV.UTIL.VALID.FECHA.FORMAT(Y.FECHA, Y.FORMATO.FECHA, FECHA.OK)
         IF FECHA.OK EQ 0 THEN
            ERR_NAME_FIELD = EB.SLV59.CMP.2.CATALOG
            ERR_POS_VALUE  = J
            STRERR         = "Valor " : Y.FECHA : ": No Valido - YYYYMMDD"
		    GOSUB CRT_ERROR				             
         END
      
         ;*VALIDACION CAMPO PERIODO PAGO
         ;*------------------------------            
         Y.PERIODO         = TRIM(FIELD(R.NEW(EB.SLV59.CMP.4.CATALOG),VM,J))
         Y.FORMATO.PERIODO = 'ddmmyyyy'
         CALL SLV.UTIL.VALID.FECHA.FORMAT('01':Y.PERIODO, Y.FORMATO.PERIODO, PERIODO.OK)
         IF PERIODO.OK EQ 0 THEN
            ERR_NAME_FIELD = EB.SLV59.CMP.4.CATALOG
            ERR_POS_VALUE  = J
            STRERR = "Valor " : Y.PERIODO : ": No Valido - MMYYYY" 
		    GOSUB CRT_ERROR
         END
      NEXT J                  
   END         
RETURN    
*-----------------------------------------------------------------------------
* FIN - VALIDATE.FIELDS
*-----------------------------------------------------------------------------  

CRT_ERROR:
;*----------------------------------------------------------------------------
   ;*Respuesta de Errores
   ;*--------------------
   AF    = ERR_NAME_FIELD
   AV 	 = ERR_POS_VALUE
   ETEXT = STRERR
   CALL STORE.END.ERROR  
RETURN
*-----------------------------------------------------------------------------
* FIN - CRT_ERROR
*-----------------------------------------------------------------------------

CREATELOG:
*-----------------------------------------------------------------------------
    FILE.DATE = TODAY
    DIR.NAME  = 'COLECTORES'    
    FILE.NAME = Y.LOG.NAME : FILE.DATE : '.txt'
    ;*Y.RUTINE  = 'SLV.E.NOF.TXN.DIA.CAJERO.b'
    Y.STRING.TO.BE.WRITTEN   = '--------------------------------'  : ' ' : CHAR(10)
    Y.STRING.TO.BE.WRITTEN  := '<LOG_RUTINE: ' : Y.RUTINE          : '>' : CHAR(10)
    Y.STRING.TO.BE.WRITTEN  := '<FECHA     : ' : TIMEDATE()        : '>' : CHAR(10)
    Y.STRING.TO.BE.WRITTEN  := '--------------------------------'  : ' ' : CHAR(10)
   
    ;*DELETESEQ DIR.NAME, FILE.NAME 
    OPENSEQ DIR.NAME, FILE.NAME TO Y.FILE.SEQ 
    ELSE
        CREATE  Y.FILE.SEQ 
        ELSE
        	CRT 'UNABLE TO CREATE RECORD IN FILE'
    	END
    END
    WRITESEQ Y.STRING.TO.BE.WRITTEN APPEND TO Y.FILE.SEQ
    ELSE
    	CRT 'UNABLE TO WRITE TO THE FILE'
    END
 RETURN
*-----------------------------------------------------------------------------
* FIN - OPENLOG
*-----------------------------------------------------------------------------

WRITELOG:
*-----------------------------------------------------------------------------
    WRITESEQ Y.STRING.TO.BE.WRITTEN APPEND TO Y.FILE.SEQ
    ELSE
    	CRT 'UNABLE TO WRITE TO THE FILE'
    END                   
RETURN
*-----------------------------------------------------------------------------
* FIN - WRITELOG
*-----------------------------------------------------------------------------


CLOSELOG:
*-----------------------------------------------------------------------------
	WEOFSEQ Y.FILE.SEQ 
	CLOSESEQ Y.FILE.SEQ
RETURN
*-----------------------------------------------------------------------------
* FIN - CLOSELOG
*-----------------------------------------------------------------------------

END
