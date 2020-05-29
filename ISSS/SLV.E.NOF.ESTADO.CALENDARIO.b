*-----------------------------------------------------------------------------
* <Rating>-37</Rating>
*-----------------------------------------------------------------------------
* Nombre:      SLV.E.NOF.ESTADO.CALENDARIO.b
* Descripcion: Rutina de prueba.
*-----------------------------------------------------------------------------
* Version		Autor			Fecha			Comentario
*-----------------------------------------------------------------------------
* 1.0			CMonterrosa		16082017		Version inicial
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.E.NOF.ESTADO.CALENDARIO(ENQ.DATA)
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.EB.SLV.KEYS.PARAMS
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS
    RETURN

INIT:
*-----------------------------------------------------------------------------
    ;*Declarar variables FN y F para archivos a utilizar
    ;*-------------------------------------------------
	FN.KEY.PARAM = 'F.EB.SLV.KEYS.PARAMS'
	F.KEY.PARAM  = ''
	
    ;*Variables para escritura de Log
    ;*-------------------------------
	Y.RUTINE               = 'SLV.E.NOF.ESTADO.CALENDARIO'		
	Y.ID.KEY.PARAM         = 'SLV.STAT.CATALOG.COL'
	
    ;*Parametros de Enquiry
	;*----------------------	
	LOCATE "ID.KEY.PARAM" IN D.FIELDS<1> SETTING POS.1 THEN
   	   Y.ID.KEY.PARAM = D.RANGE.AND.VALUE<POS.1>
	END	
RETURN
*-----------------------------------------------------------------------------	
*FIN - INIT
*-----------------------------------------------------------------------------

OPENFILE:
*-----------------------------------------------------------------------------    
    CALL OPF(FN.KEY.PARAM, F.KEY.PARAM)       
RETURN
*-----------------------------------------------------------------------------
* FIN - OPENFILE
*-----------------------------------------------------------------------------

PROCESS:
*-----------------------------------------------------------------------------    
    ;*Obtenemos los campos del registro Y. ID.KEY.PARAM
    ;*-------------------------------------------------
    CALL F.READ(FN.KEY.PARAM, Y.ID.KEY.PARAM, REG.KEY.PARAM, F.KEY.PARAM, ERR.KEY.PARAM)
    IF REG.KEY.PARAM NE '' THEN
       Y.NUM.REG = DCOUNT(REG.KEY.PARAM<EB.SLV18.PARAM.ID>, VM)
       FOR I = 1 TO Y.NUM.REG
          Y.PARAM.ID    = FIELD(REG.KEY.PARAM<EB.SLV18.PARAM.ID>, VM, I)           
	      Y.PARAM.VALOR = FIELD(REG.KEY.PARAM<EB.SLV18.VALOR>, VM, I)
	      ENQ.DATA<-1>  = Y.PARAM.ID : '*' : Y.PARAM.VALOR  
       NEXT I       	   
	END
RETURN
*-----------------------------------------------------------------------------
* FIN - PROCESS
*-----------------------------------------------------------------------------
END
