*-----------------------------------------------------------------------------
* <Rating>-35</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.I.UPD.CONT.TMMA(ID.DOCUMENTO, MONTO.MAZUL)
*----------------------------------------------------------------------------- 
* Actualizar contadores de montos y numero de transacciones de moneda azul masivo.
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------- 
* 1.0		Jonas		21.06.19	Inicial
*-----------------------------------------------------------------------------
$INSERT I_COMMON  
$INSERT I_EQUATE 
$INSERT I_GTS.COMMON   
$INSERT I_F.EB.SLV.CONTADOR.MAZUL
$INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------
	GOSUB INIT
	GOSUB OPENFILE 
	GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
    FN.CONT.MAZUL = 'F.EB.SLV.CONTADOR.MAZUL'
    F.CONT.MAZUL  = ''
RETURN
*-----------------------------------------------------------------------------
OPENFILE:
*-----------------------------------------------------------------------------
    CALL OPF(FN.CONT.MAZUL, F.CONT.MAZUL)    
RETURN
*-----------------------------------------------------------------------------
PROCESS: 
*-----------------------------------------------------------------------------
	Y.ID.REF = ID.DOCUMENTO 
	Y.MON.MAZUL = MONTO.MAZUL

    ;*Obtener registro de contadores
    CALL F.READ(FN.CONT.MAZUL, Y.ID.REF, R.CONT.MAZUL, F.CONT.MAZUL, ERR.CONT)

	;*Mensual
    Y.NEW.NUM.MAZUL = R.CONT.MAZUL<EB.SLV.TMA.ACUM.MES.TRANSAC>
    Y.NEW.MON.MAZUL = R.CONT.MAZUL<EB.SLV.TMA.ACUM.MES.MONTO>
    Y.NEW.NUM.MAZUL += 1
    Y.NEW.MON.MAZUL += Y.MON.MAZUL
    
    ;*Diario.
    ;*Se asignan los valores iniciales de forma manual. (Evitando creacion de TSA.SERVICE que sature COB diariamente para limpiar)
    IF R.CONT.MAZUL<EB.SLV.TMA.FECHA.ULT.UPDATE> EQ TODAY THEN
    	Y.NEW.NUM.DIARIO.MAZUL = R.CONT.MAZUL<EB.SLV.TMA.ACUM.DIA.TRANSAC>
    	Y.NEW.MON.DIARIO.MAZUL = R.CONT.MAZUL<EB.SLV.TMA.ACUM.DIA.MONTO>    	    	
    END ELSE
    	Y.NEW.NUM.DIARIO.MAZUL = 0
    	Y.NEW.MON.DIARIO.MAZUL = 0.0  
    END
    
    ;*Diario
	Y.NEW.NUM.DIARIO.MAZUL += 1
	Y.NEW.MON.DIARIO.MAZUL += Y.MON.MAZUL 
    
    IF Y.NEW.NUM.MAZUL GT 0 AND Y.NEW.MON.MAZUL GT 0  THEN
	    ;*Mensual
	    R.CONT.MAZUL<EB.SLV.TMA.ACUM.MES.TRANSAC> = Y.NEW.NUM.MAZUL
	    R.CONT.MAZUL<EB.SLV.TMA.ACUM.MES.MONTO>  = Y.NEW.MON.MAZUL
	    
	    ;*Diario
	    R.CONT.MAZUL<EB.SLV.TMA.ACUM.DIA.TRANSAC> = Y.NEW.NUM.DIARIO.MAZUL
	    R.CONT.MAZUL<EB.SLV.TMA.ACUM.DIA.MONTO>  = Y.NEW.MON.DIARIO.MAZUL
	    
	    ;*Fecha de modificación
	    R.CONT.MAZUL<EB.SLV.TMA.FECHA.ULT.UPDATE> = TODAY
	    R.CONT.MAZUL<EB.SLV.TMA.CO.CODE> = ID.COMPANY
    	R.CONT.MAZUL<EB.SLV.TMA.INPUTTER> = OPERATOR
    	R.CONT.MAZUL<EB.SLV.TMA.AUTHORISER> = OPERATOR
    	R.CONT.MAZUL<EB.SLV.TMA.DATE.TIME> = OCONV(DATE(),'D') : ' ' : OCONV(TIME(),'MTS')
	    
	    ;*Actualizar registro de contadores
	    CALL F.WRITE(FN.CONT.MAZUL, Y.ID.REF, R.CONT.MAZUL)
	    ;*COMMIT para las actualizaciones
*		CALL JOURNAL.UPDATE(FN.CONT.MAZUL)
	END
RETURN
END