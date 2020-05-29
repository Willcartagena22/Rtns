*-------------------------------------------------------------------------------------
* <Rating>-30</Rating>
*-------------------------------------------------------------------------------------
    SUBROUTINE SLV.E.CONV.DEPTO.RANGO
*-------------------------------------------------------------------------------------
* RUTINA QUE COMANDA EL COMPORTAMIENTO DE LOS CAMPOS RELACIONADOS AL CLIENTE ASEGURADO
*-------------------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version.
* SFUSILLO 	11.09.2017 		Initial Code
* SFUSILLO 					Esta rutina se utiliza para la funcionalidad de SEGUROS 
*							y realiza cambios al comportamiento de sus campos
*-------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_EQUATE
	$INSERT I_F.EB.SLV.KEYS.PARAMS
	
	GOSUB INIT 
	GOSUB PROCESS 
	
	RETURN

INIT:


FN.KEYS.PARAMS = 'F.EB.SLV.KEYS.PARAMS'
F.KEYS.PARAMS  = ''
VAR.KEY.PARAM.ID='SEG.DAO.RANGO'
CALL OPF(FN.KEYS.PARAMS, F.KEYS.PARAMS)
;*O.DATA=40801
VAR.DAO =O.DATA
RETURN

PROCESS:

GOSUB CONVERT.KEY.PARAM

	O.DATA = V.RANGO.INF:"*":V.RANGO.SUP

RETURN

CONVERT.KEY.PARAM:
	CALL F.READ(FN.KEYS.PARAMS,VAR.KEY.PARAM.ID,R.KEY.PARAM,F.KEYS.PARAMS,Y.ERR)
	V.CONTAR.KEY=COUNT(R.KEY.PARAM<EB.SLV18.PARAM.ID>,VM)+1
	j=1
    LOOP WHILE j LE V.CONTAR.KEY  DO
    	VAR.VALID.RANG.INF=R.KEY.PARAM<EB.SLV18.VALOR><1,j,1>
    	VAR.VALID.RANG.SUP=R.KEY.PARAM<EB.SLV18.VALOR><1,j,2>
    	
    	IF VAR.DAO GE VAR.VALID.RANG.INF AND VAR.DAO LE VAR.VALID.RANG.SUP THEN
			V.RANGO.INF=R.KEY.PARAM<EB.SLV18.VALOR><1,j,3>
			V.RANGO.SUP=R.KEY.PARAM<EB.SLV18.VALOR><1,j,4>
			RETURN
		END
		j=j+1	
	REPEAT
RETURN


END
    
    
   
