SUBROUTINE SLV.UTIL.IMP.P.GET.I.CORR(ARR.ID)

*-----------------------------------------------------------------------------
* <Rating>-15</Rating>
*-----------------------------------------------------------------------------
*@EURIAS OBTENER EL MONTO TOTAL DE INTERES CORRIENTE SE PAGO
*@RECIBE ID DE ARRANGEMENT ID
*-----------------------------------------------------------------------------
* Modification History :.
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AA.ACTIVITY.BALANCES	
$INSERT I_F.EB.SLV.GLOBAL.PARAM
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY
EQU TRUE TO 'TRUE'
EQU FALSE TO 'FALSE'

;*vars
totalCapital = 0
siguienteActividad = TRUE
actividadPresedente = ''
*-----------------------------------------------------------------------------
;*obtener parametrizacion de properties
*-----------------------------------------------------------------------------
	idParametros = "COMP.P.PROP.INT.CORR"
  
    aplicacionDataParam=''
    ErrorParam=''
  	nombreApliacionParam ="F.EB.SLV.GLOBAL.PARAM"
	aplicacionFullParam = ''
    CALL OPF(nombreApliacionParam,aplicacionFullParam)
    CALL F.READ(nombreApliacionParam,idParametros,aplicacionDataParam,aplicacionFullParam,ErrorParam)
    valores = aplicacionDataParam<EB.SLV39.VALOR.PARAM>
    countProperties  = DCOUNT(valores,'|') 
*-----------------------------------------------------------------------------
;* para los valores text de t24 los separa por enter que significan un multivalue,unir los multivalues para q el separador sea el pipe o el q se defina en el parametro    
    	contMulti = DCOUNT (valores, @VM)
        auxVar = ''
   		FOR K = 1 TO contMulti
   		auxVar:=valores<1,K>
   		NEXT K
   		valores = auxVar 
*-----------------------------------------------------------------------------
   	;*crear arreglo de parametros
   	propertiesKeys = "";*obtener las properties que corresponden al pago de capital
   	FOR H = 1 TO countProperties
    	propertiesKeys := FIELDS(valores,'|',H):@VM 
    NEXT H
*-----------------------------------------------------------------------------
;*buscar el arrangement por el numero de cuenta
	CALL SLV.UTIL.GET.ARR.X.ACC(ARR.ID)

*-----------------------------------------------------------------------------
;*obtener fecha de inicio de acuerdo
	idArr = ARR.ID
    nombreApliacion ="F.AA.ACTIVITY.BALANCES"
	aplicacionFull = ''
    aplicacionDataAA=''
    Error=''
    CALL OPF(nombreApliacion, aplicacionFull)
    CALL F.READ(nombreApliacion,idArr,aplicacionDataAA,aplicacionFull,Error)
    ACTIVIDADES = aplicacionDataAA<AA.ACT.BAL.ACTIVITY.REF>
    NUM.ACTIVIDADES = DCOUNT(ACTIVIDADES,@VM) ;*LA ULTIMA ACTIVAD ES EL ULTIMO PAGO REALIZADO
    ;*-----------------------------------------------------------------------------
;*recorrer las actividades asociadas	
numActividad= NUM.ACTIVIDADES
I = 1
	LOOP WHILE siguienteActividad EQ TRUE DO ;*si tiene actividad asociada continuar
;*-----------------------------------------------------------------------------
IF(numActividad EQ 0)THEN
	BREAK
END
;*evaluar si la actividad es hija, si es hija seguir iterando sino terminar la iteracion
	actividadId = ACTIVIDADES<1,numActividad>
	IF(I=1)THEN
			actividadPresedente = actividadId
		END	
	IF(actividadPresedente NE actividadId)THEN ;* si la actividad presendente (actividad padre en iteracion anterior) no es igual a la actual entonces break
			BREAK
	END	
    nombreApliacionActividad ="F.AA.ARRANGEMENT.ACTIVITY"
	aplicacionFullActividad = ''
    aplicacionDataActividad=''
    ErrorActividad=''
    CALL OPF(nombreApliacion, aplicacionFull)
    CALL F.READ(nombreApliacionActividad,actividadId,aplicacionDataActividad,aplicacionFullActividad,ErrorActividad)
	actividadPadre = aplicacionDataActividad<AA.ARR.ACT.MASTER.AAA>
	IF(actividadPadre EQ actividadId)THEN ;*si la actividad actual es la misma que la padre solo hay 1 actividad en la accion de pago (detener iteracion), sino continuar iterando
		siguienteActividad = FALSE
	END
	actividadPresedente = actividadPadre
*-----------------------------------------------------------------------------
;*evaluar si la actividad padre es monto 0 terminar la iteracion
	IF(siguienteActividad EQ TRUE)THEN
		actividadIdVal=actividadPadre
		nombreApliacionActividadVal ="F.AA.ARRANGEMENT.ACTIVITY"
		aplicacionFullActividadVal = ''
	    aplicacionDataActividadVal=''
	    ErrorActividadVal=''
	    plicacionFullVal=''
	    CALL OPF(nombreApliacionActividadVal, aplicacionFullVal)
	    CALL F.READ(nombreApliacionActividadVal,actividadIdVal,aplicacionDataActividadVal,aplicacionFullActividadVal,ErrorActividadVal)
	    IF(aplicacionDataActividadVal<AA.ARR.ACT.TXN.AMOUNT>='0')THEN
	    	CONTINUE
	    END
    END
*-----------------------------------------------------------------------------	
*-----------------------------------------------------------------------------
;*obtener los properties asociados y sus montos
*-----------------------------------------------------------------------------
	propertiesAsociados = aplicacionDataAA<AA.ACT.BAL.PROPERTY,numActividad>
	montosProperties  = aplicacionDataAA<AA.ACT.BAL.PROPERTY.AMT,numActividad>
	NUM.PROPERTIES = DCOUNT(propertiesAsociados,@SM)

*-----------------------------------------------------------------------------
;*obtener monto de capital
*-----------------------------------------------------------------------------
;*COMP.P.PROP.CAPITAL
;*COMP.P.PROP.SEGUROS
;*COMP.P.PROP.INT.CORR
;*COMP.P.PROP.INT.MORA
   	totalCapital=0
   	FOR I = 1 TO NUM.PROPERTIES	
   	IF (propertiesAsociados<1,1,I>)THEN
   		FIND propertiesAsociados<1,1,I> IN propertiesKeys SETTING Ap, Vp THEN
   			 totalCapital += montosProperties<1,1,I>
    	END ELSE
    		 CRT "No fue econtrado"
		END 
   	END
   	NEXT I
    ARR.ID += totalCapital
    numActividad = numActividad - 1;*actividad anterior
    I = I+1
    REPEAT
*-----------------------------------------------------------------------------
END