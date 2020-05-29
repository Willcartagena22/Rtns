SUBROUTINE SLV.UTIL.IMP.P.GET.OTROS.C(ARR.ID)
*-----------------------------------------------------------------------------
* <Rating>-9</Rating>
*-----------------------------------------------------------------------------
*@EURIAS OBTENER EL MONTO TOTAL DE OTROS CARGOS PARA ABONO A PRESTAMO
*@RECIBE ID DE ARRANGEMENT ID
*-----------------------------------------------------------------------------
* Modification History :.
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AA.ACTIVITY.BALANCES	
$INSERT I_F.EB.SLV.GLOBAL.PARAM

;*vars
totalCapital = 0
*-----------------------------------------------------------------------------
;*obtener parametrizacion de properties
*-----------------------------------------------------------------------------
	idParametros = "COMP.P.PROP.OTROS.CARGOS"
    nombreApliacionParam ="F.EB.SLV.GLOBAL.PARAM"
	aplicacionFullParam = ''
    aplicacionDataParam=''
    ErrorParam=''
    CALL OPF(nombreApliacionParam, aplicacionFullParam)
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
*-----------------------------------------------------------------------------
;*obtener los properties asociados y sus montos
*-----------------------------------------------------------------------------
	propertiesAsociados = aplicacionDataAA<AA.ACT.BAL.PROPERTY,NUM.ACTIVIDADES>
	montosProperties  = aplicacionDataAA<AA.ACT.BAL.PROPERTY.AMT,NUM.ACTIVIDADES>
	NUM.PROPERTIES = DCOUNT(propertiesAsociados,@SM)

*-----------------------------------------------------------------------------
;*obtener monto de seguros
*-----------------------------------------------------------------------------
;*COMP.P.PROP.CAPITAL
;*COMP.P.PROP.SEGUROS
;*COMP.P.PROP.INT.CORR
;*COMP.P.PROP.INT.MORA
   	
   	FOR I = 1 TO NUM.PROPERTIES	
   	IF (propertiesAsociados<1,1,I>)THEN
   		FIND propertiesAsociados<1,1,I> IN propertiesKeys SETTING Ap, Vp THEN
   			 totalCapital += montosProperties<1,1,I>
   			 CRT "ENCONTRADO:": totalCapital
    	END ELSE
    		 CRT "No fue econtrado"
		END 
   	END
   	NEXT I
    ARR.ID = totalCapital
*-----------------------------------------------------------------------------
END