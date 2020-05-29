*-------------------------------------------------------------------------------------
* <Rating>-20</Rating>
*-------------------------------------------------------------------------------------
    SUBROUTINE SLV.E.CONV.FREC.MONTO
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
	$INSERT I_F.EB.SLV.PLAN.SEGURO
	
	GOSUB INIT 
	GOSUB PROCESS 
	
	RETURN

INIT:


FN.PLAN.SEGURO= "F.EB.SLV.PLAN.SEGURO"
F.PLAN.SEGURO = ""
CALL OPF(FN.PLAN.SEGURO,F.PLAN.SEGURO)

VAR.PLAN.ID =FIELD(O.DATA,'*',1)
VAR.FREC.ID =FIELD(O.DATA,'*',2)
RETURN

PROCESS:

CALL F.READ(FN.PLAN.SEGURO,VAR.PLAN.ID,R.PLAN,F.PLAN.SEGURO,ERR) 
	IF R.PLAN THEN
	
	 	BEGIN CASE
	 	 	CASE VAR.FREC.ID EQ 'AN'
	 	 		VAR.MONTO=R.PLAN<EB.SLV.PLAN.PAGO.ANUAL>
			CASE VAR.FREC.ID EQ 'SE'
				VAR.MONTO=R.PLAN<EB.SLV.PLAN.PAGO.SEMESTRAL>
			CASE VAR.FREC.ID EQ 'TR'
				VAR.MONTO=R.PLAN<EB.SLV.PLAN.PAGO.TRIMESTRAL>
			CASE VAR.FREC.ID EQ 'ME'
				VAR.MONTO=R.PLAN<EB.SLV.PLAN.PAGO.MENSUAL>
		END CASE
	
	END
 
	O.DATA = VAR.MONTO

RETURN


END
    
    
   
