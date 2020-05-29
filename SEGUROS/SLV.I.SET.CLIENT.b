*-----------------------------------------------------------------------------------
* <Rating>-31</Rating>
*-----------------------------------------------------------------------------------
    SUBROUTINE SLV.I.SET.CLIENT
*-----------------------------------------------------------------------------------
* RUTINA QUE COMANDA EL COMPORTAMIENTO DE LOS CAMPOS RELACIONADOS AL CLIENTE PAGADOR
*-----------------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version.
* SFUSILLO 	11.09.2017 		Initial Code
* SFUSILLO 					Esta rutina se utiliza para la funcionalidad de SEGUROS 
*							y realiza cambios al comportamiento de sus campos
*-----------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_EQUATE
	$INSERT I_F.EB.SLV.ALTA.SEGURO

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

    RETURN

INIT:
;*ARCHIVOS
*------------------------------------
    FN.ALTA.SEG 	= 'F.EB.SLV.ALTA.SEGURO'
    F.ALTA.SEG	= ''
;*VARIABLES
;*-----------------------------------    
	VAR.ASEGURADO=R.NEW(EB.SEG.ASEGURADO)
	VAR.VALID.MEDIO.PAGO=R.NEW(EB.SEG.MEDIO.PAGO)
	VAR.CUSTOMER.ID=COMI
RETURN
*APERTURA DE ARCHIVOS A USAR
*-----------------------------------------------------------------------------------
OPENFILE:
    CALL OPF(FN.ALTA.SEG,F.ALTA.SEG)
RETURN
PROCESS:
VAR.CAMPO='PAGADOR'
CALL SLV.I.SET.FIELD.CLIENT(VAR.CUSTOMER.ID,VAR.ASEGURADO,VAR.VALID.MEDIO.PAGO,VAR.CAMPO)
RETURN

END
    
    
   
