*-----------------------------------------------------------------------------------
* <Rating>-31</Rating>
*-----------------------------------------------------------------------------------
    SUBROUTINE SLV.I.SET.MEDIO.PAGO
*-----------------------------------------------------------------------------------
* RUTINA QUE COMANDA EL COMPORTAMIENTO DE LOS CAMPOS RELACIONADOS AL MEDIO DE PAGO
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
*-----------------------------------------------------------------------------
    FN.ALTA.SEG 	= 'F.EB.SLV.ALTA.SEGURO'
    F.ALTA.SEG	= ''
	VAR.VALID.BANCO.EMISOR=''
	VAR.VALID.MEDIO.PAGO=''
	N.CUSTOMER.ID=R.NEW(EB.SEG.CUSTOMER.ID)
	VAR.ASEGURADO=R.NEW(EB.SEG.ASEGURADO)
	VAR.VALID.MEDIO.PAGO=COMI
 RETURN
*APERTURA DE ARCHIVOS A USAR
*-----------------------------------------------------------------------------
OPENFILE:
    CALL OPF(FN.ALTA.SEG,F.ALTA.SEG)
RETURN

PROCESS:
VAR.CAMPO='MEDIO.PAGO'
CALL SLV.I.SET.FIELD.CLIENT(N.CUSTOMER.ID,VAR.ASEGURADO,VAR.VALID.MEDIO.PAGO,VAR.CAMPO)
RETURN
*-----------------------------------------------------------------------------
    END
    
    
   
