*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE PRUEBAS.SEGUROS
*-------------------------------------------------------------------------------------

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
	$INSERT I_F.EB.SLV.ALTA.SEGURO
	
	GOSUB INIT 
	GOSUB PROCESS 
	
	RETURN

INIT:

FN.PLAN.SEGURO= "F.EB.SLV.PLAN.SEGURO"
F.PLAN.SEGURO = ""
CALL OPF(FN.PLAN.SEGURO,F.PLAN.SEGURO)

FN.ALTA.SEGURO= "F.EB.SLV.ALTA.SEGURO"
F.ALTA.SEGURO = ""
CALL OPF(FN.ALTA.SEGURO,F.ALTA.SEGURO)

VAR.COMP.ID =O.DATA
VAR.COMP.ID='aaaa-1'
VAR.ESTADO='ERR'
VAR1=FIELD(VAR.COMP.ID,'-',1)
VAR2=FIELD(VAR.COMP.ID,'-',2)
RETURN

PROCESS:

CRT 'VAR1  : ':VAR1
CRT 'VAR2  : ':VAR2

	K.STMT.ARRANGEMENT 	= "SELECT " : FN.ALTA.SEGURO : " WITH CO.CODE EQ '" : VAR.COMP.ID :"' AND ESTADO NE '" : VAR.ESTADO :"' "
	CALL EB.READLIST(K.STMT.ARRANGEMENT, ARRANGEMENT.LIST, R.NOCUS, NO.OF.RECS, Y.ARRANGEMENT.ERR1)
	
	O.DATA = NO.OF.RECS

RETURN


END
    
    
   
