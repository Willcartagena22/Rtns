*-------------------------------------------------------------------------------------
* <Rating>-55</Rating>
*-------------------------------------------------------------------------------------
    SUBROUTINE SLV.UTIL.LOAD.PAGO
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
	$INSERT I_F.EB.SLV.ALTA.SEGURO
	$INSERT I_F.EB.SLV.PLAN.SEGURO
	$INSERT I_F.CUSTOMER
	$INSERT I_F.EB.SLV.GLOBAL.PARAM
	$INSERT I_F.FUNDS.TRANSFER
	$INSERT I_F.EB.LOOKUP
	$INSERT I_F.ACCOUNT
	$INSERT I_F.CATEGORY

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB GET.REF.FIELD
    GOSUB READFILE
    GOSUB PROCESS
    

    RETURN

INIT:
;*ARCHIVOS
*-----------------------------------------------------------------------------
    FN.ALTA.SEG 	= 'F.EB.SLV.ALTA.SEGURO'
    F.ALTA.SEG	= ''
    FN.PLAN.SEGURO 	= 'F.EB.SLV.PLAN.SEGURO'
    F.PLAN.SEGURO	= ''
    Y.VERSION = APPLICATION:PGM.VERSION
    
    FN.GLOBAL.PARAM 	= 'F.EB.SLV.GLOBAL.PARAM'
    F.GLOBAL.PARAM	= ''
    
    FN.CUS 	= 'F.CUSTOMER'
    F.CUS	= ''
    FN.LOOKUP 	= 'F.EB.LOOKUP'
    F.LOOKUP	= ''
    
    FN.ACCOUNT 	= 'F.ACCOUNT'
    F.ACCOUNT	= ''
    
    FN.CAT 	= 'F.CATEGORY'
    F.CAT	= ''
   
   
   VAR.CUERPO.CORREO=''
   VAR.GLOBAL.PARAM.ID='CORREO.SEG.NOTIF.PAGO'
	
 RETURN

*APERTURA DE ARCHIVOS A USAR
*-----------------------------------------------------------------------------
OPENFILE:
    CALL OPF(FN.ALTA.SEG,F.ALTA.SEG)
    CALL OPF(FN.PLAN.SEGURO,F.PLAN.SEGURO)
    CALL OPF(FN.GLOBAL.PARAM,F.GLOBAL.PARAM)
    CALL OPF(FN.LOOKUP,F.LOOKUP)
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.CAT,F.CAT)
RETURN

GET.REF.FIELD:
CALL GET.LOC.REF('FUNDS.TRANSFER','LF.CUS.ID',LOCPOS.CUS.ID)
CALL GET.LOC.REF('FUNDS.TRANSFER','LF.CINTEX',LOCPOS.CINTEX)
VAR.CLIENTE.ID=R.NEW(FT.LOCAL.REF)<1,LOCPOS.CUS.ID>
VAR.POLIZA=R.NEW(FT.LOCAL.REF)<1,LOCPOS.CINTEX>
VAR.ACC.ID=R.NEW<FT.DEBIT.ACCT.NO>
RETURN

READFILE:
K.STMT.ARRANGEMENT 	= "SELECT " : FN.ALTA.SEG : " WITH CUSTOMER.ID EQ '" : VAR.CLIENTE.ID :"' AND NO.POLIZA EQ '": VAR.POLIZA :"'"
CALL EB.READLIST(K.STMT.ARRANGEMENT, ARRANGEMENT.LIST, R.NOCUS, NO.OF.RECS, Y.ARRANGEMENT.ERR1)
VAR.SEGURO.ID=ARRANGEMENT.LIST<1>
CALL F.READ(FN.ALTA.SEG,VAR.SEGURO.ID,R.ALTA,F.ALTA.SEG,Y.ERR)
CALL F.READ(FN.CUS,VAR.CLIENTE.ID,R.CUS,F.CUS,Y.ERR)
CALL F.READ(FN.ACCOUNT,VAR.ACC.ID,R.ACC,F.ACCOUNT,Y.ERR)
CALL F.READ(FN.GLOBAL.PARAM,VAR.GLOBAL.PARAM.ID,R.GLOBAL,F.GLOBAL.PARAM,Y.ERR)

RETURN 

PROCESS:
		;*VAR.POLIZA=21412
		;*VAR.ACC.ID=10000000038348
		;*VAR.CLIENTE.ID=100116
		
		;*Trayendo mensaje mensaje parametrizado y modificando campos para el correo
		;*-------------------------------------------------------------------------------------------------------------------------------
		VAR.SUBJECT=R.GLOBAL<EB.SLV39.DESC.PARAM>
		VAR.CUERPO.CORREO=EREPLACE(EREPLACE(EREPLACE(R.GLOBAL<EB.SLV39.VALOR.PARAM>,VM:VM,CHAR(13):CHAR(13)),',':VM,',':CHAR(13)),VM,' ')
		;*Medio de pago
		VAR.MEDIO.PAGO=R.ACC<AC.CATEGORY>
		CALL F.READ(FN.CAT,VAR.MEDIO.PAGO,R.CAT,F.CAT,Y.ERR)
		VAR.MEDIO.PAGO=R.CAT<Category_Description>
		;*Frecuencia de Pago
		VAR.LOOKUP.ID='FRECUENCIA.PAGO*':R.ALTA<EB.SEG.FECUENCIA.PAGO>;*:R.ALTA<EB.SEG.FECUENCIA.PAGO>
		CALL F.READ(FN.LOOKUP,VAR.LOOKUP.ID,R.LOOKUP,F.LOOKUP,Y.ERR)
		VAR.FREC.PAGO=R.LOOKUP<EB.LU.DESCRIPTION><1,2>
		;*Preparando reemplazo de campos
		VAR.NOMBRE.CAMPO.REPLACE='[NOMBRE.CLIENTE]':FM:'[FECHA.CARGO]':FM:'[MEDIO.PAGO]':FM:'[VALOR.PAGO]':FM:'[FRECUENCIA.PAGO]'
		VAR.VALOR.REPLACE=R.CUS<EB.CUS.SHORT.NAME>:FM:TODAY:FM:VAR.MEDIO.PAGO:FM:R.NEW<FT.DEBIT.AMOUNT>:FM:VAR.FREC.PAGO
		;*Reemplazando campos
		V.CONTAR=COUNT(VAR.NOMBRE.CAMPO.REPLACE,FM)+1
		i=1
	    LOOP WHILE i LE V.CONTAR  DO
			VAR.CUERPO.CORREO=EREPLACE(VAR.CUERPO.CORREO,VAR.NOMBRE.CAMPO.REPLACE<i>,VAR.VALOR.REPLACE<i>)
			i=i+1	
		REPEAT
		;*-------------------------------------------------------------------------------------------------------------------------------
		
		;*Enviando Correo
		;*--------------------------------------------------------------------------------------------------------------------
		THIS.METHOD.CLT	= "setConnect"
	   	VAR.CORREO.TITULAR=R.CUS<EB.CUS.EMAIL.1><1,1>
    	CALLJ.ARGUMENTS.CLT="~SEGUROS AZUL~03~VENTANILLA~MENSAJERIA~~XML~1~":VAR.CORREO.TITULAR:"~":VAR.SUBJECT:"~":VAR.CUERPO.CORREO:"~":"":"~":VAR.CORREO.TITULAR
    	;*CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    	;*--------------------------------------------------------------------------------------------------------------------
		
RETURN

*-----------------------------------------------------------------------------
    END
    
    
   
