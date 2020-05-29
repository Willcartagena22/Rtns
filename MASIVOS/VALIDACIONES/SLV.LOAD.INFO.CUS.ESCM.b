*-----------------------------------------------------------------------------
* <Rating>163</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.LOAD.INFO.CUS.ESCM(ENQ.DATA)
*-----------------------------------------------------------------------------
*
* Nombre: SLV.LOAD.INFO.CUS.ESCM
* Descripción: Rutina para cargar informacion del destino de envio segun el metodo seleccionado en la version 
*              EB.SLV.ACC.PROD.PLUS,ESCM.INPUT
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor			   Fecha		Comentario
*-----------------------------------------------------------------------------
* jhenriquez	09.10.2018	   Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.ACCOUNT
$INSERT I_TSS.COMMON
$INSERT I_GTS.COMMON
$INSERT I_F.CUSTOMER
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.EB.SLV.KEYS.PARAMS
$INSERT I_F.EB.SLV.ACC.PROD.PLUS
$INSERT I_F.EB.SLV.ESCM.GLOBAL.PARAMTER
*-----------------------------------------------------------------------------

GOSUB INI
GOSUB PROCESS
GOSUB OPENFILE
RETURN

INI:
  
  FN.CUSTOMER = 'F.CUSTOMER'
  F.CUSTOMER = ''
  
  FN.ACC = 'F.ACCOUNT'
  F.ACC  = '' 
  
  FN.KP = 'F.EB.SLV.KEYS.PARAMS'
  F.KP  = '' 
 
  FN.ACC.PRD.PLUS = 'F.EB.SLV.ACC.PROD.PLUS'
  F.ACC.PRD.PLUS = ''
  
  FN.ESCM.GBL.PARAM = 'F.EB.SLV.ESCM.GLOBAL.PARAMTER'
  F.ESCM.GBL.PARAM = ''
  
*** <region name= Iniciando Variables>
*** <desc> </desc>
	V.ID.APP 	= ''
  	V.METODO    = ''
  	
  	PosColonia 	= ''
	PosCalle 	= ''
	PosAvenida 	= ''
	PosColonia3	= ''
	PosCalle3 	= ''
	PosAvenida3 = ''
*** </region>

RETURN

OPENFILE:
 CALL OPF(FN.KP, F.KP)
 CALL OPF(FN.ACC, F.ACC)
 CALL OPF(FN.CUSTOMER, F.CUSTOMER)
 CALL OPF(FN.ACC.PRD.PLUS, F.ACC.PRD.PLUS)
 CALL OPF(FN.ESCM.GBL.PARAM, F.ESCM.GBL.PARAM)
RETURN

PROCESS:
*** <region name= Comportamiento de campos>
*** <desc> </desc>
    
    LOCATE "ACC.METHOD" IN D.FIELDS<1> SETTING POS THEN
    		V.ACC.METODO = D.RANGE.AND.VALUE<POS>
    END
    
	 V.ID.APP = FIELD(V.ACC.METODO,'*',1)
	 V.METODO = FIELD(V.ACC.METODO, '*',2)
	
	 ;*Debug
    	V.ID.APP 	= '10000000549507'
    	V.METODO 		= 'F.CASA'
    ;*
      
	;*Consultas generales
	CALL F.READ(FN.ACC, V.ID.APP, RECORD.ACC, F.ACC, ERR.ACC)
	V.CUSTOMER = RECORD.ACC<AC.CUSTOMER>
	CALL F.READ(FN.CUSTOMER, V.CUSTOMER, RECORD.CUS, F.CUSTOMER, ERR.CUS)
	
	;*Cargando catalogo de los campos que contiene la informacion dependiendo del metodo seleccionado
	;*CALL F.READ(FN.KP, 'SLV.FIELD.INFO.CUS.ESCM', RECORD.KP, F.KP, ERROR.KP)
	;*V.PARAM.ID = RECORD.KP<EB.SLV18.PARAM.ID>
	
	IF V.METODO EQ 'F.OFICINA' THEN
	   V.DIRECCION = ''
	   CALL GET.LOC.REF ("CUSTOMER","LF.COLONIA.2",PosColonia)
	   CALL GET.LOC.REF ("CUSTOMER","LF.CALLE.2",PosCalle)
	   CALL GET.LOC.REF ("CUSTOMER","LF.AVENIDA.2",PosAvenida)
	   
	   V.DIRECCION = ''
	   V.DIRECCION = RECORD.CUS<EB.CUS.LOCAL.REF><1,PosColonia>:" ":RECORD.CUS<EB.CUS.LOCAL.REF><1,PosCalle>:" ":RECORD.CUS<EB.CUS.LOCAL.REF><1,PosAvenida>
	END 
	ELSE IF V.METODO EQ 'F.CASA' THEN
	   CALL GET.LOC.REF ("CUSTOMER","LF.COLONIA",PosColonia)
	   CALL GET.LOC.REF ("CUSTOMER","LF.CALLE",PosCalle)
	   CALL GET.LOC.REF ("CUSTOMER","LF.AVENIDA",PosAvenida)
	   
	   CALL GET.LOC.REF ("CUSTOMER","LF.COLONIA.2",PosColonia3)
	   CALL GET.LOC.REF ("CUSTOMER","LF.CALLE.2",PosCalle3)
	   CALL GET.LOC.REF ("CUSTOMER","LF.AVENIDA.2",PosAvenida3)
	   
	   V.DIRECCION = ''
	   V.DIRECCION = RECORD.CUS<EB.CUS.LOCAL.REF><1,PosColonia>:" ":RECORD.CUS<EB.CUS.LOCAL.REF><1,PosCalle>:" ":RECORD.CUS<EB.CUS.LOCAL.REF><1,PosAvenida>    ;*:"*"
	   V.DIRECCION<-1> := RECORD.CUS<EB.CUS.LOCAL.REF><1,PosColonia3>:" ":RECORD.CUS<EB.CUS.LOCAL.REF><1,PosCalle3>:" ":RECORD.CUS<EB.CUS.LOCAL.REF><1,PosAvenida3>
	END
	ELSE IF V.METODO EQ 'EMAIL' THEN
	   V.DIRECCION = ''
	   V.DIRECCION = RECORD.CUS<EB.CUS.EMAIL.1>
	END
	
	ENQ.DATA<-1> = V.DIRECCION
*** </region>

RETURN


END
