*-----------------------------------------------------------------------------
* <Rating>-54</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.AML.SCR.CUS.PROSP(ID.ITEM)
*-----------------------------------------------------------------------------
* Program Description: Rutina main que envía screneo los registros de aplicacion  
* 					   local EB.SLV.ITEMS.MONEDA.AZUL para evaluar los beneficiarios
*					   de moneda azul (Multihilos).
*-----------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
*  1.0		Jonas		03.09.19     Initial 
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.ITEMS.MONEDA.AZUL
$INSERT I_SLV.B.AML.SCR.CUS.PROSP.COMMON
*----------------------------------------------------------------------------------------------------
    GOSUB INIT
    GOSUB PROCESS
RETURN
*----------------------------------------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------------------------------------
RETURN
*----------------------------------------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------------------------------------
    	;*Reiniciar variables
    	GOSUB REINICIAR.VAR
    	;*Obtener registro de contador
    	CALL F.READ(FN.ITEM.MAZUL, ID.ITEM, R.ITEM, F.ITEM.MAZUL, Y.ERR)
    	IF R.ITEM THEN
    		Y.P.NOMBRE  = R.ITEM<EB.IMA.PRIMER.NOMBRE>
    		Y.S.NOMBRE  = R.ITEM<EB.IMA.SEGUNDO.NOMBRE>
    		Y.P.APELLIDO= R.ITEM<EB.IMA.PRIMER.APELLIDO>
    		Y.S.APELLIDO= R.ITEM<EB.IMA.SEGUNDO.APELLIDO>
    		Y.NOMBRE.NIT= R.ITEM<EB.IMA.NOMBRE.COMPLETO>
    		Y.TIPO.DOC  = R.ITEM<EB.IMA.TIPO.DOCUMENTO>
    		Y.NUMERO.DOC= R.ITEM<EB.IMA.DOCUMENTO>
    		Y.NUMERO.TEL= R.ITEM<EB.IMA.MOVIL>
    		Y.FECHA.NAC = FMT(R.ITEM<EB.IMA.FECHA.NACIMIENTO>, "R(#4-#2-#2)")
    		Y.AGENCIA   = ID.COMPANY
    		Y.USUARIO   = OPERATOR
    		R.ITEM<EB.IMA.CURR.NO>   += 1
    		GOSUB GENERA.XML
    		GOSUB ENVIO.AML.SCREEN
			R.ITEM<EB.IMA.ESTADO> = Y.ESTADO.AUTORIZ
    		IF Y.IS.ERROR NE Y.RESPONSE.ERROR THEN
	    		R.ITEM<EB.IMA.SCRENEO.AML> = 1
				R.ITEM<EB.IMA.ESTADO.AML> = Y.RESPONSE
				IF Y.RESPONSE EQ Y.RESPONSE.HIT THEN
					R.ITEM<EB.IMA.ESTADO>     = Y.ESTADO.RESTRIN
				END
				R.ITEM<EB.IMA.ID.CUS.PROSPECT> = Y.ID.CUS
			END ELSE
				IF Y.RESPONSE EQ Y.IS.CUSTOMER THEN
					R.ITEM<EB.IMA.SCRENEO.AML> = 1
					R.ITEM<EB.IMA.ESTADO.AML> = Y.RESPONSE
				END ELSE
					R.ITEM<EB.IMA.SCRENEO.AML> = 0
				END				
			END
			CALL F.WRITE (FN.ITEM.MAZUL,ID.ITEM,R.ITEM)
		END
RETURN
*-----------------------------------------------------------------------------
GENERA.XML:
*-----------------------------------------------------------------------------
	Data = "PRIMERNOMBRE@" : Y.P.NOMBRE
	AmlScreening<-1> = Data

	Data = "SEGUNDONOMBRE@" : Y.S.NOMBRE
	AmlScreening<-1> = Data
		
	Data = "TERCERNOMBRE@" : Y.T.NOMBRE
	AmlScreening<-1> = Data

	Data = "PRIMERAPELLIDO@" : Y.P.APELLIDO
	AmlScreening<-1> = Data

	Data = "SEGUNDOAPELLIDO@" : Y.S.APELLIDO
	AmlScreening<-1> = Data
		
	Data = "APELLIDOCASADA@" : Y.APELLIDO.CASADA
	AmlScreening<-1> = Data
	
	Data = "NOMBRENIT@" : Y.NOMBRE.NIT
	AmlScreening<-1> = Data
		
	Data = "CODSUCURSAL@" : Y.AGENCIA
	AmlScreening<-1> = Data

	Data = "USUARIO@" : Y.USUARIO 
	AmlScreening<-1> = Data
	
	Data = "FECHANACIMIENTO@" : Y.FECHA.NAC
	AmlScreening<-1> = Data
		
	Data = "DOCUMENTOS@NOMBREDOC:DOCTO.UNICO.IDENT;NUMERODOC:" : Y.NUMERO.DOC
	AmlScreening<-1> = Data
		
	xml = DYNTOXML(AmlScreening,"",result)
RETURN
*-----------------------------------------------------------------------------
ENVIO.AML.SCREEN:
*-----------------------------------------------------------------------------
	THIS.PACKAGE.CINTEX.CLASS = "com.bancoazul.services.Services"
	THIS.METHOD.CINTEX = "AMLScreening"
	THIS.CALLJ.ARGUMENTS = xml
	CALLJ.RESPONSE.CLT = ""
	CALLJ.ERROR.CLT = ""

	CALL EB.CALLJ(THIS.PACKAGE.CINTEX.CLASS, THIS.METHOD.CINTEX, THIS.CALLJ.ARGUMENTS, CALLJ.RESPONSE.CLT, CALLJ.ERROR.CLT)

	;*Verificar si el screneo devuelve error
    Y.IS.ERROR = FIELD(CALLJ.RESPONSE.CLT,"?",1)
	;*Obtener respuesta de evaluacion AML (HIT/NO HIT)
	Y.RESPONSE = FIELD(CALLJ.RESPONSE.CLT,"?",2)
	;*Obtener ID CUSTOMER generado en VERSION de evaluacion AML (HIT/NO HIT) 
	Y.ID.CUS = FIELD(CALLJ.RESPONSE.CLT,"?",1)
RETURN
*-----------------------------------------------------------------------------
REINICIAR.VAR:
*-----------------------------------------------------------------------------
	Y.P.NOMBRE = ""
	Y.S.NOMBRE = ""
	Y.T.NOMBRE = ""
	Y.P.APELLIDO = ""
	Y.S.APELLIDO = ""
	Y.APELLIDO.CASADA = ""
	Y.NOMBRE.NIT = ""
	Y.AGENCIA = ""
    Y.USUARIO= ""
	Y.FECHA.NAC= ""
	Y.IS.ERROR = ""
	Y.RESPONSE = ""
	Y.ID.CUS   = ""
RETURN
END
