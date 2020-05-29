*-----------------------------------------------------------------------------
* <Rating>582</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.S.CUST.UIF.OTROS.MEDIOS(CUSTOMER.ID, TIPO.PERSONA, PERSONA.NAT, PERSONA.JUR)
*------------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*------------------------------------------------------------------------------------------------------
* 2.0		Jonás 		15.11.16	Agregar tipo de identificacion OTROs para no definidos.  
* 2.1		Jonas		09.12.16	Agrear EB.EB.CUS.OCUPACION.SSF para obtener ocupacion de persona.
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON 
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.EB.CUS.OCUPACION.SSF 
$INSERT I_F.COUNTRY  
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB GET_LOC_REF
GOSUB PROCESS
GOSUB SET
RETURN

	INIT:		
		FN.CUS  = 'F.CUSTOMER'
		F.CUS   = ''
		FN.PAIS = 'F.COUNTRY'
		F.PAIS  = ''
		FN.OCUP = 'F.EB.CUS.OCUPACION.SSF'
		F.OCUP  = ''
	RETURN
	
	OPENFILE:
		CALL OPF(FN.CUS, F.CUS)
		CALL OPF(FN.PAIS, F.PAIS)
		CALL OPF(FN.OCUP, F.OCUP)
	RETURN
	
	GET_LOC_REF:
		CALL GET.LOC.REF('CUSTOMER', 'LF.TYPE.CUST', 	P.LF.TYPE.CUST)
		CALL GET.LOC.REF('CUSTOMER', 'LF.LUG.NAC'  , 	P.LF.LUG.NAC)
		CALL GET.LOC.REF('CUSTOMER', 'LF.OCUPACION', 	P.LF.OCUPACION)
		CALL GET.LOC.REF("CUSTOMER", "LF.CANTON",    	P.LF.CANTON)
		CALL GET.LOC.REF('CUSTOMER', "LF.NUM.DEPTO", 	P.LF.NUM.DEPTO)
		CALL GET.LOC.REF("CUSTOMER", "LF.COLONIA",   	P.LF.COLONIA.JUR)
		CALL GET.LOC.REF('CUSTOMER', "LF.CALLE",     	P.LF.CALLE.JUR)
		CALL GET.LOC.REF('CUSTOMER', "LF.AVENIDA",   	P.LF.AVENIDA.JUR)
		CALL GET.LOC.REF("COUNTRY",  "LF.UIF.COD.PAIS",	P.LF.UIF.COD.PAIS)
		CALL GET.LOC.REF("CUSTOMER", "LF.RAZON.SOCIAL", P.LF.RAZON.SOCIAL)
		CALL GET.LOC.REF("CUSTOMER", "LF.COLONIA.3",   	P.LF.COLONIA)
		CALL GET.LOC.REF('CUSTOMER', "LF.CALLE.3",     	P.LF.CALLE)
		CALL GET.LOC.REF('CUSTOMER', "LF.AVENIDA.3",   	P.LF.AVENIDA)
		CALL GET.LOC.REF("CUSTOMER", "LF.DEPTO.3", 		P.LF.DEPARTAMENTO)
		CALL GET.LOC.REF("CUSTOMER", "LF.MUNICIPIO.3",  P.LF.MUNICIPIO)
	RETURN
	
	PROCESS:
		;*Leer Cliente
		;*------------
		CALL F.READ(FN.CUS, CUSTOMER.ID, R.CUS, F.CUS, ERR.CUS)
		
		;*Definir Tipo Persona Segun Metodo de Reporte UIF
		;*------------------------------------------------
		TIPO.PERSONA = 1
		X= R.CUS<EB.CUS.LOCAL.REF><1, P.LF.TYPE.CUST>[1,3]
		IF R.CUS<EB.CUS.LOCAL.REF><1, P.LF.TYPE.CUST>[1,3] EQ 'JUR' THEN
			TIPO.PERSONA = 2
		END
		
		DocumentType 	= R.CUS<EB.CUS.LEGAL.DOC.NAME><1,1>					;*Tipo documento


		BEGIN CASE
			CASE DocumentType EQ  'DOCTO.UNICO.IDENT'
				DocumentType = 1
			CASE DocumentType EQ 'NUM.IDEN.TRIBUT'
				DocumentType = 2
			CASE DocumentType EQ 'REGISTRO.FISCAL.IVA'
				DocumentType = 3
			CASE DocumentType EQ 'PASSPORT'
				DocumentType = 4
			CASE DocumentType EQ 'CARNET.RESIDENTE'
				DocumentType = 5
			CASE DocumentType EQ 'DRIVING.LICENSE'
				DocumentType = 6
			CASE DocumentType EQ 'CARNET.MENOR.EDAD'
				DocumentType = 11
		END CASE

		;*Para agregar otro tipo de identificacion hacerlo antes de esta condicion
		;*Para los casos donde no esta definido el tipo de identificacion
		IF ISALPHA(DocumentType) THEN
			DocumentType = 10
		END					
		
		NoDocument 			= R.CUS<EB.CUS.LEGAL.ID><1,1>						;*Numero documento
		
		;*Definir Persona Natural o Juridica
		;*----------------------------------
		IF TIPO.PERSONA = 1 THEN
			FirstLastName 	= R.CUS<EB.CUS.TEXT>								;*Primer apellido
			SecondLastName 	= R.CUS<EB.CUS.FAMILY.NAME> 						;*Segundo apellido
			MaritalLastName = R.CUS<EB.CUS.PREVIOUS.NAME>  						;*Apellido de Casada
			FirstName 		= R.CUS<EB.CUS.NAME.1> 								;*Primer nombre
			SecondName 		= R.CUS<EB.CUS.NAME.2> 								;*Segundo nombre 
			DateBirth 		= R.CUS<EB.CUS.DATE.OF.BIRTH>[0,4]  : '-'			;*Fecha nacimiento
			DateBirth 		:= R.CUS<EB.CUS.DATE.OF.BIRTH>[5,2] : '-'			
			DateBirth 		:= R.CUS<EB.CUS.DATE.OF.BIRTH>[7,2]
			DateBirth 		:= 'T00:00:00'	
			PlaceBirth 		= R.CUS<EB.CUS.LOCAL.REF><1, P.LF.LUG.NAC>  		;*Lugar de nacimiento
			
			CALL F.READ (FN.PAIS, R.CUS<EB.CUS.NATIONALITY>, R.PAIS, F.PAIS, ERR.PAIS)
			Nationality		= R.PAIS<EB.COU.LOCAL.REF><1, P.LF.UIF.COD.PAIS>	;*Nacionalidad
			
			MaritalStatus	= R.CUS<EB.CUS.MARITAL.STATUS>	 					;*Estado civil	
			IF MaritalStatus EQ 'SINGLE' THEN
				MaritalStatus = 1
			END
			ELSE IF MaritalStatus EQ 'MARRIED' THEN
				MaritalStatus = 2
			END
			ELSE IF MaritalStatus EQ 'ACOMPA' THEN
				MaritalStatus = 3
			END
			ELSE IF MaritalStatus EQ 'DIVORCED' THEN
				MaritalStatus = 4
			END
			ELSE IF MaritalStatus EQ 'WIDOWED' THEN
				MaritalStatus = 5
			END
			ELSE
				MaritalStatus = 1 ;*En caso No tenga Estado Civil dejar Soltero por Default
			END
				
			Address 		:= R.CUS<EB.CUS.LOCAL.REF><1, P.LF.COLONIA>	 		;*Colonia
			Address			:= ' ' : R.CUS<EB.CUS.LOCAL.REF><1, P.LF.CALLE>		;*Calle
			Address 		:= ' ' : R.CUS<EB.CUS.LOCAL.REF><1, P.LF.AVENIDA>	;*Avenida
			Address 		:= ' ' : R.CUS<EB.CUS.LOCAL.REF><1, P.LF.NUM.DEPTO> ;*Numero casa
			Address			:= ' ' : R.CUS<EB.CUS.LOCAL.REF><1, P.LF.CANTON>	;*Canton
			Address     	= TRIM(SWAP(Address, '  ', ' '))						
			Municipio 		= R.CUS<EB.CUS.LOCAL.REF><1, P.LF.MUNICIPIO>[3,LEN(R.CUS<EB.CUS.LOCAL.REF><1, P.LF.MUNICIPIO>)]			;*Municipio
			Department 		= R.CUS<EB.CUS.LOCAL.REF><1, P.LF.DEPARTAMENTO>[3,LEN(R.CUS<EB.CUS.LOCAL.REF><1, P.LF.DEPARTAMENTO>)]	;*Departamento
			Occupation 		= R.CUS<EB.CUS.LOCAL.REF><1, P.LF.OCUPACION>		;*Profesion persona
			
			;*Obtener codigo de profesion UIF
			CALL F.READ (FN.OCUP, Occupation, R.OCUP, F.OCUP, ERR.OCUP)
			Occupation = R.OCUP<EB.EB.17.DESCRIPTION>
		END
		ELSE
			razonSocial			  = R.CUS<EB.CUS.LOCAL.REF><1, P.LF.RAZON.SOCIAL> 			;*Razon social			
			domicilioComercial 	  = R.CUS<EB.CUS.LOCAL.REF><1, P.LF.COLONIA.JUR>	 		;*Colonia
			domicilioComercial	  := ' ' : R.CUS<EB.CUS.LOCAL.REF><1, P.LF.CALLE.JUR>		;*Calle
			domicilioComercial 	  := ' ' : R.CUS<EB.CUS.LOCAL.REF><1, P.LF.AVENIDA.JUR>		;*Avenida
			domicilioComercial    = TRIM(SWAP(domicilioComercial, '  ', ' '))			
			actividadEconomica    = R.CUS<EB.CUS.INDUSTRY>									;*Actividad economica
			tipoIdentificacionT   = DocumentType
			numeroIdentificacionT = NoDocument
		END
	RETURN
	
	SET:
		;*Persona Natural
		;*---------------
		IF TIPO.PERSONA = 1 THEN
			PERSONA.JUR = '' : @VM : '' : @VM : '' : @VM : '' : @VM : ''
			PERSONA.NAT = ''
			PERSONA.NAT := FirstLastName 	: @VM
			PERSONA.NAT := SecondLastName 	: @VM
			PERSONA.NAT := MaritalLastName 	: @VM
			PERSONA.NAT := FirstName 		: @VM
			PERSONA.NAT := SecondName 		: @VM
			PERSONA.NAT := DateBirth 		: @VM
			PERSONA.NAT := PlaceBirth 		: @VM
			PERSONA.NAT := Nationality 		: @VM
			PERSONA.NAT := MaritalStatus 	: @VM
			PERSONA.NAT := DocumentType 	: @VM
			PERSONA.NAT := NoDocument 		: @VM
			PERSONA.NAT := Address 			: @VM
			PERSONA.NAT := Municipio 		: @VM
			PERSONA.NAT := Department 		: @VM
			PERSONA.NAT := Occupation 		
		END
		;*Persona Juridica
		;*----------------
		ELSE
			PERSONA.NAT = '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' : @VM : '' 
			PERSONA.JUR = ''
			PERSONA.JUR := razonSocial			 : @VM
			PERSONA.JUR := domicilioComercial	 : @VM
			PERSONA.JUR := actividadEconomica	 : @VM
			PERSONA.JUR := tipoIdentificacionT	 : @VM
			PERSONA.JUR := numeroIdentificacionT
		END
	RETURN
END
