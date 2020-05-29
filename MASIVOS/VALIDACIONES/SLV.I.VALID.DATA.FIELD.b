*-----------------------------------------------------------------------------

* <Rating>1329</Rating>
*-----------------------------------------------------------------------------

    SUBROUTINE SLV.I.VALID.DATA.FIELD
*-----------------------------------------------------------------------------

* Validaciones sobre campo en pantalla de clientes
*-----------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version.
* GAlfaro  	17.10.2014 		Initial Code
* Galfaro	06.12.2014		Se agrega soporte para Ñ y ñ(662) , también override fecha
*							de nacimiento vs nit .
*							-validación de fecha de emision para dui (660)
*							-validacion para numero secuenciales de dui ejem: 222222222
*Galfaro	12.12.2014		-identifiación de campo descriptivo en el launch de error
*Galfaro	12.12.2014		-se agrega validación para no permitir mas de 8 caracteres en campo teléfono
*FBatres	16.02.2015		-se agrega validación para no evaluar los campos NAME.1 y LF.NOB.NIT cuando sea distinto de persona natural
*SFusillo	09.02.2017		-Modificacion por proyecto Declaracion Jurada FII. Correccion en campos multivalor del detalle de Finanzas. 
*							 Esto con el objetivo que pueda recorrer correctamente los valores y no interfiera con otras validaciones.
*rortiz     17.07.2018      -Se elimina validacion del campo LF.PRO.FON.F de la versión CUSTOMER,SLV.INPUT
*eescobar 	21.09.2018		-Validaciones para telefono movil y verificacion de SMSToken
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
   	$INSERT I_TSS.COMMON
	$INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------




    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS

    RETURN

INIT:
;*Variables de trabajo
*    V_NAME_FIELD		=''
    POS_VALUE			=''
    STRERR				=''

;*Archivos necesarios
    FN.CUS				='F.CUSTOMER'
    F.CUS				=''


;*Posiciones de campos locales
    CALL GET.LOC.REF('CUSTOMER','LF.NIT',POSnit)
    CALL GET.LOC.REF('CUSTOMER','LF.NOB.NIT',POSnomnit)
    CALL GET.LOC.REF('CUSTOMER','LF.NIT.ANTER',POSnitanter)
    CALL GET.LOC.REF('CUSTOMER','LF.LUG.NAC',POSlucnac)
    CALL GET.LOC.REF('CUSTOMER','LF.NOM.FUNC.PUB',POSfunpub)
    CALL GET.LOC.REF('CUSTOMER','LF.NUM.DEPO',POSnumdep)
    CALL GET.LOC.REF('CUSTOMER','LF.NUM.RETI',POSnumret)
    CALL GET.LOC.REF('CUSTOMER','LF.MON.EST.ING',POSahcorr)
    CALL GET.LOC.REF('CUSTOMER','LF.MON.EST.DEP',POSdep)
    CALL GET.LOC.REF('CUSTOMER','LF.TRAN.INTER',POStranint)
    CALL GET.LOC.REF('CUSTOMER','LF.MON.EST.REM',POSmonestrem)
    CALL GET.LOC.REF('CUSTOMER','LF.MON.EST.PRE',POSmonestpre)
    CALL GET.LOC.REF('CUSTOMER','LF.NUM.EMPLES',POSnumemp)
    CALL GET.LOC.REF('CUSTOMER','LF.NUM.PROV',POSprov)
    CALL GET.LOC.REF('CUSTOMER','LF.NUM.SUC',POSsuc)
    CALL GET.LOC.REF('CUSTOMER','LF.NUM.EST.DEP',POSnumstdep)
    CALL GET.LOC.REF('CUSTOMER','LF.NUM.EST.PRE',POSnumstpre)
    CALL GET.LOC.REF('CUSTOMER','LF.NRC',POSnrc)
    CALL GET.LOC.REF('CUSTOMER','LF.PRO.FON.N',POSprocfon)
    CALL GET.LOC.REF('CUSTOMER','LF.DUI',POSlfdui)

	Y.VERSION = APPLICATION:PGM.VERSION

    RETURN

OPENFILES:

    CALL OPF(FN.CUS,F.CUS)

    RETURN

;*Valida el contenido de los campos de clientes respecto de su tipo de datos
PROCESS:

;*Recuperar tipo de Persona
    CALL GET.LOC.REF('CUSTOMER','SEGMENT', Postipopersona)
    Y.TIPO.PERSONA = R.NEW(EB.CUS.LOCAL.REF)<1,Postipopersona>

*Generales del cliente
*------------------------------------------------------------
;*Validar solo si es igual a persona natural
;*Primer nombre
    IF Y.TIPO.PERSONA EQ '1' THEN
        V_CONTENT=R.NEW(EB.CUS.NAME.1)
        V_NAME_FIELD=EB.CUS.NAME.1
        V_ERR_CODE=3
        GOSUB VALID_CONTENT
    END
;*seg nombre
    V_CONTENT=R.NEW(EB.CUS.NAME.2)
    V_NAME_FIELD=EB.CUS.NAME.2
    V_ERR_CODE=3
    GOSUB VALID_CONTENT
;*seg nombre
    V_CONTENT=R.NEW(EB.CUS.GIVEN.NAMES)
    V_NAME_FIELD= EB.CUS.GIVEN.NAMES
    V_ERR_CODE=3
    GOSUB VALID_CONTENT
;*PRI APE
*V_CONTENT='GERSON Ñ'
    V_CONTENT=R.NEW(EB.CUS.TEXT)
    V_NAME_FIELD=EB.CUS.TEXT
    V_ERR_CODE=3
    GOSUB VALID_CONTENT

;*SEG APE
    V_CONTENT=R.NEW(EB.CUS.FAMILY.NAME)
    V_NAME_FIELD=EB.CUS.FAMILY.NAME
    V_ERR_CODE=3
    GOSUB VALID_CONTENT
;*TERC APE
    V_CONTENT=R.NEW(EB.CUS.PREVIOUS.NAME)
    V_NAME_FIELD=EB.CUS.PREVIOUS.NAME
    V_ERR_CODE=3
    GOSUB VALID_CONTENT

;*CONOCIDO POR
    V_CONTENT=R.NEW(EB.CUS.INTRODUCER)
    V_NAME_FIELD=EB.CUS.INTRODUCER
    V_ERR_CODE=3
    GOSUB VALID_CONTENT

;*NOM NIT
;*Validar solo si es igual a persona natural
    IF Y.TIPO.PERSONA EQ '1' THEN
        V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSnomnit>
        V_NAME_FIELD=EB.CUS.LOCAL.REF
        POS_VALUE=POSnomnit
        V_ERR_CODE=3
        GOSUB VALID_CONTENT
    END

;*NIT ANTER
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSnitanter>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSnitanter
    V_ERR_CODE=2
    GOSUB VALID_CONTENT

;*lug nac
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSlucnac>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    V_ERR_CODE=3
    POS_VALUE=POSlucnac
    GOSUB VALID_CONTENT

;* Validación de ingreso de nuemero secuenciales de dui no válidos
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSlfdui>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSlfdui
    V_ERR_CODE=4
    GOSUB VALID_CONTENT
*----------------------------------------------------------------------
*DIRECCION DE CORRESPONDENCIA
;*COUNTRY
    V_CONTENT=R.NEW(EB.CUS.COUNTRY)
    V_NAME_FIELD=EB.CUS.PHONE.1
    V_ERR_CODE=3
    GOSUB VALID_CONTENT
;*TELEFONO
    V_CONTENT=R.NEW(EB.CUS.PHONE.1)
    V_NAME_FIELD=EB.CUS.PHONE.1
    V_ERR_CODE=2
    GOSUB VALID_CONTENT

;*TELEFONO LARGO DE DATO
    NO_TEL	=	DCOUNT(R.NEW(EB.CUS.PHONE.1),VM)
    FOR Y.TL=1 TO NO_TEL
        V_CONTENT =R.NEW(EB.CUS.PHONE.1)<1,Y.TL>
        V_NAME_FIELD=EB.CUS.PHONE.1
        POS_VALUE=Y.TL
        V_ERR_CODE=5
        GOSUB VALID_CONTENT
    NEXT Y.TL

* 	Utiliza Banca en Linea
	;* Declaración Jurada - Banca en Línea - LF.AML.USA.TCIB
	CALL GET.LOC.REF('CUSTOMER', "LF.AML.USA.TCIB", P.ONLINE)
	Y.ONLINE = TRIM(R.NEW(EB.CUS.LOCAL.REF)<1, P.ONLINE>)
	
    
	IF Y.ONLINE EQ 'SI' THEN
			V_ERR_CODE=12
			V_CONTENT=R.NEW(EB.CUS.SMS.1)
			V_NAME_FIELD=EB.CUS.SMS.1
						
			GOSUB VALID_CONTENT

	END    
	
	
	
	
;*CELULAR
    V_CONTENT=R.NEW(EB.CUS.SMS.1)
    V_NAME_FIELD=EB.CUS.SMS.1
    V_ERR_CODE=10
    GOSUB VALID_CONTENT

;*CELULAR LARGO DE DATO
*   NO_CEL	=	DCOUNT(R.NEW(EB.CUS.PHONE.1),VM)
	NO_CEL	=	DCOUNT(R.NEW(EB.CUS.SMS.1),VM)
    FOR Y.TL=1 TO NO_CEL
        V_CONTENT =R.NEW(EB.CUS.SMS.1)<1,Y.TL>
        V_NAME_FIELD=EB.CUS.SMS.1
        POS_VALUE=Y.TL
        V_ERR_CODE=11
        GOSUB VALID_CONTENT
    NEXT Y.TL

;*TEL OFICINA
    V_CONTENT=R.NEW(EB.CUS.OFF.PHONE)
    V_NAME_FIELD=EB.CUS.OFF.PHONE
    V_ERR_CODE=2
    GOSUB VALID_CONTENT

;*TEL OFICINA LARGO DE DATO
    V_CONTENT=R.NEW(EB.CUS.OFF.PHONE)
    V_NAME_FIELD=EB.CUS.OFF.PHONE
    V_ERR_CODE=5
    GOSUB VALID_CONTENT

;*TEL FAX
    V_CONTENT=R.NEW(EB.CUS.FAX.1)
    V_NAME_FIELD=EB.CUS.FAX.1
    V_ERR_CODE=2
    GOSUB VALID_CONTENT

;*TEL FAX LARGO DE DATO
    NO_TEL	=	DCOUNT(R.NEW(EB.CUS.PHONE.1),VM)
    FOR Y.TL=1 TO NO_TEL
        V_CONTENT =R.NEW(EB.CUS.FAX.1)<1,Y.TL>
        V_NAME_FIELD=EB.CUS.FAX.1
        POS_VALUE=Y.TL
        V_ERR_CODE=5
        GOSUB VALID_CONTENT
    NEXT Y.TL

*----------------------------------------------------------------------
*DOCUMENTOS
;*LUGAR DE EMISION
* V_CONTENT=R.NEW(EB.CUS.LEGAL.ISS.AUTH)
* V_NAME_FIELD='EB.CUS.LEGAL.ISS.AUTH'
* V_ERR_CODE=3
*	GOSUB VALID_CONTENT

*----------------------------------------------------------------------
*detalles financieros
;*OCUAPACION DUI
NO_TEL = DCOUNT(R.NEW(EB.CUS.OCCUPATION), VM)
FOR I = 1 TO NO_TEL
    V_CONTENT=R.NEW(EB.CUS.OCCUPATION)<1,I>
    V_NAME_FIELD=EB.CUS.OCCUPATION
    POS_VALUE=I
    V_ERR_CODE=3
    GOSUB VALID_CONTENT
NEXT
;*ACTIVIDAD DE LA EMPRESA
NO_TEL = DCOUNT(R.NEW(EB.CUS.EMPLOYERS.BUSS), VM)
FOR I = 1 TO NO_TEL
    V_CONTENT=R.NEW(EB.CUS.EMPLOYERS.BUSS)<1,I>
    V_NAME_FIELD=EB.CUS.EMPLOYERS.BUSS
    V_ERR_CODE=3
    POS_VALUE=I
    GOSUB VALID_CONTENT
NEXT
;*ACTIVIDAD DE LA EMPRESA
NO_TEL = DCOUNT(R.NEW(EB.CUS.EMPLOYERS.NAME), VM)
FOR I = 1 TO NO_TEL
    V_CONTENT=R.NEW(EB.CUS.EMPLOYERS.NAME)<1,I>
    V_NAME_FIELD=EB.CUS.EMPLOYERS.NAME
    POS_VALUE=I
    V_ERR_CODE=3
    GOSUB VALID_CONTENT
NEXT
;*EGRESO MENSUAL
    V_CONTENT=R.NEW(EB.CUS.NET.MONTHLY.OUT)
    V_NAME_FIELD=EB.CUS.NET.MONTHLY.OUT
    V_ERR_CODE=1
    GOSUB VALID_CONTENT


;*OTROS INGRESOS
NO_TEL = DCOUNT(R.NEW(EB.CUS.ANNUAL.BONUS), VM)
FOR I = 1 TO NO_TEL
R.NEW(EB.CUS.CUSTOMER.CURRENCY)<1,I>='USD'
    V_CONTENT=R.NEW(EB.CUS.ANNUAL.BONUS)<1,I>
    V_NAME_FIELD=EB.CUS.ANNUAL.BONUS
    POS_VALUE=I
    V_ERR_CODE=1
    GOSUB VALID_CONTENT
NEXT

;*EGRESO MENSUAL
    V_CONTENT=R.NEW(EB.CUS.NET.MONTHLY.IN)
    V_NAME_FIELD=EB.CUS.NET.MONTHLY.IN
    V_ERR_CODE=1
    GOSUB VALID_CONTENT

*----------------------------------------------------------------------
*detalles RESIDENCIA
;*VALOR DE LA PROPIEDAD
    V_CONTENT=R.NEW(EB.CUS.RESIDENCE.VALUE)
    V_NAME_FIELD=EB.CUS.RESIDENCE.VALUE
    V_ERR_CODE=1
    GOSUB VALID_CONTENT

;*VALOR DE LA GARANTIA
    V_CONTENT=R.NEW(EB.CUS.MORTGAGE.AMT)
    V_NAME_FIELD=EB.CUS.MORTGAGE.AMT
    V_ERR_CODE=1
    GOSUB VALID_CONTENT


*----------------------------------------------------------------------
*declaracion jurada
;*FUNCIONARIO PUBLICO
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSfunpub>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSfunpub
    V_ERR_CODE=3
    GOSUB VALID_CONTENT

;*numero de depositos
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSnumdep>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSnumdep
    V_ERR_CODE=1
    GOSUB VALID_CONTENT


*		CALL GET.LOC.REF('CUSTOMER','LF.NUM.RETI',POSnumret)
;*numero de retiros
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSnumret>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSnumret
    V_ERR_CODE=1
    GOSUB VALID_CONTENT


*		CALL GET.LOC.REF('CUSTOMER','LF.MON.EST.ING',POSahcorr)
;*numero de ingresos
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSahcorr>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSahcorr
    V_ERR_CODE=1
    GOSUB VALID_CONTENT





*		CALL GET.LOC.REF('CUSTOMER','LF.MON.EST.DEP',POSdep)
;*numero de depositos
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSdep>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSdep
    V_ERR_CODE=1
    GOSUB VALID_CONTENT

*		CALL GET.LOC.REF('CUSTOMER','LF.TRAN.INTER',POStranint)
;*tranferencia interenacionales
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POStranint>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POStranint
    V_ERR_CODE=1
    GOSUB VALID_CONTENT

*		CALL GET.LOC.REF('CUSTOMER','LF.MON.EST.REM',POSmonestrem)
;*tranferencia remesa familiares
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSmonestrem>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSmonestrem
    V_ERR_CODE=1
    GOSUB VALID_CONTENT

*		CALL GET.LOC.REF('CUSTOMER','LF.MON.EST.PRE',POSmonestpre)
;*monto prestamos
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSmonestpre>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSmonestpre
    V_ERR_CODE=1
    GOSUB VALID_CONTENT

*CALL GET.LOC.REF('CUSTOMER','LF.PRO.FON.N',POSprocfon)
;*procedencia de fondos
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSprocfon>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSprocfon
    V_ERR_CODE=3000
    GOSUB VALID_CONTENT
*-----------------------------------
*JURIDICA

*	CALL GET.LOC.REF('CUSTOMER','LF.NUM.EMPLES',POSnumemp)
;*numero empleados
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSnumemp>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSnumemp
    V_ERR_CODE=2
    GOSUB VALID_CONTENT




*	CALL GET.LOC.REF('CUSTOMER','LF.NUM.PROV',POSprov)
;*numero proveedores
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSprov>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSprov
    V_ERR_CODE=2
    GOSUB VALID_CONTENT

*		CALL GET.LOC.REF('CUSTOMER','LF.NUM.SUC',POSsuc)
;*numero sucursales
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSsuc>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSsuc
    V_ERR_CODE=2
    GOSUB VALID_CONTENT

*			CALL GET.LOC.REF('CUSTOMER','LF.NUM.EST.DEP',POSnumstdep)
;*numero estimado de depositos
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSnumstdep>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSnumstdep
    V_ERR_CODE=2
    GOSUB VALID_CONTENT


*	CALL GET.LOC.REF('CUSTOMER','LF.NUM.EST.PRE',POSnumstpre)
;*numero estimado de prestamos
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSnumstpre>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSnumstpre
    V_ERR_CODE=2
    GOSUB VALID_CONTENT

*	CALL GET.LOC.REF('CUSTOMER','LF.NRC',POSnrc)
;*numero estimado de prestamos
    V_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSnrc>
    V_NAME_FIELD=EB.CUS.LOCAL.REF
    POS_VALUE=POSnrc
    V_ERR_CODE=2
    GOSUB VALID_CONTENT


;*Periodo de nombramiento
*V_CONTENT='GERSON Ñ'
    NO_TXT	=	DCOUNT(R.NEW(EB.CUS.TEXT),VM)
    FOR Y.A1=1 TO NO_TXT
        V_CONTENT =R.NEW(EB.CUS.TEXT)<1,Y.A1>
        V_NAME_FIELD=EB.CUS.TEXT
        POS_VALUE=Y.A1
        V_ERR_CODE=3
        GOSUB VALID_CONTENT
    NEXT Y.A1


*---------------------------------------------------------------------
*FECHAS

;*fecha de nacimiento
    V_DATE_CONTENT =R.NEW(EB.CUS.DATE.OF.BIRTH)
    V_NAME_FIELD=EB.CUS.DATE.OF.BIRTH
    V_ERR_CODE_DATE=1
    GOSUB VALID_DATES


    NO_TNXA	=	DCOUNT(R.NEW(EB.CUS.LEGAL.ID),VM)
    FOR Y.A=1 TO NO_TNXA
        ;*fecha expiracion
        V_DATE_CONTENT =R.NEW(EB.CUS.LEGAL.EXP.DATE)<1,Y.A>
        V_NAME_FIELD=EB.CUS.LEGAL.EXP.DATE
        POS_VALUE=Y.A
        V_ERR_CODE_DATE=3
        GOSUB VALID_DATES
    NEXT Y.A



;*residencia desde
    V_DATE_CONTENT =R.NEW(EB.CUS.RESIDENCE.SINCE)
    V_NAME_FIELD=EB.CUS.RESIDENCE.SINCE
    V_ERR_CODE_DATE=2
    GOSUB VALID_DATES

    NO_TNX	=	DCOUNT(R.NEW(EB.CUS.LEGAL.ID),VM)
    FOR Y=1 TO NO_TNX
        ;*fecha de emisión
        V_DATE_CONTENT =R.NEW(EB.CUS.LEGAL.ISS.DATE)<1,Y>
        V_NAME_FIELD=EB.CUS.LEGAL.ISS.DATE
        POS_VALUE=Y
        V_ERR_CODE_DATE=2
        GOSUB VALID_DATES
    NEXT Y

    NO_TNXB=	DCOUNT(R.NEW(EB.CUS.LEGAL.ID),VM)
    FOR Y.B=1 TO NO_TNXB
        ;*fecha de emisión obligatorio para dui
        IF R.NEW(EB.CUS.LEGAL.DOC.NAME)<1,Y.B> EQ 'DOCTO.UNICO.IDENT' THEN
            V_DATE_CONTENT =R.NEW(EB.CUS.LEGAL.EXP.DATE)<1,Y.B>
            V_NAME_FIELD=EB.CUS.LEGAL.EXP.DATE
            POS_VALUE=Y.B
            V_ERR_CODE_DATE=6
            GOSUB VALID_DATES
        END
    NEXT Y.B

;*Fecha de ingreso a la empresa
NO_TEL = DCOUNT(R.NEW(EB.CUS.EMPLOYMENT.START), VM)
FOR I = 1 TO NO_TEL
    V_DATE_CONTENT =R.NEW(EB.CUS.EMPLOYMENT.START)<1,I>
    V_NAME_FIELD=EB.CUS.EMPLOYMENT.START
    POS_VALUE=I
    V_ERR_CODE_DATE=2
    GOSUB VALID_DATES
NEXT
;*NIT
    V_DATE_CONTENT=R.NEW(EB.CUS.LOCAL.REF)<1,POSnit>
    V_NAME_FIELD='LF.NIT'
    V_ERR_CODE_DATE=5
    POS_VALUE=POSnit
    GOSUB VALID_DATES

    RETURN

;*validaciones de tipo númerico
VALID_CONTENT:

    BEGIN CASE
        CASE V_ERR_CODE EQ 1
            ;*numeric
            IF V_CONTENT NE '' AND NUM( V_CONTENT) EQ 0 THEN
                STRERR =V_NAME_FIELD :'Tipo de dato incorrecto, Númericos con decimales'
                GOSUB CRT_ERROR
                BREAK
            END

        CASE V_ERR_CODE EQ 2
            ;* numeric int
            IF  V_CONTENT NE '' AND ISDIGIT( V_CONTENT) EQ 0 THEN
                STRERR ='Tipo de dato incorrecto, Solo Enteros'
                GOSUB CRT_ERROR
                BREAK
            END

        CASE V_ERR_CODE EQ 3
            ;*char with symbols
            
            CHANGE 'Ñ' TO 'N' IN V_CONTENT
            CHANGE 'Ã‘' TO 'N' IN V_CONTENT

            CHANGE 'Ã±' TO 'N' IN V_CONTENT
            CHANGE CHAR(465) TO 'N' IN V_CONTENT
            CHANGE CHAR(497) TO 'N' IN V_CONTENT
            
            IF V_CONTENT MATCHES "0A'.'0A" AND V_CONTENT NE '' THEN
                STRERR ='Tipo de dato incorrecto, Solo Alfabeticos '
                GOSUB CRT_ERROR
                BREAK
            END

*            ;*Tratamiento de caracteres
*            CHANGE '.' TO 'A'IN V_CONTENT
*            CHANGE ',' TO 'A'IN V_CONTENT
*            CHANGE ':' TO 'A'IN V_CONTENT
*            CHANGE ';' TO 'A'IN V_CONTENT
*            CHANGE '(' TO 'A'IN V_CONTENT
*            CHANGE ')' TO 'A'IN V_CONTENT
             CHANGE ' ' TO 'A' IN V_CONTENT
            
            IF V_CONTENT NE '' AND ISALPHA(V_CONTENT) EQ 0  THEN
                STRERR ='Tipo de dato incorrecto, Solo Alfabeticos '
                GOSUB CRT_ERROR
                BREAK
            END

        CASE V_ERR_CODE EQ 4
            ;* Número de dui secuencial no valido
            LEN_NODOC = LEN(TRIM(V_CONTENT))
            FOR N.I=1 TO LEN_NODOC
                N_VALUE+=V_CONTENT[N.I,1]
            NEXT N.I
            N_INDEX=N_VALUE/LEN_NODOC

            IF  V_CONTENT NE '' AND V_CONTENT[1,1] EQ N_INDEX THEN
                STRERR ='Numero de documento no valido ':V_CONTENT
                GOSUB CRT_ERROR
                BREAK
            END
            ;*valida longitud de valor ingresado TELEFONO
        CASE V_ERR_CODE EQ 5
            IF  V_CONTENT NE '' AND LEN(TRIM(V_CONTENT))<>8 THEN
                STRERR ='Numero de telefono no valido ':V_CONTENT
                GOSUB CRT_ERROR
                BREAK
            END

		CASE V_ERR_CODE EQ 10
            ;* numeric int
            IF  V_CONTENT NE '' AND ISDIGIT( V_CONTENT) EQ 0 THEN
                STRERR =' Invalido. Favor ingrese 8 numeros enteros.'
                GOSUB CRT_ERROR
                BREAK
            END
            
        CASE V_ERR_CODE EQ 11
            ;* numeric inT
           IF  V_CONTENT NE '' AND LEN(TRIM(V_CONTENT))<>8 THEN
                STRERR =' Invalido. Favor ingrese 8 numeros enteros. '
                GOSUB CRT_ERROR
                BREAK
            END
           
         CASE V_ERR_CODE EQ 12
        
				Y.USA.SMSTOKEN = 0
				Y.CUSTOMER = ID.NEW
				CALL SLV.V.VALIDA.SMS(Y.CUSTOMER, Y.VERSION, OUT.DATA) 
				Y.USA.SMSTOKEN = OUT.DATA
			
				IF  V_CONTENT EQ '' AND Y.USA.SMSTOKEN EQ '1' THEN
					STRERR = ' Usuario posee SMS Token. Favor agregar informacion.'
					GOSUB CRT_ERROR
                	BREAK
            	END
            	
            	IF  V_CONTENT EQ '' AND Y.USA.SMSTOKEN NE '1' THEN
					STRERR = ' Complete campo Numero de Telefono Movil si cliente utilizara SMS Token'
					GOSUB CRT_OVERRIDE
                	BREAK
            	END

    END CASE

    RETURN

;*validaciones de tipo Date y otros
VALID_DATES:
    BEGIN CASE
        CASE V_ERR_CODE_DATE EQ 1

            IF  V_DATE_CONTENT NE '' AND (LEFT(V_DATE_CONTENT,4) < '1901' OR LEFT(V_DATE_CONTENT,4) > '2999') THEN
                STRERR ='Fecha no valida, ':V_DATE_CONTENT
                GOSUB CRT_ERROR
                BREAK
            END

        CASE V_ERR_CODE_DATE EQ 2
            IF V_DATE_CONTENT NE '' AND (V_DATE_CONTENT > TODAY OR LEFT(V_DATE_CONTENT,4) < '1901') THEN
                STRERR ='Fecha no valida : ':V_DATE_CONTENT
                GOSUB CRT_ERROR
                BREAK
            END

        CASE V_ERR_CODE_DATE EQ 3
            IF V_DATE_CONTENT NE '' AND LEFT(V_DATE_CONTENT,4) > LEFT(TODAY,4) + 10 THEN
                STRERR ='Fecha de expiracion no valida  ':V_DATE_CONTENT
                GOSUB CRT_ERROR
                BREAK
            END

        CASE V_ERR_CODE_DATE EQ 4
            IF V_DATE_CONTENT NE '' AND LEFT(V_DATE_CONTENT,4) > LEFT(TODAY,4) + 100 THEN
                STRERR ='Fecha de expiracion ':V_DATE_CONTENT
                GOSUB CRT_ERROR
                BREAK
            END

        CASE V_ERR_CODE_DATE EQ 5
            ;*Para validación de fecha en NIT
            FECHANIT=R.NEW(EB.CUS.DATE.OF.BIRTH)[7,2]:R.NEW(EB.CUS.DATE.OF.BIRTH)[5,2]:R.NEW(EB.CUS.DATE.OF.BIRTH)[3,2]
            IF FECHANIT NE  V_DATE_CONTENT[5,6] THEN
                STRERR ='Fecha de nacimiento diferente de fecha en NIT ':V_DATE_CONTENT
                GOSUB CRT_OVERRIDE
                BREAK
            END
        CASE  V_ERR_CODE_DATE EQ 6
            IF V_DATE_CONTENT EQ '' THEN
                STRERR ='Fecha de vencimiento para ':R.NEW(EB.CUS.LEGAL.DOC.NAME)<1,POS_VALUE>:' no puede estar vacia'
                GOSUB CRT_ERROR
                BREAK
            END
    END CASE

    RETURN

;*Lazadores de error
*-----------------------------------------------------------------------------------------------------------------------
CRT_ERROR:
    AF  = V_NAME_FIELD
    AV 	= POS_VALUE
    ETEXT = STRERR
    CALL STORE.END.ERROR
    RETURN

CRT_OVERRIDE:
    TEXT =STRERR
    CURR.NO =V_NAME_FIELD
    CALL STORE.OVERRIDE(CURR.NO)
    RETURN

    END
