*-----------------------------------------------------------------------------
* <Rating>672</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.AML.CHECK.INP
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.GLOBAL.PARAM
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
    FN.TABLE.PA = 'F.EB.SLV.GLOBAL.PARAM'
    F.TABLE.PA = ''

    CALL OPF(FN.TABLE.PA, F.TABLE.PA)

    RETURN



PROCESS:
    Y.AML.CHECK = 'AML.SCREEN.CHECK'
    CALL F.READ(FN.TABLE.PA, Y.AML.CHECK, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    Y.ACTIVATE = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
	
	GOSUB REMOVERCARACTERES;*EURIAS20150715 REMOVER CARATERES ESPECIALES

    IF Y.ACTIVATE EQ '1' THEN
		CALL VL.CONSTRUCT.CUS.INDIVIDUAL.FIELDS
    END
    GOSUB ESTADOINICIAL
    RETURN

	;*EURIAS REMOVER CARACTERES ESPECIALES
	REMOVERCARACTERES:
		idParamCaracteres='AML.CARACTERES.ESPECIALES'
		R.TABLE.PA=''
		F.ERR.PA=''
		FN.TABLE.PA='F.EB.SLV.GLOBAL.PARAM'
		CALL F.READ(FN.TABLE.PA, idParamCaracteres, R.TABLE.PA, parametrosData, F.ERR.PA)
		caracteresAML = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
		numCaracteres = DCOUNT(caracteresAML,VM)
		
		i=1
		datosIniciales<i> =R.NEW(EB.CUS.ACCOUNT.OFFICER)      ;i++
		datosIniciales<i> =R.NEW(EB.CUS.CHANGE.DATE)          ;i++
		datosIniciales<i> =R.NEW(EB.CUS.CHANGE.REASON)        ;i++
		datosIniciales<i> =R.NEW(EB.CUS.COUNTRY)              ;i++
		datosIniciales<i> =R.NEW(EB.CUS.CUSTOMER.RATING)      ;i++
		datosIniciales<i> =R.NEW(EB.CUS.CUSTOMER.SINCE)       ;i++
		datosIniciales<i> =R.NEW(EB.CUS.CUSTOMER.STATUS)      ;i++
		datosIniciales<i> =R.NEW(EB.CUS.DATE.OF.BIRTH)        ;i++
		datosIniciales<i> =R.NEW(EB.CUS.DOMICILE)             ;i++
		datosIniciales<i> =R.NEW(EB.CUS.EMAIL.1)              ;i++
		datosIniciales<i> =R.NEW(EB.CUS.EMPLOYERS.ADD)        ;i++
		datosIniciales<i> =R.NEW(EB.CUS.EMPLOYERS.BUSS)       ;i++
		datosIniciales<i> =R.NEW(EB.CUS.EMPLOYERS.NAME)       ;i++
		datosIniciales<i> =R.NEW(EB.CUS.FAMILY.NAME)          ;i++
		datosIniciales<i> =R.NEW(EB.CUS.GENDER)               ;i++
		datosIniciales<i> =R.NEW(EB.CUS.GIVEN.NAMES)          ;i++
		datosIniciales<i> =R.NEW(EB.CUS.INDUSTRY)             ;i++
		datosIniciales<i> =R.NEW(EB.CUS.INTRODUCER)           ;i++
		datosIniciales<i> =R.NEW(EB.CUS.JOB.TITLE)            ;i++
		datosIniciales<i> =R.NEW(EB.CUS.LANGUAGE)             ;i++
		datosIniciales<i> =R.NEW(EB.CUS.LEGAL.DOC.NAME)       ;i++
		datosIniciales<i> =R.NEW(EB.CUS.LEGAL.ID)             ;i++
		datosIniciales<i> =R.NEW(EB.CUS.LEGAL.ISS.AUTH)       ;i++
		datosIniciales<i> =R.NEW(EB.CUS.LEGAL.ISS.DATE)       ;i++
		datosIniciales<i> =R.NEW(EB.CUS.LOCAL.REF)            ;i++
		datosIniciales<i> =R.NEW(EB.CUS.MARITAL.STATUS)       ;i++
		datosIniciales<i> =R.NEW(EB.CUS.MNEMONIC)             ;i++
		datosIniciales<i> =R.NEW(EB.CUS.NAME.1)               ;i++
		datosIniciales<i> =R.NEW(EB.CUS.NAME.2)               ;i++
		datosIniciales<i> =R.NEW(EB.CUS.NATIONALITY)          ;i++
		datosIniciales<i> =R.NEW(EB.CUS.NO.OF.DEPENDENTS)     ;i++
		datosIniciales<i> =R.NEW(EB.CUS.OCCUPATION)           ;i++
		datosIniciales<i> =R.NEW(EB.CUS.OTHER.OFFICER)        ;i++
		datosIniciales<i> =R.NEW(EB.CUS.PHONE.1)              ;i++
		datosIniciales<i> =R.NEW(EB.CUS.POST.CODE)            ;i++
		datosIniciales<i> =R.NEW(EB.CUS.PREVIOUS.NAME)        ;i++
		datosIniciales<i> =R.NEW(EB.CUS.REL.CUSTOMER)         ;i++
		datosIniciales<i> =R.NEW(EB.CUS.RESIDENCE)            ;i++
		datosIniciales<i> =R.NEW(EB.CUS.RESIDENCE.SINCE)      ;i++
		datosIniciales<i> =R.NEW(EB.CUS.RESIDENCE.STATUS)     ;i++
		datosIniciales<i> =R.NEW(EB.CUS.RESIDENCE.TYPE)       ;i++
		datosIniciales<i> =R.NEW(EB.CUS.RESIDENCE.VALUE)      ;i++
		datosIniciales<i> =R.NEW(EB.CUS.SECTOR)               ;i++
		datosIniciales<i> =R.NEW(EB.CUS.SHORT.NAME)           ;i++
		datosIniciales<i> =R.NEW(EB.CUS.SMS.1)                ;i++
		datosIniciales<i> =R.NEW(EB.CUS.SPOKEN.LANGUAGE)      ;i++
		datosIniciales<i> =R.NEW(EB.CUS.STREET)               ;i++
		datosIniciales<i> =R.NEW(EB.CUS.TARGET)               ;i++
		datosIniciales<i> =R.NEW(EB.CUS.TEXT)                 ;i++
		datosIniciales<i> =R.NEW(EB.CUS.TITLE)                ;i++
		datosIniciales<i> =R.NEW(EB.CUS.TOWN.COUNTRY)         ;
		
		
		FOR j=1 TO	numCaracteres
			caracterRemovido = FIELD(caracteresAML,VM,j)
			FOR k = 1 TO i 
				datosAuxiliares<k> = CHANGE(datosIniciales<k>,caracterRemovido,"")
			NEXT k
		NEXT j
		
		i=1	
		R.NEW(EB.CUS.ACCOUNT.OFFICER)      = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.CHANGE.DATE)		   = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.CHANGE.REASON)        = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.COUNTRY)              = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.CUSTOMER.RATING)      = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.CUSTOMER.SINCE)       = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.CUSTOMER.STATUS)      = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.DATE.OF.BIRTH)        = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.DOMICILE)             = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.EMAIL.1)              = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.EMPLOYERS.ADD)        = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.EMPLOYERS.BUSS)       = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.EMPLOYERS.NAME)       = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.FAMILY.NAME)          = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.GENDER)               = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.GIVEN.NAMES)          = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.INDUSTRY)             = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.INTRODUCER)           = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.JOB.TITLE)            = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.LANGUAGE)             = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.LEGAL.DOC.NAME)       = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.LEGAL.ID)             = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.LEGAL.ISS.AUTH)       = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.LEGAL.ISS.DATE)       = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.LOCAL.REF)            = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.MARITAL.STATUS)       = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.MNEMONIC)             = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.NAME.1)               = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.NAME.2)               = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.NATIONALITY)          = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.NO.OF.DEPENDENTS)     = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.OCCUPATION)           = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.OTHER.OFFICER)        = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.PHONE.1)              = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.POST.CODE)            = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.PREVIOUS.NAME)        = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.REL.CUSTOMER)         = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.RESIDENCE)            = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.RESIDENCE.SINCE)      = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.RESIDENCE.STATUS)     = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.RESIDENCE.TYPE)       = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.RESIDENCE.VALUE)      = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.SECTOR)               = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.SHORT.NAME)           = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.SMS.1)                = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.SPOKEN.LANGUAGE)      = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.STREET)               = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.TARGET)               = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.TEXT)                 = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.TITLE)                = datosAuxiliares<i> ;i++
		R.NEW(EB.CUS.TOWN.COUNTRY)         = datosAuxiliares<i> 
	RETURN
	
	ESTADOINICIAL:;*eurias20150716 regresar los r.new al estado inicial
			i=1
			R.NEW(EB.CUS.ACCOUNT.OFFICER)      = datosIniciales<i> ;i++
			R.NEW(EB.CUS.CHANGE.DATE)		   = datosIniciales<i> ;i++
			R.NEW(EB.CUS.CHANGE.REASON)        = datosIniciales<i> ;i++
			R.NEW(EB.CUS.COUNTRY)              = datosIniciales<i> ;i++
			R.NEW(EB.CUS.CUSTOMER.RATING)      = datosIniciales<i> ;i++
			R.NEW(EB.CUS.CUSTOMER.SINCE)       = datosIniciales<i> ;i++
			R.NEW(EB.CUS.CUSTOMER.STATUS)      = datosIniciales<i> ;i++
			R.NEW(EB.CUS.DATE.OF.BIRTH)        = datosIniciales<i> ;i++
			R.NEW(EB.CUS.DOMICILE)             = datosIniciales<i> ;i++
			R.NEW(EB.CUS.EMAIL.1)              = datosIniciales<i> ;i++
			R.NEW(EB.CUS.EMPLOYERS.ADD)        = datosIniciales<i> ;i++
			R.NEW(EB.CUS.EMPLOYERS.BUSS)       = datosIniciales<i> ;i++
			R.NEW(EB.CUS.EMPLOYERS.NAME)       = datosIniciales<i> ;i++
			R.NEW(EB.CUS.FAMILY.NAME)          = datosIniciales<i> ;i++
			R.NEW(EB.CUS.GENDER)               = datosIniciales<i> ;i++
			R.NEW(EB.CUS.GIVEN.NAMES)          = datosIniciales<i> ;i++
			R.NEW(EB.CUS.INDUSTRY)             = datosIniciales<i> ;i++
			R.NEW(EB.CUS.INTRODUCER)           = datosIniciales<i> ;i++
			R.NEW(EB.CUS.JOB.TITLE)            = datosIniciales<i> ;i++
			R.NEW(EB.CUS.LANGUAGE)             = datosIniciales<i> ;i++
			R.NEW(EB.CUS.LEGAL.DOC.NAME)       = datosIniciales<i> ;i++
			R.NEW(EB.CUS.LEGAL.ID)             = datosIniciales<i> ;i++
			R.NEW(EB.CUS.LEGAL.ISS.AUTH)       = datosIniciales<i> ;i++
			R.NEW(EB.CUS.LEGAL.ISS.DATE)       = datosIniciales<i> ;i++
			R.NEW(EB.CUS.LOCAL.REF)            = datosIniciales<i> ;i++
			R.NEW(EB.CUS.MARITAL.STATUS)       = datosIniciales<i> ;i++
			R.NEW(EB.CUS.MNEMONIC)             = datosIniciales<i> ;i++
			R.NEW(EB.CUS.NAME.1)               = datosIniciales<i> ;i++
			R.NEW(EB.CUS.NAME.2)               = datosIniciales<i> ;i++
			R.NEW(EB.CUS.NATIONALITY)          = datosIniciales<i> ;i++
			R.NEW(EB.CUS.NO.OF.DEPENDENTS)     = datosIniciales<i> ;i++
			R.NEW(EB.CUS.OCCUPATION)           = datosIniciales<i> ;i++
			R.NEW(EB.CUS.OTHER.OFFICER)        = datosIniciales<i> ;i++
			R.NEW(EB.CUS.PHONE.1)              = datosIniciales<i> ;i++
			R.NEW(EB.CUS.POST.CODE)            = datosIniciales<i> ;i++
			R.NEW(EB.CUS.PREVIOUS.NAME)        = datosIniciales<i> ;i++
			R.NEW(EB.CUS.REL.CUSTOMER)         = datosIniciales<i> ;i++
			R.NEW(EB.CUS.RESIDENCE)            = datosIniciales<i> ;i++
			R.NEW(EB.CUS.RESIDENCE.SINCE)      = datosIniciales<i> ;i++
			R.NEW(EB.CUS.RESIDENCE.STATUS)     = datosIniciales<i> ;i++
			R.NEW(EB.CUS.RESIDENCE.TYPE)       = datosIniciales<i> ;i++
			R.NEW(EB.CUS.RESIDENCE.VALUE)      = datosIniciales<i> ;i++
			R.NEW(EB.CUS.SECTOR)               = datosIniciales<i> ;i++
			R.NEW(EB.CUS.SHORT.NAME)           = datosIniciales<i> ;i++
			R.NEW(EB.CUS.SMS.1)                = datosIniciales<i> ;i++
			R.NEW(EB.CUS.SPOKEN.LANGUAGE)      = datosIniciales<i> ;i++
			R.NEW(EB.CUS.STREET)               = datosIniciales<i> ;i++
			R.NEW(EB.CUS.TARGET)               = datosIniciales<i> ;i++
			R.NEW(EB.CUS.TEXT)                 = datosIniciales<i> ;i++
			R.NEW(EB.CUS.TITLE)                = datosIniciales<i> ;i++
			R.NEW(EB.CUS.TOWN.COUNTRY)         = datosIniciales<i> 
			RETURN
END





