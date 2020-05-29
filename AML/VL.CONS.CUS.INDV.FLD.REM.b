*-----------------------------------------------------------------------------
* <Rating>58</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE VL.CONS.CUS.INDV.FLD.REM
*-----------------------------------------------------------------------------
*  Esta Rutina es una copia de [VL.CONSTRUCT.CUS.INDIVIDUAL.FIELDS], se ha creado 
*  para enviar registros que no existen en la tbl CUSTOMER, esto debido al 
*  proyecto de REMESAS. 
*-----------------------------------------------------------------------------
* 1.0		Jonas		01.01.18	Inicial
* 1.1		Jonas		18.02.18	Almacenar de remitente de remesa familiar.
* 2.0		Jonas		06.04.18	Definir automatico override VL-VL.SEND.AML.REMESA.
* 3.0		Jonas		06.04.18	Implementar uso en remintente y beneficiario.
* 3.1		Jonas		19.09.18	Enviar "*REMESAS*" en campo STREET, se utilizará para
*									disparar trigger TMRSMSG_BRK_AINS.sql en DB AMLDB.
* 4.0		Jonas		30.10.18	Enviar screneo en autorizacion de usuario.
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.PAGO.REMESA
$INSERT I_AMLService_CusTxnDetails
$INSERT I_F.EB.SLV.GLOBAL.PARAM
*---------PENDIENTE OBTENER ARCHIVO I_AMLService_CusTxnDetails
****************************************************************************
    GOSUB INITIALIZATION
   GOSUB LOGGER
TEXTO.ARCHIVO = 'VL.CONS.CUS.INDV.FLD.REM-->'
GOSUB ESCRIBIR.ARCHIVO
TEXTO.ARCHIVO = 'FUNCION: ':V$FUNCTION
GOSUB ESCRIBIR.ARCHIVO
   
    IF Y.VERSION NE Y.VER.AUTH AND Y.VERSION NE Y.VER.USR THEN
	    IF V$FUNCTION NE 'I' AND V$FUNCTION NE 'C' THEN
TEXTO.ARCHIVO = 'SALIO....'
GOSUB ESCRIBIR.ARCHIVO

	        RETURN
	    END
	END
    GOSUB OPENFILES
    GOSUB PROCESS
    GOSUB CALL.SERVICE.RTN.OVERRIDE
RETURN
**********************************************************************
INITIALIZATION:
*
    CURR.NO = ''
    iCusTxnDetails = ''
	
    Y.APPL    = "CUSTOMER"
    Y.APPLFLD = ""
    Y.POS     = ""

    Y.APP 		  = APPLICATION
    Y.PGM.VERSION = PGM.VERSION
    Y.VERSION     = Y.APP : Y.PGM.VERSION
    Y.VER.AUTH    = 'CUSTOMER,SLV.AUTH.REME.USR'
    Y.VER.USR    = 'CUSTOMER,SLV.INPUT.REMESA.USR'
    EQU REMESA.CUST  TO 'CUSTOMER,SLV.INPUT.REMESA'
    EQU REMESA.REMIT TO 'CUSTOMER,SLV.INPUT.REMESA.REMIT'
RETURN
*
*-----------------------------------------------------------------------------
OPENFILES:
*-----------------------------------------------------------------------------
    FN.PAGO.REME = 'F.EB.SLV.PAGO.REMESA'
    F.PAGO.REME  = ''
    
    CALL OPF(FN.PAGO.REME, F.PAGO.REME)
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
	;*Obtener información de pago de remesa
	IF Y.VERSION EQ REMESA.REMIT THEN
		APPL.NAME = "CUSTOMER"    	
	    FIELDNAME.APP = "LF.LAST.ID.REME"
		POS.CUS = ''
	
	    CALL MULTI.GET.LOC.REF(APPL.NAME, FIELDNAME.APP, POS.CUS)
	    POS.ID  = POS.CUS<1,1>
		Y.ID.REMESA = R.NEW(EB.CUS.LOCAL.REF)<1, POS.ID>
	
		CALL F.READ(FN.PAGO.REME, Y.ID.REMESA, R.PAGO.REME, F.PAGO.REME, ERR.PAGREM)
		Y.REMIT.NAME  = TRIM(R.PAGO.REME<EB.SLV.PRF.REMIT.FIRST.NAME>):" ":TRIM(R.PAGO.REME<EB.SLV.PRF.REMIT.SECOND.NAME>):" ":
		Y.REMIT.NAME := TRIM(R.PAGO.REME<EB.SLV.PRF.REMIT.LAST.NAME>) :" ":TRIM(R.PAGO.REME<EB.SLV.PRF.REMIT.SE.LAST.NAME>)
		Y.REMIT.NAME = TRIM(Y.REMIT.NAME)
		Y.REMIT.SHORT.NAME = TRIM(R.PAGO.REME<EB.SLV.PRF.REMIT.FIRST.NAME>):" ":TRIM(R.PAGO.REME<EB.SLV.PRF.REMIT.LAST.NAME>)
		Y.REMIT.SHORT.NAME = TRIM(Y.REMIT.SHORT.NAME)
		Y.SHORT.NAME = Y.REMIT.SHORT.NAME
		Y.NAME = Y.REMIT.NAME  
	END ELSE
		Y.SHORT.NAME = R.NEW(EB.CUS.SHORT.NAME)
		Y.NAME       = TRIM(R.NEW(EB.CUS.NAME.1):" ":R.NEW(EB.CUS.NAME.2):" ":R.NEW(EB.CUS.GIVEN.NAMES):" ":R.NEW(EB.CUS.TEXT):" ":R.NEW(EB.CUS.FAMILY.NAME))
	END
    ;*SHORT.NAME.LIST = R.NEW(EB.CUS.SHORT.NAME)
	SHORT.NAME.LIST = Y.SHORT.NAME 
    CONVERT VM TO " " IN SHORT.NAME.LIST
*    iCusTxnDetails<CusTxnDetails.shortName> = SHORT.NAME.LIST
    iCusTxnDetails<1> = SHORT.NAME.LIST
    STR.DATO :="iCusTxnDetails<1>: ":iCusTxnDetails<1>:"*"
*
    ;*NAME1.LIST = R.NEW(EB.CUS.NAME.1) : " " : R.NEW(EB.CUS.NAME.2) : " " : R.NEW(EB.CUS.GIVEN.NAMES) : " " : R.NEW(EB.CUS.TEXT) : " " : R.NEW(EB.CUS.FAMILY.NAME)
    NAME1.LIST = Y.NAME
    CONVERT VM TO " " IN NAME1.LIST
*    iCusTxnDetails<CusTxnDetails.name1> = NAME1.LIST
    iCusTxnDetails<2> = NAME1.LIST
    STR.DATO :="iCusTxnDetails<2>: ":iCusTxnDetails<2>:"*"
*
    ;*NAME2.LIST = R.NEW(EB.CUS.NAME.1) : " " : R.NEW(EB.CUS.NAME.2) : " " : R.NEW(EB.CUS.GIVEN.NAMES) : " " : R.NEW(EB.CUS.TEXT) : " " : R.NEW(EB.CUS.PREVIOUS.NAME)
    NAME2.LIST = Y.NAME
    CONVERT VM TO " " IN NAME2.LIST
*    iCusTxnDetails<CusTxnDetails.name2> = NAME2.LIST
    iCusTxnDetails<3> = NAME2.LIST
	STR.DATO :="iCusTxnDetails<3>: ":iCusTxnDetails<3>:"*"    
*
    ;*STREET.LIST = R.NEW(EB.CUS.STREET)
    STREET.LIST = "*REMESAS*"
    ;*CONVERT VM TO " " IN STREET.LIST
*    iCusTxnDetails<CusTxnDetails.street> = STREET.LIST
    iCusTxnDetails<4> = STREET.LIST
*
    TOWN.LIST = R.NEW(EB.CUS.TOWN.COUNTRY)
    CONVERT VM TO " " IN TOWN.LIST
*    iCusTxnDetails<CusTxnDetails.townCountry> = TOWN.LIST
    iCusTxnDetails<5> = TOWN.LIST
*
    POST.LIST = R.NEW(EB.CUS.POST.CODE)
    CONVERT VM TO " " IN POST.LIST
*    iCusTxnDetails<CusTxnDetails.postCode> = POST.LIST
    iCusTxnDetails<6> = POST.LIST
*
    CNTRY.LIST = R.NEW(EB.CUS.COUNTRY)
    CONVERT VM TO " " IN CNTRY.LIST
*    iCusTxnDetails<CusTxnDetails.country> = CNTRY.LIST
    iCusTxnDetails<7> = CNTRY.LIST
*
*    iCusTxnDetails<CusTxnDetails.accountOfficer> = R.NEW(EB.CUS.ACCOUNT.OFFICER)
*    *
*    iCusTxnDetails<CusTxnDetails.industry> = R.NEW(EB.CUS.INDUSTRY)
**
*    iCusTxnDetails<CusTxnDetails.nationality> = R.NEW(EB.CUS.NATIONALITY)
**
*    iCusTxnDetails<CusTxnDetails.residence> = R.NEW(EB.CUS.RESIDENCE)
**
*    iCusTxnDetails<CusTxnDetails.language> = R.NEW(EB.CUS.LANGUAGE)
**
    iCusTxnDetails<8> = R.NEW(EB.CUS.ACCOUNT.OFFICER)
    iCusTxnDetails<9> = R.NEW(EB.CUS.INDUSTRY)
    iCusTxnDetails<10> = R.NEW(EB.CUS.NATIONALITY)
    iCusTxnDetails<11> = R.NEW(EB.CUS.RESIDENCE)
    iCusTxnDetails<12> = R.NEW(EB.CUS.LANGUAGE)
    PHN.LIST = R.NEW(EB.CUS.PHONE.1)
    CONVERT VM TO " " IN PHN.LIST
*    iCusTxnDetails<CusTxnDetails.phone1> = PHN.LIST
    iCusTxnDetails<13> = PHN.LIST
*
    SMS.LIST = R.NEW(EB.CUS.SMS.1)
    CONVERT VM TO " " IN SMS.LIST
*    iCusTxnDetails<CusTxnDetails.sms1> = SMS.LIST
    iCusTxnDetails<14> = SMS.LIST
*
    EMAIL.LIST = R.NEW(EB.CUS.EMAIL.1)
    CONVERT VM TO " " IN EMAIL.LIST
*    iCusTxnDetails<CusTxnDetails.email1> = EMAIL.LIST
    iCusTxnDetails<15> = EMAIL.LIST
*
*    iCusTxnDetails<CusTxnDetails.mnemonic> = R.NEW(EB.CUS.MNEMONIC)
**
*    iCusTxnDetails<CusTxnDetails.relCustomer> = R.NEW(EB.CUS.REL.CUSTOMER)
**
*    iCusTxnDetails<CusTxnDetails.sector> = R.NEW(EB.CUS.SECTOR)
**
*    iCusTxnDetails<CusTxnDetails.target>  = R.NEW(EB.CUS.TARGET)
**
*    iCusTxnDetails<CusTxnDetails.customerStatus> = R.NEW(EB.CUS.CUSTOMER.STATUS)
**
    iCusTxnDetails<16> = R.NEW(EB.CUS.MNEMONIC)
    iCusTxnDetails<17> = R.NEW(EB.CUS.REL.CUSTOMER)
    iCusTxnDetails<18> = R.NEW(EB.CUS.SECTOR)
    iCusTxnDetails<19> = R.NEW(EB.CUS.TARGET)
    iCusTxnDetails<20> = R.NEW(EB.CUS.CUSTOMER.STATUS)

    CHNGE.DATE.LIST = R.NEW(EB.CUS.CHANGE.DATE)
    CONVERT VM TO " " IN CHNGE.DATE.LIST
*    iCusTxnDetails<CusTxnDetails.changeDate> = CHNGE.DATE.LIST
    iCusTxnDetails<21> = CHNGE.DATE.LIST
*
    CHNGE.RSN.LIST = R.NEW(EB.CUS.CHANGE.REASON)
    CONVERT VM TO " " IN CHNGE.RSN.LIST
*    iCusTxnDetails<CusTxnDetails.changeReason> = CHNGE.RSN.LIST
    iCusTxnDetails<22> = CHNGE.RSN.LIST

*
*    iCusTxnDetails<CusTxnDetails.customerSince> = R.NEW(EB.CUS.CUSTOMER.SINCE)
    iCusTxnDetails<23> = R.NEW(EB.CUS.CUSTOMER.SINCE)    
*
    LEGAL.LIST = R.NEW(EB.CUS.LEGAL.ID)
    CONVERT VM TO " " IN LEGAL.LIST
*    iCusTxnDetails<CusTxnDetails.legalId> = LEGAL.LIST
    iCusTxnDetails<24> = LEGAL.LIST
*
    LEGAL.DOC.LIST = R.NEW(EB.CUS.LEGAL.DOC.NAME)
    CONVERT VM TO " " IN LEGAL.DOC.LIST
*    iCusTxnDetails<CusTxnDetails.legalDocName> = LEGAL.DOC.LIST
	iCusTxnDetails<25> = LEGAL.DOC.LIST    
*
	Y.FLD = "LF.NOB.NIT"
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FLD,Y.POS)
    Y.CHANNEL.POS = Y.POS<1,1>
	
    ;*LEGAL.HOLDER.LIST = R.NEW(EB.CUS.LOCAL.REF)<1, Y.CHANNEL.POS>;*LF.NOB.NIT
    LEGAL.HOLDER.LIST = Y.NAME
    CONVERT VM TO " " IN LEGAL.HOLDER.LIST
*    iCusTxnDetails<CusTxnDetails.legalHolderName> = LEGAL.HOLDER.LIST
    iCusTxnDetails<26> = LEGAL.HOLDER.LIST
	STR.DATO :="iCusTxnDetails<26>: ":iCusTxnDetails<26>:"*"
*
    LEGAL.ISS.LIST = R.NEW(EB.CUS.LEGAL.ISS.AUTH)
    CONVERT VM TO " " IN LEGAL.ISS.LIST
*    iCusTxnDetails<CusTxnDetails.legalIssAuth> = LEGAL.ISS.LIST
    iCusTxnDetails<27> = LEGAL.ISS.LIST
*
    LEGAL.ISS.DATE.LIST = R.NEW(EB.CUS.LEGAL.ISS.DATE)
    CONVERT VM TO " " IN LEGAL.ISS.DATE.LIST
*    iCusTxnDetails<CusTxnDetails.legalIssDate> = LEGAL.ISS.DATE.LIST
    iCusTxnDetails<28> = LEGAL.ISS.DATE.LIST
*
*    iCusTxnDetails<CusTxnDetails.noOfDependents> = R.NEW(EB.CUS.NO.OF.DEPENDENTS)
    iCusTxnDetails<29> = R.NEW(EB.CUS.NO.OF.DEPENDENTS)
*
    SPOKEN.LIST = R.NEW(EB.CUS.SPOKEN.LANGUAGE)
    CONVERT VM TO " " IN SPOKEN.LIST
*    iCusTxnDetails<CusTxnDetails.spokenLanguage> = SPOKEN.LIST
    iCusTxnDetails<30> = SPOKEN.LIST
*
    JOB.LIST = R.NEW(EB.CUS.JOB.TITLE)
    CONVERT VM TO " " IN JOB.LIST
*    iCusTxnDetails<CusTxnDetails.jobTitle> = JOB.LIST
    iCusTxnDetails<31> = JOB.LIST    
*
*    iCusTxnDetails<CusTxnDetails.introducer> = R.NEW(EB.CUS.INTRODUCER)
    iCusTxnDetails<32> = R.NEW(EB.CUS.INTRODUCER)    
*
    EMP.LIST = R.NEW(EB.CUS.EMPLOYERS.NAME)
    CONVERT VM TO " " IN EMP.LIST
*    iCusTxnDetails<CusTxnDetails.employersName> = EMP.LIST
    iCusTxnDetails<33> = EMP.LIST
*
    EMP.ADD.LIST = R.NEW(EB.CUS.EMPLOYERS.ADD)
    CONVERT SM TO " " IN EMP.ADD.LIST
*    iCusTxnDetails<CusTxnDetails.employersAdd> = EMP.ADD.LIST
    iCusTxnDetails<34> = EMP.ADD.LIST
*
    EMP.BUSS.LIST = R.NEW(EB.CUS.EMPLOYERS.BUSS)
    CONVERT VM TO " " IN EMP.BUSS.LIST
*    iCusTxnDetails<CusTxnDetails.employersBuss> = EMP.BUSS.LIST
    iCusTxnDetails<35> = EMP.BUSS.LIST
*
    RES.LIST = R.NEW(EB.CUS.RESIDENCE.STATUS)
    CONVERT VM TO " " IN RES.LIST
*    iCusTxnDetails<CusTxnDetails.residenceStatus> = RES.LIST
    iCusTxnDetails<36> = RES.LIST
*
    RES.TYPE = R.NEW(EB.CUS.RESIDENCE.TYPE)
    CONVERT VM TO " " IN RES.TYPE
*    iCusTxnDetails<CusTxnDetails.residenceType> = RES.TYPE
    iCusTxnDetails<37> = RES.TYPE
*
    RES.SINCE = R.NEW(EB.CUS.RESIDENCE.SINCE)
    CONVERT VM TO " " IN RES.SINCE
*    iCusTxnDetails<CusTxnDetails.residenceSince> = RES.SINCE
    iCusTxnDetails<38> = RES.SINCE
*
    RES.VALUE = R.NEW(EB.CUS.RESIDENCE.VALUE)
    CONVERT VM TO " " IN RES.VALUE
*    iCusTxnDetails<CusTxnDetails.residenceValue> = RES.VALUE
    iCusTxnDetails<39> = RES.VALUE
*
*    iCusTxnDetails<CusTxnDetails.dateOfBirth> = R.NEW(EB.CUS.DATE.OF.BIRTH)
    iCusTxnDetails<40> = R.NEW(EB.CUS.DATE.OF.BIRTH)
*
    OTH.LIST = R.NEW(EB.CUS.OTHER.OFFICER)
    CONVERT VM TO " " IN OTH.LIST
*    iCusTxnDetails<CusTxnDetails.otherOfficer> = OTH.LIST
    iCusTxnDetails<41> = OTH.LIST
*
*    iCusTxnDetails<CusTxnDetails.title> = R.NEW(EB.CUS.TITLE)
**
*    iCusTxnDetails<CusTxnDetails.givenNames> = R.NEW(EB.CUS.GIVEN.NAMES)
**
*    iCusTxnDetails<CusTxnDetails.familyName> = R.NEW(EB.CUS.FAMILY.NAME)
**
*    iCusTxnDetails<CusTxnDetails.gender> = R.NEW(EB.CUS.GENDER)
**
*    iCusTxnDetails<CusTxnDetails.maritalStatus> = R.NEW(EB.CUS.MARITAL.STATUS)
*
    iCusTxnDetails<42> = R.NEW(EB.CUS.TITLE)
    iCusTxnDetails<43> = R.NEW(EB.CUS.GIVEN.NAMES)
    iCusTxnDetails<44> = R.NEW(EB.CUS.FAMILY.NAME)
    iCusTxnDetails<45> = R.NEW(EB.CUS.GENDER)
    iCusTxnDetails<46> = R.NEW(EB.CUS.MARITAL.STATUS)


    OCC.LIST = R.NEW(EB.CUS.OCCUPATION)
    CONVERT VM TO " " IN OCC.LIST
*    iCusTxnDetails<CusTxnDetails.occupation> = OCC.LIST
    iCusTxnDetails<47> = OCC.LIST
*
*    iCusTxnDetails<CusTxnDetails.domicile> = R.NEW(EB.CUS.DOMICILE)
    iCusTxnDetails<48> = R.NEW(EB.CUS.DOMICILE)
*
    RAT.LIST = R.NEW(EB.CUS.CUSTOMER.RATING)
    CONVERT VM TO " " IN RAT.LIST
*    iCusTxnDetails<CusTxnDetails.customerRating> = RAT.LIST
    iCusTxnDetails<49> = RAT.LIST
*
*    iCusTxnDetails<CusTxnDetails.id> = ID.NEW
    iCusTxnDetails<50> = ID.NEW
*
*   iCusTxnDetails<CusTxnDetails.userDefinedFields> = R.NEW(Local ref field)
*
    RETURN
********************************************************************
CALL.SERVICE.RTN.OVERRIDE:
    iCusTxnDetails = LOWER(iCusTxnDetails)

    CALL AMLService.doCustomerScreening(iCusTxnDetails)

	GOSUB REGISTRO
*    TEXT  = "VL-VL.CONT.SENT.AML.REME"
    TEXT  = "VL-VL.CONT.SENT.AML"
    CALL STORE.OVERRIDE(CURR.NO)

RETURN
*-----------------------------------------------------------------------------
LOGGER:
*-----------------------------------------------------------------------------
	FN.GBL       = 'F.EB.SLV.GLOBAL.PARAM'
	F.GBL        = ''
	CALL OPF(FN.GBL, F.GBL)
	
	;*Extraer Parametrizacion de Log
	CALL F.READ(FN.GBL, 'LOG.REMESA', R.GBL, F.GBL, E.GBL)
	Y.DIR.NAME = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',1)	;*Ruta de Salida del Archivo
	R.ID       = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',3) : '.' : TODAY : '.log'	;*Nombre de Archivo Log
	LOG.ACTIVO = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',2)	;*Bandera de Log Activo/Inactivo
RETURN
*-----------------------------------------------------------------------------
ESCRIBIR.ARCHIVO:
*-----------------------------------------------------------------------------
	;*Si el parametro de Log esta Activo Escribir Archivo
	IF LOG.ACTIVO EQ 'Y' THEN
		OPENSEQ Y.DIR.NAME,R.ID TO SEQ.PTR
		WRITESEQ '[' : LEFT(TIMEDATE(),8) : ']  ' : TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
		END
		CLOSESEQ SEQ.PTR
	END
RETURN
************************************************************************
REGISTRO:
     ;*Para generar archivo csv
	 edLine  ='|'
     DIR.NAME='SIF.OUT'
     NAME.FILE='RegistroAml.txt'
     NAME.FILE2='RegistroAml2.txt'
		;* Eliminando archivo existente
		;*------------------------------
	    DELETESEQ DIR.NAME,NAME.FILE THEN
	    END
		;* Abriendo archivo para escritura
		;*---------------------------------
	    OPENSEQ DIR.NAME,NAME.FILE TO SEQ.PTR THEN
	        WEOFSEQ NAME.FILE
	    END
	    WRITESEQ iCusTxnDetails APPEND TO SEQ.PTR THEN
	    END
	    OPENSEQ DIR.NAME,NAME.FILE2 TO SEQ.PTR THEN
	        WEOFSEQ NAME.FILE2
	    END
	    WRITESEQ STR.DATO APPEND TO SEQ.PTR THEN
	    END

	    CLOSESEQ SEQ.PTR
	    	RETURN
	    END
RETURN	    
*-----------------------------------------------------------------------------
LOGGER:
*-----------------------------------------------------------------------------
	FN.GBL       = 'F.EB.SLV.GLOBAL.PARAM'
	F.GBL        = ''
	CALL OPF(FN.GBL, F.GBL)
	
	;*Extraer Parametrizacion de Log
	CALL F.READ(FN.GBL, 'LOG.REMESA', R.GBL, F.GBL, E.GBL)
	DIR.NAME   = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',1)	;*Ruta de Salida del Archivo
	R.ID       = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',3) : '.' : TODAY : '.log'	;*Nombre de Archivo Log
	LOG.ACTIVO = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',2)	;*Bandera de Log Activo/Inactivo
RETURN
*-----------------------------------------------------------------------------
ESCRIBIR.ARCHIVO:
*-----------------------------------------------------------------------------
	;*Si el parametro de Log esta Activo Escribir Archivo
	IF LOG.ACTIVO EQ 'Y' THEN
		OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
		WRITESEQ '[':DATE():'-' : LEFT(TIMEDATE(),8) : ']  ' : TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
		END
		CLOSESEQ SEQ.PTR
	END
RETURN

END
