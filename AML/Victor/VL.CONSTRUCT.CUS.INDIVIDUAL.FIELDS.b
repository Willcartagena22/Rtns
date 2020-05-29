*-----------------------------------------------------------------------------
* <Rating>78</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE VL.CONSTRUCT.CUS.INDIVIDUAL.FIELDS
**********************************************************************
* Routine to fetch data from r.new and pass the same as input parameter to
* AMLService.doCustomerScreening which will push the event to outside environment.
*
*******************************************************************************************
* 01/07/13 - Task 718137
*            Validation changed to convert VM and SM to space so that IF can recognize and
*            send the value to Aml engine.
*
* 10/07/13 - Task 724326
*            Data's will be sent to AML watch for screening only if the Fuction is Input.
*
* 28/03/14 - 968674
*            AML Nordea changes.
*
* 29102018	VBURGOS  Se agrega logica usuario externo
*************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_AMLService_CusTxnDetails
*---------PENDIENTE OBTENER ARCHIVO I_AMLService_CusTxnDetails
****************************************************************************
    IF V$FUNCTION NE 'I' AND V$FUNCTION NE 'C' THEN
        RETURN
    END
    
    GOSUB INITIALIZATION
    GOSUB PROCESS
    GOSUB CALL.SERVICE.RTN.OVERRIDE
    RETURN
**********************************************************************
INITIALIZATION:
*
    CURR.NO = ''
    iCusTxnDetails = ''
    Y.APPL = "CUSTOMER"
    Y.APPLFLD = ""
    Y.POS = ""
    Y.APP 		  = APPLICATION
    Y.PGM.VERSION = PGM.VERSION
    Y.VERSION     = Y.APP : Y.PGM.VERSION
    EQU EXT.USER  TO 'CUSTOMER,SLV.CLIENTE.BANCA'
    Y.TXT =  'VERSION=': Y.VERSION
	GOSUB WRITE_LOG_FILE	
    RETURN
*
PROCESS:
*
    SHORT.NAME.LIST = R.NEW(EB.CUS.SHORT.NAME)
    CONVERT VM TO " " IN SHORT.NAME.LIST
    iCusTxnDetails<CusTxnDetails.shortName> = SHORT.NAME.LIST
*
    NAME1.LIST = R.NEW(EB.CUS.NAME.1) : " " : R.NEW(EB.CUS.NAME.2) : " " : R.NEW(EB.CUS.GIVEN.NAMES) : " " : R.NEW(EB.CUS.TEXT) : " " : R.NEW(EB.CUS.FAMILY.NAME)
    CONVERT VM TO " " IN NAME1.LIST
    iCusTxnDetails<CusTxnDetails.name1> = NAME1.LIST
*
    NAME2.LIST = R.NEW(EB.CUS.NAME.1) : " " : R.NEW(EB.CUS.NAME.2) : " " : R.NEW(EB.CUS.GIVEN.NAMES) : " " : R.NEW(EB.CUS.TEXT) : " " : R.NEW(EB.CUS.PREVIOUS.NAME)
    CONVERT VM TO " " IN NAME2.LIST
    iCusTxnDetails<CusTxnDetails.name2> = NAME2.LIST
*

    STREET.LIST = R.NEW(EB.CUS.STREET)
    CONVERT VM TO " " IN STREET.LIST
    
    ;*Validando si es usuario externo
    IF Y.VERSION EQ EXT.USER THEN
		STREET.LIST =  '*EXTERNO*'
	END
	
    Y.TXT =  'STREET =': STREET.LIST 
	GOSUB WRITE_LOG_FILE
	
    iCusTxnDetails<CusTxnDetails.street> = STREET.LIST
*
    TOWN.LIST = R.NEW(EB.CUS.TOWN.COUNTRY)
    CONVERT VM TO " " IN TOWN.LIST
    iCusTxnDetails<CusTxnDetails.townCountry> = TOWN.LIST
*
    POST.LIST = R.NEW(EB.CUS.POST.CODE)
    CONVERT VM TO " " IN POST.LIST
    iCusTxnDetails<CusTxnDetails.postCode> = POST.LIST
*
    CNTRY.LIST = R.NEW(EB.CUS.COUNTRY)
    CONVERT VM TO " " IN CNTRY.LIST
    iCusTxnDetails<CusTxnDetails.country> = CNTRY.LIST
*
    iCusTxnDetails<CusTxnDetails.accountOfficer> = R.NEW(EB.CUS.ACCOUNT.OFFICER)
*
    iCusTxnDetails<CusTxnDetails.industry> = R.NEW(EB.CUS.INDUSTRY)
*
    iCusTxnDetails<CusTxnDetails.nationality> = R.NEW(EB.CUS.NATIONALITY)
*
    iCusTxnDetails<CusTxnDetails.residence> = R.NEW(EB.CUS.RESIDENCE)
*
    iCusTxnDetails<CusTxnDetails.language> = R.NEW(EB.CUS.LANGUAGE)
*
    PHN.LIST = R.NEW(EB.CUS.PHONE.1)
    CONVERT VM TO " " IN PHN.LIST
    iCusTxnDetails<CusTxnDetails.phone1> = PHN.LIST
*
    SMS.LIST = R.NEW(EB.CUS.SMS.1)
    CONVERT VM TO " " IN SMS.LIST
    iCusTxnDetails<CusTxnDetails.sms1> = SMS.LIST
*
    EMAIL.LIST = R.NEW(EB.CUS.EMAIL.1)
    CONVERT VM TO " " IN EMAIL.LIST
    iCusTxnDetails<CusTxnDetails.email1> = EMAIL.LIST
*
    iCusTxnDetails<CusTxnDetails.mnemonic> = R.NEW(EB.CUS.MNEMONIC)
*
    iCusTxnDetails<CusTxnDetails.relCustomer> = R.NEW(EB.CUS.REL.CUSTOMER)
*
    iCusTxnDetails<CusTxnDetails.sector> = R.NEW(EB.CUS.SECTOR)
*
    iCusTxnDetails<CusTxnDetails.target>  = R.NEW(EB.CUS.TARGET)
*
    iCusTxnDetails<CusTxnDetails.customerStatus> = R.NEW(EB.CUS.CUSTOMER.STATUS)
*
    CHNGE.DATE.LIST = R.NEW(EB.CUS.CHANGE.DATE)
    CONVERT VM TO " " IN CHNGE.DATE.LIST
    iCusTxnDetails<CusTxnDetails.changeDate> = CHNGE.DATE.LIST
*
    CHNGE.RSN.LIST = R.NEW(EB.CUS.CHANGE.REASON)
    CONVERT VM TO " " IN CHNGE.RSN.LIST
    iCusTxnDetails<CusTxnDetails.changeReason> = CHNGE.RSN.LIST
*
    iCusTxnDetails<CusTxnDetails.customerSince> = R.NEW(EB.CUS.CUSTOMER.SINCE)
*
    LEGAL.LIST = R.NEW(EB.CUS.LEGAL.ID)
    CONVERT VM TO " " IN LEGAL.LIST
    iCusTxnDetails<CusTxnDetails.legalId> = LEGAL.LIST
*
    LEGAL.DOC.LIST = R.NEW(EB.CUS.LEGAL.DOC.NAME)
    CONVERT VM TO " " IN LEGAL.DOC.LIST
    iCusTxnDetails<CusTxnDetails.legalDocName> = LEGAL.DOC.LIST
*
	Y.FLD = "LF.NOB.NIT"
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FLD,Y.POS)
    Y.CHANNEL.POS = Y.POS<1,1>
	
    LEGAL.HOLDER.LIST = R.NEW(EB.CUS.LOCAL.REF)<1, Y.CHANNEL.POS>;*LF.NOB.NIT
    CONVERT VM TO " " IN LEGAL.HOLDER.LIST
    iCusTxnDetails<CusTxnDetails.legalHolderName> = LEGAL.HOLDER.LIST
*
    LEGAL.ISS.LIST = R.NEW(EB.CUS.LEGAL.ISS.AUTH)
    CONVERT VM TO " " IN LEGAL.ISS.LIST
    iCusTxnDetails<CusTxnDetails.legalIssAuth> = LEGAL.ISS.LIST
*
    LEGAL.ISS.DATE.LIST = R.NEW(EB.CUS.LEGAL.ISS.DATE)
    CONVERT VM TO " " IN LEGAL.ISS.DATE.LIST
    iCusTxnDetails<CusTxnDetails.legalIssDate> = LEGAL.ISS.DATE.LIST
*
    iCusTxnDetails<CusTxnDetails.noOfDependents> = R.NEW(EB.CUS.NO.OF.DEPENDENTS)
*
    SPOKEN.LIST = R.NEW(EB.CUS.SPOKEN.LANGUAGE)
    CONVERT VM TO " " IN SPOKEN.LIST
    iCusTxnDetails<CusTxnDetails.spokenLanguage> = SPOKEN.LIST
*
    JOB.LIST = R.NEW(EB.CUS.JOB.TITLE)
    CONVERT VM TO " " IN JOB.LIST
    iCusTxnDetails<CusTxnDetails.jobTitle> = JOB.LIST
*
    iCusTxnDetails<CusTxnDetails.introducer> = R.NEW(EB.CUS.INTRODUCER)
*
    EMP.LIST = R.NEW(EB.CUS.EMPLOYERS.NAME)
    CONVERT VM TO " " IN EMP.LIST
    iCusTxnDetails<CusTxnDetails.employersName> = EMP.LIST
*
    EMP.ADD.LIST = R.NEW(EB.CUS.EMPLOYERS.ADD)
    CONVERT SM TO " " IN EMP.ADD.LIST
    iCusTxnDetails<CusTxnDetails.employersAdd> = EMP.ADD.LIST
*
    EMP.BUSS.LIST = R.NEW(EB.CUS.EMPLOYERS.BUSS)
    CONVERT VM TO " " IN EMP.BUSS.LIST
    iCusTxnDetails<CusTxnDetails.employersBuss> = EMP.BUSS.LIST
*
    RES.LIST = R.NEW(EB.CUS.RESIDENCE.STATUS)
    CONVERT VM TO " " IN RES.LIST
    iCusTxnDetails<CusTxnDetails.residenceStatus> = RES.LIST
*
    RES.TYPE = R.NEW(EB.CUS.RESIDENCE.TYPE)
    CONVERT VM TO " " IN RES.TYPE
    iCusTxnDetails<CusTxnDetails.residenceType> = RES.TYPE
*
    RES.SINCE = R.NEW(EB.CUS.RESIDENCE.SINCE)
    CONVERT VM TO " " IN RES.SINCE
    iCusTxnDetails<CusTxnDetails.residenceSince> = RES.SINCE
*
    RES.VALUE = R.NEW(EB.CUS.RESIDENCE.VALUE)
    CONVERT VM TO " " IN RES.VALUE
    iCusTxnDetails<CusTxnDetails.residenceValue> = RES.VALUE
*
    iCusTxnDetails<CusTxnDetails.dateOfBirth> = R.NEW(EB.CUS.DATE.OF.BIRTH)
*
    OTH.LIST = R.NEW(EB.CUS.OTHER.OFFICER)
    CONVERT VM TO " " IN OTH.LIST
    iCusTxnDetails<CusTxnDetails.otherOfficer> = OTH.LIST
*
    iCusTxnDetails<CusTxnDetails.title> = R.NEW(EB.CUS.TITLE)
*
    iCusTxnDetails<CusTxnDetails.givenNames> = R.NEW(EB.CUS.GIVEN.NAMES)
*
    iCusTxnDetails<CusTxnDetails.familyName> = R.NEW(EB.CUS.FAMILY.NAME)
*
    iCusTxnDetails<CusTxnDetails.gender> = R.NEW(EB.CUS.GENDER)
*
    iCusTxnDetails<CusTxnDetails.maritalStatus> = R.NEW(EB.CUS.MARITAL.STATUS)
*
    OCC.LIST = R.NEW(EB.CUS.OCCUPATION)
    CONVERT VM TO " " IN OCC.LIST
    iCusTxnDetails<CusTxnDetails.occupation> = OCC.LIST
*
    iCusTxnDetails<CusTxnDetails.domicile> = R.NEW(EB.CUS.DOMICILE)
* 
    RAT.LIST = R.NEW(EB.CUS.CUSTOMER.RATING)
    CONVERT VM TO " " IN RAT.LIST
    iCusTxnDetails<CusTxnDetails.customerRating> = RAT.LIST
*
    iCusTxnDetails<CusTxnDetails.id> = ID.NEW
    Y.TXT =  'CusTxnDetails.id =': ID.NEW
	GOSUB WRITE_LOG_FILE
*
*   iCusTxnDetails<CusTxnDetails.userDefinedFields> = R.NEW(Local ref field)
*
    RETURN
********************************************************************
CALL.SERVICE.RTN.OVERRIDE:
*
    iCusTxnDetails = LOWER(iCusTxnDetails)
	 Y.TXT =  'iCusTxnDetails =': iCusTxnDetails
	GOSUB WRITE_LOG_FILE
    CALL AMLService.doCustomerScreening(iCusTxnDetails)
*
    TEXT = "VL-VL.CONT.SENT.AML"
    CALL STORE.OVERRIDE(CURR.NO)
*
    RETURN
************************************************************************
WRITE_LOG_FILE:
		DIR.NAME = 'SIF.OUT'
		R.ID = 'LOG.AML.FLAG' : '_' : TODAY : '.txt'
		OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
			WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
			END
		CLOSESEQ SEQ.PTR 
	RETURN	


END

