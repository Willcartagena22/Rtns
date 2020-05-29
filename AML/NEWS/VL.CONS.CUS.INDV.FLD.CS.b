*-----------------------------------------------------------------------------
* <Rating>52</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE VL.CONS.CUS.INDV.FLD.CS(CUST)
*-----------------------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
$INSERT I_AMLService_CusTxnDetails
$INSERT I_F.EB.SLV.GLOBAL.PARAM
*---------PENDIENTE OBTENER ARCHIVO I_AMLService_CusTxnDetails
****************************************************************************
    GOSUB INITIALIZATION
    GOSUB PROCESS
    GOSUB CALL.SERVICE.RTN.OVERRIDE
RETURN

INITIALIZATION:

    CURR.NO = ''
    iCusTxnDetails = ''
	
    Y.APPL    = "CUSTOMER"
    Y.APPLFLD = ""
    Y.POS     = ""
    FN.CUST				='F.CUSTOMER'
    F.CUST			=''

    CALL OPF(FN.CUST, F.CUST)
RETURN
*
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
PROCESS:
	TEXTO.ARCHIVO='INICIA.PROCESS 2'
	GOSUB ESCRIBIR.ARCHIVO
*-----------------------------------------------------------------------------
CUSTOMER=CUST
	CALL F.READ(FN.CUST, CUSTOMER, R.CUS, F.CUST, F.ERR.CUS)
	TEXTO.ARCHIVO='R.CUS':R.CUS
	GOSUB ESCRIBIR.ARCHIVO
*-----------------------------------------------------------------------------
	Y.SHORT.NAME = R.CUS<EB.CUS.SHORT.NAME>
	Y.NAME       = TRIM(R.CUS<EB.CUS.NAME.1>:" ":R.CUS<EB.CUS.NAME.2>:" ":R.CUS<EB.CUS.GIVEN.NAMES>:" ":R.CUS<EB.CUS.TEXT>:" ":R.CUS<EB.CUS.FAMILY.NAME>)
	SHORT.NAME.LIST = Y.SHORT.NAME 
    CONVERT VM TO " " IN SHORT.NAME.LIST
    iCusTxnDetails<1> = SHORT.NAME.LIST
    STR.DATO :="iCusTxnDetails<1>: ":iCusTxnDetails<1>:"*"
    NAME1.LIST = Y.NAME
    CONVERT VM TO " " IN NAME1.LIST
    iCusTxnDetails<2> = NAME1.LIST
    STR.DATO :="iCusTxnDetails<2>: ":iCusTxnDetails<2>:"*"
    NAME2.LIST = Y.NAME
    CONVERT VM TO " " IN NAME2.LIST
    iCusTxnDetails<3> = NAME2.LIST
	STR.DATO :="iCusTxnDetails<3>: ":iCusTxnDetails<3>:"*"    
    TOWN.LIST = R.CUS<EB.CUS.TOWN.COUNTRY>
    CONVERT VM TO " " IN TOWN.LIST
    iCusTxnDetails<5> = TOWN.LIST
    POST.LIST = R.CUS<EB.CUS.POST.CODE>
    CONVERT VM TO " " IN POST.LIST
    iCusTxnDetails<6> = POST.LIST
    CNTRY.LIST = R.CUS<EB.CUS.COUNTRY>
    CONVERT VM TO " " IN CNTRY.LIST
    iCusTxnDetails<7> = CNTRY.LIST
    iCusTxnDetails<8> = R.CUS<EB.CUS.ACCOUNT.OFFICER>
    iCusTxnDetails<9> = R.CUS<EB.CUS.INDUSTRY>
    iCusTxnDetails<10> = R.CUS<EB.CUS.NATIONALITY>
    iCusTxnDetails<11> = R.CUS<EB.CUS.RESIDENCE>
    iCusTxnDetails<12> = R.CUS<EB.CUS.LANGUAGE>
    PHN.LIST = R.CUS<EB.CUS.PHONE.1>
    CONVERT VM TO " " IN PHN.LIST
    iCusTxnDetails<13> = PHN.LIST
    SMS.LIST = R.CUS<EB.CUS.SMS.1>
    CONVERT VM TO " " IN SMS.LIST
    iCusTxnDetails<14> = SMS.LIST
    EMAIL.LIST = R.CUS<EB.CUS.EMAIL.1>
    CONVERT VM TO " " IN EMAIL.LIST
    iCusTxnDetails<15> = EMAIL.LIST
    iCusTxnDetails<16> = R.CUS<EB.CUS.MNEMONIC>
    iCusTxnDetails<17> = R.CUS<EB.CUS.REL.CUSTOMER>
    iCusTxnDetails<18> = R.CUS<EB.CUS.SECTOR>
    iCusTxnDetails<19> = R.CUS<EB.CUS.TARGET>
    iCusTxnDetails<20> = R.CUS<EB.CUS.CUSTOMER.STATUS>
    CHNGE.DATE.LIST = R.CUS<EB.CUS.CHANGE.DATE>
    CONVERT VM TO " " IN CHNGE.DATE.LIST
    iCusTxnDetails<21> = CHNGE.DATE.LIST
    CHNGE.RSN.LIST = R.CUS<EB.CUS.CHANGE.REASON>
    CONVERT VM TO " " IN CHNGE.RSN.LIST
    iCusTxnDetails<22> = CHNGE.RSN.LIST
    iCusTxnDetails<23> = R.CUS<EB.CUS.CUSTOMER.SINCE>
    LEGAL.LIST = R.CUS<EB.CUS.LEGAL.ID>
    CONVERT VM TO " " IN LEGAL.LIST
    iCusTxnDetails<24> = LEGAL.LIST
    LEGAL.DOC.LIST = R.CUS<EB.CUS.LEGAL.DOC.NAME>
    CONVERT VM TO " " IN LEGAL.DOC.LIST
	iCusTxnDetails<25> = LEGAL.DOC.LIST    
	Y.FLD = "LF.NOB.NIT"
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FLD,Y.POS)
    Y.CHANNEL.POS = Y.POS<1,1>
	LEGAL.HOLDER.LIST = Y.NAME
    CONVERT VM TO " " IN LEGAL.HOLDER.LIST
    iCusTxnDetails<26> = LEGAL.HOLDER.LIST
	STR.DATO :="iCusTxnDetails<26>: ":iCusTxnDetails<26>:"*"
    LEGAL.ISS.LIST = R.CUS<EB.CUS.LEGAL.ISS.AUTH>
    CONVERT VM TO " " IN LEGAL.ISS.LIST
    iCusTxnDetails<27> = LEGAL.ISS.LIST
    LEGAL.ISS.DATE.LIST = R.CUS<EB.CUS.LEGAL.ISS.DATE>
    CONVERT VM TO " " IN LEGAL.ISS.DATE.LIST
    iCusTxnDetails<28> = LEGAL.ISS.DATE.LIST
    iCusTxnDetails<29> = R.CUS<EB.CUS.NO.OF.DEPENDENTS>
    SPOKEN.LIST = R.CUS<EB.CUS.SPOKEN.LANGUAGE>
    CONVERT VM TO " " IN SPOKEN.LIST
    iCusTxnDetails<30> = SPOKEN.LIST
    JOB.LIST = R.CUS<EB.CUS.JOB.TITLE>
    CONVERT VM TO " " IN JOB.LIST
    iCusTxnDetails<31> = JOB.LIST    
    iCusTxnDetails<32> = R.CUS<EB.CUS.INTRODUCER>    
    EMP.LIST = R.CUS<EB.CUS.EMPLOYERS.NAME>
    CONVERT VM TO " " IN EMP.LIST
    iCusTxnDetails<33> = EMP.LIST
    EMP.ADD.LIST = R.CUS<EB.CUS.EMPLOYERS.ADD>
    CONVERT SM TO " " IN EMP.ADD.LIST
    iCusTxnDetails<34> = EMP.ADD.LIST
    EMP.BUSS.LIST = R.CUS<EB.CUS.EMPLOYERS.BUSS>
    CONVERT VM TO " " IN EMP.BUSS.LIST
    iCusTxnDetails<35> = EMP.BUSS.LIST
    RES.LIST = R.CUS<EB.CUS.RESIDENCE.STATUS>
    CONVERT VM TO " " IN RES.LIST
    iCusTxnDetails<36> = RES.LIST
    RES.TYPE = R.CUS<EB.CUS.RESIDENCE.TYPE>
    CONVERT VM TO " " IN RES.TYPE
    iCusTxnDetails<37> = RES.TYPE
    RES.SINCE = R.CUS<EB.CUS.RESIDENCE.SINCE>
    CONVERT VM TO " " IN RES.SINCE
    iCusTxnDetails<38> = RES.SINCE
    RES.VALUE = R.CUS<EB.CUS.RESIDENCE.VALUE>
    CONVERT VM TO " " IN RES.VALUE
    iCusTxnDetails<39> = RES.VALUE
    iCusTxnDetails<40> = R.CUS<EB.CUS.DATE.OF.BIRTH>
    OTH.LIST = R.CUS<EB.CUS.OTHER.OFFICER>
    CONVERT VM TO " " IN OTH.LIST
    iCusTxnDetails<41> = OTH.LIST
    iCusTxnDetails<42> = R.CUS<EB.CUS.TITLE>
    iCusTxnDetails<43> = R.CUS<EB.CUS.GIVEN.NAMES>
    iCusTxnDetails<44> = R.CUS<EB.CUS.FAMILY.NAME>
    iCusTxnDetails<45> = R.CUS<EB.CUS.GENDER>
    iCusTxnDetails<46> = R.CUS<EB.CUS.MARITAL.STATUS>
    OCC.LIST = R.CUS<EB.CUS.OCCUPATION>
    CONVERT VM TO " " IN OCC.LIST
    iCusTxnDetails<47> = OCC.LIST
    iCusTxnDetails<48> = R.CUS<EB.CUS.DOMICILE>
    RAT.LIST = R.CUS<EB.CUS.CUSTOMER.RATING>
    CONVERT VM TO " " IN RAT.LIST
    iCusTxnDetails<49> = RAT.LIST
    iCusTxnDetails<50> = CUSTOMER
    RETURN

CALL.SERVICE.RTN.OVERRIDE:
    iCusTxnDetails = LOWER(iCusTxnDetails)
	TEXTO.ARCHIVO=iCusTxnDetails
	GOSUB ESCRIBIR.ARCHIVO
	
    CALL AMLService.doCustomerScreening(iCusTxnDetails)
	GOSUB REGISTRO

RETURN


ESCRIBIR.ARCHIVO:
	    DIR.NAME= 'MASIVOS'
	    R.ID   = 'Cuentas.txt'
	    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
	    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
	    END
	    CLOSESEQ SEQ.PTR
    RETURN
    
    
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


END
