*-----------------------------------------------------------------------------
* <Rating>152</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.I.OFS.IM.UPLOAD.MAS
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.ITEMS.MASIVOS
    $INSERT I_F.IM.DOCUMENT.IMAGE
	$INSERT I_F.AA.ARRANGEMENT
*-----------------------------------------------------------------------------
    GOSUB INICIAR
    GOSUB ABRIR
    GOSUB GENERAR.OFS


INICIAR:
    FN.EB.SLV.VALIDACIONES.MASIVOS='F.EB.SLV.VALIDACIONES.MASIVOS'
    F.EB.SLV.VALIDACIONES.MASIVOS=''
    FN.EB.SLV.MASTER.MASIVOS='F.EB.SLV.MASTER.MASIVOS'
    F.EB.SLV.MASTER.MASIVOS=''
    FN.EB.SLV.ITEMS.MASIVOS='F.EB.SLV.ITEMS.MASIVOS'
    F.EB.SLV.ITEMS.MASIVOS=''
    FN.ARR 		= 'F.AA.ARRANGEMENT'
    F.ARR  		= ''
    RETURN

ABRIR:
    CALL OPF(FN.EB.SLV.ITEMS.MASIVOS, F.EB.SLV.ITEMS.MASIVOS)
    RETURN



GENERAR.OFS:
	SHORT.DECS=R.NEW(IM.DOC.SHORT.DESCRIPTION)
	DESCRIPT=R.NEW(IM.DOC.DESCRIPTION)
	UPLOAD.ID=ID.NEW
	
*	SHORT.DECS='1554680830819.18725124386403100~CC'
*	DESCRIPT='CONTRATO CUENTA'
*	UPLOAD.ID='IM19651950'
	
	IF DESCRIPT EQ 'ENTREVISTA Y PERFIL CLIENTE' OR DESCRIPT EQ 'DECLARACION JURADA' OR DESCRIPT EQ 'CONTRATO CUENTA' THEN
	SHORT.DECS='BKML':SHORT.DECS
 
    S = CHANGE(SHORT.DECS,'~',VM)
    ID.ITEM = FIELD(S,VM,1)
    DOC=''
    DOC = FIELD(S,VM,2)
    OFS.CLI=''
    ID.CARGA = FIELD(ID.ITEM,'.',1)  
    
    CALL F.READ(FN.EB.SLV.ITEMS.MASIVOS,ID.ITEM,RS.ITEM,F.EB.SLV.ITEMS.MASIVOS,ITEM.ERR)
    IF RS.ITEM<EB.SLV3.ID.CUSTOMER> THEN
        CUSTOMER=RS.ITEM<EB.SLV3.ID.CUSTOMER>
        DUI=RS.ITEM<EB.SLV3.LF.DUI>
        NIT=RS.ITEM<EB.SLV3.LF.NIT>
        S.ARR.ID=RS.ITEM<EB.SLV3.ACCOUNT>
        DOC2=RS.ITEM<EB.SLV3.LEGAL.ID.2>
        CALL F.READ(FN.ARR, S.ARR.ID, R.ARR, F.ARR, Y.ERR.ARR)
        CUENTA=R.ARR<AA.ARR.LINKED.APPL.ID>
        		
		IF DOC EQ 'EP' THEN
			DESCRIPT=CUSTOMER:'.pdf'
			GOSUB OFS.IMAGE
	    	OFS.CLI=''
    	
		END
		IF DOC EQ 'DJ' THEN
			DESCRIPT=CUENTA:'.pdf'
			GOSUB OFS.IMAGE
	    	OFS.CLI=''		
		END
*		IF DOC EQ 'CC' THEN
*			DESCRIPT='CTMSV-': ID.CARGA:'-':CUSTOMER:'.pdf'
*			GOSUB OFS.IMAGE
*	    	OFS.CLI=''		
*		END
		
	
	END

		
	END
	ELSE
	DESCRIPT='BKML':DESCRIPT 
    S = CHANGE(DESCRIPT,'~',VM)
    ID.ITEM = FIELD(S,VM,1)
    DOC = FIELD(S,VM,2)
    OFS.CLI=''

    ID.CARGA = FIELD(ID.ITEM,'.',1)  
    
    CALL F.READ(FN.EB.SLV.ITEMS.MASIVOS,ID.ITEM,RS.ITEM,F.EB.SLV.ITEMS.MASIVOS,ITEM.ERR)
    IF RS.ITEM<EB.SLV3.ID.CUSTOMER> THEN
        CUSTOMER=RS.ITEM<EB.SLV3.ID.CUSTOMER>
        DUI=RS.ITEM<EB.SLV3.LF.DUI>
        NIT=RS.ITEM<EB.SLV3.LF.NIT>
        S.ARR.ID=RS.ITEM<EB.SLV3.ACCOUNT>
        DOC2=RS.ITEM<EB.SLV3.LEGAL.ID.2>
        CALL F.READ(FN.ARR, S.ARR.ID, R.ARR, F.ARR, Y.ERR.ARR)
        CUENTA=R.ARR<AA.ARR.LINKED.APPL.ID>
        
		IF DOC EQ 'D' THEN
			DESCRIPT=DUI:'.jpg'
			OFS.CLI=''
    		GOSUB OFS.IMAGE
		END
		IF DOC EQ 'C' THEN
			DESCRIPT=DOC2:'.jpg'
			OFS.CLI=''
    		GOSUB OFS.IMAGE
		END
		IF DOC EQ 'P' THEN
			DESCRIPT=DOC2:'.jpg'
			OFS.CLI=''
    		GOSUB OFS.IMAGE
		END
		
		IF DOC EQ 'N' THEN
		    DESCRIPT=NIT :'.jpg'
    		GOSUB OFS.IMAGE
    		OFS.CLI=''
		END

	
	END
	

    
*       DESCRIPT='1554513716418.18723418946971601~EP'


    END

RETURN

OFS.IMAGE:

    OFS.CLI=OFS.CLI :'IM.DOCUMENT.UPLOAD,SLV.SET.DUI.NIT.MAS/I/PROCESS//0,/,':UPLOAD.ID:',UPLOAD.ID:1:1=':UPLOAD.ID:',FILE.UPLOAD:1:1=':DESCRIPT:','
    Y.OPTIONS='SLVOFSPS'
*    CRT OFS.CLI
    CALL OFS.POST.MESSAGE(OFS.CLI,Y.RESPONSE,Y.OPTIONS,TXN.RESULT)
*CRT DUI
    RETURN
    
OFS.DOC:
    OFS.CLI=OFS.CLI :'IM.DOCUMENT.UPLOAD,SLV.SET.DUI.NIT.MAS/I/PROCESS//0,/,':UPLOAD.ID:',UPLOAD.ID:1:1=':UPLOAD.ID:',FILE.UPLOAD:1:1=':DESCRIPT:','
    Y.OPTIONS='SLVOFSPS'
*    CRT OFS.CLI
    
    CALL OFS.POST.MESSAGE(OFS.CLI,Y.RESPONSE,Y.OPTIONS,TXN.RESULT)

    RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'MASIVOS'
    R.ID   = 'DIGITALIZACION.txt'
    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN


    END
