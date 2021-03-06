*-----------------------------------------------------------------------------
* <Rating>425</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.I.OFS.IMAGE.MAS
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.MASTER.MASIVOS
    $INSERT I_F.EB.SLV.ITEMS.MASIVOS
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
    CALL OPF(FN.EB.SLV.MASTER.MASIVOS, F.EB.SLV.MASTER.MASIVOS)
    CALL OPF(FN.EB.SLV.ITEMS.MASIVOS, F.EB.SLV.ITEMS.MASIVOS)
    CALL OPF(FN.ARR, F.ARR)
    RETURN





GENERAR.OFS:
    CREAR=R.NEW(EB.SLV32.RESERVADO.1)
*CREAR='SI'

    IF CREAR='SI' THEN

        ID.CARGA= ID.NEW
*        ID.CARGA='BKML1554680830819'

        STMT.ARRANGEMENT2 = "SELECT ":FN.EB.SLV.ITEMS.MASIVOS:" WITH @ID LIKE ":ID.CARGA:"%"
        CALL EB.READLIST(STMT.ARRANGEMENT2, ARRANGEMENT.LIST2,'',NO.OF.RECS2,Y.ARRANGEMENT.ERR2)

        FOR I=1 TO NO.OF.RECS2
            ID.ITEM = ARRANGEMENT.LIST2<I>
            OFS.CLI=''

            ;*OBTENEMOS EL DETALLE DEL PAGO DEL COLECTOR
            CALL F.READ(FN.EB.SLV.ITEMS.MASIVOS,ID.ITEM,RS.ITEM,F.EB.SLV.ITEMS.MASIVOS,ITEM.ERR)
            IF RS.ITEM<EB.SLV3.ID.CUSTOMER> THEN
                CUSTOMER=RS.ITEM<EB.SLV3.ID.CUSTOMER>
                DUI=RS.ITEM<EB.SLV3.LF.DUI>
                DOC=RS.ITEM<EB.SLV3.LEGAL.ID.2>
                NIT=RS.ITEM<EB.SLV3.LF.NIT>
                S.ARR.ID=RS.ITEM<EB.SLV3.ACCOUNT>
				CALL F.READ(FN.ARR, S.ARR.ID, R.ARR, F.ARR, Y.ERR.ARR)	
				CUENTA=R.ARR<AA.ARR.LINKED.APPL.ID>
				NOMDOC=RS.ITEM<EB.SLV3.LEGAL.DOC.NAME.2>
				ID.ITEM2 = FIELD(ID.ITEM,'L',2)
				
				IF NOMDOC EQ 'DOCTO.UNICO.IDENT' THEN
				DOCUMENTO='DUI'
                DESCRIPT=ID.ITEM2:'~D'
                ARCHIVO=DUI:'.jpg'                
                GOSUB MOVER.IM
                GOSUB OFS.IMAGE
                OFS.CLI=''
				END
				
				IF NOMDOC EQ 'CARNET.RESIDENTE' THEN
				DOCUMENTO='CARNET DE RESIDENTE'
                DESCRIPT=ID.ITEM2:'~C'
                ARCHIVO=DOC:'.jpg'                
                GOSUB MOVER.IM
                GOSUB OFS.IMAGE
                OFS.CLI=''
				END
				IF NOMDOC EQ 'PASSPORT' THEN
				DOCUMENTO='PASAPORTE'
                DESCRIPT=ID.ITEM2:'~P'
                ARCHIVO=DOC:'.jpg'                
                GOSUB MOVER.IM
                GOSUB OFS.IMAGE
                OFS.CLI=''
				END
               
                
                
                ARCHIVO=NIT :'.jpg'
                DOCUMENTO='NIT'
                GOSUB MOVER.IM
                DESCRIPT=ID.ITEM2:'~N'
                GOSUB OFS.IMAGE
                OFS.CLI=''
                
                
                DOCUMENTO='ENTREVISTA Y PERFIL CLIENTE'
                ARCHIVO=CUSTOMER:'.pdf'
                ;*DECJURPNPRMSV-BKML1552085584443-154097-10000000780039
                DESCRIPT=ID.ITEM2:'~EP'
                GOSUB MOVER.DOC
                GOSUB OFS.DOC
                
                
                OFS.CLI=''
                DOCUMENTO='DECLARACION JURADA'
                ARCHIVO=CUENTA:'.pdf'
                ;*DECJURPNCTMSV-BKML1552085584443-154097-10000000780039
                DESCRIPT=ID.ITEM2:'~DJ'
                GOSUB MOVER.DOC
                GOSUB OFS.DJ
                
				DOCUMENTO=''
				 OFS.CLI=''
*                DOCUMENTO='CONTRATO CUENTA'
*                ;*CONMSV-BKML1552085584z443-154095-10000000780012
*                DESCRIPT=ID.ITEM2:'~CC'
*                ARCHIVO='CTMSV-': ID.CARGA:'-':CUSTOMER:'.pdf'
*                GOSUB MOVER.DOC
*                GOSUB OFS.DOC


            END


        NEXT


    END

    RETURN


OFS.IMAGE:

    OFS.CLI=OFS.CLI :'IM.DOCUMENT.IMAGE,SLV.SET.DUI.MAS/I/PROCESS//0,/,,IMAGE.APPLICATION:1:1=CUSTOMER,IMAGE.REFERENCE:1:1=':CUSTOMER:',SHORT.DESCRIPTION:1:1=':DOCUMENTO:',DESCRIPTION:1:1=':DESCRIPT:','
    Y.OPTIONS='SLVOFSPS'
    CRT OFS.CLI

    CALL OFS.POST.MESSAGE(OFS.CLI,Y.RESPONSE,Y.OPTIONS,TXN.RESULT)
*    TEXTO.ARCHIVO='OFS.IMAGE : ':OFS.CLI
*    GOSUB ESCRIBIR.ARCHIVO

    RETURN

OFS.DOC:

    OFS.CLI=OFS.CLI :'IM.DOCUMENT.IMAGE,SLV.SET.DOC.MAS/I/PROCESS//0,/,,IMAGE.APPLICATION:1:1=CUSTOMER,IMAGE.REFERENCE:1:1=':CUSTOMER:',SHORT.DESCRIPTION:1:1=':DESCRIPT:',DESCRIPTION:1:1=':DOCUMENTO:','
    Y.OPTIONS='SLVOFSPS'

    CALL OFS.POST.MESSAGE(OFS.CLI,Y.RESPONSE,Y.OPTIONS,TXN.RESULT)
*CRT OFS.CLI


    RETURN
    
    
    OFS.DJ:

  OFS.CLI=OFS.CLI :'IM.DOCUMENT.IMAGE,SLV.SET.DOC.MAS/I/PROCESS//0,/,,IMAGE.APPLICATION:1:1=AA.ARRANGEMENT,IMAGE.REFERENCE:1:1=':S.ARR.ID:',SHORT.DESCRIPTION:1:1=':DESCRIPT:',DESCRIPTION:1:1=':DOCUMENTO:','
  Y.OPTIONS='SLVOFSPS'

    CALL OFS.POST.MESSAGE(OFS.CLI,Y.RESPONSE,Y.OPTIONS,TXN.RESULT)
*CRT OFS.CLI


    RETURN


ESCRIBIR.ARCHIVO:
    DIR.NAME= 'MASIVOS'
    R.ID   = 'DIGITALIZACION.txt'
    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
 
    RETURN


MOVER.IM:
    THIS.PACKAGE.CLASS ="filest24.FilesT24";
    THIS.METHOD.CLT= "MoverArchivos"
    RUTA.IN='bnk/UD/im.images/masivostemp/'
    RUTA.IN=RUTA.IN:ID.CARGA:'/':ARCHIVO
    RUTA.OUT='bnk/UD/im.images/photos/':ARCHIVO
    CALLJ.ARGUMENTS.CLT = RUTA.IN:"~":RUTA.OUT
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "

    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA= CALLJ.RESPONSE.CLT

    RETURN

MOVER.DOC:
    THIS.PACKAGE.CLASS ="filest24.FilesT24";
    THIS.METHOD.CLT= "MoverArchivos"
    RUTA.IN='bnk/UD/im.images/masivostemp/'
    RUTA.IN=RUTA.IN:ID.CARGA:'/':ARCHIVO
    RUTA.OUT='bnk/UD/im.images/documents/':ARCHIVO
    CALLJ.ARGUMENTS.CLT = RUTA.IN:"~":RUTA.OUT
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
	CRT CALLJ.ARGUMENTS.CLT
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)

    RETURN
    
BORRAR:
    THIS.PACKAGE.CLASS ="filest24.FilesT24";
    THIS.METHOD.CLT= "LimpiarDirectorio"
    CALLJ.ARGUMENTS.CLT = "C:\\Users\\calvarado\\Desktop\\Proyectos\\Creacion masiva de clientes\\DIGITALIZACION\\CARPETA1\\"
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "

    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA= CALLJ.RESPONSE.CLT

    RETURN




    END
