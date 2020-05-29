*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.NOF.REPORTE.CARGA(A.INFO)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.ITEMS.MASIVOS
$INSERT I_ENQUIRY.COMMON
*-----------------------------------------------------------------------------

GOSUB INICIAR
GOSUB ABRIR
GOSUB PROCESAR
INICIAR:
    FN.EB.SLV.ITEMS.MASIVOS='F.EB.SLV.ITEMS.MASIVOS'
    F.EB.SLV.ITEMS.MASIVOS=''
    RETURN
    
    
ABRIR:
    LOCATE 'ID.CARGA' IN D.FIELDS<1> SETTING ITEM.POS THEN
    ID.CARGA = D.RANGE.AND.VALUE<ITEM.POS>
    END
    LOCATE 'ESTADO1' IN D.FIELDS<1> SETTING ITEM.POS2 THEN
    ESTADO1 = D.RANGE.AND.VALUE<ITEM.POS2>
    END
    CALL OPF(FN.EB.SLV.ITEMS.MASIVOS,F.EB.SLV.ITEMS.MASIVOS)


RETURN

PROCESAR:
CONT.ACC =0
CONT.CLI= 0
*ID.CARGA='BKML1554241192318'
*ESTADO1='Rechazado'
IF ESTADO1 THEN
SELECT.ITEMS= "SELECT " :FN.EB.SLV.ITEMS.MASIVOS:" WITH @ID LIKE %":ID.CARGA:"%" :" AND RESERVADO.1 EQ ":"'":ESTADO1:"'"
END
ELSE 
SELECT.ITEMS= "SELECT " :FN.EB.SLV.ITEMS.MASIVOS:" WITH @ID LIKE %":ID.CARGA:"%";* :" AND RESERVADO.1 EQ ":"'":ESTADO1:"'"
END
    CALL EB.READLIST(SELECT.ITEMS, ID.ITEM, '' , NO.OF.RECS.ITEMS, ERR.ITEMS)
    IF NO.OF.RECS.ITEMS NE 0 THEN
        FOR H = 1 TO NO.OF.RECS.ITEMS
            CALL F.READ(FN.EB.SLV.ITEMS.MASIVOS,ID.ITEM<H>, LISTA.ITEMS,F.EB.SLV.ITEMS.MASIVOS, ERR.ITEMS.LS)
            P.ID= ID.ITEM<H>
            NOMBRE    = LISTA.ITEMS<EB.SLV3.NAME.1>
            NOMBRE2		   = LISTA.ITEMS<EB.SLV3.NAME.2>
            APELLIDO=LISTA.ITEMS<EB.SLV3.TEXT>
            APELLIDO2=LISTA.ITEMS<EB.SLV3.FAMILY.NAME>
            NIT=LISTA.ITEMS<EB.SLV3.LF.NIT>
            DUI=LISTA.ITEMS<EB.SLV3.LF.DUI>
            GENERO=LISTA.ITEMS<EB.SLV3.GENDER>
            CLIENTE=LISTA.ITEMS<EB.SLV3.ID.CUSTOMER>
            CUENTA=LISTA.ITEMS<EB.SLV3.ACCOUNT>
            ESTADO=LISTA.ITEMS<EB.SLV3.RESERVADO.1>
            IF CLIENTE THEN
            CONT.CLI=CONT.CLI+1
            END
            IF CUENTA THEN
            CONT.ACC =CONT.ACC +1
            END
           
            A.INFO<-1>=  P.ID:'*':NOMBRE:'*':NOMBRE2:'*':APELLIDO:'*':APELLIDO2:'*':NIT:'*':DUI:'*':GENERO:'*':CLIENTE:'*':CUENTA:'*':ESTADO  
        
        NEXT H
    END

			A.INFO<-1>="Clientes Creados : ":'*':CONT.CLI:"*":" Cuentas Creadas : ":"*":CONT.ACC:'*':"":'*':"":'*':"":'*':"":'*':"":'*':"":'*':""  
	 CRT A.INFO		
RETURN 
    
    
END
