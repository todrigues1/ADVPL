#Include "Protheus.ch" 
#include "rwmake.ch" 
#Include "TopConn.ch"  
     
/*                                                                        
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³ RelNF ºAutor  Vitor Rodrigues Santos-P2P Data ³  17/09/2019    º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³Geração de relatório de Nota Fiscal em Excel                    º±±
±±º          ³                                                                º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß 
*/
User Function RelNF()  

SetPrvt("_BBLOCO,_DDATAINI,_DDATAFIM,_CCLIINI,_CCLIFIM")

_bBloco   := {|lEnd| GerRel()}
_dDataIni := ctod("")
_dDataFim := ctod("")
_cCliIni  := "      "
_cCliFim  := "      "



@ 96,42 TO 323,505 DIALOG oDlg1 TITLE "Geração de Relatório de Nota Fiscal"
@ 8,10 TO 84,222
@ 91,139 BMPBUTTON TYPE 5 ACTION Perg()
@ 91,168 BMPBUTTON TYPE 1 ACTION msaguarde(_bBloco, "Aguarde", "Processando...", .F.)
@ 91,196 BMPBUTTON TYPE 2 ACTION Close(oDlg1)
@ 23,14 SAY "Este programa tem por finalidade, Gerar um relatório de Nota Fiscal."
@ 33,14 SAY "Utilize o botao de parametros abaixo para selecionar o periodo que deseja visualizar"
ACTIVATE DIALOG oDlg1 CENTER

return        

Static Function Perg()

@ 96,42 TO 323,505 DIALOG oDlg2 TITLE "Parametros"
@ 8,10 TO 84,222
@ 91,139 BUTTON "_Ok" Size 70,20  ACTION close(oDlg2)
@ 23,14 say "Data de Emissão Inicial:"
@ 33,14 say "Data de Emissão Final:"
@ 23,74 Get _dDataIni Size 50, 10 Picture "@D"
@ 33,74 Get _dDataFim Size 50, 10 Picture "@D" valid ! empty(_dDataFim) .or. _dDataFim >= _dDataIni 

@ 43,14 say "Cod. Cliente Inicial:"
@ 53,14 say "Cod. Cliente Final:"
@ 43,74 Get _cCliIni Size 50, 10
@ 53,74 Get _cCliFim Size 50, 10 valid ! empty(_cCliFim) .or. _cCliFim >= _cCliIni

ACTIVATE DIALOG oDlg2 CENTERED  

Return

Static Function GerRel()
    Local cArquivo    := GetTempPath()+'RelNF.xml'
    Local oFWMSEx     := FWMsExcelEx():New()
    Local oExcel   
    Local cQuery  := "" 
    //Local cPlnTit := "Nota Fiscal"
    //Local cTblTit := "RELATÓRIO NOTA FISCAL" 
    
    cQuery := "SELECT F2_DOC,F2_STATUS,F2_EMISSAO,F2_CLIENTE,A1_NOME,F2_VALBRUT,E1_BAIXA, (E1_VALOR-E1_PIS-E1_COFINS-E1_CSLL-E1_ISS-E1_IRRF) AS VALLIQ "   
    cQuery += " FROM " +RetSQLName("SF2")+ " SF2 "
    cQuery += " INNER JOIN " +RetSQLName("SA1")+ " SA1 " 
    cQuery += " ON F2_CLIENTE = A1_COD  " 
    cQuery += " INNER JOIN " +RetSQLName("SE1")+ " SE1 " 
    cQuery += " ON E1_NUM = F2_DOC  "
    cQuery += " WHERE (E1_BAIXA = '' "
    cQuery += " OR (F2_EMISSAO BETWEEN '"+DTOS(_dDataIni)+"' AND '"+DTOS(_dDataFim)+"' "  
    cQuery += " AND F2_CLIENTE BETWEEN '"+_cCliIni+"' AND '"+_cCliFim+"' ))" 
    cQuery += " AND SF2.D_E_L_E_T_ <> '*' "
    TcQuery cQuery New Alias "QRY"
    

           
    oFWMSEx:SetBgGeneralColor("#ffffff")
    oFWMSEx:SetBgColorHeader("#000000")


    oFWMSEx:AddworkSheet("Nota Fiscal")
        //Adicionando a tabela
        oFWMSEx:AddTable("Nota Fiscal","RELATÓRIO NOTA FISCAL") 
        
          //Adicionando as colunas 
            oFWMSEx:AddColumn("Nota Fiscal","RELATÓRIO NOTA FISCAL","NÚMERO",2) 
            oFWMSEx:AddColumn("Nota Fiscal","RELATÓRIO NOTA FISCAL","STATUS",2)
            oFWMSEx:AddColumn("Nota Fiscal","RELATÓRIO NOTA FISCAL","EMISSÃO",2) 
            oFWMSEx:AddColumn("Nota Fiscal","RELATÓRIO NOTA FISCAL","TOMADOR",2) 
            oFWMSEx:AddColumn("Nota Fiscal","RELATÓRIO NOTA FISCAL","VALOR BRUTO",2)
            oFWMSEx:AddColumn("Nota Fiscal","RELATÓRIO NOTA FISCAL","VALOR RECEBIDO",2)
            oFWMSEx:AddColumn("Nota Fiscal","RELATÓRIO NOTA FISCAL","TX. CARTÃO",2)
            oFWMSEx:AddColumn("Nota Fiscal","RELATÓRIO NOTA FISCAL","DATA DO PAGAMENTO",2) 
           
            dbSelectArea("QRY")  
            ("QRY")->(dbGoTop())
            
            While !("QRY")->(EoF())
	            //Adicionando as linhas
	            oFWMSEx:AddRow("Nota Fiscal","RELATÓRIO NOTA FISCAL",;
		            {("QRY")->(F2_DOC),;     //NÚMERO
		             ("QRY")->(F2_STATUS),;  //STATUS
		             DTOC(STOD(("QRY")->(F2_EMISSAO))),; //EMISSÃO   
		             ("QRY")->(A1_NOME),;    //TOMADOR(CLIENTE)
		             ("QRY")->(F2_VALBRUT),; //VALOR BRUTO
		             ("QRY")->(VALLIQ),;     //VALOR RECEBIDO
		             "",;                   //TX. CARTÃO
		             DTOC(STOD(("QRY")->(E1_BAIXA)))})  //DATA DO PAGAMENTO   
	            ("QRY")->(dbSkip())
            EndDo    
                                                                
           ("QRY")->(dbCloseArea())
    //Criando o XML
    oFWMSEx:Activate()
    oFWMSEx:GetXMLFile(cArquivo)
     
    //Abrindo o excel e abrindo o arquivo xml
    oExcel := MsExcel():New()             //Abre uma nova conexão com Excel
    oExcel:WorkBooks:Open(cArquivo)     //Abre uma planilha
    oExcel:SetVisible(.T.)                 //Visualiza a planilha
    oExcel:Destroy()                        //Encerra o processo do gerenciador de tarefas
Return                
