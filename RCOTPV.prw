#include "rwmake.ch"     
#include "TopConn.ch"    
#Include "AP5MAIL.ch"
#INCLUDE "MSOLE.CH"
//#INCLUDE "GPEWORD.CH"          
#INCLUDE "PROTHEUS.CH"
#INCLUDE "REPORT.CH"

#Include "rptdef.ch"
#Include "FWPrintSetup.ch"


/*                                                                        
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³ RCOTPV ºAutor  Vitor Rodrigues Santos-P2P Data ³  25/08/2020   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³Imprimir Cotação de pedidos de venda Grafico                    º±±
±±º          ³                                                                º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß 
*/

User Function RCOTPV(cAlias, nReg, nOpcx) 

Local cCont := 0
Local cAssunto := ''
Local cCorpo := ''

SetPrvt("CDESC1,CDESC2,CDESC3,_CSTRING,AORD,J")
SetPrvt("oFont1,oFont2,oFont3,oFont4,oFont5,oFont6,oFont7")
Private nPag  := 1
Private nPagd := 0
Private NumPed   := Space(6)
Private cPFornec, cEmailForn, cEmailNome, cFornece, cObsPed, cPedEntr
Private cPerg   := Pad("RCOTPV",10)
Private cMsg, nLinha, nLinhaD, nLinhaO, cObs   
Private oDlg,oGet
Private cGet1 := Space(2)
Private cDest		:= SuperGetMV("FS_EMAILLS",,"")

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Verifica as perguntas selecionadas                           ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Variaveis utilizadas para parametros                         ³
//³ mv_par01	     	  Do Numero                              ³
//³ mv_par02     	  	  Ate o Numero 		                     ³
//³ mv_par03	     	  A partir da Data                       ³
//³ mv_par04              Ate a Data           			     	 ³
//³ mv_par05              Do Cliente                 	   	     ³
//³ mv_par06              Ate o CLiente                          ³
//³ mv_par07              Da Filial                              ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

//ValidPerg()  
            
If !Pergunte(cPerg,.T.)
	Return
EndIF
	
RptStatus({||Relato()})

Return


/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³ Relato ºAutor  Vitor Rodrigues Santos-P2P Data ³ 25/08/2020º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±º          ³                                                            º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function Relato()

Local nOrder
Local cCondBus                                                                            
//Local aSavRec := {}
Local cNumCJ := SCJ->CJ_NUM
Local cCliCJ := SCJ->CJ_CLIENTE
Local nLiHor  := 0
Local ncw := 0
//Local i := 0
Local cCont := 0
Local cAssunto := ''
Local cCorpo := ''
Local cFilePrint := ''

Private lEnc    := .f.
Private cTitulo
Private oFont
Private cCode
Private cArquivo := "ImpCot"
Private cLocal := AllTrim(GetMV("MV_RELT",," "))
Private oPrn := TMSPrinter():New(OemToAnsi('Cotacao de Pedido de Vendas'))//FWMSPrinter():New(cArquivo,IMP_PDF,.T.,,.F.)//
Private cCGCPict, cCepPict    
Private lPrimPag :=.t. 
Private MV_PART1, MV_PART2, MV_PART3, MV_PART4, MV_PART5, MV_PART6, MV_PART7
Private lVerArq  := .F.
Private nValTot  := 0 
Private nTotPec  := 0
Private nTotServ := 0
Private nFrete   := 0
Private aServ := {}

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Definir as pictures                                           ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
cCepPict:=PesqPict("SA2","A2_CEP")
cCGCPict:=PesqPict("SA2","A2_CGC")

oFont1 := TFont():New( "Arial",,16,,.t.,,,,,.f. )
oFont2 := TFont():New( "Arial",,16,,.f.,,,,,.f. )
oFont3 := TFont():New( "Arial",,10,,.t.,,,,,.f. )
oFont4 := TFont():New( "Arial",,10,,.f.,,,,,.f. )
oFont5 := TFont():New( "Arial",,08,,.t.,,,,,.f. )  
oFont6 := TFont():New( "Arial",,08,,.f.,,,,,.f. ) 
oFont7 := TFont():New( "Arial",,14,,.t.,,,,,.f. )  
oFont8 := TFont():New( "Arial",,14,,.f.,,,,,.f. )
oFont9 := TFont():New( "Arial",,12,,.t.,,,,,.f. )  
oFont10:= TFont():New( "Arial",,12,,.f.,,,,,.f. ) 
oFont11:= TFont():New( "Arial",,07,,.t.,,,,,.f. )  
oFont12:= TFont():New( "Arial",,07,,.f.,,,,,.f. )
oFont13:= TFont():New( "Arial",,09,,.t.,,,,,.f. )  
oFont14:= TFont():New( "Arial",,09,,.f.,,,,,.f. )

oFont1c := TFont():New( "Courier New",,16,,.t.,,,,,.f. )
oFont2c := TFont():New( "Courier New",,16,,.f.,,,,,.f. )
oFont3c := TFont():New( "Courier New",,10,,.t.,,,,,.f. )
oFont4c := TFont():New( "Courier New",,10,,.f.,,,,,.f. )
oFont5c := TFont():New( "Courier New",,09,,.t.,,,,,.f. )  
oFont6c := TFont():New( "Courier New",,09,,.T.,,,,,.f. )
oFont7c := TFont():New( "Courier New",,14,,.t.,,,,,.f. )  
oFont8c := TFont():New( "Courier New",,14,,.f.,,,,,.f. )
oFont9c := TFont():New( "Courier New",,12,,.t.,,,,,.f. )  
oFont10c:= TFont():New( "Courier New",,12,,.f.,,,,,.f. ) 
oFont11c:= TFont():New( "Courier New",,07,,.T.,,,,,.f. )

//oPrn:cPathPDF     := '\relato\'//AllTrim(GetMV("MV_RELT",," "))//cPasta

cCondBus := mv_par01
nOrder	:=	1
nPagD:=1   
cObsPed  :=""      
cPedEntr :="" 
nValFrete2:=0     
cObsPed := ""   

If Empty(mv_par01)
	MV_PART1:=cNumCJ
	MV_PART2:=cNumCJ
Else
	MV_PART1:=mv_par01
	MV_PART2:=mv_par02	
Endif

If Empty(mv_par05)
	MV_PART5:=cCliCJ
	MV_PART6:=cCliCJ
Else
	MV_PART5:=mv_par05
	MV_PART6:=mv_par06	
Endif

If Empty(mv_par07)
	MV_PART7:=xFilial("SCJ")
Else
	MV_PART7:=mv_par07
EndIf



	
	dbSelectArea("SCJ")
	SCJ->(dbGoTop())
	SCJ->(dbSetOrder(nOrder))
	//SCJ->(SetRegua(nPagD))
	//dbSeek(MV_PART7+MV_PART1,.T.)
	//SCJ->(dbSeek(MV_PART7+MV_PART1,.T.))
	

	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Faz manualmente porque nao chama a funcao Cabec()                 ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

	
	While SCJ->(!Eof()) 
	
		If SCJ->CJ_FILIAL == MV_PART7 .And. SCJ->CJ_NUM >= MV_PART1 .And. SCJ->CJ_NUM <= MV_PART2 
		
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³ Cria as variaveis para armazenar os valores do pedido        ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			nOrdem   := 1
			nPag     := 1
			lVerArq := .T.
			
			
			If (SCJ->CJ_EMISSAO < mv_par03) .Or. (SCJ->CJ_EMISSAO > mv_par04)
				SCJ->(dbSkip())
				Loop
			Endif 

			If (SCJ->CJ_CLIENTE < mv_par05) .Or. (SCJ->CJ_CLIENTE > mv_par06)
				SCJ->(dbSkip())
				Loop
			Endif 
			
			
			//For ncw := 1 To mv_par09		// Imprime o numero de vias informadas
				
				ImpCabec()
				
				nTotPec  := 0
				nValTot  := 0
				nTotServ := 0
				nFrete   := SCJ->CJ_FRETE
				NumPed   := SCJ->CJ_NUM
				li       := 1145//1075        
				nTotDesc := 0
				aServ    := {}

				dbSelectArea("SCK")
				SCK->(dbSetOrder(1))
				
				While SCK->(!Eof()) 
					If SCK->CK_FILIAL == MV_PART7 .And. SCK->CK_NUM == NumPed //INDEPENDE DE FILIAL
					
						nLiHor++
						
						//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
						//³ Verifica se havera salto de formulario                       ³
						//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
						If li > 2165
							nOrdem++
							//nPag++
							ImpRodape()			// Imprime rodape do formulario e salta para a proxima folha
							ImpCabec()
							li  := 1145
						Endif
	
					EndIf

				dbSelectArea("SCK")
				SCK->(dbSkip())
				EndDo
				
				//oPrn:Line( li+50,0020,li+50,3180 )
				
		
				If li>2165
					nOrdem++
					ImpRodape()		// Imprime rodape do formulario e salta para a proxima folha
					ImpCabec()
					li  := 1145
				Endif
		
				FinalPed()		// Imprime os dados complementares do PC
				SCK->(dbCloseArea())
			//Next
			
			/*	
			If !MsgYesNo(cValToChar(cCont) + ' - Registro num: '+SCJ->(CJ_NUM)+' Continuar?')
				Return()
			EndIf
			cCont++
			*/
		EndIf
		SCJ->(dbSkip())
	EndDo

	If !lVerArq
	oPrn:Say( 0035, 0035, OemToAnsi("Não existe registros com estes parametros!"),oFont9c,100 )	
	EndIf
	

   oPrn:Preview()
   //oPrn:Print()
   //FreeObj(oPrn)
   //MS_FLUSH()
   
Return .T.

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³ ImpCabec ³ Autor ³ Vitor Santos         ³ Data ³          ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Imprime o Cabecalho do Pedido de Compra                    ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function ImpCabec()

Local nConta := 0
Local cStartPath := GetSrvProfString("Startpath","")

Public cAprovador := ""
Public cAprovador2 := ""
Public cAprovNum := ""
Public cAprovNum2 := ""
Public cAprovStat := ""
Public cAprovStat := ""
Private cSubject


If !lPrimPag
   oPrn:EndPage()
   oPrn:StartPage() 
Else
   lPrimPag := .f.
   lEnc     := .t.
//   oPrn  := TMSPrinter():New()
   oPrn:Setup()
EndIF  
oPrn:Say( 0020, 0020, " ",oFont,100 ) // startando a impressora   

cAprovador := LEFT(AllTrim(UsrRetName(cAprovNum)),25)
cAprovador2 := LEFT(AllTrim(UsrRetName(cAprovNum2)),25)
	
nConta:=0


	//Cabecalho (Logomarca e Titulo)
	oPrn:Box( 0020, 0020, 0370,2470)
		
	//oPrn:SayBitmap( 0030,0050,"LOGO.bmp",0500,0135 ) 
	oPrn:SayBitmap( 0030,0050,"lgrl22.bmp",0500,0135 ) 
	oPrn:SayBitmap( 0030,0600,"Hyva_Logo.png",0500,0135 ) 

	//Cabecalho (Documento)
	oPrn:Box( 0270, 0020, 0370,836)//Num Documento    
	oPrn:Box( 0270, 0836, 0370,1653)//Data Documento
	oPrn:Box( 0270, 1653, 0370,2470)//ID Parc. Negocios

	oPrn:Box( 0380, 0020, 0685,1230)//Cliente  
	oPrn:Box( 0380, 1240, 0685,2470)//Entrega  

	oPrn:Box( 0695, 0020, 1025,2470)//Info. Cotação  
	

	//Cabecalho Produto do Pedido
	oPrn:Box( 1040, 0020, 1115,2470)//Peças

	oPrn:Box( 1115, 0020, 1190,0130)//Linha
	oPrn:Box( 1115, 0130, 1190,1300)//Desc dos Bens  
	oPrn:Box( 1115, 1300, 1190,1550)//Data Entrega 
	oPrn:Box( 1115, 1550, 1190,1750)//Unidade 
	oPrn:Box( 1115, 1750, 1190,1950)//Quantidade
	oPrn:Box( 1115, 1950, 1190,2200)//Preço un.
	//oPrn:Box( 1225, 2000, 1300,2200)//ICMS
	oPrn:Box( 1115, 2200, 1190,2470)//Montante
	
	//Espaco dos Itens do Pedido
	oPrn:Box( 1115, 0020, 2255,0130)//Linha
	oPrn:Box( 1115, 0130, 2255,1300)//Desc dos Bens  
	oPrn:Box( 1115, 1300, 2255,1550)//Data Entrega 
	oPrn:Box( 1115, 1550, 2255,1750)//Unidade 
	oPrn:Box( 1115, 1750, 2255,1950)//Quantidade
	oPrn:Box( 1115, 1950, 2255,2200)//Preço un.
	//oPrn:Box( 1225, 2000, 2365,2200)//ICMS
	oPrn:Box( 1115, 2200, 2255,2470)//Montante


	//Titulo  
	oPrn:Say( 0200, 0030, "COTAÇÃO DE PEDIDO DE VENDAS",oFont1,100 )
	oPrn:SayBitmap( 0050, 0050, cStartPath+"LGRL"+cEmpAnt+".BMP",368,100)

	oPrn:Say( 0080, 2880, "FOLHA:" ,oFont3,100 )
	oPrn:Say( 0080, 3032, Alltrim(StrZero(nPag,2)),oFont3,100 )

	//Informações do documento
	oPrn:Say( 0280, 0030, "Número do Documento",oFont11,100 )
	oPrn:Say( 0305, 0030, SCJ->CJ_NUM,oFont9,100 )
	oPrn:Say( 0280, 0846, "Data do Documento",oFont11,100 )
	oPrn:Say( 0305, 0846, DTOC(SCJ->CJ_EMISSAO),oFont9,100 )
	oPrn:Say( 0280, 1663, "ID de parceiro de negócios",oFont11,100 )
	oPrn:Say( 0305, 1663, SCJ->CJ_CLIENTE,oFont9,100 )

	
	//Informações do Cliente
	dbSelectArea("SA1")
	SA1->(dbSetOrder(1))
	SA1->(dbSeek(xFilial("SA1")+SCJ->CJ_CLIENTE+SCJ->CJ_LOJA))
	
	oPrn:Say( 0390, 0030, "Cliente" ,oFont11,100 )
	oPrn:Say( 0415, 0060, UPPER(SA1->A1_NOME) ,oFont7,100 )
	oPrn:Say( 0460, 0060, UPPER(SA1->A1_END) ,oFont8,100 )
	oPrn:Say( 0505, 0060, TRANSFORM(SA1->A1_CEP,'@R 99999-999') + ' ' + UPPER(SA1->A1_MUN) ,oFont8,100 )

	oPrn:Say( 0655, 0030, "CNPJ: " + TRANSFORM(SA1->A1_CGC,'@R 99999999/999-99') ,oFont12,100 )
	oPrn:Say( 0655, 0880, "Telefone: " + SA1->A1_TEL ,oFont12,100 )

	dbSelectArea('SYA')
	SYA->(dbSetOrder(1))
	SYA->(dbSeek(xFilial("SYA")+SA1->A1_PAIS))
		oPrn:Say( 0550, 0060, AllTrim(SYA->YA_DESCR) ,oFont10,100 ) //TABELA SYA
	SYA->(dbCloseArea())

	SA1->(dbCloseArea())
 

	//Informações da Entrega
	dbSelectArea("SA1")
	SA1->(dbSetOrder(1))
	SA1->(dbSeek(xFilial("SA1")+SCJ->CJ_CLIENT+SCJ->CJ_LOJAENT))

	oPrn:Say( 0390, 1250, "Entrega" ,oFont11,100 )
	oPrn:Say( 0415, 1280, UPPER(SA1->A1_NOME) ,oFont7,100 )
	oPrn:Say( 0460, 1280, UPPER(SA1->A1_END) ,oFont8,100 )
	oPrn:Say( 0505, 1280, TRANSFORM(SA1->A1_CEP,'@R 99999-999') + ' ' + UPPER(SA1->A1_MUN) ,oFont8,100 )

	oPrn:Say( 0655, 1250, "CNPJ: " + TRANSFORM(SA1->A1_CGC,'@R 99999999/999-99') ,oFont12,100 )
	oPrn:Say( 0655, 2120, "Telefone: " + SA1->A1_TEL ,oFont12,100 )

	dbSelectArea('SYA')
	SYA->(dbSetOrder(1))
	SYA->(dbSeek(xFilial("SYA")+SA1->A1_PAIS))
		oPrn:Say( 0550, 1280, AllTrim(SYA->YA_DESCR) ,oFont10,100 ) //TABELA SYA
	SYA->(dbCloseArea())

	SA1->(dbCloseArea())

	//Informações da Cotação
	oPrn:Say( 0700, 0030, "Nossa Cotação" ,oFont3,100 )
	oPrn:Say( 0700, 0416, ": " + SCJ->CJ_NUM ,oFont3,100 )

	oPrn:Say( 0740, 0030, "Representante" ,oFont3,100 )
	oPrn:Say( 0740, 0416, ": " + '' ,oFont3,100 )

	oPrn:Say( 0780, 0030, "Suas Referências" ,oFont3,100 )
	oPrn:Say( 0780, 0416, ": " + '' ,oFont3,100 )

	oPrn:Say( 0820, 0030, "Termos de Entrega" ,oFont3,100 )
	oPrn:Say( 0820, 0416, ": " + '' ,oFont3,100 )

	oPrn:Say( 0860, 0030, "Data do Pedido" ,oFont3,100 )
	oPrn:Say( 0860, 0416, ": " + DTOC(SCJ->CJ_EMISSAO) ,oFont3,100 )

	oPrn:Say( 0900, 0030, "Termos de Pagamento"  ,oFont3,100 )
	dbSelectArea('SE4')
	SE4->(dbSetOrder(1))
	SE4->(dbSeek(xFilial("SE4")+SCJ->CJ_CONDPAG))
		oPrn:Say( 0900, 0416, ": " + SE4->E4_DESCRI ,oFont3,100 )// TABELA SE4
	SE4->(dbCloseArea())
	

	oPrn:Say( 0940, 0030, "Cotação Expira",oFont3,100 )
	oPrn:Say( 0940, 0416, ": " + DTOC(SCJ->CJ_VALIDA) ,oFont3,100 )

	oPrn:Say( 0980, 0030, "Tipo de Ordem",oFont3,100 )
	oPrn:Say( 0980, 0416, ": " + SC7->C7_NUM ,oFont3,100 )


	oPrn:Say( 1050, 0030, "PEÇAS" ,oFont9,100 )

	oPrn:Say( 1125, 0030, "Linha"  ,oFont3,100 )
	oPrn:Say( 1125, 0140, "Descrição dos bens" ,oFont3,100 )
	oPrn:Say( 1125, 1310, "Dt. Entrega" ,oFont3,100 )
	oPrn:Say( 1125, 1560, "Unidade" ,oFont3,100 )
	oPrn:Say( 1125, 1760, "Quant." ,oFont3,100 )
	oPrn:Say( 1125, 1960, "Preço un." ,oFont3,100 )
	//oPrn:Say( 1235, 2010, "ICMS"  ,oFont3,100 )
	oPrn:Say( 1125, 2210, "Montante" ,oFont3,100 )	


 //////////////////////////////////////////////////////////////////////

Return .T.


/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³ ImpCampos³ Autor ³ Vitor Santos         ³ Data ³ 01/09/2020³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Imprimir dados dos itens da cotação.                       ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function ImpCampos()

	dbSelectArea('SCK')

	oPrn:Say( li, 0040, StrZero(Val(SCK->CK_ITEM),4)  ,oFont5,100 )//ITEM
	oPrn:Say( li, 0140, UPPER(SCK->(CK_PRODUTO)) + " - " + UPPER(SCK->(CK_DESCRI)),oFont5,100 )//DESCRICAO

	oPrn:Say( li, 1360, DTOC(SCK->CK_ENTREG) ,oFont5,100 )//ENTREGA

	oPrn:Say( li, 1660, SCK->CK_UM ,oFont5,100 ) //UNIDADE

	oPrn:Say( li, 1860, cValToChar(SCK->CK_QTDVEN) ,oFont5,100 )//QUANTIDADE
	
	oPrn:Say( li, 2030, Transform(SCK->CK_PRCVEN,"@E 9,999,999.99") ,oFont5,100 )//PREÇO UNITÁRIO
	
	oPrn:Say( li, 2300, Transform(SCK->CK_VALOR,"@E 9,999,999.99") ,oFont5,100 )//MONTANTE

Return .T.  

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³ ImpRodape³ Autor ³ Vitor Santos          ³ Data ³01/09/2020³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Imprime o rodape do formulario e salta para a proxima folha³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function ImpRodape()

oPrn:Say( 1850, 1450, "CONTINUA ..." ,oFont3,100 )
nPag += 1                                       


Return .T. 

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³ FinalPed ³ Autor ³ Vitor Santos          ³ Data ³01/09/2020³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Imprime os itens do tipo serviço e observações a cotação   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function FinalPed()

Local i 
Local nIni := 0
Local nChar
Local nLiObs

	oPrn:Box( 2255, 0020, 2310,2470)//Total peças
	oPrn:Box( 2255, 1950, 2310,2200)//Total texto
	
	oPrn:Box( 2255, 1950, 2310,2470)//Total peças

	//Cabecalho Produto do Pedido
	oPrn:Box( 2310, 0020, 2385,2470)//Serviços
	
	//Espaco dos Itens do Pedido
	oPrn:Box( 2385, 0020, 2625,0130)//Linha
	oPrn:Box( 2385, 0130, 2625,1300)//Desc dos Bens  
	oPrn:Box( 2385, 1300, 2625,1550)//Data Entrega 
	oPrn:Box( 2385, 1550, 2625,1750)//Unidade 
	oPrn:Box( 2385, 1750, 2625,1950)//Quantidade
	oPrn:Box( 2385, 1950, 2625,2200)//Preço un.
	//oPrn:Box( 2440, 2000, 2680,2200)//ICMS
	oPrn:Box( 2385, 2200, 2625,2470)//Montante

	oPrn:Box( 2625, 0020, 2680,2470)//Total Serviços
	oPrn:Box( 2625, 1950, 2680,2200)//Total texto
	
	oPrn:Box( 2625, 1950, 2680,2470)//Total Serviços

	oPrn:Box( 2680, 0020, 2830,2470)
	//oPrn:Box( 2130, 0020, 2400,2470) // Local de Entrega 
	//oPrn:Box( 2680, 1750, 2755,2470) // ICMS
	//oPrn:Box( 2755, 1750, 2830,2470) // Total
	oPrn:Box( 2680, 1030, 2755,1750) // ICMS
	oPrn:Box( 2755, 1030, 2830,1750) // Frete
	oPrn:Box( 2680, 1750, 2830,2470) // Total

	oPrn:Box( 2830, 0020, 2905,2470)//Observações
	oPrn:Box( 2905, 0020, 3205,2470)

	
	oPrn:Say( 2265, 1960, "Total Pçs.:" ,oFont3,100 )
	oPrn:Say( 2635, 1960, "Total Serv.:" ,oFont3,100 )
	

	oPrn:Say( 2320, 0030, "SERVIÇOS" ,oFont9,100 )
	oPrn:Say( 2840, 0030, "OBSERVAÇÕES" ,oFont9,100 )
	//If len(RTrim(SCJ->CJ_MSGORC)) > 147
		
		//oPrn:Say( 2920, 0030, RTrim(SCJ->CJ_MSGORC) ,oFont5,100 )//OBS
	nLiObs := 2920
	//For  nChar:=230 to len(RTrim(SCJ->CJ_MSGORC)) 
	For  nChar:=230 to len(RTrim(SCJ->CJ_ZOBS)) 

		//oPrn:Say( nLiObs, 0030, RTrim(SubStr(SCJ->CJ_MSGORC,nIni,230)) ,oFont5,100 )//OBS
		oPrn:Say( nLiObs, 0030, RTrim(SubStr(SCJ->CJ_ZOBS,nIni,230)) ,oFont5,100 )//OBS
		nIni:=nIni+230		
		nLiObs:=nLiObs+30

	Next nChar
	//EndIf

			dbSelectArea("SCK")
			//SCK->(dbSetOrder(1))
			li := 2345

			For  i:=1 to Len(aServ) 
				SCK->(dbGoTo(aServ[i,1]))
				
				li:=li+60

				ImpCampos()

			Next i
		

	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Impressso dos totais                                         ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	
	oPrn:Say( 2265, 2260, Transform(nTotPec,"@E 9,999,999.99"),oFont3,100 )//Total Peças
	oPrn:Say( 2635, 2260, Transform(nTotServ,"@E 9,999,999.99"),oFont3,100 )//Total Serviços

	oPrn:Say( 2690, 1040, "ICMS: ",oFont9,100 ) 
	oPrn:Say( 2765, 1040, "FRETE: ",oFont9,100 )
	oPrn:Say( 2765, 1560, Transform(nFrete,"@E 9,999,999.99"),oFont9,100 )
	oPrn:Say( 2730, 1760, "TOTAL: ",oFont9,100 )
	oPrn:Say( 2730, 2250, Transform((nValTot + nFrete),"@E 9,999,999.99"),oFont9,100 )
	

	
	oPrn:EndPage()
	
Return .T.
