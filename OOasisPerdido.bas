'Titulo: O Oasis Perdido: A Travessia do Deserto
'Aluno 1: Ian Cleverton dos Santos Silva
'Aluno 2: Euler Cruz da Silva

Screen 12
DECLARE SUB TelaInicial ()
DECLARE SUB AtualizaStatus (acao$, dificuldade%)
DECLARE SUB ExibeStatus ()
DECLARE SUB VerificaEventosAleatorios ()
DECLARE SUB DesenhaGraficoColorido ()
DECLARE SUB TocarMusica()
DECLARE SUB PararMusica()
DECLARE SUB CreditosFinais()

Dim Shared energia, sede, moral, temperatura, dia As Integer
Dim Shared dificuldade As Integer
Dim Shared status_energia(20), status_sede(20), status_moral(20), status_temperatura(20) As Integer

' Inicializacao
energia = 100
sede = 0
moral = 100
temperatura = 28
dia = 1

' Tela Inicial
TelaInicial

' Escolha do nivel de dificuldade
Cls
Color 14
Print ""
Print "Voce eh um viajante atravessando o deserto em busca de um oasis. Boa sorte!"
Print ""
Print "Para vencer, eh necessario estar com pelo menos esses requisitos ao final:"
Print "Energia tem que ser maior que 40 pontos"
Print " A sede menor que 50 pontos"
Print "A motivacao maior que 50 pontos"
Print "E a temperatura corporal entre 25 e 38 graus celsius"
Print ""
Print "A jornada dura 20 dias. Sobreviva para vencer!"
Print ""
Print "Escolha o nivel de dificuldade:"
Print "1. Facil"
Print "2. Medio"
Print "3. Dificil"

Do
    Input dificuldade
    If dificuldade = 1 Or dificuldade = 2 Or dificuldade = 3 Then
        Exit Do
    Else
        Print "Digite apenas um dos numeros disponiveis nas opcoes"
    End If
Loop

If dificuldade = 1 Then
    dificuldade = -2
ElseIf dificuldade = 2 Then
    dificuldade = -3
Else
    dificuldade = -4
End If

Do While dia <= 20 And energia > 0
    Cls ' Limpa a tela a cada novo turno
    Color 14
    Print "Dia "; dia
    ExibeStatus

    Print "Escolha uma acao:"
    Color 12
    Print "1. Andar"
    Color 3
    Print "2. Descansar"
    Color 1
    Print "3. Refrescar-se"
    Color 9
    If dificuldade <> -4 Then
        Print "4. Beber Agua"
    End If

    Do
        Input escolha$
        If escolha$ = "1" Or escolha$ = "2" Or escolha$ = "3" Or (dificuldade <> -4 And escolha$ = "4") Then
            Exit Do
        Else
            Print "Digite apenas um dos numeros disponiveis nas opcoes"
        End If
    Loop

    ' Processa a escolha do jogador e exibe mensagens
    Select Case escolha$
        Case "1"
            AtualizaStatus "andar", dificuldade
            Print "Voce andou um arduo dia no deserto, sua energia e moral diminuiram, e sua sede e temperatura aumentaram."
        Case "2"
            AtualizaStatus "descansar", dificuldade
            Print "Voce recuperou energia para sua jornada, mas ficar parado tambem aumenta sua temperatura."
        Case "3"
            AtualizaStatus "refrescar", dificuldade
            Print "Voce encontrou um pequeno poco, sua sede e temperatura diminuiram, mas sua energia e moral tambem."
        Case "4"
            If dificuldade <> -4 Then
                AtualizaStatus "beber agua", dificuldade
                Print "Voce bebeu agua e diminuiu a sua sede."
            Else
                Print "Beber agua nao esta disponivel neste nivel de dificuldade."
            End If
    End Select

    ' Registro dos status
    status_energia(dia) = energia
    status_sede(dia) = sede
    status_moral(dia) = moral
    status_temperatura(dia) = temperatura

    dia = dia + 1

    ' Verificar eventos aleatórios
    VerificaEventosAleatorios

    ' Esperar o jogador pressionar Enter antes de continuar
    Print "Pressione Enter para continuar..."
    While InKey$ <> Chr$(13) ' 13 é o codigo ASCII para Enter
    Wend
    Cls ' Limpa a tela antes de continuar para o proximo turno
Loop

' Verificacao final de vitoria ou derrota apos os 20 dias
Cls ' Limpa a tela antes de exibir a verificacao final
If energia > 40 And sede < 50 And moral >= 50 And temperatura >= 25 And temperatura <= 39 Then
    Print "        Parabens! Voce sobreviveu a travessia e chegou ao oasis!"
    Print "                @@@@@@"
    Print "    @@@@@@      @@--@@"
    Print "    mm@@++@@MM@@----@@"
    Print "      @@----@@@@----@@@@@@@@               "
    Print "      @@@@----@@::@@@@....@@@@             "
    Print "      @@@@@@@@@@@@......mm@@               "
    Print "  @@@@mm----++@@@@@@@@@@@@"
    Print "::@@--------@@@@@@..mm@@"
    Print "  @@@@@@@@@@@@++@@......@@"
    Print "          mm@@++@@@@mm..MM@@"
    Print "          @@++++@@  @@@@@@@@"
    Print "          @@++++@@"
    Print "          @@++++@@"
    Print "          @@++++@@                      ++mm++.."
    Print "    @@@@@@@@mm++@@@@@@@@@@      @@@@@@@@MM++MM@@@@@@@@"
    Print "@@@@......mm@@++@@......MM@@@@@@++..................::@@"
    Print "@@@@........@@++@@..........++@@@@....................@@"
    Print "@@@@........@@++##MM............++@@@@................@@"
    Print "@@@@........@@++++@@..............  ++@@@@............@@"
    Print "@@@@@@@@@@@@@@@@@@@@..MM@@@@@@@@@@@@@@@@@@@@@@@@@@MM..@@"
    Print "@@@@............  @@@@@@                          MM@@@@"
    Print "@@@@............      @@@@@@                          @@"
    Print "@@@@..............        ::@@@@@@mm              --@@@@"
    Print "@@@@................              @@@@@@@@@@@@@@@@@@..@@"
    Print "@@@@................                                  @@"
    Print "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"

    TocarMusicaVitoria ' Toca a musica de vitoria
Else
    Print "Devido as mas condicoes, voce acabou morrendo!."
    Print "        ########                          ######"
    Print "        ##----##                        ##++--++##"
    Print "      ##------mm##                      ##------mm##"
    Print "  ####--####--mm##                      ##----####--####"
    Print "  ##----------mm    ########MM########  ##----::----mm##"
    Print "  ##----------++####  ####            ####----------mm##"
    Print "  ##--------mm##    @@##--              --##--------mm##"
    Print "    ##########        ####                ::##########"
    Print "            ##      ####                  --##"
    Print "          ##          ##                    --##"
    Print "          ##                                --##"
    Print "          ##      ::##              ##      --##"
    Print "          ##    ########          ######--  --##"
    Print "          ##    ##########      ##########  --##"
    Print "          ##    ##########      ##########  --##"
    Print "          ##    ########          ########  --##"
    Print "            ##    ####      ##    mm####  --##"
    Print "              ##            mm          --##"
    Print "      ##    ##mm##                    --##++##    ##"
    Print "  ####--####----@@                    mm::----####--####"
    Print "  ##------------MM      ##      ##    mm------------mm##"
    Print "  ##----##----mm####    ##MM  ####  --####------@@--mm##"
    Print "  ##----##----mm    ####  ######  ####  ##------##--mm##"
    Print "    ####------mm##                      ##------mm####"
    Print "      ##------mm##                      ##------mm"
    Print "        ##@@@@##                          ##MM####"


    TocarMusicaDerrota ' Toca a musica de derrota
End If

' Esperar o jogador pressionar Enter antes de mostrar o gráfico
Print "Pressione Enter para ver o grafico de status..."
While InKey$ <> Chr$(13) ' 13 eh o código ASCII para Enter
Wend
Cls ' Limpa a tela antes de mostrar o gráfico

' Parar musica antes de exibir o gráfico
PararMusicaFinal

' Desenhar gráfico
DesenhaGraficoColorido

' Parar musica
PararMusicaFinal

CreditosFinais

End

Sub CreditosFinais ()
    Cls
    Print "Creditos finais:"
    Print ""
    Print "O Oasis Perdido: A Atrevessia do Deserto"
    Print "Trabalho da disciplina de Programacao Imperativa"
    Print "Universidade Federal de Sergipe | Campus Sao Cristovao"
    Print "Discentes: Ian Cleverton dos Santos Silva"
    Print "Euler Cruz da Silva"
    Print "Obrigado a todos!"
    Print ""
    Print ""
    Print "Pressione qualquer tecla para sair..."

    'Esperar o usuario pressionar uma tecla para sair
    While InKey$ = "": Wend
End Sub

' Música de vitória
Sub TocarMusicaVitoria ()
    ' Uma melodia simples para a vitoria
    Sound 784, 10 ' Sol
    Sound 880, 10 ' La
    Sound 988, 10 ' Si
    Sound 1047, 10 ' Do
    Sound 1175, 10 ' Re
    Sound 1319, 10 ' Mi
    Sound 1480, 10 ' Fa
    Sound 1568, 10 ' Sol
    Sound 0, 10
End Sub

' Música de derrota
Sub TocarMusicaDerrota ()
    ' Uma melodia simples para a derrota
    Sound 262, 15 ' Do
    Sound 233, 15 ' Si bemol
    Sound 196, 15 ' Sol
    Sound 174, 15 ' Fa
    Sound 196, 15 ' Sol
    Sound 233, 15 ' Si bemol
    Sound 262, 15 ' Do
    Sound 0, 15
End Sub

' Parar música final
Sub PararMusicaFinal ()
    ' Finaliza a música
    Sound 0, 0
End Sub

' Definicao das SUBs

Sub TelaInicial ()
    Cls
    ' Definir o texto da tela inicial
    TEXT$ = "O Oasis Perdido: A Travessia do Deserto"
    Color 14 ' Amarelo

    ' Calcular a largura da tela para centralizar o texto
    TEXTWIDTH = 80
    TEXTLENGTH = Len(TEXT$)

    ' Calcular a posição horizontal para centralizar
    X = (TEXTWIDTH - TEXTLENGTH) \ 2

    ' Desenhar o texto centralizado
    Locate 5, X + 1
    Print TEXT$

    ' Restaurar a cor do texto
    Color 7 ' Branco

    Print
    Print "Pressione qualquer tecla para comecar..."

    ' Tocar musica
    TocarMusica

    ' Esperar a tecla ser pressionada para continuar
    While InKey$ = "": Wend

    ' Parar a musica
    PararMusica
End Sub

' Verifica e Atualiza os Status com a logica adequada
Sub AtualizaStatus (acao$, dificuldade%)
    Select Case acao$
        Case "andar"
            energia = energia + dificuldade%
            sede = sede + Abs(dificuldade%)
            moral = moral + dificuldade% \ 2
            temperatura = temperatura + 1
        Case "descansar"
            energia = energia + (10 + dificuldade%)
            sede = sede + 2
            temperatura = temperatura + 2 ' Aumenta a temperatura ao descansar
        Case "refrescar"
            If dificuldade = -2 Then
                temperatura = temperatura - 3
                sede = sede - 5
                energia = energia - 4
                moral = moral - 4
            ElseIf dificuldade = -3 Then
                temperatura = temperatura - 2
                sede = sede - 4
                energia = energia - 5
                moral = moral - 5
            ElseIf dificuldade = -4 Then
                temperatura = temperatura - 1.5
                sede = sede - 3
                energia = energia - 6
                moral = moral - 6
            End If
        Case "beber agua"
            If dificuldade = -2 Then
                sede = sede - 5
            ElseIf dificuldade = -3 Then
                sede = sede - 4
            End If
            If sede < 0 Then sede = 0
    End Select

    ' Garante que os status estejam dentro dos limites aceitaveis
    If energia < 0 Then energia = 0
    If energia >= 100 Then energia = 100
    If sede < 0 Then sede = 0
    If sede >= 100 Then sede = 100
    If moral < 0 Then moral = 0
    If moral >= 100 Then moral = 100
    If temperatura > 100 Then temperatura = 100
End Sub

Sub ExibeStatus ()
    Print "Energia: "; energia
    Print "Sede: "; sede
    Print "Moral: "; moral
    Print "Temperatura Corporal: "; temperatura
End Sub

Sub VerificaEventosAleatorios ()
    Randomize Timer
    evento = Int(Rnd * 12) + 1 ' Probabilidade de 1/12 para cada evento

    Select Case evento
        Case 1 ' Tempestade de areia
            Print ""
            Print "Uma tempestade de areia apareceu!"
            Print "              ########################"
            Print "        ####--------------------------::##"
            Print "        ##--##########################--##"
            Print "            ######++++++++++++++@@########"
            Print "            ####........................##"
            Print "          ##  ####......................####"
            Print "          ####    ........##......########"
            Print "          ##  ##  ##..................##  ##"
            Print "            ##    ######@@..........######"
            Print "                ##................##      ##"
            Print "                ......####@@######    ##"
            Print "              ##..............####"
            Print "              ##............####  ##"
            Print "      ##  ######::........##  ##  ##"
            Print "              MM........MM####  ##"
            Print "      ##  ################"
            Print "        ####  ##........  ##      ##"
            Print "                ##......      ####"
            Print "  ##################....####            ##########"
            Print "                    ######  ######................"
            Print "                            ########.............."
            Print "                                      ############"

            'Som de vento forte
            energia = energia - 6
            sede = sede + 5
            temperatura = temperatura + 2
            TocarSomVentoForte

        Case 2 ' Fonte de água
            Print ""
            Print "Voce encontrou uma fonte de agua!"
            Print "                                        ........"
            Print "                                    ................"
            Print "                                  ...................."
            Print "                                ........................"
            Print "                               ........::..::::..::::......"
            Print "                             ............::::..::::..::...."
            Print "         ..........      .................................."
            Print "       ...................................................."
            Print "     ......................................................"
            Print "    ...................................................."
            Print "    ......::..::::..::::................................"
            Print "    ........::....::....::............................"
            Print "    ................................................"
            Print "      ........::::::::::::::............"
            Print "      ................................"
            Print "        ............................"
            Print "          ........................"
            Print "                  .........."

            sede = sede - 10
            temperatura = temperatura - 3
            'Som de bebendo agua
            TocarSomBebendoAgua

    End Select

    ' Garante que a sede fique entre 0 e 100
    If sede < 0 Then sede = 0
    If sede > 100 Then sede = 100
End Sub

Sub DesenhaGraficoColorido ()
    Cls
    Print "Grafico de Status ao longo dos 20 dias:"

    ' Desenho do grafico
    For dia = 1 To 20
        ' Cor para Energia (Verde)
        Color 2
        Print "Dia "; dia; " - Energia: "; String$(status_energia(dia) \ 2, Chr$(219))

        ' Cor para Sede (Azul)
        Color 1
        Print "Dia "; dia; " - Sede: "; String$(status_sede(dia) \ 2, Chr$(219))

        ' Cor para Motivacao (Amarelo)
        Color 14
        Print "Dia "; dia; " - Moral: "; String$(status_moral(dia) \ 2, Chr$(219))

        ' Cor para Temperatura (Vermelho)
        Color 4
        Print "Dia "; dia; " - Temperatura: "; String$(status_temperatura(dia) \ 2, Chr$(219))

        ' Separar os dias no grafico
        Print

        ' A cada 4 dias, pausar a exibicao para evitar corte
        If dia Mod 4 = 0 Then
            Print "Pressione qualquer tecla para continuar..."
            While InKey$ = "": Wend ' Esperar o usuario pressionar uma tecla
            Cls ' Limpa a tela antes de exibir os proximos dias
        End If
    Next dia

    ' Restaurar cor padrão
    Color 7
End Sub

Sub TocarMusica ()
    Sound 262, 8 ' Do
    Sound 294, 8 ' Re
    Sound 330, 8 ' Mi
    Sound 349, 8 ' Fa
    Sound 392, 8 ' Sol
    Sound 440, 8 ' La
    Sound 494, 8 ' Si
    Sound 523, 8 ' Do
    Sound 587, 8 ' Re
    Sound 659, 8 ' Mi
    Sound 698, 8 ' Fa
    Sound 784, 8 ' Sol
    Sound 880, 8 ' La
    Sound 988, 8 ' Si
    Sound 1047, 8 ' Do
End Sub

Sub TocarSomVentoForte ()
    Sound 392, 15 ' Sol
    Sound 349, 15 ' Fa
    Sound 330, 15 ' Mi
    Sound 311, 15 ' Mi bemol
    Sound 294, 15 ' Re
    Sound 262, 15 ' Do
    Sound 233, 15 ' Si bemol
    Sound 196, 15 ' Sol
    Sound 0, 10 ' Pausa
End Sub

Sub TocarSomBebendoAgua ()
    Sound 330, 5 ' Mi
    Sound 392, 5 ' Sol
    Sound 523, 5 ' Do
    Sound 330, 5 ' Mi
    Sound 392, 5 ' Sol
    Sound 262, 5 ' Do
    Sound 0, 10 ' Pausa
End Sub


Sub PararMusica ()
    ' Finaliza a musica (se necessario)
    Sound 0, 0
End Sub
