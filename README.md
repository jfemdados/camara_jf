# Análise de Dados da Câmara Municipal de Juiz de Fora 

A ideia desse projeto é avaliar a produção legislativa da cidade e atuação de cada vereador.

ANTES DE TUDO: **IGNORE AS PASTAS JF_LEGIS e CAMARA_JF"**. São os primeiros projetos que tinhamos que scrapear as páginas. Não é mais necessário pois exsite um "Exportar Excel" no site do [Sistema de Apoio Legislativo - SAL](http://www.camarajf.mg.gov.br/sal/)

No estado atual, dividimos as pastas em:

1) __pls_completos__ temos todos os PLs disponíveis no sistemas, faxinados e com nossa classificação de PLs Inúteis, digo de Baixo Impacto;
2) __mocoes__ temos todas as Moções de Aplauso ou pesar, parabéns que os vereadores dão a certas pessoas da cidade. Dados faxinados e um pouco classificadas.
3) _requeriementos_ temos todos os requerimentos, pedidos à prefeitura. Dados faxinados e satisfatóriamente faxinados.

Inicialmente, a ideia era pegar os projetos aprovado, diretamente pelo sistema da prefeitura sobre legislação municipal, o [JFLegis](https://jflegis.pjf.mg.gov.br/indexConsulta.php#tbr). É o que está nessa pasta inicial _jf_legis_.

4) Fazer de Representação (sei la o que é isso);
5) Fazer um Mapa de Requerimentos na cidade;
6) Fazer o "Tô de Olho" um shiny do seu vereador;

Mas está funcionando. Por enquanto, fiquemos com o que temos.

OBS: Não estranhem a quantiade de data frames foram criados nos finais dos codigos. Estou exportando esses dados pra fazer os gráficos no floruish.studio.

OBS2: na pasta camara_jf, existem quatro scripts:

*camara_jf_analise_inicial*.R , em que usei pra fazer analise dos PLs de Baixo Impacto. 

*gráficos_camarajf*.r, ggplots desse inicial. 

*scrapper_cmjf2*.R foi eu tentando fazer um web scrapping sobre a camara municipal. 

*scrapper_cmjf3_funcao_julio.R* foi o Julio trecenti fazendo um de moções de aplauso (Cara é brabo). 

*filtros_por_assunto.R* é a tentativa de classificar o tema de "outros". Por enquanto só tem de covid.
