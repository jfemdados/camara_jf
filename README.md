# Análise de Dados da Câmara Municipal de Juiz de Fora 

A ideia desse projeto é avaliar a produção legislativa da cidade e atuação de cada vereador.

Inicialmente, a ideia era pegar os projetos aprovado, diretamente pelo sistema da prefeitura sobre legislação municipal, o [JFLegis](https://jflegis.pjf.mg.gov.br/indexConsulta.php#tbr). É o que está nessa pasta inicial _jf_legis_.

Entretanto, o Sistema da Câmara tem informações sobre PLs arquivados e aprovados, além de moções e requerimentos, o chamado [Sal](http://www.camarajf.mg.gov.br/sal/).
Foi o amplamente utilizado.

Inicialmente era necessário fazer WebScrapping. Na pasta _camara_jf_ temos códigos de scrapper até as primeiras análises, de forma um pouco desorganizada. Agora graças a deus colocaram a hipótese de exportar para excel.

A meta era só projeto de lei, e deixamos a meta aberta para moções de aplauso e requerimentos. __BATEMOS A META!!!___

Nos códigos mais novos e organizados, temos as três novas pastas:

1) _pls_completos_ temos todos os PLs disponíveis no sistemas, faxinados e com nossa classificação de PLs Inúteis, digo de Baixo Impacto;
2) _mocoes_ temos todas as Moções de Aplauso ou pesar, parabéns que os vereadores dão. Dados faxinados e um pouco classificadas.
3) _requeriementos_ temos todos os requerimentos, pedidos à prefeitura. Dados faxinados e satisfatóriamente faxinados

Agora, vamos dobrar a meta:

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
