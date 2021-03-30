# jf_legis
web scrapper para avaliar quão inúteis são nossos vereadores

Inicialmente eu estava pegando pelo sistema da prefeitura, o JF Leis, de legislação municipal. <https://jflegis.pjf.mg.gov.br/indexConsulta.php#tbr>.
É o que está nessa pasta inicial jf_legis.

Mas o sistema da camara é mais detalhado e é mais simples, não tem um captcha. <http://www.camarajf.mg.gov.br/sal/>
Foi o que eu usei pra fazer as analises e está na pasta camara_jf.


Por enquanto está só no projeto de lei. é a nossa meta. Nós vamos deixar a meta aberta (importar as tabelas, e se achar, as mmoções de aplauso e requerimentos) e quando nós atingirmos a meta, a gente dobra a meta (fazer um shiny sobre, buscar outras informações sobre gastos, votação e ordem do dia).

Mas está funcionando. Por enquanto, fiquemos com o que temos.

na pasta camara_jf, existem quatro scripts:

camara_jf_analise_inicial.R , em que usei pra fazer analise dos PLs de Baixo Impacto
gráficos_camarajf.r, ggplots desse inicial
scrapper_cmjf2.R foi eu tentando fazer um web scrapping sobre
scrapper_cmjf3_funcao_julio.R foi o Julio trecenti fazendo um de moções de aplauso (Cara é brabo)
filtros_por_assunto.R é a tentativa de classificar o tema de "outros". Por enquanto só tem de covid.

Não estranhem a quantiade de data frames foram criados nos finais dos codigos. Estou exportando esses dados pra fazer os gráficos no floruish.studio.
