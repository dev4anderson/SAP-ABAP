REPORT zsgeraet.

***------------------------------------------------------------------***
***  TABELAS                                                         ***
***------------------------------------------------------------------***
TABLES trdir.

***------------------------------------------------------------------***
***  INCLUDES                                                        ***
***------------------------------------------------------------------***
INCLUDE zsgeraet_1.

***------------------------------------------------------------------***
***  Tabelas Internas                                                ***
***------------------------------------------------------------------***

DATA: BEGIN OF t_prog OCCURS 0,
        linha(255) TYPE c,
      END OF t_prog.
DATA: BEGIN OF t_busca OCCURS 0.
        INCLUDE STRUCTURE t_prog.
DATA: END OF t_busca.

DATA: tk  TYPE TABLE OF stokes WITH HEADER LINE,
      stm TYPE TABLE OF sstmnt WITH HEADER LINE.            "#EC NEEDED

DATA: BEGIN OF t_tab OCCURS 0,
        tabela LIKE dd02t-tabname,
      END OF t_tab.

DATA: BEGIN OF t_desc_tab OCCURS 0,
        tabname    LIKE dd02t-tabname,
        ddlanguage LIKE dd02t-ddlanguage,
        as4local   LIKE dd02t-as4local,
        as4vers    LIKE dd02t-as4vers,
        ddtext     LIKE dd02t-ddtext,
      END OF t_desc_tab.

DATA: BEGIN OF t_const OCCURS 0,
        constante(200) TYPE c,
      END OF t_const.
DATA: BEGIN OF t_tab_int OCCURS 0,
        linha(200) TYPE c,
      END OF t_tab_int.
DATA: BEGIN OF t_var OCCURS 0,
        linha(200) TYPE c,
      END OF t_var.

DATA: BEGIN OF t_par OCCURS 0,
        par(200) TYPE c,
      END OF t_par.
DATA: BEGIN OF t_sel_opt OCCURS 0,
        par TYPE string,
      END OF t_sel_opt.

DATA: BEGIN OF t_include OCCURS 0,                          "#EC NEEDED
        include(200) TYPE c,
      END OF t_include.
DATA: BEGIN OF t_rotinas OCCURS 0,
        rotinas(200) TYPE c,
      END OF t_rotinas.
DATA: BEGIN OF t_funcoes OCCURS 0,                          "#EC NEEDED
        funcoes(200) TYPE c,
      END OF t_funcoes.
DATA: BEGIN OF t_select OCCURS 0,                           "#EC NEEDED
        select(200) TYPE c,
      END OF t_select.

*** Tabelas para a criação da tabela de seleção
DATA: BEGIN OF t_tabela OCCURS 0,
        tabela    TYPE string,
        descricao TYPE string,
        apelido   TYPE string,
      END OF t_tabela,

      BEGIN OF t_campos OCCURS 0,
        campo TYPE string,
        desc  TYPE string,
      END OF t_campos,

      BEGIN OF t_destino OCCURS 0,
        tabela    TYPE string,
        descricao TYPE string,
      END OF t_destino,

      BEGIN OF t_join OCCURS 0,
        tipo      TYPE string,
        tabela    TYPE string,
        descricao TYPE string,
        apelido   TYPE string,
      END OF t_join,

      BEGIN OF t_c_join OCCURS 0,
        t_a      TYPE string,
        operador TYPE string,
        t_b      TYPE string,
        con      TYPE string,
      END OF t_c_join,

      BEGIN OF t_condicoes OCCURS 0,
        tabela   TYPE string,
        operador TYPE string,
        campo    TYPE string,
        con      TYPE string,
      END OF t_condicoes.


TYPES: BEGIN OF ty_source,
         tipo     TYPE char1,
         comando  TYPE as4text,
         sentenca TYPE as4text,
       END OF ty_source.

DATA: tl_selecao TYPE TABLE OF ty_source WITH HEADER LINE,
      t_source   TYPE TABLE OF ty_source WITH HEADER LINE.

*** Tabela de seleção
DATA: BEGIN OF t_selecao OCCURS 0,
        tabela  LIKE TABLE OF t_tabela,
        tipo    TYPE string,
        campos  LIKE TABLE OF t_campos,
        destino LIKE TABLE OF t_destino,
        join    LIKE TABLE OF t_join,
        c_join  LIKE TABLE OF t_c_join,
        for_all TYPE string,
        cond    LIKE TABLE OF t_condicoes,
        classf  TYPE string,
        agrup   TYPE string,
      END OF t_selecao.

DATA: t_trdir TYPE trdir OCCURS 0 WITH HEADER LINE.

DATA: programa LIKE sy-repid,
      linha    LIKE tk-row.
DATA: BEGIN OF t_forms OCCURS 0,
        form(200) TYPE c,
        e_form    TYPE c,
        e_endform TYPE c,
      END OF t_forms.
DATA: BEGIN OF t_forms_aux OCCURS 0,
        inicio TYPE i,
        fim    TYPE i,
      END OF t_forms_aux.

DATA: BEGIN OF t_eventos OCCURS 0,
        eventos(200) TYPE c,
      END OF t_eventos.
DATA: BEGIN OF t_eventos_aux OCCURS 0,
        inicio TYPE i,
        fim    TYPE i,
      END OF t_eventos_aux.

DATA: BEGIN OF t_tabcampo OCCURS 0,                         "#EC NEEDED
        campotbl(61) TYPE c,
        tabela(30)   TYPE c,
        campo(30)    TYPE c,
      END OF t_tabcampo.

TYPES: BEGIN OF type_split,
         campo(255) TYPE c,
       END   OF type_split.

DATA: t_split TYPE TABLE OF type_split WITH HEADER LINE.

DATA: BEGIN OF t_parameter OCCURS 0,
        nome    TYPE string,  "Nome do campo
        tipo    TYPE string,  "Tipo do parâmetro
        texto   TYPE string,  "Texto da tela
        campo   TYPE string,  "Tipo de campo/referência
        default TYPE string,  "Valor inicial
        cons    TYPE string,  "Consistência/Observação
      END OF t_parameter.

DATA: BEGIN OF t_select_opt OCCURS 0,
        nome    TYPE string,  "Nome do campo
        tipo    TYPE string,  "Tipo do parâmetro
        texto   TYPE string,  "Texto da tela
        campo   TYPE string,  "Tipo de campo/referência
        default TYPE string,  "Valor inicial
        cons    TYPE string,  "Consistência/Observação
      END OF t_select_opt.

DATA: BEGIN OF t_texto_tra OCCURS 0,
        portug(255)   TYPE c, "Campo em Português
        ingles(255)   TYPE c, "Campo em Ingles
        outro_id(255) TYPE c, "Outro Idioma
      END OF t_texto_tra.

TYPES: BEGIN OF zerequest,
         num_ordem       TYPE  verskorrno,
         catg_obj        TYPE  versobjtyp,
         versao          TYPE  versno,
         autor           TYPE  versuser,
         data            TYPE  versdate,
         hora            TYPE  verstime,
         ultima_versao   TYPE  verslast,
         primeira_versao TYPE  versfirst,
         release_sap     TYPE  versrels,
         tipo_ordem      TYPE  trfunction,
         status          TYPE  trstatus,
         destino         TYPE  tr_target,
         titular         TYPE  tr_as4user,
         data_ult_mod    TYPE  as4date,
         hora_ult_mod    TYPE  as4time,
         odem_super      TYPE  strkorr,
         idioma          TYPE  ddlanguage,
         descricao       TYPE  as4text,
         mandt           TYPE  trclient,
         task            TYPE  verskorrno,
         retcode         TYPE  trretcode,
         message         TYPE  natxt,
         obj_name        TYPE  versobjnam,
       END OF zerequest,

       BEGIN OF zecaract,
         programa       TYPE  programm,
         protecao       TYPE  sqlx,
         bloqueio       TYPE  edtx,
         maiusc_minusc  TYPE  varcl,
         banco_dados    TYPE  dbapl,
         banco_logico   TYPE  dbna,
         classe_prog    TYPE  clas,
         versao_tl_selc TYPE  rdir_type,
         geracao_aut    TYPE  occu,
         catg_prog      TYPE  subc,
         aplicacao      TYPE  rdir_appl,
         grupo_autoriz  TYPE  secu,
         autor          TYPE  cnam,
         data_criacao   TYPE  rdir_cdate,
         autor_ult_mod  TYPE  unam,
         data_ult_modf  TYPE  rdir_udate,
         versao         TYPE  vern,
         status         TYPE  rdir_levl,
         status2        TYPE  rdir_rstat,
         mandt          TYPE  rmand,
         idioma         TYPE  rload,
         ponto_fixo     TYPE  fixpt,
         variante       TYPE  sset,
         data_ger_tela  TYPE  rdir_sdate,
         hora_ger_tela  TYPE  rdir_stime,
         data_ger_sel   TYPE  rdir_idate,
         hora_ger_sel   TYPE  rdir_itime,
         nome_bd_logico TYPE  ldbnam,
         flag_unicode   TYPE  uccheck,
         titulo_prog    TYPE  repti,
         object_type    TYPE  trobjtype,
         sist_orig      TYPE  srcsystem,
         dev_class      TYPE  devclass,
       END OF zecaract,

       BEGIN OF zetext,
         id       TYPE  textpoolid,
         chave    TYPE  textpoolky,
         texto    TYPE  textpooltx,
         tamanho  TYPE  textpoolln,
         traducao TYPE  txt255,
       END OF zetext,

       BEGIN OF zetransaction,
         transacao    TYPE  tcode,
         tela         TYPE  dynpronr,
         descricao    TYPE  ttext_stct,
         sap_gui_html TYPE  s_webgui,
         sap_gui_win  TYPE  s_win32,
         sap_gui_java TYPE  s_platin,
         service      TYPE  iacservic_,
         obj_aut      TYPE  xuobject,
         campo_aut    TYPE  xufield,
         valor_aut    TYPE  xuval,
         parametros   TYPE  tcdparam,
         autor        TYPE  responsibl,
         object_type  TYPE  trobjtype,
         sist_orig    TYPE  srcsystem,
         dev_class    TYPE  devclass,
       END OF zetransaction,

       BEGIN OF zevariant,
         report       TYPE  vari_reprt,
         variante     TYPE  variant,
         descricao    TYPE  rvart_vtxt,
         trans_aut    TYPE  vari_trans,
         ambiente     TYPE  varid_env,
         protegido    TYPE  rsscr_cflg,
         grupo_aut    TYPE  secu,
         versao       TYPE  varid_vers,
         user_criacao TYPE  uname,
         data_criacao TYPE  sydats,
         hora_criacao TYPE  sytime,
         user_modif   TYPE  uname,
         data_modif   TYPE  sydats,
         hora_modif   TYPE  sytime,
         idioma       TYPE  langu,
         tela         TYPE  sydynnr,
       END OF zevariant.


** Criação de Estrutura para os dados da Captura.
* Dados de Request.
DATA t_request TYPE TABLE OF zerequest.
DATA e_request TYPE zerequest.

** Dados de Caracteriscas.
DATA t_caract TYPE zecaract.

** Dados de texts
DATA: t_textos TYPE TABLE OF zetext.
DATA: e_textos TYPE zetext.

**Dados das transações
DATA: t_transacao TYPE TABLE OF zetransaction.
DATA: e_transacao TYPE zetransaction.

**Dados das Variantes
DATA: t_variant TYPE TABLE OF zevariant.
DATA: e_variant TYPE zevariant.

**Dados dos Jobs
DATA: t_job TYPE TABLE OF v_op.
DATA: e_job TYPE v_op.

**Estruturas para o quadro de select
DATA: e_tabela    LIKE t_tabela,
      e_campos    LIKE t_campos,
      e_destino   LIKE t_destino,
      e_join      LIKE t_join,
      e_c_join    LIKE t_c_join,
      e_condicoes LIKE t_condicoes,
      i_lines     TYPE i,
      i_join      TYPE i.                                   "#EC NEEDED

***------------------------------------------------------------------***
***  Variaveis Globais                                               ***
***------------------------------------------------------------------***
**Variavel que contem a quantidade de Requests
DATA: v_qtd_request(3) TYPE c,
      v_qtd_tabela(6)  TYPE c,
      v_file(128)      TYPE c,
      v_variant(200)   TYPE c,
      v_job(200)       TYPE c.

DATA: v_linha_form(72)       TYPE c,
**Variaveis para o quadro de selects
      v_tab_e_desc(200)      TYPE c,
      v_tipo(50)             TYPE c,
      v_destino(50)          TYPE c,
      v_tab_join(20)         TYPE c,
      v_cond_join(100)       TYPE c,
      v_for_all(20)          TYPE c,
      v_cond_where(100)      TYPE c,
**Variavel de quantidade de prametros de seleção.
      v_qtd_param(3)         TYPE c,
      v_qtd_selop(3)         TYPE c,

**Variaveis com os select options.
      v_selop_nome(100)      TYPE c,
      v_selop_tipo(100)      TYPE c,
      v_selop_texto(100)     TYPE c,
      v_selop_campo(100)     TYPE c,
      v_selop_default(100)   TYPE c,
      v_selop_cons(100)      TYPE c,

**Variaveis com os Parameters
      v_par_nome(100)        TYPE c,
      v_par_tipo(100)        TYPE c,
      v_par_texto(100)       TYPE c,
      v_par_campo(100)       TYPE c,
      v_par_default(100)     TYPE c,
      v_par_cons(100)        TYPE c,

**Variavel com a quantidade de textos.
      v_qtd_textos(3)        TYPE c,

**Variavel com o nome da pasta Batch imput.
      v_nome_pasta_batch(30) TYPE c.

***Constantes
CONSTANTS:
  c_file            LIKE  rlgrap-filename VALUE '', "#EC NOTEXT
  c_rotina(06)      TYPE  c               VALUE 'Rotina',   "#EC NOTEXT
  c_fimdarotina(13) TYPE  c               VALUE 'Fim da Rotina'. "#EC NOTEXT

***------------------------------------------------------------------***
***  Parametros de Seleção                                           ***
***------------------------------------------------------------------***

SELECTION-SCREEN BEGIN OF BLOCK c1 WITH FRAME TITLE TEXT-c01.
PARAMETERS: p_prog    LIKE trdir-name,
            p_tit(60) TYPE c.
SELECTION-SCREEN END OF BLOCK c1.

SELECTION-SCREEN BEGIN OF BLOCK c3 WITH FRAME TITLE TEXT-c03.
PARAMETER: p_loc LIKE rlgrap-filename DEFAULT c_file.

SELECTION-SCREEN END OF BLOCK c3.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_loc.
  PERFORM ws_filename_get USING p_loc 'S'.

AT SELECTION-SCREEN.

***------------------------------------------------------------------***
***  Evento Start of selection                                       ***
***------------------------------------------------------------------***
START-OF-SELECTION.

  PERFORM progresso USING TEXT-006. "'Inicializando as tabelas'.

  SELECT SINGLE *
    FROM trdir
    INTO t_trdir
    WHERE name EQ p_prog.

  IF sy-subrc IS INITIAL.

    READ TABLE t_trdir INDEX 1.
    CLEAR v_file.
    programa = t_trdir-name.

    v_file = p_loc.

  ELSE.
    MESSAGE i208(00) WITH TEXT-002.
*   Não existem dados para os parâmetros informados
    STOP.
  ENDIF.

***Busca o código fonte do programa indicado.
  PERFORM scan_source_get_report TABLES t_prog USING programa.
  PERFORM progresso USING TEXT-003. "'Procurando as tabela utilizadas'.
  PERFORM busca_tabelas.

  PERFORM progresso USING TEXT-004. "'Procurando as contantes utilizadas'.
  PERFORM busca_constantes.

  PERFORM progresso USING TEXT-005. "'Procurando as tabelas internas e variáveis utilizadas'.
  PERFORM busca_variaveis_tab_int.

***   Captura as informações de Request, Textos e Caracteriscas da
***   classe global ZCL_SCAN_SOURCE_ATRIBUTES da programa selecionado.
  PERFORM progresso USING TEXT-007. "'Procurando as características dos campos utilizados no programa'.
  PERFORM busca_caract.

  PERFORM progresso USING TEXT-008. "'Procurando os parâmetros'.
  PERFORM busca_parametros.

  PERFORM progresso USING TEXT-009. "'Procurando os SELECT-OPTIONS'.
  PERFORM busca_select_options.

  PERFORM progresso USING TEXT-010. "'Procurando as rotinas do programa'.
  PERFORM busca_rotinas.
  PERFORM busca_forms.

  PERFORM progresso USING TEXT-011. "'Procurando as funções utilizadas no programa'.
  PERFORM busca_funcoes.

  PERFORM progresso USING TEXT-012. "'Procurando as includes utilizadas no programa'.
  PERFORM busca_includes.

  PERFORM progresso USING TEXT-013. "'Procurando as seleções do programa'.
  PERFORM busca_select.

  PERFORM progresso USING TEXT-014. "'Procurando os eventos do programa'.
  PERFORM busca_eventos.

  PERFORM progresso USING TEXT-015. "'Procurando a Pasta Batch'.
  PERFORM busca_pastabatch.

***------------------------------------------------------------------***
***  Imprimir Espec. tec. em documento Word.                         ***
***------------------------------------------------------------------***
** Cria objeto OLE,busca entidades principais para manipular variaveis
  PERFORM progresso USING TEXT-018. "'Criando o documento do Word.'.
  PERFORM z_cria_documento.
  t_caract-programa = p_prog.
  READ TABLE t_transacao INDEX 1 INTO e_transacao.

  PERFORM z_info_geral USING t_caract-titulo_prog  "Titulo
                             t_caract-programa     "Codigo do Programa
                             e_transacao-transacao   "Transação
                             ''                      "Modulo
                             t_caract-data_criacao   "Data Criação
                             ''                      "Cliente
                             ''                      "Responsavel Proc
                             ''                      "Responsavel Func
                             ''                      "Responsavel Tec
                             ''                      "Complexidade
                             p_tit.                  "Titulo

  PERFORM z_new_line.

  CLEAR: v_qtd_request.
  DESCRIBE TABLE t_request LINES v_qtd_request.
  CLEAR e_request.

  LOOP AT t_request INTO e_request.
    PERFORM z_hist_mod_items USING  e_request-data       "Data
                                    e_request-descricao  "Descrição
                                    e_request-autor      "Solicitado
                                    e_request-num_ordem  "Change
                                    v_qtd_request."Qtdade de Request
  ENDLOOP.
** Pula uma linha
  PERFORM z_new_line.
  PERFORM z_hist_mod_header.
  PERFORM z_hist_mod_obs.

  PERFORM z_pag_conteudo.
  PERFORM z_pag_fluxo. "1.1
*
****
  PERFORM z_pag_proc_neg.  "1.2
  PERFORM z_pag_proc_neg2. "1.3

  PERFORM z_pag_proc_neg3. "2
  PERFORM z_consid_oper USING ''                     "PT  2.1
                              ''                     "EN
                              e_transacao-transacao  "Trans
                              ''                     "Tipo Proces
                              ''                     "Freq
                              t_caract-dev_class     "Pacote
                              ''                     "Depen
                              v_variant              "Vari
                              v_job                  "Job name
                              v_nome_pasta_batch.    "Pasta


  PERFORM z_pag_espec. "3
  PERFORM z_new_line.
  PERFORM z_tab_transp_header.

*Imprime quadro de tabela.
  CLEAR v_qtd_tabela.
  DESCRIBE TABLE t_desc_tab LINES v_qtd_tabela.
  LOOP AT t_desc_tab.
    PERFORM z_tab_transp_items  USING  t_desc_tab-tabname "Tabela
                                       t_desc_tab-ddtext  "Descrição
                                     ''                 "Especificação
                                     v_qtd_tabela.  "Qtdade de tabelas

    CLEAR t_desc_tab.
  ENDLOOP.

  PERFORM z_new_line.

  PERFORM  z_incl_tab_header.

  PERFORM z_incl_tab_items  USING 'N/A'   "Seq
                                  'N/A'   "Include
                                  'N/A'   "Tabela
                                   '1'.  "Qtidade de Includes de tabelas

  PERFORM z_new_line.

  PERFORM  z_campos_tab_header.

  PERFORM z_campos_tab_items USING  'N/A' "Campo
                                    'N/A' "Chave
                                    'N/A' "Elem.
                                    'N/A' "Tipo
                                    'N/A' "Tam
                                    'N/A' "Descrição
                                    'N/A' "Observação
                         '1'. "Qtdade de campos da tabela a ser criada

  PERFORM z_new_line.
  PERFORM z_trans_header.
  PERFORM z_trans_items USING  'N/A'  "Transação
                               'N/A'  "Descrição
                               '1'.         "Qtd de transações

  PERFORM z_new_line.
  PERFORM z_arq_header.
  PERFORM z_arq_items USING   'N/A' "Nome Arq
                              'N/A' "Nome Campo
                              'N/A' "Posição
                              'N/A' "Tamanho
                              'N/A' "Descrição
                              '1'.         "Qtd de Arquivos

  PERFORM z_new_line.
  PERFORM z_new_line.
  PERFORM z_param_imp_func_header.
  PERFORM z_param_imp_func_items USING  'N/A' "Campo
                                        'N/A' "Formato
                                        'N/A' "Observação
                                        '1'.

  PERFORM z_new_line.
  PERFORM z_param_exp_func_header.
  PERFORM z_param_exp_func_items USING  'N/A' "Campo
                                        'N/A' "Formato
                                        'N/A' "Observação
                                         '1'.

  PERFORM z_new_line.
  PERFORM z_param_tab_func_header.
  PERFORM z_param_tab_func_items USING  'N/A' "Tabela
                                        'N/A' "Estrutura
                                        'N/A' "Tipo de refer
                                        'N/A' "Observação
                                        '1'.

  PERFORM z_new_line.
  PERFORM z_func_excecoes_header.
  PERFORM z_func_excecoes_items USING  'N/A' "Campo
                                       'N/A' "Conteudo
                                       '1'.

  PERFORM z_new_line.
  PERFORM z_tela_selecao.
  PERFORM z_new_line.


  IF t_parameter[] IS INITIAL AND t_select_opt[] IS INITIAL.
    PERFORM z_param_selec_header.
    PERFORM z_param_selec_items USING 'N/A' "Nome
                                      'N/A' "Tipo de Parâmetro
                                      'N/A' "Texto da Tela
                                      'N/A' "Tipo de Referencia
                                      'N/A' "Valor Inicial
                                      'N/A' "Observação
                                      '1'.
    PERFORM z_new_line.
  ELSEIF NOT t_parameter[] IS INITIAL AND t_select_opt[] IS INITIAL.

*** Escreve os Parameters
    DESCRIBE TABLE t_parameter LINES v_qtd_param.
    PERFORM z_param_selec_header.
    LOOP AT t_parameter.


      CLEAR: v_par_nome, v_par_tipo, v_par_texto, v_par_campo,
             v_par_default, v_par_cons.


      MOVE: t_parameter-nome    TO v_par_nome,
            t_parameter-tipo    TO v_par_tipo,
            t_parameter-texto   TO v_par_texto,
            t_parameter-campo   TO v_par_campo,
            t_parameter-default TO v_par_default,
            t_parameter-cons    TO v_par_cons.

      PERFORM z_param_selec_items USING
                               v_par_nome      "Nome
                               v_par_tipo      "Tipo de Parâmetro
                               v_par_texto     "Texto da Tela
                               v_par_campo     "Tipo de Referencia
                               v_par_default   "Valor Inicial
                               v_par_cons      "Observação
                               v_qtd_param.
      CLEAR t_parameter.
    ENDLOOP.

    PERFORM z_new_line.

  ELSEIF NOT t_select_opt[] IS INITIAL AND t_parameter[] IS INITIAL.

*** Escreve os Select-Options.
    DESCRIBE TABLE t_select_opt LINES v_qtd_selop.
    PERFORM z_param_selec_header.
    LOOP AT t_select_opt.

      CLEAR: v_selop_nome, v_selop_tipo, v_selop_texto, v_selop_campo,
             v_selop_default, v_selop_cons.

      MOVE: t_select_opt-nome    TO v_selop_nome,
            t_select_opt-tipo    TO v_selop_tipo,
            t_select_opt-texto   TO v_selop_texto,
            t_select_opt-campo   TO v_selop_campo,
            t_select_opt-default TO v_selop_default,
            t_select_opt-cons    TO v_selop_cons.

      PERFORM z_param_selec_items USING
                                  v_selop_nome     "Nome
                                  v_selop_tipo     "Tipo de Parâmetro
                                  v_selop_texto    "Texto da Tela
                                  v_selop_campo    "Tipo de Referencia
                                  v_selop_default  "Valor Inicial
                                  v_selop_cons     "Observação
                                  v_qtd_selop.
      CLEAR t_select_opt.
    ENDLOOP.

    PERFORM z_new_line.

  ELSEIF NOT t_select_opt[] IS INITIAL AND NOT t_parameter[] IS INITIAL.

    CLEAR t_parameter.
    LOOP AT t_select_opt.

      MOVE: t_select_opt-nome    TO t_parameter-nome,
            t_select_opt-tipo    TO t_parameter-tipo,
            t_select_opt-texto   TO t_parameter-texto,
            t_select_opt-campo   TO t_parameter-campo,
            t_select_opt-default TO t_parameter-default,
            t_select_opt-cons    TO t_parameter-cons.

      APPEND t_parameter.
      CLEAR t_parameter.
    ENDLOOP.

*** Escreve os Parameters
    DESCRIBE TABLE t_parameter LINES v_qtd_param.
    PERFORM z_param_selec_header.
    LOOP AT t_parameter.

      CLEAR: v_par_nome, v_par_tipo, v_par_texto, v_par_campo,
             v_par_default, v_par_cons.

      MOVE: t_parameter-nome    TO v_par_nome,
            t_parameter-tipo    TO v_par_tipo,
            t_parameter-texto   TO v_par_texto,
            t_parameter-campo   TO v_par_campo,
            t_parameter-default TO v_par_default,
            t_parameter-cons    TO v_par_cons.

      PERFORM z_param_selec_items USING
                               v_par_nome      "Nome
                               v_par_tipo      "Tipo de Parâmetro
                               v_par_texto     "Texto da Tela
                               v_par_campo     "Tipo de Referencia
                               v_par_default   "Valor Inicial
                               v_par_cons      "Observação
                               v_qtd_param.
      CLEAR t_parameter.
    ENDLOOP.

    PERFORM z_new_line.

  ENDIF.

  PERFORM z_dados_proj_header.
  PERFORM z_dados_proj_items USING   'N/A' "Projeto
                                     'N/A' "Ampliação
                                     'N/A' "Componentes
                                     '1'.

  PERFORM z_new_line.
  PERFORM z_prog_form USING     'N/A' "Programa Standard
                                'N/A' "Formulário Standard
                                'N/A' "Transação Form
                                'N/A'. "Transação

  PERFORM z_new_line.
  PERFORM z_campos_tela_header.
  PERFORM z_campos_tela_items USING   'N/A' "Transação SAP
                                      'N/A' "Programa
                                      'N/A' "Nr.Tela
                                      'N/A' "Nome campo na tela
                                      'N/A' "Nome do Data Element
                                      'N/A' "Id Parameter
                                      'N/A' "Observações
                                      '1'.

  PERFORM z_new_line.
  PERFORM z_func_header.
  PERFORM z_func_items USING   'N/A' "Seq
                               'N/A' "Nome do Data Element
                               'N/A' "Get Parameter
                               'N/A' "Set Parameter
                               'N/A' "Função
                               '1'.

  PERFORM z_new_line.
  PERFORM z_map_campos_header.
  PERFORM z_map_campos_items USING    '  ' "Seq
                                      '  ' "Form
                                      '  ' "Tam.
                                      '  ' "Trunc.
                                      '  ' "Descrição Campo
                                      '  ' "Tabela
                                      '  ' "Campo
                                      '  ' "Observações
                                      '1'.

  PERFORM z_new_line.
  PERFORM z_textos_header.

  DESCRIBE TABLE t_texto_tra LINES v_qtd_textos.
  LOOP AT t_texto_tra.
    PERFORM z_textos_items USING  t_texto_tra-portug      "Campo Português
                                  t_texto_tra-ingles      "'Campo Inglês'
                                  '  '                    "'Outro Idioma'
                                      v_qtd_textos.
  ENDLOOP.

  PERFORM z_new_line.
  PERFORM z_msgs_header.
  PERFORM z_msgs_items USING          '  ' "Classe
                                      '  ' "Nº
                                      '  ' "Português
                                      '  ' "Inglês
                                      '  ' "Outro Idioma
                                      '1'.

  PERFORM z_new_line.
  PERFORM z_menu_header.
  PERFORM z_menu_items USING        'N/A' "Cod.
                                    'N/A' "Texto
                                    '1'.

  PERFORM z_new_line.
  PERFORM z_botoes_header.
  PERFORM z_botoes_items USING        'N/A' "Código Funcão
                                      'N/A' "Categoria de Função
                                      'N/A' "Texto da Função
                                      'N/A' "Nome do Icone
                                      'N/A' "Texto de Icones
                                      'N/A' "Texto Informativo
                                      'N/A' "Selecão Direta
                                      '1'.

  PERFORM z_pag_logic_proc."Logica de Processamento
  PERFORM z_new_line.
  PERFORM z_escreve_texto USING 'Arial'            "Font
                                '10'               "Size
                                '1'                "Bold
                                '0'                "Italic
                                '0'                "Underline
                                wdcolorautomatic   "Font Color
                                wdcolorautomatic   "BGColor
                                ''                 "Índice
                                TEXT-016.          "Texto  -  Lógica principal

  LOOP AT t_eventos.
    PERFORM z_escreve_texto USING 'Arial'            "Font
                                  '10'               "Size
                                  '0'                "Bold
                                  '0'                "Italic
                                  '0'                "Underline
                                  wdcolorautomatic   "Font Color
                                  wdcolorautomatic   "BGColor
                                  ''                 "Índice
                                  t_eventos-eventos. "Texto
    CLEAR t_eventos.
  ENDLOOP. "t_eventos

  PERFORM z_escreve_texto USING 'Arial'            "Font
                                '10'               "Size
                                '1'                "Bold
                                '0'                "Italic
                                '0'                "Underline
                                wdcolorautomatic   "Font Color
                                wdcolorautomatic   "BGColor
                                ''                 "Índice
                                TEXT-017.          "Texto  - Lógica de processamento


  LOOP AT t_forms.
    IF t_forms-e_form EQ 'X'.
      PERFORM z_escreve_texto USING 'Arial'            "Font
                                    '10'               "Size
                                    '1'                "Bold
                                    '0'                "Italic
                                    '0'                "Underline
                                    wdcolorautomatic   "Font Color
                                    wdcolorautomatic   "BGColor
                                    ''                 "Índice
                                    t_forms-form.      "Texto
    ELSEIF t_forms-e_endform EQ 'X'.
      PERFORM z_escreve_texto USING 'Arial'            "Font
                                    '10'               "Size
                                    '1'                "Bold
                                    '0'                "Italic
                                    '0'                "Underline
                                     wdcolorautomatic  "Font Color
                                     wdcolorautomatic  "BGColor
                                     ''                "Índice
                                     t_forms-form.     "Texto

      PERFORM z_new_line.

    ELSE.
      PERFORM z_escreve_texto USING 'Arial'            "Font
                                    '10'               "Size
                                    '0'                "Bold
                                    '0'                "Italic
                                    '0'                "Underline
                                    wdcolorautomatic   "Font Color
                                    wdcolorautomatic   "BGColor
                                    ''                 "Índice
                                    t_forms-form.    "Texto
    ENDIF.




    CLEAR t_forms.
  ENDLOOP.


  PERFORM z_new_line.
  PERFORM z_acesso_dados.
  PERFORM z_new_line.

***>> Tabela de Select
  LOOP AT t_selecao.

    PERFORM z_selec_from.

    DESCRIBE TABLE t_selecao-tabela LINES i_lines.
    IF i_lines GT 1.
      LOOP AT t_selecao-tabela INTO e_tabela.
        CLEAR v_tab_e_desc.
        CONCATENATE e_tabela-tabela
                    e_tabela-descricao
                    INTO
                    v_tab_e_desc
                    SEPARATED BY space.
        PERFORM z_escreve_texto USING 'Arial'        "Font
                                  '10'               "Size
                                  '0'                "Bold
                                  '0'                "Italic
                                  '0'                "Underline
                                  wdcolorautomatic   "Font Color
                                  wdcolorautomatic   "BGColor
                                  ''                 "Índice
                                  v_tab_e_desc.
      ENDLOOP. "t_selecao-tabela into e_tabela.
    ELSE.
      IF NOT t_selecao-tabela[] IS INITIAL.
        READ TABLE t_selecao-tabela INDEX i_lines INTO e_tabela.
        CLEAR v_tab_e_desc.
        CONCATENATE e_tabela-tabela
                    e_tabela-descricao
                    INTO v_tab_e_desc SEPARATED BY space.
        PERFORM z_escreve_texto USING 'Arial'        "Font
                                  '10'               "Size
                                  '0'                "Bold
                                  '0'                "Italic
                                  '0'                "Underline
                                  wdcolorautomatic   "Font Color
                                  wdcolorautomatic   "BGColor
                                  ''                 "Índice
                                  v_tab_e_desc.

      ENDIF.
    ENDIF.

    CLEAR v_tipo.
    MOVE: t_selecao-tipo TO v_tipo.
    IF NOT t_selecao-tipo IS INITIAL.
      PERFORM z_selec_tp_acesso USING v_tipo.
    ELSE.
      PERFORM z_selec_tp_acesso USING 'N/A'.
    ENDIF.

    PERFORM z_select_campos.

    CLEAR i_lines.
    DESCRIBE TABLE t_selecao-campos LINES i_lines.

    IF i_lines GT 1.
      LOOP AT t_selecao-campos INTO e_campos.
        PERFORM z_escreve_texto USING 'Arial'        "Font
                                  '10'               "Size
                                  '0'                "Bold
                                  '0'                "Italic
                                  '0'                "Underline
                                  wdcolorautomatic   "Font Color
                                  wdcolorautomatic   "BGColor
                                  ''                 "Índice
                                  e_campos-campo.

      ENDLOOP.
    ELSE.
      IF NOT t_selecao-campos[] IS INITIAL.
        READ TABLE t_selecao-campos INDEX i_lines INTO e_campos.
        PERFORM z_escreve_texto USING 'Arial'        "Font
                                  '10'               "Size
                                  '0'                "Bold
                                  '0'                "Italic
                                  '0'                "Underline
                                  wdcolorautomatic   "Font Color
                                  wdcolorautomatic   "BGColor
                                  ''                 "Índice
                                  e_campos-campo.

      ENDIF.
    ENDIF.

    CLEAR i_lines.
    DESCRIBE TABLE t_selecao-destino LINES i_lines.

    IF i_lines GT 1.
      LOOP AT t_selecao-destino INTO e_destino.
        CLEAR v_destino.
        MOVE: e_destino-tabela TO v_destino.
        PERFORM z_selec_destino USING v_destino.
      ENDLOOP.
    ELSE.
      IF NOT t_selecao-destino[] IS INITIAL.
        READ TABLE t_selecao-destino INDEX i_lines INTO e_destino.
        CLEAR v_destino.
        MOVE: e_destino-tabela TO v_destino.
        PERFORM z_selec_destino USING v_destino.
      ELSE.
        PERFORM z_selec_destino USING 'N/A'.
      ENDIF.
    ENDIF.

    CLEAR i_lines.
    DESCRIBE TABLE t_selecao-join LINES i_lines.

    IF i_lines GT 1.
      LOOP AT t_selecao-join INTO e_join.

        CLEAR v_tab_join.
        MOVE: e_join-tabela TO v_tab_join.
        PERFORM z_selec_join USING v_tab_join
                                   i_lines.

        PERFORM z_select_cond_join.
        CLEAR i_join.
        DESCRIBE TABLE t_selecao-c_join LINES i_join.
        LOOP AT t_selecao-c_join INTO e_c_join.
          CLEAR v_cond_join.
          CONCATENATE e_c_join-t_a
                      e_c_join-operador
                      e_c_join-t_b
                      e_c_join-con
                      INTO
                      v_cond_join
                      SEPARATED BY space.


          PERFORM z_escreve_texto USING 'Arial'        "Font
                                    '10'               "Size
                                    '0'                "Bold
                                    '0'                "Italic
                                    '0'                "Underline
                                    wdcolorautomatic   "Font Color
                                    wdcolorautomatic   "BGColor
                                    ''                 "Índice
                                    v_cond_join.


        ENDLOOP. "t_selecao-c_join

      ENDLOOP. "t_selecao-join
    ELSE.
      IF NOT t_selecao-join[] IS INITIAL.
        CLEAR v_tab_join.
        MOVE: e_join-tabela TO v_tab_join.
        PERFORM z_selec_join USING v_tab_join
                                   i_lines.

        PERFORM z_select_cond_join.
        CLEAR i_join.
        DESCRIBE TABLE t_selecao-c_join LINES i_join.
        LOOP AT t_selecao-c_join INTO e_c_join.
          CLEAR v_cond_join.
          CONCATENATE e_c_join-t_a
                      e_c_join-operador
                      e_c_join-t_b
                      e_c_join-con
                      INTO
                      v_cond_join
                      SEPARATED BY space.


          PERFORM z_escreve_texto USING 'Arial'        "Font
                                    '10'               "Size
                                    '0'                "Bold
                                    '0'                "Italic
                                    '0'                "Underline
                                    wdcolorautomatic   "Font Color
                                    wdcolorautomatic   "BGColor
                                    ''                 "Índice
                                    v_cond_join.


        ENDLOOP.
      ENDIF.
    ENDIF.

    CLEAR v_for_all.
    MOVE: t_selecao-for_all TO v_for_all.

    IF NOT v_for_all IS INITIAL.
      PERFORM z_selec_for_all_entries USING v_for_all.
    ELSE.
      PERFORM z_selec_for_all_entries USING 'N/A'.
    ENDIF.

    PERFORM z_select_where.

    CLEAR i_lines.
    DESCRIBE TABLE t_selecao-cond LINES i_lines.

    IF i_lines GT 1.
      LOOP AT t_selecao-cond INTO e_condicoes.

        CLEAR v_cond_where.
        CONCATENATE e_condicoes-tabela
                    e_condicoes-operador
                    e_condicoes-campo
                    e_condicoes-con
                    INTO v_cond_where SEPARATED BY space.


        PERFORM z_escreve_texto USING 'Arial'        "Font
                                  '10'               "Size
                                  '0'                "Bold
                                  '0'                "Italic
                                  '0'                "Underline
                                  wdcolorautomatic   "Font Color
                                  wdcolorautomatic   "BGColor
                                  ''                 "Índice
                                  v_cond_where.


      ENDLOOP.
    ELSE.
      READ TABLE t_selecao-cond INDEX 1 INTO e_condicoes.
      CLEAR v_cond_where.
      CONCATENATE e_condicoes-tabela
                  e_condicoes-operador
                  e_condicoes-campo
                  e_condicoes-con
                  INTO v_cond_where SEPARATED BY space.


      PERFORM z_escreve_texto USING 'Arial'        "Font
                                '10'               "Size
                                '0'                "Bold
                                '0'                "Italic
                                '0'                "Underline
                                wdcolorautomatic   "Font Color
                                wdcolorautomatic   "BGColor
                                ''                 "Índice
                                v_cond_where.
    ENDIF.

    IF NOT t_selecao-classf IS INITIAL.
      PERFORM z_selec_order_by USING t_selecao-classf. "<campo>
    ELSE.
      PERFORM z_selec_order_by USING 'N/A'. "<campo>
    ENDIF.

    IF NOT t_selecao-agrup IS INITIAL.
      PERFORM z_selec_group_by USING t_selecao-agrup. "<campo>
    ELSE.
      PERFORM z_selec_group_by USING 'N/A'. "<campo>
    ENDIF.

    PERFORM z_avanca_fim_tabela.
    PERFORM z_new_line.
    PERFORM z_new_line.
  ENDLOOP.


  PERFORM z_new_line.
  PERFORM  z_tratam_dados.
  PERFORM z_pag_roteiro_testes.
  PERFORM z_cria_conteudo USING '2'.

* Salvar Como
  PERFORM z_salva_documento USING v_file.

* Fecha MS-Word
  PERFORM z_close_application.

* Limpar as tabelas e liberar a memoria.
  FREE: t_prog    , t_busca  , tk       ,
        stm       , t_tab    , t_const  , t_tab_int,
        t_var     , t_par    , t_include, t_rotinas,
        t_funcoes , t_select .
  CLEAR: t_prog   , t_busca  ,
         tk       , stm      , t_tab    , t_const  ,
         t_tab_int, t_var    , t_par    , t_include,
         t_rotinas, t_funcoes, t_select.

***------------------------------------------------------------------***
***  FORMS                                                           ***
***------------------------------------------------------------------***

*---------------------------------------------------------------------*
*       FORM ws_filename_get                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ARQUIVO                                                       *
*  -->  P_MODE                                                        *
*---------------------------------------------------------------------*
FORM ws_filename_get USING arquivo p_mode.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = p_mode
    IMPORTING
      file_name     = arquivo.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM busca_tabelas                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM busca_tabelas.
  t_busca-linha = 'TABLES'.
  APPEND t_busca.
  CLEAR  t_busca.
  SCAN ABAP-SOURCE t_prog
        TOKENS     INTO tk
        STATEMENTS INTO stm
        KEYWORDS   FROM t_busca.
  DELETE tk WHERE str = 'TABLES'.
  CLEAR t_tab.
  LOOP AT tk.
    t_tab-tabela = tk-str.
    APPEND t_tab.
    CLEAR t_tab.
  ENDLOOP.


  IF NOT t_tab[] IS INITIAL.

    SELECT tabname     "tabela
           ddlanguage  "idioma
           ddtext      "Descrição
    FROM dd02t
    INTO TABLE t_desc_tab
    FOR ALL ENTRIES IN t_tab
    WHERE tabname    EQ t_tab-tabela
    AND   ddlanguage EQ sy-langu.

    IF sy-subrc IS INITIAL.
    ENDIF.
  ENDIF.

  FREE:  t_busca, tk, stm.
  CLEAR: t_busca, tk, stm, t_tab.
ENDFORM. " busca_tabelas

*---------------------------------------------------------------------*
*       FORM busca_constantes                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM busca_constantes.
  t_busca-linha = 'CONSTANTS'.
  APPEND t_busca.
  CLEAR  t_busca.
  SCAN ABAP-SOURCE t_prog
        TOKENS      INTO tk
        STATEMENTS  INTO stm
        KEYWORDS    FROM t_busca.
  DELETE tk WHERE str = 'CONSTANTS'.
  LOOP AT tk.
    CLEAR t_const.
    t_const-constante = tk-str.
    APPEND t_const.
  ENDLOOP.
  FREE:  t_busca, tk, stm.
  CLEAR: t_busca, tk, stm, t_const.

  IF NOT t_const[] IS INITIAL.
***-----------------------------------------------------------------***
***Função para traduzir o código em Português estruturado           ***
***-----------------------------------------------------------------***

    PERFORM funcao_espec_tec TABLES t_const USING 'X' space space.

  ENDIF.

ENDFORM. " busca_constantes

*---------------------------------------------------------------------*
*       FORM busca_variaveis_tab_int                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM busca_variaveis_tab_int.
  DATA: aux1 LIKE sy-index,
        aux2 LIKE sy-index.
  t_busca-linha = 'DATA'.
  APPEND t_busca.
  CLEAR: t_busca, linha.
  SCAN ABAP-SOURCE t_prog
        TOKENS     INTO tk
        STATEMENTS INTO stm
        KEYWORDS   FROM t_busca.
  DELETE tk WHERE str = 'DATA'.
  CLEAR t_tab_int.
  LOOP AT tk.
    IF linha IS INITIAL.
      linha = tk-row.
      t_tab_int-linha = tk-str.
    ELSE.
      IF linha <> tk-row.
        APPEND t_tab_int.
        CLEAR t_tab_int.
        linha = tk-row.
        t_tab_int-linha = tk-str.
      ELSE.
        CONCATENATE t_tab_int-linha tk-str INTO t_tab_int-linha
                                             SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF NOT t_tab_int-linha IS INITIAL.
    APPEND t_tab_int.
  ENDIF.
  FREE:  t_busca, tk, stm.
  CLEAR: t_busca, tk, stm, t_tab_int.
  t_var[] = t_tab_int[].
  DATA vl_linha TYPE c.

  WHILE  vl_linha <> 'X'.
    LOOP AT t_var.
      IF t_var-linha(6) = 'BEGIN '.
        aux1 = sy-tabix.
      ELSEIF t_var-linha(4) = 'END '.
        aux2 = sy-tabix.
        DELETE t_var FROM aux1 TO aux2.
        CLEAR: aux1, aux2.
        EXIT.
      ENDIF.
    ENDLOOP.
    READ TABLE t_var WITH KEY linha(6) = 'BEGIN '.
    IF NOT sy-subrc IS INITIAL.
      vl_linha = 'X'.
    ENDIF.
  ENDWHILE.

  LOOP AT t_tab_int.
    IF t_tab_int-linha(6) = 'BEGIN ' AND aux1 IS INITIAL.
      aux1 = sy-tabix.
    ELSE.
      IF t_tab_int-linha(4) = 'END '.
        CLEAR aux1.
      ELSEIF aux1 IS INITIAL.
        DELETE t_tab_int INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.
  CLEAR: t_var, t_tab_int.

***-----------------------------------------------------------------***
***Função para traduzir o código em Português estruturado           ***
***-----------------------------------------------------------------***
  PERFORM funcao_espec_tec TABLES t_tab_int USING 'X' space space.

***-----------------------------------------------------------------***
***Função para traduzir o código em Português estruturado           ***
***-----------------------------------------------------------------***
  PERFORM funcao_espec_tec TABLES t_var USING 'X' space space.

ENDFORM. " busca_variaveis_tab_int

*---------------------------------------------------------------------*
*       FORM busca_parametros                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM busca_parametros.
  DATA: i_linha TYPE i,
        t_aux   LIKE t_split OCCURS 0 WITH HEADER LINE.

  t_busca-linha = 'PARAMETERS'.
  APPEND t_busca.
  CLEAR: t_busca.
  t_busca-linha = 'PARAMETER'.
  APPEND t_busca.
  CLEAR: t_busca, linha.
  SCAN ABAP-SOURCE t_prog
        TOKENS      INTO tk
        STATEMENTS  INTO stm
        KEYWORDS    FROM t_busca.

  LOOP AT tk.

    IF tk-str CS '-'.
      CLEAR t_tabcampo.
      t_tabcampo-campotbl = tk-str.
      APPEND t_tabcampo.
    ENDIF.

    IF  tk-str EQ 'PARAMETER'
    OR  tk-str EQ 'PARAMETERS'.

      IF t_par-par IS INITIAL.
        CONTINUE.
      ELSE.
        APPEND t_par.
        CLEAR t_par.
      ENDIF.

    ELSE.

      CONCATENATE t_par-par tk-str INTO t_par-par
                              SEPARATED BY space.

    ENDIF.

  ENDLOOP. "tk

  IF NOT t_par-par IS INITIAL.
    APPEND t_par.
    CLEAR t_par.
  ENDIF.


******************
  CLEAR t_par.
  LOOP AT t_par.
    SHIFT t_par-par LEFT DELETING LEADING space.
    TRANSLATE t_par-par USING ' #'.
    SPLIT t_par-par AT '#' INTO TABLE t_split.
    CLEAR t_split.
    t_aux[] = t_split[].
*** Pego o parâmetro de selecao.
    READ TABLE t_split INDEX 1.
    MOVE t_split-campo TO t_parameter-nome.
*** Pego tudo que é like ou type
    READ TABLE t_split WITH KEY campo = 'LIKE'.
    IF sy-subrc IS INITIAL.
      MOVE t_split-campo TO t_parameter-campo.
      i_linha = sy-tabix + 1.
      READ TABLE t_aux INDEX i_linha.
      CONCATENATE t_parameter-campo t_aux-campo
             INTO t_parameter-campo SEPARATED BY space.
      CLEAR i_linha.
    ELSE.
      READ TABLE t_split WITH KEY campo = 'TYPE'.
      IF sy-subrc IS INITIAL.
        MOVE t_split-campo TO t_parameter-campo.
        i_linha = sy-tabix + 1.
        READ TABLE t_aux INDEX i_linha.
        CONCATENATE t_parameter-campo t_aux-campo
               INTO t_parameter-campo SEPARATED BY space.
        CLEAR i_linha.
      ENDIF.
    ENDIF.

*** Procuro o texto da tela se aplicável.
*    READ TABLE t_textos INTO e_textos WITH KEY chave = t_aux-campo.
    IF sy-subrc IS INITIAL.
*      t_parameter-campo = e_textos-texto.
    ENDIF.


*** Verifico se o parâmetro tem valor default.
    READ TABLE t_split WITH KEY campo = 'DEFAULT'.
    IF sy-subrc IS INITIAL.
      i_linha = sy-tabix + 1.
      READ TABLE t_aux INDEX i_linha.
      MOVE t_aux-campo TO t_parameter-default.
      CLEAR i_linha.
    ENDIF.

*** Verifico o tipo de parâmetro.
    LOOP AT t_split.
      CASE t_split-campo.
        WHEN 'RADIOBUTTON'.
          t_parameter-tipo = t_split-campo.
          CONTINUE.
        WHEN 'AS'.
          i_linha = sy-tabix + 1.
          READ TABLE t_aux INDEX i_linha.
          IF t_aux-campo = 'SEARCH'.
            CONCATENATE t_split-campo t_aux-campo
                   INTO t_parameter-tipo SEPARATED BY space.
            ADD 1 TO i_linha.
            READ TABLE t_aux INDEX i_linha.
            CONCATENATE t_parameter-tipo t_aux-campo
                   INTO t_parameter-tipo SEPARATED BY space.
          ELSE.
            CONCATENATE t_split-campo t_aux-campo
                   INTO t_parameter-tipo SEPARATED BY space.
          ENDIF.
          CLEAR i_linha.
          CONTINUE.
        WHEN OTHERS.
          IF t_parameter-tipo IS INITIAL.
            t_parameter-tipo = 'PARAMETERS'.
            CONTINUE.
          ENDIF.
      ENDCASE.
*** Pego as opções complementares se houver.
      CASE t_split-campo.
        WHEN 'DECIMALS'.
          i_linha = sy-tabix + 1.
          READ TABLE t_aux INDEX i_linha.
          CONCATENATE t_split-campo t_aux-campo
                 INTO t_parameter-cons SEPARATED BY space.
          CLEAR i_linha.
          CONTINUE.
        WHEN 'MATCHCODE' OR 'FOR' OR 'VISIBLE' OR 'MODIF'.
          i_linha = sy-tabix + 1.
          READ TABLE t_aux INDEX i_linha.
          CONCATENATE t_split-campo t_aux-campo
                 INTO t_parameter-cons SEPARATED BY space.
          ADD 1 TO i_linha.
          READ TABLE t_aux INDEX i_linha.
          CONCATENATE t_parameter-cons t_aux-campo
                 INTO t_parameter-cons SEPARATED BY space.
          CLEAR i_linha.
          CONTINUE.
        WHEN 'GROUP'.
          i_linha = sy-tabix + 1.
          READ TABLE t_aux INDEX i_linha.
          CONCATENATE t_split-campo t_aux-campo
                 INTO t_parameter-cons SEPARATED BY space.
          CLEAR i_linha.
          CONTINUE.
        WHEN OTHERS.
          IF t_parameter-cons IS INITIAL AND
           ( t_parameter-cons NE 'LIKE' OR t_parameter-cons NE 'TYPE' ).
            t_parameter-cons = t_split-campo.
            CONTINUE.
          ENDIF.
      ENDCASE.
    ENDLOOP.
    APPEND t_parameter.
    CLEAR: t_split, t_parameter, t_aux.
    REFRESH: t_split, t_aux.
  ENDLOOP.
*****************
*** Procuro o texto da tela se aplicável.

  LOOP AT t_parameter.

    IF sy-subrc IS INITIAL.
*      t_parameter-texto = e_textos-texto.
      MODIFY t_parameter.
    ENDIF.
    CLEAR t_parameter.
  ENDLOOP.
*****


  FREE:  t_busca, tk, stm.
  CLEAR: t_busca, tk, stm, t_par.

ENDFORM. " busca_parametros

*---------------------------------------------------------------------*
*       FORM busca_rotinas                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM busca_rotinas.
  t_busca-linha = 'PERFORM'.
  APPEND t_busca.
  CLEAR: t_busca, linha, t_rotinas.
  REFRESH t_rotinas.

  SCAN ABAP-SOURCE t_prog
        TOKENS     INTO tk
        STATEMENTS INTO stm
        KEYWORDS   FROM t_busca.

  LOOP AT tk WHERE str = 'PERFORM'.
    IF tk-row <> linha.
      linha = tk-row.
    ELSE.
      DELETE tk INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  LOOP AT tk.
    IF linha IS INITIAL.
      linha = tk-row.
      t_rotinas-rotinas = tk-str.
    ELSE.
      IF linha <> tk-row.
        IF NOT t_rotinas-rotinas IS INITIAL.
          APPEND t_rotinas.
        ENDIF.
        CLEAR t_rotinas.
        linha = tk-row.
        t_rotinas-rotinas = tk-str.
      ELSE.
        IF tk-str NE 'PERFORM'.
          CONCATENATE t_rotinas-rotinas tk-str INTO t_rotinas-rotinas
                                                   SEPARATED BY space.
        ELSE.
          APPEND t_par.
          CLEAR t_par.
          linha = tk-row.
          t_par-par = tk-str.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF NOT t_rotinas-rotinas IS INITIAL.
    APPEND t_rotinas.
  ENDIF.

***-----------------------------------------------------------------***
***Função para traduzir o código em Português estruturado           ***
***-----------------------------------------------------------------***
  PERFORM funcao_espec_tec TABLES t_rotinas USING space 'X' space.

  FREE:  t_busca, tk, stm.
  CLEAR: t_busca, tk, stm, t_rotinas.
ENDFORM. " busca_rotinas

*---------------------------------------------------------------------*
*       FORM busca_funcoes                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM busca_funcoes.
  t_busca-linha = 'FUNCTION'.
  APPEND t_busca.
  CLEAR  t_busca.
  SCAN ABAP-SOURCE t_prog
        TOKENS      INTO tk
        STATEMENTS  INTO stm
        KEYWORDS    FROM t_busca.
  DELETE tk WHERE str = 'FUNCTION'.
  LOOP AT tk.
    IF linha IS INITIAL.
      linha = tk-row.
      t_funcoes-funcoes = tk-str.
    ELSE.
      IF linha <> tk-row.
        IF NOT t_funcoes-funcoes IS INITIAL.
          APPEND t_funcoes.
        ENDIF.
        CLEAR t_funcoes.
        linha = tk-row.
        t_funcoes-funcoes = tk-str.
      ELSE.
        CONCATENATE t_funcoes-funcoes tk-str INTO t_funcoes-funcoes
                                                 SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF NOT t_funcoes-funcoes IS INITIAL.
    APPEND t_funcoes.
  ENDIF.
  FREE:  t_busca, tk, stm.
  CLEAR: t_busca, tk, stm, t_funcoes.
ENDFORM. " busca_funçoes

*---------------------------------------------------------------------*
*       FORM busca_includes                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM busca_includes.
  t_busca-linha = 'INCLUDE'.
  APPEND t_busca.
  CLEAR  t_busca.
  SCAN ABAP-SOURCE t_prog
        TOKENS     INTO tk
        STATEMENTS INTO stm
        KEYWORDS   FROM t_busca.
  DELETE tk WHERE str = 'INCLUDE'.

  LOOP AT tk.
    IF linha IS INITIAL.
      linha = tk-row.
      t_include-include = tk-str.
    ELSE.
      IF linha <> tk-row.
        IF NOT t_include-include IS INITIAL.
          APPEND t_include.
        ENDIF.
        CLEAR t_include.
        linha = tk-row.
        t_include-include = tk-str.
      ELSE.

        CONCATENATE t_include-include tk-str INTO t_include-include
                                                 SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF NOT t_include-include IS INITIAL.
    APPEND t_include.
  ENDIF.
  FREE:  t_busca, tk, stm.
  CLEAR: t_busca, tk, stm, t_include.


ENDFORM. " busca_includes


*---------------------------------------------------------------------*
*       FORM busca_select                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM busca_select.

  DATA: i_c    TYPE i,
        i_f    TYPE i,
        tk_aux LIKE tk OCCURS 0 WITH HEADER LINE.

  t_busca-linha = 'SELECT'.
  APPEND t_busca.
  CLEAR  t_busca.
  SCAN ABAP-SOURCE t_prog
        TOKENS     INTO tk
        STATEMENTS INTO stm
        KEYWORDS   FROM t_busca.
  LOOP AT tk.
    IF linha IS INITIAL.
      linha = tk-row.
      t_select-select = tk-str.
    ELSE.
      IF linha <> tk-row.
        IF NOT t_select-select IS INITIAL.
          APPEND t_select.
        ENDIF.
        CLEAR t_select.
        linha = tk-row.
        t_select-select = tk-str.
      ELSE.
        CONCATENATE t_select-select tk-str INTO t_select-select
                                                 SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF NOT t_select-select IS INITIAL.
    APPEND t_select.
  ENDIF.

  CLEAR tk.
  tk_aux[] = tk[].
  LOOP AT tk WHERE str = 'SELECT'.
    i_c = sy-tabix.
*** Busca o final do select para poder trabalhar em bloco
    PERFORM busca_final TABLES   tk_aux
                        USING    i_c
                        CHANGING i_f.
*** Busca o tipo de select se aplicável
    PERFORM busca_tipo TABLES tk_aux
                        USING i_c
                              i_f.
*** Busca os campos do select
    PERFORM busca_campos TABLES tk_aux
                          USING i_c
                                i_f.
*** Busca a tabela do select
    PERFORM busca_from TABLES tk_aux
                        USING i_c
                              i_f.
*** Busca a condição geral do select
    PERFORM busca_where TABLES tk_aux
                         USING i_c
                               i_f.
*** Busca o tipo de join e sua condição se aplicável
    PERFORM busca_join TABLES tk_aux
                          USING i_c
                                i_f.
*** Busca a tabela de destino se aplicável
    PERFORM busca_into TABLES tk_aux
                        USING i_c
                              i_f.
*** Busca o for all entries se aplicável
    PERFORM busca_for TABLES tk_aux
                       USING i_c
                             i_f.
*** Busca a classificação se aplicável
    PERFORM busca_classificacao TABLES tk_aux
                                 USING i_c
                                       i_f.
*** Busca o agrupento se aplicável
    PERFORM busca_agrupamento TABLES tk_aux
                               USING i_c
                                     i_f.
    APPEND t_selecao.
    CLEAR: t_selecao,         t_selecao-tabela[], t_selecao-campos[],
           t_selecao-destino[], t_selecao-join[],   t_selecao-c_join[],
           t_selecao-cond[].
  ENDLOOP.

  FREE:  t_busca, tk, stm.
  CLEAR: t_busca, tk, stm, t_select.
ENDFORM. " busca_select


*&---------------------------------------------------------------------*
*&      Form  busca_forms
*&---------------------------------------------------------------------*
*  Rotina que busca o conteudo de todos os forms.
*----------------------------------------------------------------------*
FORM busca_forms.

  DATA: l_endform(72) TYPE c,
        l_inicio      TYPE i,
        l_fim         TYPE i.

  t_busca-linha = 'FORM'.
  APPEND t_busca.
  CLEAR t_busca.
  t_busca-linha = 'ENDFORM'.
  APPEND t_busca.
  CLEAR t_busca.

  SCAN ABAP-SOURCE t_prog
        TOKENS     INTO tk
        STATEMENTS INTO stm
        KEYWORDS   FROM t_busca.

  DELETE tk WHERE str <> 'FORM'
            AND   str <> 'ENDFORM'.

  CLEAR: l_inicio, l_fim.
  CLEAR t_forms_aux.
  LOOP AT tk.


    IF l_inicio IS INITIAL
    AND tk-str  EQ 'FORM'.

      l_inicio = tk-row.
      CONTINUE.

    ELSEIF l_endform IS INITIAL
    AND    tk-str    EQ 'ENDFORM'.
      l_fim = tk-row.

      t_forms_aux-inicio = l_inicio.
      t_forms_aux-fim = l_fim.
      APPEND t_forms_aux.
      CLEAR t_forms_aux.

      CLEAR: l_inicio, l_fim.
    ENDIF.

    CLEAR tk.
  ENDLOOP.

  LOOP AT t_forms_aux.

    LOOP AT t_prog FROM t_forms_aux-inicio TO t_forms_aux-fim.
      CLEAR t_forms.
      t_forms-form = t_prog-linha.
      APPEND t_forms.
      CLEAR t_prog.
    ENDLOOP.

    CLEAR t_forms_aux.
  ENDLOOP.

  FREE:  t_busca, tk, stm.
  CLEAR: t_busca, tk, stm, t_rotinas, t_forms.

***-----------------------------------------------------------------***
***Função para traduzir o código em Português estruturado           ***
***-----------------------------------------------------------------***
  PERFORM funcao_espec_tec TABLES t_forms USING space 'X' 'X'.

  CLEAR: t_forms, v_linha_form.
  LOOP AT t_forms.

    CLEAR v_linha_form.
    MOVE t_forms-form TO v_linha_form.
    SHIFT v_linha_form LEFT  DELETING LEADING  space.

    IF v_linha_form(06) EQ c_rotina.

      t_forms-e_form = 'X'.
      MODIFY t_forms.


    ELSEIF v_linha_form(13) EQ c_fimdarotina.

      t_forms-e_endform = 'X'.
      MODIFY t_forms.

    ENDIF.

    CLEAR t_forms.
  ENDLOOP. "t_forms


ENDFORM. " busca_forms
*&---------------------------------------------------------------------*
*&      Form  busca_eventos
*&---------------------------------------------------------------------*
*  Buscar lógica principal, conteudo dos eventos.
*----------------------------------------------------------------------*
FORM busca_eventos.

  DATA: l_evento(72)    TYPE c,      "evento
        l_inicio        TYPE i,
        l_fim           TYPE i,
        l_ultimo_evento TYPE i,
        l_ult_linha_cod TYPE i.

  t_busca-linha = 'START-OF-SELECTION'.
  APPEND t_busca.
  CLEAR t_busca.
  t_busca-linha = 'END-OF-SELECTION'.
  APPEND t_busca.
  CLEAR t_busca.
  t_busca-linha = 'AT'.
  APPEND t_busca.
  CLEAR t_busca.
  t_busca-linha = 'INITIALIZATION'.
  APPEND t_busca.
  CLEAR t_busca.
  t_busca-linha = 'LOAD-OF-PROGRAM'.
  APPEND t_busca.
  CLEAR t_busca.
  t_busca-linha = 'TOP-OF-PAGE'.
  APPEND t_busca.
  CLEAR t_busca.
  t_busca-linha = 'END-OF-PAGE'.
  APPEND t_busca.
  CLEAR t_busca.
  t_busca-linha = 'FORM'.
  APPEND t_busca.
  CLEAR t_busca.

  SCAN ABAP-SOURCE t_prog
        TOKENS     INTO tk
        STATEMENTS INTO stm
        KEYWORDS   FROM t_busca.

  DELETE tk WHERE str <> 'START-OF-SELECTION'
            AND   str <> 'END-OF-SELECTION'
            AND   str <> 'SELECTION-SCREEN'
            AND   str <> 'LINE-SELECTION'
            AND   str <> 'USER-COMMAND'
            AND   str <> 'INITIALIZATION'
            AND   str <> 'LOAD-OF-PROGRAM'
            AND   str <> 'TOP-OF-PAGE'
            AND   str <> 'END-OF-PAGE'
            AND   str <> 'FORM'.


  CLEAR: l_ultimo_evento, l_ult_linha_cod.
  DESCRIBE TABLE tk LINES l_ultimo_evento.
  DESCRIBE TABLE t_prog LINES l_ult_linha_cod.

  LOOP AT tk.

    IF l_inicio IS INITIAL AND
       tk-str   NE 'FORM'.
      l_evento = tk-str.
      l_inicio = tk-row.

      IF l_ultimo_evento EQ sy-tabix.

        l_fim = l_ult_linha_cod.
        CLEAR t_eventos_aux.
        t_eventos_aux-inicio = l_inicio.
        t_eventos_aux-fim = l_fim.
        APPEND t_eventos_aux.

      ENDIF.

      CONTINUE.

    ELSEIF l_fim    IS INITIAL AND
       NOT l_inicio IS INITIAL AND
           tk-str   <> l_evento.

      CLEAR l_evento.
      l_evento = tk-str.
      l_fim    = tk-row - 1.

      CLEAR t_eventos_aux.
      t_eventos_aux-inicio = l_inicio.
      t_eventos_aux-fim = l_fim.
      APPEND t_eventos_aux.

      IF l_evento <> 'FORM'.
        CLEAR: l_inicio.
        l_inicio = l_fim.

      ELSE.

        CLEAR: l_inicio, l_fim, l_evento.
      ENDIF.
      CLEAR: l_fim.
    ENDIF.
    CLEAR tk.
  ENDLOOP. " TK

  LOOP AT t_eventos_aux.
    LOOP AT t_prog FROM t_eventos_aux-inicio TO t_eventos_aux-fim.

      IF t_prog-linha(1) NE '*'.
        t_eventos-eventos = t_prog-linha.
        APPEND t_eventos.
        CLEAR t_eventos.
      ENDIF.

      CLEAR t_prog.
    ENDLOOP.
    CLEAR t_eventos_aux.
  ENDLOOP.

  FREE:  t_busca, tk, stm.
  CLEAR: t_busca, tk, stm.


***-----------------------------------------------------------------***
***Função para traduzir o código em Português estruturado           ***
***-----------------------------------------------------------------***
  PERFORM funcao_espec_tec TABLES t_eventos USING space 'X' 'X'.

ENDFORM. " busca_eventos
*&---------------------------------------------------------------------*
*&      Form  busca_caract
*&---------------------------------------------------------------------*
*    Captura as informações de Request, Textos e Caracteriscas do
*    Programa
*----------------------------------------------------------------------*
FORM busca_caract.

**** Criação de Variaveis de tipos compativeis para Classe.
  DATA : l_prog     TYPE programm,      " Nome do Programa
         l_var(200) TYPE c,
         l_job(200) TYPE c.

*** Objetos para captura das Informações.
  SELECT SINGLE tcode  FROM tstc
                     INTO (e_transacao-transacao)
                     WHERE pgmna EQ p_prog.

*** Captura o nome do Programa
  IF NOT p_prog IS INITIAL.
    MOVE: p_prog TO l_prog.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR t_texto_tra.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR v_variant.

    CLEAR v_job.
    LOOP AT t_job INTO e_job.

      IF v_job IS INITIAL.
        MOVE: e_job-jobname TO v_job.
      ELSE.
        CLEAR l_var.
        MOVE e_job-jobname TO l_job.
        CONCATENATE v_job
                    l_job
                    INTO
                    v_job
                    SEPARATED BY ' / '.
      ENDIF.

    ENDLOOP.
  ENDIF.


ENDFORM. " busca_caract

*** ==> Início AB - 26/04/2006
*&---------------------------------------------------------------------*
*&      Form  busca_final
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TK_AUX  text
*      <--P_I_C  text
*----------------------------------------------------------------------*
FORM busca_final TABLES p_tk_aux STRUCTURE tk
                 USING    p_i_c
                 CHANGING p_i_f.

  p_i_f = p_i_c.
  IF p_i_c NE 1.
    ADD 1 TO p_i_f.
    READ TABLE p_tk_aux INDEX p_i_f.
  ENDIF.

  WHILE p_tk_aux-str <> 'SELECT'.
    ADD 1 TO p_i_f.
    READ TABLE p_tk_aux INDEX p_i_f.
    IF sy-subrc <> 0.
      SUBTRACT 1 FROM p_i_f.
      EXIT.
    ENDIF.
  ENDWHILE.
ENDFORM. " busca_final
*&---------------------------------------------------------------------*
*&      Form  BUSCA_TIPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TK_AUX  text
*      -->P_I_C  text
*      -->P_I_F  text
*----------------------------------------------------------------------*
FORM busca_tipo TABLES p_tk_aux STRUCTURE tk
                USING    p_i_c
                         p_i_f.
  DATA: t_aux LIKE tk OCCURS 0 WITH HEADER LINE,
        linha TYPE i.

  LOOP AT p_tk_aux FROM p_i_c TO p_i_f.
    CASE p_tk_aux-str.
      WHEN 'SINGLE' OR 'DISTINCT' OR 'MAX' OR 'COUNT'.
        t_selecao-tipo = p_tk_aux-str.
      WHEN 'UP'.
        t_aux[] = p_tk_aux[].
        READ TABLE t_aux INDEX sy-tabix.
        linha = sy-tabix.
        DO 4 TIMES.
          IF t_selecao-tipo IS INITIAL.
            t_selecao-tipo = t_aux-str.
          ELSE.
            CONCATENATE t_selecao-tipo t_aux-str
                   INTO t_selecao-tipo SEPARATED BY space.
          ENDIF.
          ADD 1 TO linha.
          READ TABLE t_aux INDEX linha.
        ENDDO.
    ENDCASE.
  ENDLOOP.

  CLEAR: linha, t_aux[].
  REFRESH: t_aux.
  FREE: t_aux, linha.
ENDFORM. " BUSCA_TIPO
*&---------------------------------------------------------------------*
*&      Form  busca_from
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TK_AUX  text
*      -->P_I_C  text
*      -->P_I_F  text
*----------------------------------------------------------------------*
FORM busca_from TABLES p_tk_aux STRUCTURE tk
                USING    p_i_c
                         p_i_f.
  TABLES: dd02t.

  DATA: tl_aux   LIKE tk OCCURS 0 WITH HEADER LINE,
        tl_dd02t TYPE dd02t OCCURS 0 WITH HEADER LINE,
        linha    TYPE i,
        linha1   TYPE i,
        linha_i  TYPE i,
        linha_f  TYPE i.

  tl_aux[] = p_tk_aux[].

  LOOP AT p_tk_aux FROM p_i_c TO p_i_f.
    CASE p_tk_aux-str.

      WHEN 'FROM'.
        linha_i = sy-tabix.
      WHEN 'WHERE' OR 'LEFT' OR 'FOR' OR 'INNER'.
        linha_f = sy-tabix - 1.
        EXIT.
    ENDCASE.
  ENDLOOP.

  LOOP AT p_tk_aux FROM linha_i TO linha_f.
    IF p_tk_aux-str = 'FROM'.
      tl_aux[] = p_tk_aux[].
      linha = sy-tabix + 1.
      DO.
        READ TABLE tl_aux INDEX linha.
        IF tl_aux-str EQ 'WHERE' OR
           tl_aux-str EQ 'LEFT'  OR
           tl_aux-str EQ 'INTO'  OR
           tl_aux-str EQ 'FOR'   OR
           tl_aux-str EQ 'INNER'.
          EXIT.
        ENDIF.
        IF tl_aux-str EQ 'AS'.
          t_tabela-apelido = tl_aux-str.
          linha1 = linha.
          ADD 1 TO linha1.
          READ TABLE tl_aux INDEX linha1.
          CONCATENATE t_tabela-apelido tl_aux-str
                 INTO t_tabela-apelido SEPARATED BY space.
        ENDIF.

        t_tabela-tabela = tl_aux-str.
        PERFORM scan_source_get_field_descr TABLES tl_dd02t USING tl_aux-str.

        t_tabela-descricao = dd02t-ddtext.
        APPEND t_tabela.
        ADD 1 TO linha.
      ENDDO.
    ENDIF.
  ENDLOOP.

  t_selecao-tabela[] = t_tabela[].
  CLEAR: t_tabela[], tl_aux[], linha, linha1.
  REFRESH: t_tabela, tl_aux.
  FREE:  t_tabela, tl_aux, linha, linha1.

ENDFORM. " busca_from
*&---------------------------------------------------------------------*
*&      Form  busca_where
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TK_AUX  text
*      -->P_I_C  text
*      -->P_I_F  text
*----------------------------------------------------------------------*
FORM busca_where TABLES p_tk_aux STRUCTURE tk
                 USING    p_i_c
                          p_i_f.
  DATA: t_aux  LIKE tk OCCURS 0 WITH HEADER LINE,
        linha  TYPE i,
        linha1 TYPE i,
        vezes  TYPE i.

  LOOP AT p_tk_aux FROM p_i_c TO p_i_f.
    IF p_tk_aux-str EQ 'WHERE'.
      t_aux[] = p_tk_aux[].
      linha = sy-tabix + 1.
      DO.
        READ TABLE t_aux INDEX linha.
        IF t_aux-str EQ 'GROUP' OR
           t_aux-str EQ 'HAVING'  OR
           t_aux-str EQ 'ORDER' OR
           linha     GT p_i_f.
          EXIT.
        ENDIF.

        IF t_aux-str = '('.
          linha1 = linha.
          ADD 1 TO vezes.
          WHILE t_aux-str <> ')'.
            ADD 1 TO: vezes, linha1.
            READ TABLE t_aux INDEX linha1.
          ENDWHILE.
        ELSE.
          vezes = 3.
        ENDIF.

        DO vezes TIMES.
          CASE vezes.
            WHEN 3.
              IF t_condicoes-tabela IS INITIAL.
                t_condicoes-tabela = t_aux-str.
              ELSEIF t_condicoes-operador IS INITIAL.
                t_condicoes-operador = t_aux-str.
              ELSE.
                t_condicoes-campo = t_aux-str.
              ENDIF.
            WHEN OTHERS.
              READ TABLE t_aux INDEX linha.
              IF t_condicoes-tabela IS INITIAL.
                t_condicoes-tabela = t_aux-str.
              ELSEIF t_condicoes-tabela = '('.
                CONCATENATE t_condicoes-tabela t_aux-str
                       INTO t_condicoes-tabela SEPARATED BY space.
              ELSEIF t_condicoes-operador IS INITIAL.
                t_condicoes-operador = t_aux-str.
              ELSEIF t_condicoes-campo IS INITIAL.
                t_condicoes-campo = t_aux-str.
              ELSEIF t_aux-str EQ ')'.
                CONCATENATE t_condicoes-campo t_aux-str
                       INTO t_condicoes-campo SEPARATED BY space.
              ELSEIF t_aux-str = 'AND' OR t_aux-str = 'OR'.
                t_condicoes-con = t_aux-str.
              ELSEIF NOT ( t_condicoes-tabela   IS INITIAL AND
                           t_condicoes-operador IS INITIAL AND
                           t_condicoes-campo    IS INITIAL ).
                APPEND t_condicoes.
                CLEAR: t_condicoes.
                t_condicoes-tabela = t_aux-str.
              ENDIF.
          ENDCASE.
          ADD 1 TO linha.
          READ TABLE t_aux INDEX linha.
        ENDDO.

        IF t_aux-str = 'AND' OR t_aux-str = 'OR'.
          t_condicoes-con = t_aux-str.
        ENDIF.

        IF vezes NE 5.
          ADD 1 TO linha.
        ENDIF.
        APPEND t_condicoes.
        CLEAR: t_condicoes, vezes.
      ENDDO.
    ENDIF.
  ENDLOOP.
  t_selecao-cond[] = t_condicoes[].
  CLEAR: t_condicoes[], t_aux[], linha, linha1, vezes.
  REFRESH: t_condicoes, t_aux.
  FREE:  t_condicoes, t_aux, linha, linha1, vezes.
ENDFORM. " busca_where
*&---------------------------------------------------------------------*
*&      Form  busca_join
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TK_AUX  text
*      -->P_I_C  text
*      -->P_I_F  text
*----------------------------------------------------------------------*
FORM busca_join TABLES p_tk_aux STRUCTURE tk
                USING    p_i_c
                         p_i_f.
  DATA: t_aux   LIKE tk OCCURS 0 WITH HEADER LINE,
        linha   TYPE i,
        linha1  TYPE i,
        vezes   TYPE i,
        linha_i TYPE i,
        linha_f TYPE i.

  t_aux[] = p_tk_aux[].
  LOOP AT p_tk_aux FROM p_i_c TO p_i_f.
    IF p_tk_aux-str = 'LEFT' OR p_tk_aux-str = 'INNER'.
      linha_i = sy-tabix.
    ELSEIF p_tk_aux-str = 'INTO' OR p_tk_aux-str = 'WHERE'.
      linha_f = sy-tabix - 1.
      EXIT.
    ENDIF.
  ENDLOOP.

*** Lógica para identificar o tipo, a tabela, e as condições do join
  LOOP AT p_tk_aux FROM linha_i TO linha_f.
    IF linha_i IS INITIAL.
      EXIT.
    ENDIF.
    linha = sy-tabix.
    CASE p_tk_aux-str.
      WHEN 'LEFT'.  " o join é do tipo left outer
        DO 4 TIMES.
          IF vezes EQ 3.
            READ TABLE t_aux INDEX linha.
            t_join-tabela = t_aux-str.
            CLEAR vezes.
          ELSE.
            READ TABLE t_aux INDEX linha.
            IF t_join-tipo IS INITIAL.
              t_join-tipo = t_aux-str.
            ELSE.
              CONCATENATE t_join-tipo t_aux-str
                     INTO t_join-tipo SEPARATED BY space.
            ENDIF.
            ADD 1 TO: linha, vezes.
          ENDIF.
        ENDDO.
        APPEND t_join.
        CLEAR: vezes, t_join.
      WHEN 'INNER'.  " o join é do tipo inner
        DO 3 TIMES.
          IF vezes EQ 2.
            READ TABLE t_aux INDEX linha.
            t_join-tabela = t_aux-str.
            CLEAR vezes.
          ELSE.
            READ TABLE t_aux INDEX linha.
            IF t_join-tipo IS INITIAL.
              t_join-tipo = t_aux-str.
            ELSE.
              CONCATENATE t_join-tipo t_aux-str
                     INTO t_join-tipo SEPARATED BY space.
            ENDIF.
            ADD 1 TO: linha, vezes.
          ENDIF.
        ENDDO.
        APPEND t_join.
        CLEAR: vezes, t_join.
      WHEN 'ON'.  "estou lendo a condição do join
        WHILE linha > linha_i AND linha < linha_f.
          IF vezes NE 3.
            ADD 1 TO linha.
            READ TABLE t_aux INDEX linha.
          ENDIF.
          IF vezes IS INITIAL.
            ADD 1 TO vezes.
          ENDIF.

          CASE vezes.
            WHEN 1.
              t_c_join-t_a = t_aux-str.
              ADD 1 TO vezes.
            WHEN 2.
              t_c_join-operador = t_aux-str.
              ADD 1 TO vezes.
            WHEN 3.
              ADD 1 TO linha.
              READ TABLE t_aux INDEX linha.
              t_c_join-t_b = t_aux-str.
              ADD 1 TO vezes.
              IF t_aux-str NE 'AND' OR t_aux-str NE 'OR'.
                CLEAR vezes.
                APPEND t_c_join.
              ENDIF.
            WHEN 4.
              t_c_join-con = t_aux-str.
              ADD 1 TO vezes.
          ENDCASE.
          IF ( t_aux-str EQ 'AND' OR t_aux-str EQ 'OR' ) AND vezes EQ 4.
            CLEAR vezes.
            APPEND t_c_join.
          ENDIF.
        ENDWHILE.
        t_selecao-join[]   = t_join[].
        t_selecao-c_join[] = t_c_join[].
*        APPEND t_selecao.
        CLEAR: t_c_join[], t_join[].

      WHEN 'AS'.  "a tabela tem apelido
        t_join-apelido = t_aux-str.
        linha1 = linha.
        ADD 1 TO linha1.
        READ TABLE t_aux INDEX linha1.
        IF t_join-apelido IS INITIAL.
          t_join-apelido = t_aux-str.
        ELSE.
          CONCATENATE t_join-apelido t_aux-str
                 INTO t_join-apelido SEPARATED BY space.
        ENDIF.
        CLEAR linha1.
    ENDCASE.
  ENDLOOP.
ENDFORM. " busca_join
*&---------------------------------------------------------------------*
*&      Form  busca_into
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TK_AUX  text
*      -->P_I_C  text
*      -->P_I_F  text
*----------------------------------------------------------------------*
FORM busca_into TABLES p_tk_aux STRUCTURE tk
                USING    p_i_c
                         p_i_f.
  DATA: linha_i TYPE i,
        linha_f TYPE i,
        c_from  TYPE c.

  LOOP AT p_tk_aux FROM p_i_c TO p_i_f.
    IF p_tk_aux-str = 'FROM' AND linha_i IS INITIAL.
      c_from = 'X'.
    ENDIF.

    IF c_from = 'X'.
      IF p_tk_aux-str = 'INTO'.
        linha_i = sy-tabix.
      ELSEIF p_tk_aux-str = 'WHERE' OR p_tk_aux-str = 'FOR'.
        linha_f = sy-tabix - 1.
        EXIT.
      ENDIF.
    ELSE.
      IF p_tk_aux-str = 'INTO'.
        linha_i = sy-tabix.
      ELSEIF p_tk_aux-str = 'FROM' OR p_tk_aux-str = 'FOR'.
        linha_f = sy-tabix - 1.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF NOT linha_i IS INITIAL.
    LOOP AT p_tk_aux FROM linha_i TO linha_f.
      IF sy-tabix = linha_f.
        t_destino-tabela = p_tk_aux-str.
        APPEND t_destino.
        t_selecao-destino[] = t_destino[].
        CLEAR t_destino[].
      ELSE.
        IF p_tk_aux-str NE 'INTO' AND p_tk_aux-str NE 'TABLE'.
          t_destino-tabela = p_tk_aux-str.
          APPEND t_destino.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM. " busca_into
*&---------------------------------------------------------------------*
*&      Form  busca_for
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TK_AUX  text
*      -->P_I_C  text
*      -->P_I_F  text
*----------------------------------------------------------------------*
FORM busca_for TABLES p_tk_aux STRUCTURE tk
               USING    p_i_c
                        p_i_f.
  DATA: t_aux   LIKE tk OCCURS 0 WITH HEADER LINE,
        linha   TYPE i,
        linha_i TYPE i,
        linha_f TYPE i.


  t_aux[] = p_tk_aux[].

  LOOP AT p_tk_aux FROM p_i_c TO p_i_f.
    IF p_tk_aux-str = 'FOR'.
      linha_i = sy-tabix.
      linha = sy-tabix + 1.
      READ TABLE t_aux INDEX linha.
      IF t_aux-str EQ 'UPDATE'.
        CLEAR linha_i.
        CONTINUE.
      ENDIF.
    ELSEIF p_tk_aux-str = 'WHERE'.
      linha_f = sy-tabix - 1.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF NOT linha_i IS INITIAL.
    LOOP AT p_tk_aux FROM linha_i TO linha_f.
      linha = sy-tabix.
      IF p_tk_aux-str EQ 'IN'.
        ADD 1 TO linha.
        READ TABLE t_aux INDEX linha.
        t_selecao-for_all = t_aux-str.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM. " busca_for
*&---------------------------------------------------------------------*
*&      Form  busca_classificacao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TK_AUX  text
*      -->P_I_C  text
*      -->P_I_F  text
*----------------------------------------------------------------------*
FORM busca_classificacao TABLES p_tk_aux STRUCTURE tk
                         USING    p_i_c
                                  p_i_f.
  DATA: linha_i TYPE i,
        linha_f TYPE i.

  LOOP AT p_tk_aux FROM p_i_c TO p_i_f.
    IF p_tk_aux-str EQ 'ORDER'.
      linha_i = sy-tabix.
    ELSEIF sy-tabix EQ p_i_f.
      linha_f = sy-tabix.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF NOT linha_i IS INITIAL.
    LOOP AT p_tk_aux FROM linha_i TO linha_f.
      IF p_tk_aux-str EQ 'ORDER' OR p_tk_aux-str = 'BY'.
        CONTINUE.
      ELSE.
        IF t_selecao-classf IS INITIAL.
          t_selecao-classf = p_tk_aux-str.
        ELSE.
          CONCATENATE t_selecao-classf p_tk_aux-str INTO
                      t_selecao-classf SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM. " busca_classificacao
*&---------------------------------------------------------------------*
*&      Form  busca_agrupamento
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TK_AUX  text
*      -->P_I_C  text
*      -->P_I_F  text
*----------------------------------------------------------------------*
FORM busca_agrupamento TABLES p_tk_aux STRUCTURE tk
                       USING    p_i_c
                                p_i_f.
  DATA: linha_i TYPE i,
        linha_f TYPE i.

  LOOP AT p_tk_aux FROM p_i_c TO p_i_f.
    IF p_tk_aux-str EQ 'GROUP'.
      linha_i = sy-tabix.
    ELSEIF sy-tabix EQ p_i_f.
      linha_f = sy-tabix.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF NOT linha_i IS INITIAL.
    LOOP AT p_tk_aux FROM linha_i TO linha_f.
      IF p_tk_aux-str EQ 'GROUP' OR p_tk_aux-str = 'BY'.
        CONTINUE.
      ELSE.
        IF t_selecao-classf IS INITIAL.
          t_selecao-classf = p_tk_aux-str.
        ELSE.
          CONCATENATE t_selecao-classf p_tk_aux-str INTO
                      t_selecao-classf SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM. " busca_agrupamento
*&---------------------------------------------------------------------*
*&      Form  busca_campos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TK_AUX  text
*      -->P_I_C  text
*      -->P_I_F  text
*----------------------------------------------------------------------*
FORM busca_campos TABLES p_tk_aux STRUCTURE tk
                  USING    p_i_c
                           p_i_f.
  DATA: t_aux   LIKE tk OCCURS 0 WITH HEADER LINE,
        linha   TYPE i,
        linha_i TYPE i,
        linha_f TYPE i.

  t_aux[] = p_tk_aux[].

  LOOP AT p_tk_aux FROM p_i_c TO p_i_f.
    IF p_tk_aux-str = 'SELECT'.
      linha_i = sy-tabix + 1.
      linha = sy-tabix + 1.
      READ TABLE t_aux INDEX linha.
      IF t_aux-str EQ 'SINGLE'.
        linha_i = sy-tabix + 1.
        CONTINUE.
      ENDIF.
    ELSEIF p_tk_aux-str = 'FROM' OR p_tk_aux-str = 'INTO' OR
           p_tk_aux-str = 'UP'.
      linha_f = sy-tabix - 1.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF NOT linha_i IS INITIAL.
    IF linha_i EQ linha_f.
      READ TABLE p_tk_aux INDEX linha_i.
      t_campos-campo = p_tk_aux-str.
      APPEND t_campos.
    ELSE.
      LOOP AT p_tk_aux FROM linha_i TO linha_f.
        t_campos-campo = p_tk_aux-str.
        APPEND t_campos.
      ENDLOOP.
    ENDIF.
    t_selecao-campos[] = t_campos[].
    CLEAR t_campos[].
  ENDIF.
ENDFORM. " busca_campos

*&---------------------------------------------------------------------*
*&      Form  busca_select_options
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM busca_select_options.
  DATA: i_linha TYPE i,
        t_aux   LIKE t_split OCCURS 0 WITH HEADER LINE.

  t_busca-linha = 'SELECT-OPTIONS'.
  APPEND t_busca.
  CLEAR: t_busca, linha.
  SCAN ABAP-SOURCE t_prog
        TOKENS     INTO tk
        STATEMENTS INTO stm
        KEYWORDS   FROM t_busca.

  LOOP AT tk.


    IF  tk-str EQ 'SELECT-OPTIONS'.

      IF t_sel_opt-par IS INITIAL.
        CONTINUE.
      ELSE.
        APPEND t_sel_opt.
        CLEAR t_sel_opt.
      ENDIF.

    ELSE.

      CONCATENATE t_sel_opt-par tk-str INTO t_sel_opt-par
                                       SEPARATED BY space.

    ENDIF.

  ENDLOOP.
  IF NOT t_sel_opt-par IS INITIAL.
    APPEND t_sel_opt.
  ENDIF.
******************
  CLEAR t_sel_opt.
  LOOP AT t_sel_opt.
    SHIFT t_sel_opt-par LEFT DELETING LEADING space.
    TRANSLATE t_sel_opt-par USING ' #'.
    SPLIT t_sel_opt-par AT '#' INTO TABLE t_split.
    CLEAR t_split.
    t_aux[] = t_split[].
*** Pego o parâmetro de selecao.
    READ TABLE t_split INDEX 1.
    MOVE t_split-campo TO t_select_opt-nome.
*** Pego tudo que é like ou type
    READ TABLE t_split WITH KEY campo = 'FOR'.
    IF sy-subrc IS INITIAL.
      MOVE t_split-campo TO t_select_opt-campo.
      i_linha = sy-tabix + 1.
      READ TABLE t_aux INDEX i_linha.
      CONCATENATE t_select_opt-campo t_aux-campo
             INTO t_select_opt-campo SEPARATED BY space.
      CLEAR i_linha.
    ELSE.
      READ TABLE t_split WITH KEY campo = 'TYPE'.
      IF sy-subrc IS INITIAL.
        MOVE t_split-campo TO t_select_opt-campo.
        i_linha = sy-tabix + 1.
        READ TABLE t_aux INDEX i_linha.
        CONCATENATE t_select_opt-campo t_aux-campo
               INTO t_select_opt-campo SEPARATED BY space.
        CLEAR i_linha.
      ENDIF.
    ENDIF.

*** Verifico se o parâmetro tem valor default.
    READ TABLE t_split WITH KEY campo = 'DEFAULT'.
    IF sy-subrc IS INITIAL.
      i_linha = sy-tabix + 1.
      READ TABLE t_aux INDEX i_linha.
      MOVE t_aux-campo TO t_select_opt-default.
      CLEAR i_linha.
    ENDIF.

*** Verifico o tipo de parâmetro.
    LOOP AT t_split.
      CASE t_split-campo.
        WHEN 'RADIOBUTTON'.
          t_select_opt-tipo = t_split-campo.
          CONTINUE.
        WHEN 'AS'.
          i_linha = sy-tabix + 1.
          READ TABLE t_aux INDEX i_linha.
          IF t_aux-campo = 'SEARCH'.
            CONCATENATE t_split-campo t_aux-campo
                   INTO t_select_opt-tipo SEPARATED BY space.
            ADD 1 TO i_linha.
            READ TABLE t_aux INDEX i_linha.
            CONCATENATE t_select_opt-tipo t_aux-campo
                   INTO t_select_opt-tipo SEPARATED BY space.
          ELSE.
            CONCATENATE t_split-campo t_aux-campo
                   INTO t_select_opt-tipo SEPARATED BY space.
          ENDIF.
          CLEAR i_linha.
          CONTINUE.
        WHEN OTHERS.
          IF t_select_opt-tipo IS INITIAL.
            t_select_opt-tipo = 'SELECT-OPTIONS'.
            CONTINUE.
          ENDIF.
      ENDCASE.
*** Pego as opções complementares se houver.
      CASE t_split-campo.
        WHEN 'DECIMALS'.
          i_linha = sy-tabix + 1.
          READ TABLE t_aux INDEX i_linha.
          CONCATENATE t_split-campo t_aux-campo
                 INTO t_select_opt-cons SEPARATED BY space.
          CLEAR i_linha.
          CONTINUE.
        WHEN 'MATCHCODE' OR 'FOR' OR 'VISIBLE' OR 'MODIF'.
          i_linha = sy-tabix + 1.
          READ TABLE t_aux INDEX i_linha.
          CONCATENATE t_split-campo t_aux-campo
                 INTO t_select_opt-cons SEPARATED BY space.
          ADD 1 TO i_linha.
          READ TABLE t_aux INDEX i_linha.
          CONCATENATE t_select_opt-cons t_aux-campo
                 INTO t_select_opt-cons SEPARATED BY space.
          CLEAR i_linha.
          CONTINUE.
        WHEN 'GROUP'.
          i_linha = sy-tabix + 1.
          READ TABLE t_aux INDEX i_linha.
          CONCATENATE t_split-campo t_aux-campo
                 INTO t_select_opt-cons SEPARATED BY space.
          CLEAR i_linha.
          CONTINUE.
        WHEN OTHERS.
          IF t_select_opt-cons IS INITIAL AND
           ( t_select_opt-cons NE 'LIKE' OR
             t_select_opt-cons NE 'TYPE' ).
            t_select_opt-cons = t_split-campo.
            CONTINUE.
          ENDIF.
      ENDCASE.
    ENDLOOP.
    APPEND t_select_opt.
    CLEAR: t_split, t_select_opt.
    REFRESH t_split.
  ENDLOOP.
*****************
*****************
*** Procuro o texto da tela se aplicável.

  LOOP AT t_select_opt.

  ENDLOOP.

  FREE:  t_busca, tk, stm.
  CLEAR: t_busca, tk, stm, t_sel_opt.

ENDFORM. " busca_select_options
*&---------------------------------------------------------------------*
*&      Form  progresso
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXTO  text
*----------------------------------------------------------------------*
FORM progresso USING p_texto.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = p_texto.
ENDFORM. " progresso
*&---------------------------------------------------------------------*
*&      Form  busca_pastabatch
*&---------------------------------------------------------------------*
* Buscar pasta btch
*----------------------------------------------------------------------*
FORM busca_pastabatch .


  DATA: vl_tabix      LIKE sy-tabix,
        vl_tabix_nome LIKE sy-tabix,
        vl_str(20)    TYPE c,
        vl_check      TYPE c,
        vl_igual      TYPE c.

  CLEAR:  vl_tabix,
          vl_tabix_nome,
          vl_str,
          vl_check,
          vl_igual,
          v_nome_pasta_batch.

  CONCATENATE: TEXT-018  " '
               'BDC_OPEN_GROUP'
               TEXT-018  " '
               INTO
               vl_str.


  t_busca-linha = 'CALL'.
  APPEND t_busca.
  CLEAR: t_busca, linha.
  SCAN ABAP-SOURCE t_prog
        TOKENS     INTO tk
        STATEMENTS INTO stm
        KEYWORDS   FROM t_busca.



  CLEAR tk.
  READ TABLE tk WITH KEY str = vl_str.
  IF sy-subrc IS INITIAL.
    vl_tabix =  sy-tabix.

    LOOP AT tk FROM vl_tabix.

      IF tk-str EQ 'GROUP'.
        vl_tabix_nome = sy-tabix + 2.

      ENDIF.

    ENDLOOP. "tk
  ENDIF.

  READ TABLE tk INDEX vl_tabix_nome.
  IF sy-subrc IS INITIAL.

    MOVE: tk-str TO v_nome_pasta_batch.

  ENDIF.


ENDFORM. " busca_pastabatch
*&---------------------------------------------------------------------*
*&      Form  FUNCAO_ESPEC_TEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM funcao_espec_tec TABLES t_entrada USING declaracao evento rotina.

* Verifica se a tabela contem dados.
  IF NOT t_entrada[] IS INITIAL.
* Selecionar Dados
    PERFORM z_seleciona_dados USING declaracao
                                    evento
                                    rotina.
*  ELSE.
*    MESSAGE i208(00) WITH 'Entrada está vazia'.
*    raise tabela_inicial.
  ENDIF.


* Traduz os códigos de Linguagem ABAP para Português Estruturado.
  PERFORM z_traduz_codigos TABLES t_entrada
                                  t_entrada.


ENDFORM. " FUNCAO_ESPEC_TEC
*&---------------------------------------------------------------------*
*&      Form  SCAN_SOURCE_GET_FIELD_DESCR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_source_get_field_descr TABLES t_dd02 USING p_str.


  CHECK NOT p_str IS INITIAL.

  SELECT *
    FROM dd02t
    INTO t_dd02
    UP TO 1 ROWS
    WHERE tabname = p_str AND
          ddlanguage = sy-langu.
  ENDSELECT.

ENDFORM. " SCAN_SOURCE_GET_FIELD_DESCR
*&---------------------------------------------------------------------*
*&      Form  SCAN_SOURCE_GET_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_source_get_report TABLES ex_source USING im_obj_name.

  READ REPORT im_obj_name INTO ex_source.

  IF NOT sy-subrc IS INITIAL.
    RAISE read_report_error.
  ENDIF.

ENDFORM. " SCAN_SOURCE_GET_REPORT
*&---------------------------------------------------------------------*
*&      Form  z_seleciona_dados
*&---------------------------------------------------------------------*
* Essas validações são feitas para que quando for traduzido um tipo de
* código especifico não precise verificar com códigos que não vão
* existir nesse bloco de códigos.
*----------------------------------------------------------------------*
*      -->P_DECLARACAO  text
*      -->P_EVENTO  text
*      -->P_ROTINA  text
*----------------------------------------------------------------------*
FORM z_seleciona_dados USING p_declaracao
                                 p_evento
                                 p_rotina.

* Quando for flegado somente declaração fara a seleção somente dos
* códigos referentes a Declarações
  IF  NOT p_declaracao IS INITIAL AND p_evento IS INITIAL
      AND p_rotina     IS INITIAL.

    PERFORM z_seleciona_declaracao..

* Quando for flegado somente evento fara a seleção somente dos
* códigos referentes aos eventos que existem no código.
  ELSEIF p_declaracao IS INITIAL AND NOT p_evento IS INITIAL
         AND p_rotina IS INITIAL.


    PERFORM z_seleciona_evento.

* Quando for flegado somente rotinas fara a seleção somente dos
* códigos referentes a todas as rotinas do programa.
  ELSEIF p_declaracao IS INITIAL AND p_evento IS INITIAL
     AND NOT p_rotina IS INITIAL.

    PERFORM z_seleciona_rotina.

* Quando não for flegado nada.
  ELSEIF p_declaracao IS INITIAL AND p_evento IS INITIAL
     AND p_rotina     IS INITIAL.

*    RAISE tipo_de_codigo_vazio. "preencher pelo menos um tipo de código.

* Quando for flegado mais de um tipo.
  ELSE.
*    RAISE tipo_de_codigo_mais_de_um.
    "Preencher somente um tipo de código.
  ENDIF.


ENDFORM. " z_seleciona_dados
*&---------------------------------------------------------------------*
*&      Form  z_seleciona_declaracao
*&---------------------------------------------------------------------*
* Seleciona só os comados do tipo D 'Declaração'
*----------------------------------------------------------------------*
FORM z_seleciona_declaracao.
  TYPES: BEGIN OF ty_source,
           tipo     TYPE char1,
           comando  TYPE as4text,
           sentenca TYPE as4text,
         END OF ty_source.
*Tabela local para selecionar os tipos de comandos.
*Não foi possivel selecionar direto na t_source porque a t_source tem
*que ser do tipo string é assim não podemos fazer uma seleção de campos
*diferentes.
*  DATA: BEGIN OF tl_selecao OCCURS 0,
*          tipo     LIKE zege_source-linha,
*          comando  LIKE zege_source-linha,
*          sentenca LIKE zege_source-linha,
*        END OF tl_selecao.

  CLEAR: t_source, tl_selecao.
  REFRESH: t_source, tl_selecao.

*  SELECT tipo
*         comando
*         sentenca
*  FROM ztge_source
*  INTO TABLE tl_selecao
*  WHERE tipo = 'D'.
*
  REFRESH tl_selecao.
  PERFORM ztge_source TABLES tl_selecao.
  DELETE tl_selecao WHERE tipo NE 'D'.

  IF sy-subrc IS INITIAL.
    SORT tl_selecao BY comando DESCENDING.
    LOOP AT tl_selecao.
      MOVE tl_selecao-tipo     TO t_source-tipo.
      MOVE tl_selecao-comando  TO t_source-comando.
      MOVE tl_selecao-sentenca TO t_source-sentenca.
      APPEND t_source.
    ENDLOOP.
  ENDIF.

ENDFORM. " z_seleciona_declaracao
*&---------------------------------------------------------------------*
*&      Form  z_seleciona_evento
*&---------------------------------------------------------------------*
*  Para os eventos cabem todos os tipos de códigos
*----------------------------------------------------------------------*
FORM z_seleciona_evento.

*Tabela local para selecionar os tipos de comandos.
*Não foi possivel selecionar direto na t_source porque a t_source tem
*que ser do tipo string é assim não podemos fazer uma seleção de campos
*diferentes.
*  DATA: BEGIN OF tl_selecao OCCURS 0,
*          tipo     TYPE CHAR1,
*          comando  LIKE zege_source-linha,
*          sentenca LIKE zege_source-linha,
*        END OF tl_selecao.
*  TYPES: BEGIN OF ty_source,
*        tipo     TYPE char1,
*        comando  TYPE AS4TEXT,
*        sentenca TYPE AS4TEXT,
*         END OF ty_source.

  CLEAR: t_source, tl_selecao.
  REFRESH: t_source, tl_selecao.

*  SELECT tipo
*         comando
*         sentenca
*  FROM ztge_source
*  INTO TABLE tl_selecao.
  REFRESH tl_selecao.
  PERFORM ztge_source TABLES tl_selecao.

  IF sy-subrc IS INITIAL.
    SORT tl_selecao BY comando DESCENDING.
    LOOP AT tl_selecao.
      MOVE tl_selecao-tipo TO t_source-tipo.
      MOVE tl_selecao-comando TO t_source-comando.
      MOVE tl_selecao-sentenca TO t_source-sentenca.
      APPEND t_source.
    ENDLOOP.
  ENDIF.

ENDFORM. " z_seleciona_evento
*&---------------------------------------------------------------------*
*&      Form  ztge_source
*----------------------------------------------------------------------*
FORM ztge_source TABLES tl_selecao STRUCTURE t_source.

  SPLIT   'C  ; .<. ; .menor que.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .<=.  ; .menor igual que.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .<>.  ; .diferente.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .=. ; .igual.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .>. ; .maior que.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .>=.  ; .maior igual que.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ADD. ; .Adicionar.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .AND. ; .e.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .AND.WAIT.  ; .aguardar retorno.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .APPEND.  ; .Adicionar.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .APPENDING. ; .adicionando.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ARCHIVE.MODE.  ; .com modo de arquivamento.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ARCHIVE.PARAMETERS.  ; .com parâmetros de arquivamento.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .AS.  ; .como.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .AS.TEXT. ; .como texto.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ASSIGN.  ; .Associar.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .AT.  ; .Ao.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .AT.END.  ; .No último.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .AT.LAST. ; .No úlitmo.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .AT.NEW.  ; .No primeiro.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .BINARY.SEARCH. ; .utilizando pesquisa binária.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .BLUE.  ; .azul.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .BY.  ; .por.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CALL.  ; .Chamar/Executar.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CALL.METHOD. ; .Chamar o método.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CASE.  ; .Verificar conteúdo de.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CHAIN. ; .processar em sequência.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CHANGING.  ; .atualizando parâmetro.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CHECK. ; .Para continuar, verifique se.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CLEAR. ; .Inicializar conteudo de.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CLEAR:.  ; .Limpar:.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .COLLECT. ; .Coletar.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .COLOR. ; .cor.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .COMMIT.WORK. ; .Efetivar dados na Base.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .COMPUTE. ; .Calcular.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CONCATENATE. ; .Concatenar.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .COPIES.  ; .cópias.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CORRESPONDING. ; .Correspondente a.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .COVER.TEXT.  ; .com título de fila.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CPI. ; .caracteres por inch.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CREATE.  ; .criar.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .CREATE.OBJECT. ; .Criar o objeto.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .DATASET.EXPIRATION.  ; .com data de expiração do dataset.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .DELETE.  ; .Eliminar.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .DEPARTMENT.  ; .nome do departamento.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .DESCRIBE.  ; .Retornar os atributos de.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .DESCRIBE.DISTANCE.BETWEEN. ; .Determinar a distância entre .'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .DESCRIBE.FIELD.  ; .Retornar os atributos do campo.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .DESCRIBE.TABLE.  ; .Retornar os atributos da tabela.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .DESTINATION. ; .com destino.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .DO.  ; .Executar repetidamente.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ELSE.  ; .Senão.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ELSEIF.  ; .Se não, se.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ENDAT. ; .Final do AT.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ENDCASE. ; .Fim da Verificação.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ENDCHAIN.  ; .finalizar sequência.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ENDDO. ; .Fim da Repetição.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ENDIF. ; .Fim Se.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ENDLOOP. ; .Fim do Laço.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ENDSELECT. ; .Fim Selecionar.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .EQ.  ; .igual.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .EXCEPTIONS.  ; .Exceções.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .EXIT.FROM.STEP-LOOP. ; .Sair do laço.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .EXTRACT. ; .extrair.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .FIELD. ; .o campo.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .FIELDS.  ; .os campos.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .FONT.  ; .com o tamanho da fonte.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .FOR. ; .para.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .FOR.ALL.ENTRIES.IN.  ; .para todas as entradas na tabela.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .FORMAT.  ; .formatar.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .FREE.  ; .Inicializar/Liberar área de.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .FREE.MEMORY.ID.  ; .Liberar da memória a identificação.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .FREE:. ; .Liberar da memória:.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .FROM.  ; .da/de.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .FUNCTION.  ; .Função.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .GE.  ; .maior igual que.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .GET. ; .obter da tabela.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .GREEN. ; .verde.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .GT.  ; .maior que.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .HOTSPOT. ; .interativo.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .I. ; .numérico inteiro.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .IF.  ; .Se.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .IF.FOUND.  ; .se encontrado.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .IMMEDIATELY. ; .imprimindo imediatamente.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .IN.  ; .Em.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .IN.PROGRAM.  ; .no programa.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .INCLUDE. ; .Incluir Sub Programa.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .INDEX-LINE.  ; .com índice de linha.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .INDEX. ; .pelo índice.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .INNER.JOIN.  ; .juntando.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .INPUT. ; .entrada.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .INSERT.  ; .Inserir.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .INTENSIFIED. ; .negrito.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .INTO.  ; .Colocar resultado em,.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .INTO.TABLE.  ; .na tabela interna.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .INVERSE. ; .inverso.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .IS ; é'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .IS.INITIAL.  ; .estiver com valor inicial.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .IS.NOT.INITIAL.  ; .Não é inicial.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .KEEP.IN.SPOOL. ; .manter na fila após impressão.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LATE.  ; .por final.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LAYOUT.  ; .padrão.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LE.  ; .menor igual que.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LEAVE. ; .sair do modo chamado.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LEAVE.LIST-PROCESSING. ; .voltar do processamento de lista para a tela anterior.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND
  tl_selecao.
  SPLIT   'C  ; .LEAVE.SCREEN.  ; .sair da tela e processar a próxima.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LEAVE.TO.LIST-PROCESSING.  ; .voltar para o processamento da lista.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LEFT.  ; .esquerda.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LEFT.MARGIN. ; .margem esquerda.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LINE-COUNT.  ; .linhas por página.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LINE-SIZE. ; .tamanho da linha.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LINES. ; .com o número de linhas em.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LIST.AUTHORITY.  ; .com autorização requerida para.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LIST.DATASET.  ; .com o dataset.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LIST.NAME. ; .com nome da lista.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LIST.NUMBER.OF.LINES.  ; .atributos da lista, número de linhas.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LIST.NUMBER.OF.PAGES.  ; .atributos da lista, número de páginas.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LOOP.AT. ; .Montar Laço para.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LPI. ; .letras por inch.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .LT.  ; .menor que.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .MESSAGE. ; .Emitir mensagem.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .MODIFY.  ; .Atualizar/Inserir dados de.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .MODULE.  ; .Módulo.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .MOVE.  ; .Atribuir.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .MOVE:. ; .Atribuir.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .MULTIPLY.  ; .Multiplos.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .NE.  ; .diferente.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .NEW-PAGE.  ; .Nova página.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .NEW-SECTION. ; .iniciar nova sessão.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .NEW.LIST.IDENTIFICATION. ; .nova requisição de fila.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .NO-HEADING . ; .sem cabeçalho.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .NO-TITLE.  ; .sem título.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .NO.DIALOG. ; .omitindo tela de controle de impressora.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .NOT. ; .(não).'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .OFF. ; .desligado.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ON.  ; .ativado.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ORDER.BY.  ; .organizando por.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .OUTER.JOIN.  ; .excluindo.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .PARAMETERS.  ; .com parâmetros.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .PERFORM. ; .Chamar rotina.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .PERFORM:.  ; .Chamar rotina.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .PINK.  ; .rosa.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .POSITION.  ; .posição.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .PRINT-CONTROL. ; .determinar o controle de impressão.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .PRINT.OFF. ; .liberar da fila.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .PRINT.ON.  ; .manter na fila.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .READ.  ; .Ler.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .READ.TABLE.  ; .Ler tabela interna.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .RECEIVER.  ; .com o recebedor.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .RED. ; .vermelho.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .REFRESH. ; .Reinicializar.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .REFRESH:.  ; .Eliminar da memória:.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .RETURN.  ; .Retornar.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .RETURN.TO.SCREEN.  ; .voltar para a tela.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .RIGHT. ; .direita.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SAP.COVER.PAGE.  ; .com folha de rosto da SAP.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SELECT.  ; .Selecionar.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SEPARATED. ; .separado.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SET.BLANK.LINES.OFF. ; .Desativar o uso de linhas em branco.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SET.BLANK.LINES.ON.  ; .Ativar o uso de linhas em branco.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SET.HOLD.DATA.OFF. ; .Desativar valores padrão para a tela.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SET.HOLD.DATA.ON.  ; .Ativar valores padrão para a tela.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SET.MARGIN.  ; .definir a margem de impressão na tela para.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SET.SCREEN.  ; .setar o número da próxima tela.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SIZE.  ; .tamanho do script.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SKIP.  ; .pular linha.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SORT.  ; .Ordenar conteudo de.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .STOP.  ; .Pare.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SUBMIT.  ; .enviar.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SUBSCREEN. ; .sub-tela.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SUM. ; .Sumarizar.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SUPPRESS.DIALOG. ; .Suprimir a saída da tela corrente.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SY-COLNO.  ; .número da coluna em que o cursor está posicionado.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SY-LINCT.  ; .número de linhas por página da lista corrente.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SY-LINNO.  ; .número corrente da linha.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SY-PAGNO.  ; .número corrente da página.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .SY-SUBRC.  ; .Retorno de erro do sistema.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .TIMES. ; .vezes.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .TO.  ; .para.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .TRANSPORTING.  ; .somente para os campos.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .TRANSLATE.  ; .modifique.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .ULINE. ; .imprimir linha horizontal.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .UNASSIGN.  ; .Desassociar.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .UNASSIGN:. ; .Desassociar.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .USING. ; .utilizando o parâmetro.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .VLINE. ; .imprimir linha vertical.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .WAIT.  ; .Executar comando de espera.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .WHEN.  ; .Quando valor for.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .WHERE. ; .Onde o campo.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .WITH-TITLE.  ; .com título.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .WRITE. ; .imprimir no relatório a linha.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .WRITE:.  ; .imprimir no relatório as linhas.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'C  ; .YELLOW.  ; .amarelo.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .AS.  ; .como.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .BEGIN. ; .Início.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .BLOCK. ; .bloco.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .COMMON.PART. ; .área comum.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .CONSTANTS. ; .Declarar a constante.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .CONSTANTS:.  ; .Declarar as constantes.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .CONTROLS.  ; .Definir o controle.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .CONTROLS:. ; .Definir os controles.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .DATA.  ; .Definir variável.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .DATA:. ; .Definir as variáveis.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .DATA:.BEGIN.OF.  ; .Definir tabela interna.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .DECIMALS.  ; .com decimais.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .DEFAULT. ; .padrão.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .END. ; .Final.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .FIELD-GROUPS.  ; .grupo de campos.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .FOR. ; .para.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .HEADER.LINE. ; .linha de cabeçalho.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .INITIAL.SIZE.  ; .tamanho inicial.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .INTERVALS. ; .intervalos.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .KEY. ; .chave.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .LENGTH.  ; .tamanho.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .LIKE.  ; .com as mesmas caracteristicas de.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .NO.  ; .sem.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .NON-UNIQUE.  ; .chave composta.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .OCCURS.  ; .ocorrendo.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .OF.  ; .de.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .PARAMETERS.  ; .Parâmetros de seleção.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .PARAMETERS:. ; .Parâmetros de seleção.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .RANGE. ; .intervalo.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .RANGES.  ; .intervalo.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .REPORT.  ; .Relatório.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .SCREEN.  ; .tela.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .SELECT-OPTIONS.  ; .Entrada de opções.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .STANDARD.  ; .padrão.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .STANDARD.PAGE.HEADING. ; .cabeçalho padrão.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .STATICS. ; .Declarar variavel local.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .STATICS.:. ; .Declarar variavel local.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .STRUCTURE. ; .estrutura.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .TABLE. ; .tabela.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .TABLES.  ; .Declarar a seguinte tabela.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .TABLES:. ; .Declarar as seguintes tabelas.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .TABLEVIEW. ; .visão de tabela.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .TABSTRIP.  ; .aba de tela.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .TYPE-POOLS.  ; .grupo de tipos.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .TYPE.  ; .com as mesmas caracteristicas de.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .TYPES. ; .Definir campo estruturado.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .TYPES:.  ; .Definir campo estruturado como segue.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .UNIQUE.  ; .chave única.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .VALUE. ; .valor padrão.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'D  ; .WITH.  ; .com.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .AFTER.INPUT. ; .depois da entrada so usuário.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .AT.SELECTION-SCREEN. ; .Na tela de seleção.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .AT.USER-COMMAND. ; .Ao comando do usuário.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .BEFORE.OUTPUT. ; .antes da tela ser exibida.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .BEGIN.OF.BLOCK.  ; .início de bloco.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .BEGIN.OF.LINE. ; .início da linha.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .COMMENT. ; .descrição.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .DURING.LINE-SELECTION. ; .quando gerado em uma lista secundária.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .END-OF-PAGE. ; .final da página.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .END-OF-SELECTION.  ; .Fim do Processamento.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .END.OF.BLOCK.  ; .final do bloco.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .END.OF.LINE. ; .final da linha.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .FRAME.TITLE. ; .título do frame.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .GROUP. ; .grupo.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .INITIALIZATION.  ; .Rotina para Inicialização.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .LINE-SELECTION.  ; .Linha de seleção.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .LOWER.CASE.  ; .caixa baixa.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .MODIF.ID.  ; .com identificação de modificação.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .ON.HELP-REQUEST. ; .quando o usuário pressionar F1.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .ON.VALUE-REQUEST.  ; .quando o usuário pressionar F4.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .ON.VALUE-REQUEST.FOR.  ; .quando for alterado o campo.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .PARAMETER:.  ; .Parâmetro de seleção.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .PROCESS. ; .Processar.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .RADIOBUTTON. ; .botão de rádio.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .SELECTION-SCREEN.  ; .Tela de seleção.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .SELECTION-SCREEN.PUSHBUTTON. ; .Gerar um botão na tela de seleção.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .SELECTION-SCREEN:. ; .Tela de seleção.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .SELECTION-SCREEN:.PUSHBUTTON.  ; .Gerar um botão na tela de seleção.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .START-OF-SELECTION.  ; .Inicio do Processamento.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .TABBED.BLOCK.  ; .bloco de abas.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .TOP-OF-PAGE. ; .topo da página.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .USER-COMMAND.  ; .comando do usuário.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'E  ; .WITH.FRAME.TITLE.  ; .com o título.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'R  ; .ENDFORM. ; .Fim da Rotina.'   AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'R  ; .EXPORTING. ; .Parâmetros de Exportação.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'R  ; .FORM.  ; .Rotina.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'R  ; .IMPORTING. ; .Parâmetros de Importação.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'R  ; .PERFORM. ; .Chamar rotina .'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.
  SPLIT   'R  ; .USING. ; .utilizando o parâmetro.'  AT ';'    INTO tl_selecao-tipo tl_selecao-comando tl_selecao-sentenca. CONDENSE: tl_selecao-tipo, tl_selecao-comando, tl_selecao-sentenca. APPEND tl_selecao.


ENDFORM. "ztge_source
*&---------------------------------------------------------------------*
*  Para as rotinas não cabem as declarações de eventos.
*----------------------------------------------------------------------*
FORM z_seleciona_rotina.

*Tabela local para selecionar os tipos de comandos.
*Não foi possivel selecionar direto na t_source porque a t_source tem
*que ser do tipo string é assim não podemos fazer uma seleção de campos
*diferentes.
*  DATA: BEGIN OF tl_selecao OCCURS 0,
*          tipo     LIKE zege_source-linha,
*          comando  LIKE zege_source-linha,
*          sentenca LIKE zege_source-linha,
*        END OF tl_selecao.


  CLEAR: t_source, tl_selecao.
  REFRESH: t_source, tl_selecao.

*  SELECT tipo
*         comando
*         sentenca
*  FROM ztge_source
*  INTO TABLE tl_selecao
*  WHERE tipo NE 'E'.
  REFRESH tl_selecao.
  PERFORM ztge_source TABLES tl_selecao.

  DELETE tl_selecao WHERE tipo EQ 'E'.

  IF sy-subrc IS INITIAL.
    SORT tl_selecao BY comando DESCENDING.
    LOOP AT tl_selecao.
      MOVE tl_selecao-tipo TO t_source-tipo.
      MOVE tl_selecao-comando TO t_source-comando.
      MOVE tl_selecao-sentenca TO t_source-sentenca.
      APPEND t_source.
    ENDLOOP.
  ENDIF.

ENDFORM. " z_seleciona_rotina
*&---------------------------------------------------------------------*
*&      Form  z_traduz_codigos
*&---------------------------------------------------------------------*
* Rotina para traduzir os códigos em Português estruturado.
*----------------------------------------------------------------------*
*      -->t_entrada  ==> Tabela que contem o código em si.
*      -->t_retorno  ==> Tabela que retornara a tradução do código.
*----------------------------------------------------------------------*
FORM z_traduz_codigos TABLES t_entrada STRUCTURE t_source
                               t_retorno STRUCTURE t_source.

* Variáveis locais
  DATA: vl_tamanho TYPE i,
        "tamanho do campo que contem o comando da tabela t_source.
        vl_check   TYPE c,
        vl_e_form  TYPE c,
        vl_form.

* Constantes locais
  CONSTANTS: cl_form(8)     TYPE c VALUE '..FORM..',     "FORM
             cl_endform(11) TYPE c VALUE '..ENDFORM..'.  "ENDFORM

* Tabela interna para colocar cada linha de codigo a ser tratada.
  DATA : BEGIN OF t_comando OCCURS 0,
           text(200),
         END OF t_comando.

* Tabela interna para guardar o codigo em si.
  DATA : BEGIN OF tl_codigo OCCURS 0,
           text(200),
         END OF tl_codigo.

  CLEAR: tl_codigo.
  REFRESH: tl_codigo.

  tl_codigo[] = t_entrada[].


  CLEAR: t_comando, t_retorno, vl_tamanho.
  REFRESH: t_comando, t_retorno.


  LOOP AT tl_codigo INTO t_comando-text.


**-- Linhas a desconsiderar
    CHECK t_comando-text(3) NE '***'.
    CHECK t_comando-text(2) NE '*&'.
    CHECK t_comando-text(1) NE '*'.


**-- Desloca a primeira posição deixando um branco
    SHIFT t_comando-text RIGHT.

**-- subtitui todos os brancos por pontos
    TRANSLATE t_comando-text USING ' .'.

*--Substitui as letras minusculas por maiusculas.
    TRANSLATE t_comando-text TO UPPER CASE.


**-- Substitui comando por sentença
    LOOP AT t_source.

*>> Este comando será utilizado na versão 4.7 do R/3
**      REPLACE ALL OCCURRENCES
**      OF
**      t_replace-comando
**      IN
**      t_source-text
**      WITH
**      t_replace-sentenca IGNORING CASE.
*<<


      vl_check = 'X'.
      WHILE vl_check = 'X'.

        vl_tamanho = strlen( t_source-comando ).

        SEARCH t_comando-text FOR cl_form." AND MARK.
        IF sy-subrc IS INITIAL AND vl_form EQ 'X'.

*          IF NOT t_source-comando EQ '.FORM.'.
*            REPLACE t_source-comando LENGTH vl_tamanho
*                      WITH t_source-sentenca
*                        INTO t_comando-text.
*
*            IF NOT sy-subrc IS INITIAL.
          CLEAR: vl_check.
*            ENDIF.
*          ENDIF.

        ELSEIF sy-subrc IS INITIAL.


          REPLACE t_source-comando IN t_comando-text
          WITH t_source-sentenca
          REPLACEMENT LENGTH vl_tamanho.
*          TRANSLATE t_source-sentenca USING ' .'.
*          REPLACE t_source-comando LENGTH vl_tamanho
*                    WITH t_source-sentenca
*                      INTO t_comando-text.

          IF NOT sy-subrc IS INITIAL.
            CLEAR: vl_check.
          ENDIF.

          SEARCH t_comando-text FOR cl_form." AND MARK.
          IF NOT sy-subrc IS INITIAL.
            MOVE 'X' TO vl_form.
          ENDIF.

        ELSE.

          SEARCH t_comando-text FOR cl_endform. " AND MARK.
          IF sy-subrc IS INITIAL.
            MOVE space TO vl_form.
          ENDIF.


          REPLACE t_source-comando IN t_comando-text
          WITH t_source-sentenca
          REPLACEMENT LENGTH vl_tamanho.
*          TRANSLATE t_source-sentenca USING ' .'.
*          REPLACE t_source-comando LENGTH vl_tamanho
*          WITH t_source-sentenca
*            INTO t_comando-text.


          IF NOT sy-subrc IS INITIAL.
            CLEAR: vl_check.
          ENDIF.

        ENDIF.

      ENDWHILE.

    ENDLOOP. "t_source.

**-- Retira os pontos e devolve os espacos
    TRANSLATE t_comando-text USING '. '.

**-- Deixa o ponto final em linhas validas
*    IF t_comando-text CA 'AEIOUaeiou'.
*      CONCATENATE t_comando-text '.' INTO t_comando-text .
*    ENDIF.
    APPEND t_comando.
  ENDLOOP.   "tl_codigo

**--Carregar a tabela t_retorno com o código já traduzido. Para que a
*   tabela com o código em si não se perca e a tradução fique em outra
*   tabela.
  t_retorno[] = t_comando[].



ENDFORM. " z_traduz_codigos


*Messages
*----------------------------------------------------------
*
* Message class: 00
*208   &
*
* Message class: SU
*000   & & & &
