*&---------------------------------------------------------------------*
*& Report ZPPR0001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr0001 MESSAGE-ID zpp.

TABLES: mchb.

TYPES: BEGIN OF ty_classif,
         matnr TYPE matnr,
         werks TYPE werks_d,
         charg TYPE charg_d,
         item  TYPE i,
         atnam TYPE atnam,
         atwtb TYPE atwtb,
       END OF ty_classif.

TYPES: ty_tbclassif TYPE STANDARD TABLE OF ty_classif.

DATA: gt_dynfcat TYPE cl_abap_structdescr=>component_table,
      gt_classif TYPE ty_tbclassif.

DATA: gr_atnam TYPE RANGE OF atnam.

FIELD-SYMBOLS: <fs_data>  TYPE STANDARD TABLE.

CONSTANTS: c_check   TYPE char1  VALUE 'X',
           c_initval TYPE labst  VALUE '0.000',
           c_exibir  TYPE char2  VALUE '03'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_matnr FOR mchb-matnr,
                s_werks FOR mchb-werks,
                s_lgort FOR mchb-lgort,
                s_charg FOR mchb-charg.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.

  PERFORM zf_get_fieldcat.

AT SELECTION-SCREEN.

  PERFORM zf_verifica_autorizacao.

START-OF-SELECTION.

  SELECT a~matnr, a~werks, a~lgort, a~charg,
         a~lvorm, a~sperc, a~clabs, a~cumlm,
         a~cinsm, a~ceinm, a~cspem, a~cretm
    FROM mchb AS a
    LEFT JOIN t320 AS b
    ON a~werks EQ b~werks
    AND a~lgort EQ b~lgort
    WHERE a~matnr IN @s_matnr
      AND a~werks IN @s_werks
      AND a~lgort IN @s_lgort
      AND a~charg IN @s_charg
      AND a~lvorm NE @c_check
      AND a~charg NE @space
      AND b~werks IS NULL
      AND b~lgort IS NULL
      AND (  a~clabs NE @c_initval
          OR a~cumlm NE @c_initval
          OR a~cinsm NE @c_initval
          OR a~ceinm NE @c_initval
          OR a~cspem NE @c_initval
          OR a~cretm NE @c_initval )
    INTO TABLE @DATA(gt_mchb).

  SELECT a~lgnum, a~matnr, a~werks, a~charg,
         a~bestq, a~sobkz, a~sonum, a~lgtyp,
         a~lgpla, a~gesme, a~verme, a~lgort
    FROM lqua AS a
    INNER JOIN t320 AS b
    ON a~lgnum EQ b~lgnum
    AND a~werks EQ b~werks
    AND a~lgort EQ b~lgort
      WHERE a~matnr IN @s_matnr
        AND a~werks IN @s_werks
        AND a~lgort IN @s_lgort
        AND a~charg IN @s_charg
        AND a~verme NE @c_initval
    INTO TABLE @DATA(gt_lqua).


END-OF-SELECTION.

  PERFORM zf_busca_caracteristica CHANGING gt_classif.

  PERFORM zf_cria_tab_dinamica CHANGING gt_dynfcat.

  PERFORM zf_monta_saida USING gt_classif.

  PERFORM zf_alv.

*&---------------------------------------------------------------------*
*&      Form  ZF_GET_FIELDCAT
*&---------------------------------------------------------------------*
FORM zf_get_fieldcat.

  CONSTANTS: c_struc TYPE dd02l-tabname VALUE 'ZPPST_LOTE',
             c_sep   TYPE char1 VALUE '-'.

  DATA: lt_fieldcat TYPE lvc_t_fcat.

  DATA: le_dynfcat LIKE LINE OF gt_dynfcat.

  DATA: lv_type TYPE string.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = c_struc
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc = 0.

    LOOP AT lt_fieldcat INTO DATA(le_fieldcat).

      CLEAR le_dynfcat.

      le_dynfcat-name = le_fieldcat-fieldname.
      CONDENSE le_dynfcat-name NO-GAPS.

      lv_type = |{ c_struc }{ c_sep }{ le_fieldcat-fieldname }|.
      DATA(lo_tmptype) = cl_abap_typedescr=>describe_by_name( lv_type ).
      MOVE lo_tmptype ?TO le_dynfcat-type.

      APPEND le_dynfcat TO gt_dynfcat.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CRIA_TAB_DINAMICA
*&---------------------------------------------------------------------*
FORM zf_cria_tab_dinamica CHANGING pt_dynfcat TYPE cl_abap_structdescr=>component_table.

  CONSTANTS: c_tipo  TYPE string VALUE 'ATNAM'.

  DATA: lo_newtype TYPE REF TO cl_abap_structdescr,
        lo_tabtype TYPE REF TO cl_abap_tabledescr,
        lo_data    TYPE REF TO data.

  DATA: le_dynfcat LIKE LINE OF pt_dynfcat.

  LOOP AT gr_atnam INTO DATA(le_atnam).

    CLEAR le_dynfcat.

    le_dynfcat-name = le_atnam-low.
    CONDENSE le_dynfcat-name NO-GAPS.

    DATA(lo_tmptype) = cl_abap_typedescr=>describe_by_name( c_tipo ).

    MOVE lo_tmptype ?TO le_dynfcat-type.
    APPEND le_dynfcat TO pt_dynfcat.

  ENDLOOP.

  lo_newtype = cl_abap_structdescr=>create( pt_dynfcat ).
  lo_tabtype = cl_abap_tabledescr=>create( lo_newtype ).

  CLEAR lo_data.
  CREATE DATA lo_data TYPE HANDLE lo_tabtype.

  ASSIGN lo_data->* TO <fs_data>.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_SAIDA
*&---------------------------------------------------------------------*
FORM zf_monta_saida USING pt_classif TYPE ty_tbclassif.

  DATA: le_outtab TYPE zppst_lote.

  FIELD-SYMBOLS: <fs_line> TYPE any,
                 <fs_cell> TYPE any.

  LOOP AT gt_mchb INTO DATA(le_mchb).

    CLEAR le_outtab.

    le_outtab-werks = le_mchb-werks.
    le_outtab-lgort = le_mchb-lgort.
    le_outtab-matnr = le_mchb-matnr.
    le_outtab-charg = le_mchb-charg.
    le_outtab-sperc = le_mchb-sperc.
    le_outtab-clabs = le_mchb-clabs.
    le_outtab-cumlm = le_mchb-cumlm.
    le_outtab-cinsm = le_mchb-cinsm.
    le_outtab-ceinm = le_mchb-ceinm.
    le_outtab-cspem = le_mchb-cspem.
    le_outtab-cretm = le_mchb-cretm.

    APPEND INITIAL LINE TO <fs_data> ASSIGNING <fs_line>.

    <fs_line> = le_outtab.

    READ TABLE pt_classif WITH KEY matnr = le_mchb-matnr
                                   werks = le_mchb-werks
                                   charg = le_mchb-charg
                          TRANSPORTING NO FIELDS
                          BINARY SEARCH.

    IF sy-subrc = 0.

      LOOP AT pt_classif INTO DATA(le_classif) FROM sy-tabix.

        IF le_classif-matnr = le_mchb-matnr AND
           le_classif-werks = le_mchb-werks AND
           le_classif-charg = le_mchb-charg.

          ASSIGN COMPONENT le_classif-atnam OF STRUCTURE <fs_line> TO <fs_cell>.

          IF <fs_cell> IS ASSIGNED.
            <fs_cell> = le_classif-atwtb.
          ENDIF.

        ELSE.

          EXIT.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_CARACTERISTICA
*&---------------------------------------------------------------------*
FORM zf_busca_caracteristica CHANGING pt_classif TYPE ty_tbclassif.

  DATA: lt_batch TYPE TABLE OF clbatch.

  DATA(lt_mchb_aux) = gt_mchb[].
  DATA(lt_lqua_aux) = gt_lqua[].

  DATA: le_classif LIKE LINE OF pt_classif,
        le_atnam   LIKE LINE OF gr_atnam.

  CONSTANTS: c_sign   TYPE char1 VALUE 'I',
             c_option TYPE char2 VALUE 'EQ'.

  SORT lt_mchb_aux BY matnr werks charg.
  DELETE ADJACENT DUPLICATES FROM lt_mchb_aux COMPARING matnr werks charg.

  SORT lt_lqua_aux BY matnr werks charg.
  DELETE ADJACENT DUPLICATES FROM lt_lqua_aux COMPARING matnr werks charg.

  LOOP AT lt_mchb_aux INTO DATA(le_mchb).

    CALL FUNCTION 'VB_BATCH_GET_DETAIL'
      EXPORTING
        matnr              = le_mchb-matnr
        charg              = le_mchb-charg
        werks              = le_mchb-werks
        get_classification = c_check
      TABLES
        char_of_batch      = lt_batch
      EXCEPTIONS
        no_material        = 1
        no_batch           = 2
        no_plant           = 3
        material_not_found = 4
        plant_not_found    = 5
        no_authority       = 6
        batch_not_exist    = 7
        lock_on_batch      = 8
        OTHERS             = 9.

    IF sy-subrc = 0.

      LOOP AT lt_batch INTO DATA(le_batch).

        IF le_batch-atnam NOT IN gr_atnam OR gr_atnam IS INITIAL.

          le_atnam-sign   = c_sign.
          le_atnam-option = c_option.
          le_atnam-low    = le_batch-atnam.

          APPEND le_atnam TO gr_atnam.
          CLEAR le_atnam.

        ENDIF.

        le_classif-matnr = le_mchb-matnr.
        le_classif-charg = le_mchb-charg.
        le_classif-werks = le_mchb-werks.

        le_classif-item = sy-tabix.
        le_classif-atnam = le_batch-atnam.
        le_classif-atwtb = le_batch-atwtb.

        APPEND le_classif TO pt_classif.
        CLEAR le_classif.

      ENDLOOP.

    ENDIF.

  ENDLOOP.

  LOOP AT lt_lqua_aux INTO DATA(le_lqua).

    IF NOT line_exists( pt_classif[ matnr = le_lqua-matnr
                                    werks = le_lqua-werks
                                    charg = le_lqua-charg ] ).

      CALL FUNCTION 'VB_BATCH_GET_DETAIL'
        EXPORTING
          matnr              = le_mchb-matnr
          charg              = le_mchb-charg
          werks              = le_mchb-werks
          get_classification = c_check
        TABLES
          char_of_batch      = lt_batch
        EXCEPTIONS
          no_material        = 1
          no_batch           = 2
          no_plant           = 3
          material_not_found = 4
          plant_not_found    = 5
          no_authority       = 6
          batch_not_exist    = 7
          lock_on_batch      = 8
          OTHERS             = 9.

      IF sy-subrc = 0.

        LOOP AT lt_batch INTO le_batch.

          IF le_batch-atnam NOT IN gr_atnam OR gr_atnam IS INITIAL.

            le_atnam-sign   = c_sign.
            le_atnam-option = c_option.
            le_atnam-low    = le_batch-atnam.

            APPEND le_atnam TO gr_atnam.
            CLEAR le_atnam.

          ENDIF.

          le_classif-matnr = le_mchb-matnr.
          le_classif-charg = le_mchb-charg.
          le_classif-werks = le_mchb-werks.

          le_classif-item = sy-tabix.
          le_classif-atnam = le_batch-atnam.
          le_classif-atwtb = le_batch-atwtb.

          APPEND le_classif TO pt_classif.
          CLEAR le_classif.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDLOOP.

  SORT pt_classif BY matnr werks charg.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV
*&---------------------------------------------------------------------*
FORM zf_alv.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                              CHANGING  t_table      = <fs_data> ).

      DATA(lo_columns) = lo_alv->get_columns( ).

      LOOP AT gr_atnam INTO DATA(le_atnam).

        DATA(lo_column) = lo_columns->get_column( le_atnam-low ).

        lo_column->set_long_text( CONV scrtext_l( le_atnam-low ) ).
        lo_column->set_medium_text( CONV scrtext_m( le_atnam-low ) ).
        lo_column->set_short_text( CONV scrtext_s( le_atnam-low ) ).

      ENDLOOP.

      lo_columns->set_optimize( abap_true ).

      lo_alv->display( ).

    CATCH cx_salv_not_found INTO DATA(lo_msg1).
      MESSAGE lo_msg1 TYPE 'S' DISPLAY LIKE 'E'.
    CATCH cx_salv_msg INTO DATA(lo_msg).
      MESSAGE lo_msg TYPE 'S' DISPLAY LIKE 'E'.

  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFICA_AUTORIZACAO
*&---------------------------------------------------------------------*
FORM zf_verifica_autorizacao.

  AUTHORITY-CHECK OBJECT 'ZAC_WERKS'
           ID 'ACTVT' FIELD c_exibir
           ID 'WERKS' FIELD s_werks.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e050 WITH sy-tcode. "Sem autorização para a transação &1.
  ENDIF.

ENDFORM.
