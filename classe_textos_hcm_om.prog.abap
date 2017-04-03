class ZCL_UTIL_OM_TEXTS definition
  public
  final
  create public .

public section.

  methods GET_WERKS_TEXT
    importing
      !IV_WERKS type PERSA
    returning
      value(RV_STEXT) type PBTXT .
  methods GET_ORGEH_TEXT
    importing
      !IV_ORGEH type ORGEH
      !IV_DATE type DATUM default SY-DATUM
    returning
      value(RV_STEXT) type STEXT .
  methods GET_PERSG_TEXT
    importing
      !IV_PERSG type PERSG
    returning
      value(RV_STEXT) type PGTXT .
  methods GET_PERSK_TEXT
    importing
      !IV_PERSK type PERSK
    returning
      value(RV_STEXT) type PKTXT .
  methods GET_PLANS_TEXT
    importing
      !IV_PLANS type PLANS
      !IV_DATE type DATUM default SY-DATUM
    returning
      value(RV_STEXT) type STEXT .
  methods GET_STELL_TEXT
    importing
      !IV_STELL type STELL
      !IV_DATE type DATUM default SY-DATUM
    returning
      value(RV_STEXT) type STEXT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_UTIL_OM_TEXTS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_UTIL_OM_TEXTS->GET_ORGEH_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ORGEH                       TYPE        ORGEH
* | [--->] IV_DATE                        TYPE        DATUM (default =SY-DATUM)
* | [<-()] RV_STEXT                       TYPE        STEXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_orgeh_text.


    CALL FUNCTION 'HRWPC_RFC_ORGEH_TEXT_GET'
      EXPORTING
        orgeh       = iv_orgeh
        begda       = iv_date
        endda       = iv_date
      IMPORTING
        orgeh_text2 = rv_stext.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_UTIL_OM_TEXTS->GET_PERSG_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PERSG                       TYPE        PERSG
* | [<-()] RV_STEXT                       TYPE        PGTXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_persg_text.


    CALL FUNCTION 'HRWPC_RFC_PERSG_TEXT_GET'
      EXPORTING
        persg       = iv_persg
      IMPORTING
        persg_text = rv_stext.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_UTIL_OM_TEXTS->GET_PERSK_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PERSK                       TYPE        PERSK
* | [<-()] RV_STEXT                       TYPE        PKTXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_persk_text.


    CALL FUNCTION 'HRWPC_RFC_PERSK_TEXT_GET'
      EXPORTING
        persk      = iv_persk
      IMPORTING
        persk_text = rv_stext.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_UTIL_OM_TEXTS->GET_PLANS_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PLANS                       TYPE        PLANS
* | [--->] IV_DATE                        TYPE        DATUM (default =SY-DATUM)
* | [<-()] RV_STEXT                       TYPE        STEXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_plans_text.

    CALL FUNCTION 'HRWPC_RFC_PLANS_TEXT_GET'
      EXPORTING
        plans       = iv_plans
        begda       = iv_date
        endda       = iv_date
      IMPORTING
        plans_text2 = rv_stext.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_UTIL_OM_TEXTS->GET_STELL_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STELL                       TYPE        STELL
* | [--->] IV_DATE                        TYPE        DATUM (default =SY-DATUM)
* | [<-()] RV_STEXT                       TYPE        STEXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_STELL_TEXT.


    CALL FUNCTION 'HRWPC_RFC_STELL_TEXT_GET'
      EXPORTING
        stell       = iv_stell
        begda       = iv_date
        endda       = iv_date
      IMPORTING
        stell_text2 = rv_stext.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_UTIL_OM_TEXTS->GET_WERKS_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_WERKS                       TYPE        PERSA
* | [<-()] RV_STEXT                       TYPE        PBTXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_werks_text.

    CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
      EXPORTING
        werks       = iv_werks
      IMPORTING
        werks_text = rv_stext.

  ENDMETHOD.
ENDCLASS.
