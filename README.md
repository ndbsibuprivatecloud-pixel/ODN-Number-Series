# ODN-Number-Series
ODN Number Series Enhancement

**=> Include RV60AFZZ
FORM USEREXIT_NUMBER_RANGE USING US_RANGE_INTERN.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$"$\SE:(1) Form USEREXIT_NUMBER_RANGE, Start                                                                                                                 A
*$*$-Start: (1)---------------------------------------------------------------------------------$*$*
ENHANCEMENT 1  ZSD_E_03_INV_RANGE_GENERATE.    "active version

  INCLUDE ZSD_E03_INV_RANGE_GENERATE.

ENDENHANCEMENT.
*$*$-End:   (1)---------------------------------------------------------------------------------$*$*

* Example: Number range from TVFK like in standard
* US_RANGE_INTERN = TVFK-NUMKI.
ENDFORM.

*&---------------------------------------------------------------------*
*& Include          ZSD_E03_INV_RANGE_GENERATE
*&---------------------------------------------------------------------*

**& Data Declaration
DATA:
  lv_bukrs      TYPE vtvdeta_tr-bukrs,
  lv_budat      TYPE ftis_datum,
  lv_gjahr      TYPE ftis_gjahr,
  lv_nextyr     TYPE ftis_gjahr,
  lv_monat      TYPE ftis_monat,
  lv_number     TYPE vbrk-xblnr,
  lv_quantity   TYPE inri-quantity,
  lv_returncode TYPE inri-returncode,
  lv_currm      TYPE bkpf-monat,
  lv_curry      TYPE bkpf-gjahr,
  lv_prevm      TYPE bkpf-monat,
  lv_prevy      TYPE bkpf-gjahr,
  lv_xblnr      TYPE vbrk-xblnr.


*& Constant Declaration
CONSTANTS : gc_vf01  TYPE string      VALUE 'VF01',
            gc_vf02  TYPE string      VALUE 'J1IGSUBCON',
            gc_kvgr2 TYPE knvv-kvgr2  VALUE 'B2B',
            gc_kvgr3 TYPE knvv-kvgr2  VALUE 'B2C',
            gc_cprog TYPE string      VALUE 'SAPMSSY1'.

*& Create Object
DATA(go_obj)   = NEW zcl_sd_e_03_invrange( ).

IF ( sy-tcode EQ gc_vf01 ) or ( sy-tcode EQ gc_vf02 ) or ( sy-cprog EQ gc_cprog ).
  CLEAR : lv_xblnr.

  SELECT SINGLE * FROM zsd_odn INTO @DATA(ls_ordno) WHERE fkart = @xvbrk-fkart AND bupla = @xvbrk-bupla.
  IF sy-subrc EQ 0.

    SELECT SINGLE * FROM knvv INTO @DATA(ls_knvv) WHERE kunnr = @xvbrk-kunrg AND vkorg = @xvbrk-vkorg AND vtweg = @xvbrk-vtweg AND spart = @xvbrk-spart.
    IF ( sy-subrc EQ 0 ) AND ( ( ls_knvv-kvgr2 = gc_kvgr2 ) OR ( ls_knvv-kvgr2 = gc_kvgr3 ) OR ( ls_knvv-kvgr2 = ' ' ) ).

      CALL FUNCTION 'GET_CURRENT_YEAR'
        EXPORTING
          bukrs = xvbrk-bukrs
          date  = xvbrk-fkdat
        IMPORTING
          currm = lv_currm
          curry = lv_curry
          prevm = lv_prevm
          prevy = lv_prevy.

      go_obj->export_inv( EXPORTING iv_nr_range_nr = ls_ordno-nrrangenr
                                    iv_object      = ls_ordno-object
                                    iv_subobject   = ls_ordno-subobject
                                    iv_fisc_yr     = lv_curry
                          IMPORTING ev_number      = lv_number
                                    ev_quantity    = lv_quantity
                                    ev_returncode  = lv_returncode ) .

      CLEAR: xvbrk-xblnr.

      xvbrk-xblnr = |{ ls_ordno-zgstin }| && |{ ls_ordno-zprocess_cd }| && |{ lv_curry+2(2) }| && |{ lv_number }|.
      lv_xblnr = xvbrk-xblnr.

*      EXPORT lv_xblnr FROM lv_xblnr TO MEMORY ID 'ZZ_SI_XBLNR'.
      EXPORT lv_xblnr TO SHARED BUFFER indx(xy) ID 'ZZ_SI_XBLNR'.
    ENDIF.
    CLEAR : ls_knvv.
  ENDIF.
  CLEAR: ls_ordno.
ENDIF.

*&---------------------------------------------------------------------*
*&                        ZCL_SD_E_03_INVRANGE
*&---------------------------------------------------------------------*
class ZCL_SD_E_03_INVRANGE definition
  public
  final
  create public .

public section.

  methods EXPORT_INV
    importing
      value(IV_NR_RANGE_NR) type INRI-NRRANGENR
      value(IV_OBJECT) type INRI-OBJECT
      value(IV_SUBOBJECT) type INRI-SUBOBJECT
      value(IV_FISC_YR) type GJAHR
    exporting
      value(EV_NUMBER) type VBRK-XBLNR
      value(EV_QUANTITY) type INRI-QUANTITY
      value(EV_RETURNCODE) type INRI-RETURNCODE .

  METHOD export_inv.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = iv_nr_range_nr
        object                  = iv_object
        quantity                = '1'
        subobject               = iv_subobject
        toyear                  = iv_fisc_yr      "'0000'
        ignore_buffer           = ' '
      IMPORTING
        number                  = ev_number
        quantity                = ev_quantity
        returncode              = ev_returncode
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.
      
