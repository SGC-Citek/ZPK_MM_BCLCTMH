CLASS zcl_mm_bclctmh_manage DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: gt_data    TYPE TABLE OF zi_mm_bclctmh.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_mm_bclctmh_manage,
      get_data
        IMPORTING io_request TYPE REF TO if_rap_query_request
        EXPORTING et_data    LIKE gt_data.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: instance TYPE REF TO zcl_mm_bclctmh_manage.
    CLASS-METHODS:
      get_data_db
        IMPORTING io_request TYPE REF TO if_rap_query_request
        EXPORTING et_data    LIKE gt_data.
ENDCLASS.



CLASS ZCL_MM_BCLCTMH_MANAGE IMPLEMENTATION.


  METHOD get_data.
    " get list field requested ----------------------
    DATA(lt_reqs_element) = io_request->get_requested_elements( ).
    DATA(lt_aggr_element) = io_request->get_aggregation( )->get_aggregated_elements( ).
    IF lt_aggr_element IS NOT INITIAL.
      LOOP AT lt_aggr_element ASSIGNING FIELD-SYMBOL(<lfs_aggr_elements>).
        DELETE lt_reqs_element WHERE table_line = <lfs_aggr_elements>-result_element.
        DATA(lv_aggr) = |{ <lfs_aggr_elements>-aggregation_method }( { <lfs_aggr_elements>-input_element } ) as { <lfs_aggr_elements>-result_element }|.
        APPEND lv_aggr TO lt_reqs_element.
      ENDLOOP.
    ENDIF.

    DATA(lv_reqs_element) = concat_lines_of( table = lt_reqs_element sep = `, ` ).
    " get list field requested ----------------------

    " get list field ordered ------------------------
    DATA(lt_sort) = io_request->get_sort_elements( ).

    DATA(lt_sort_criteria) = VALUE string_table( FOR ls_sort IN lt_sort ( ls_sort-element_name && COND #( WHEN ls_sort-descending = abap_true THEN ` descending`
                                                                                                                                              ELSE ` ascending` ) ) ).

    DATA(lv_sort_element) = COND #( WHEN lt_sort_criteria IS INITIAL
                                    THEN `SoDH, ItemDH, MaHang DESCENDING, SoDNMH, ItemDNMH, InboundDelivery, InboundDeliveryItem, MaterialDocumentYear, MaterialDocument, MaterialDocumentItem, FiscalYear, InvoiceDocument, InvoiceDocumentItem`
                                    ELSE concat_lines_of( table = lt_sort_criteria sep = `, ` ) ).
    " get list field ordered ------------------------

    " get range of row data -------------------------
    DATA(lv_top)      = io_request->get_paging( )->get_page_size( ).
    DATA(lv_skip)     = io_request->get_paging( )->get_offset( ).
    DATA(lv_max_rows) = COND #( WHEN lv_top = if_rap_query_paging=>page_size_unlimited THEN 0
                                ELSE lv_top ).
    IF lv_max_rows = -1 .
      lv_max_rows = 1.
    ENDIF.
    " get range of row data -------------------------

    "get data --------------------------------------
    DATA: lv_fieldname   TYPE c LENGTH 30,
          lv_count       TYPE int1,
          lv_prev_serial TYPE c LENGTH 18.

    get_data_db( EXPORTING io_request = io_request IMPORTING et_data = DATA(lt_data) ).

    SELECT (lv_reqs_element)
    FROM @lt_data AS data
    ORDER BY (lv_sort_element)
    INTO CORRESPONDING FIELDS OF TABLE @et_data
    OFFSET @lv_skip UP TO @lv_max_rows ROWS.
    "get data --------------------------------------
  ENDMETHOD.


  METHOD get_data_db.
    TYPES: lv_purorg TYPE c LENGTH 4,
           lv_matgrp TYPE c LENGTH 9,
           lv_sodnmh TYPE c LENGTH 10.
    DATA: lr_purorg        TYPE RANGE OF lv_purorg,
          lr_mahang        TYPE RANGE OF matnr,
          lr_matgrp        TYPE RANGE OF lv_matgrp,
          lr_mancc         TYPE RANGE OF lifnr,
          lr_sodnmh        TYPE RANGE OF lv_sodnmh,
          lr_sodh          TYPE RANGE OF ebeln,
          lr_usercrtpo     TYPE RANGE OF usnam,
          lr_ngaydh        TYPE RANGE OF dats,
          lr_inbound       TYPE RANGE OF vbeln_vl,
          lr_usercrdinb    TYPE RANGE OF usnam,
          lr_ngaynhanyc    TYPE RANGE OF dats,
          lr_ngaynhankh    TYPE RANGE OF dats,
          lr_ngaynhantt    TYPE RANGE OF dats,
          lv_calvat        TYPE zde_yes_no,
          lr_material_type TYPE RANGE OF zi_mm_bclctmh-producttype.

    DATA: lt_data TYPE TABLE OF zi_mm_bclctmh,
          ls_data TYPE zi_mm_bclctmh.

    DATA: lv_prev_poitem     TYPE c LENGTH 15,
          lv_prev_inbitem    TYPE c LENGTH 16,
          lv_prev_matdoc     TYPE c LENGTH 10,
          lv_prev_matdocitem TYPE c LENGTH 18,
          lv_prev_inv        TYPE c LENGTH 10.

    " get filter by parameter -----------------------
    TRY.
        DATA(lt_filter_cond) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range INTO DATA(lx_no_sel_option).
    ENDTRY.
    IF lt_filter_cond IS NOT INITIAL.
      LOOP AT lt_filter_cond REFERENCE INTO DATA(ls_filter_cond).
        CASE ls_filter_cond->name.
          WHEN 'PURCHASINGORGANIZATION'.
            lr_purorg = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'MAHANG'.
            lr_mahang = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'MATERIALGROUP'.
            lr_matgrp = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'MANCC'.
            lr_mancc = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'SODNMH'.
            lr_sodnmh = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'SODH'.
            lr_sodh = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'CREATEDBYUSERPO'.
            lr_usercrtpo = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'NGAYDH'.
            lr_ngaydh = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'INBOUNDDELIVERY'.
            lr_inbound = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'CREATEDBYUSERINB'.
            lr_usercrdinb = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'NGAYNHANHANGYC'.
            lr_ngaynhanyc = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'NGAYNHANHANGKH'.
            lr_ngaynhankh = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'NGAYNHANHANGTT'.
            lr_ngaynhantt = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'PRODUCTTYPE'.
            lr_material_type = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'FLAGCALVAT'.
            lv_calvat = ls_filter_cond->range[ 1 ]-low .
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDIF.
    " get filter by parameter -----------------------

    " PR PO INB MatDoc Inv
    SELECT
      i_purchaseorderitemapi01~purchaseorder                        AS sodh,
      i_purchaseorderitemapi01~purchaseorderitem                    AS itemdh,
      CASE
        WHEN i_purchaseorderapi01~yy1_nguoilapphieu_pdh IS NOT INITIAL THEN i_purchaseorderapi01~yy1_nguoilapphieu_pdh
        ELSE zi_user~userdescription
      END                                                           AS nguoimuahang,
      i_purchaseorderapi01~supplier                                 AS mancc,
      zcore_i_profile_supplier~supplierfullname                     AS tenncc,
      i_purchaseorderapi01~creationdate                             AS ngaydh,
      i_purchaseorderitemapi01~material                             AS mahang,
      i_purchaseorderitemapi01~purchaseorderquantityunit            AS dvt,
      i_purchaseorderitemapi01~orderquantity                        AS soluongkh,
      i_purchaseorderitemapi01~documentcurrency                     AS currencypo,
      i_purchaseorderitemapi01~netpriceamount                       AS dongia,
*      pricing~conditionratevalue                                    AS vat,
      pricing~conditionratevalue                                    AS vatcalc,
      i_purchaseorderitemapi01~netamount,
      i_purchaseorderitemapi01~netpricequantity,
      i_purchaseorderitemapi01~purchaserequisition                  AS sodnmh,
      i_purchaseorderitemapi01~purchaserequisitionitem              AS itemdnmh,
      i_purchaseorderitemapi01~purchaseorderitemtext,
      i_purchaseorderitemapi01~manualdeliveryaddressid,
      i_purchaseorderitemapi01~referencedeliveryaddressid,
      i_purchaseorderitemapi01~purchasingdocumentdeletioncode,
      i_purchaseorderitemapi01~iscompletelydelivered                AS iscompletelydelivered,
      i_purchaseorderstatus~purchasingdocumentstatus,
      i_purchaseorderapi01~creationdate                             AS ngaytaopo,
      i_plant~addressid,
      i_purchaseorderitemapi01~isreturnsitem,
      i_productgrouptext_2~productgroupname AS nhomhang
      FROM i_purchaseorderapi01
      INNER JOIN i_purchaseorderitemapi01
      ON i_purchaseorderitemapi01~purchaseorder = i_purchaseorderapi01~purchaseorder
      INNER JOIN i_plant
      ON i_purchaseorderitemapi01~plant = i_plant~plant
      LEFT JOIN i_purchaseorderstatus
             ON i_purchaseorderstatus~purchaseorder = i_purchaseorderapi01~purchaseorder
      LEFT OUTER JOIN zcore_i_profile_supplier
      ON i_purchaseorderapi01~supplier = zcore_i_profile_supplier~supplier
      LEFT JOIN i_supplierinvoiceapi01 ON i_supplierinvoiceapi01~supplierinvoice = '323'
*      LEFT OUTER JOIN i_businesspartner  AS personpo
*      ON personpo~businesspartner = i_purchaseorderapi01~createdbyuser
      LEFT JOIN zi_user
      ON zi_user~userid = i_purchaseorderapi01~createdbyuser
      LEFT OUTER JOIN i_supplier
      ON i_supplier~supplier = i_purchaseorderapi01~supplier
      LEFT OUTER JOIN i_productgrouptext_2
      ON i_productgrouptext_2~productgroup = i_purchaseorderitemapi01~materialgroup
      AND i_productgrouptext_2~language = @sy-langu
      LEFT JOIN i_purorditmpricingelementapi01  AS pricing
                                                ON pricing~purchaseorder     = i_purchaseorderitemapi01~purchaseorder
                                               AND pricing~purchaseorderitem = i_purchaseorderitemapi01~purchaseorderitem
                                               AND pricing~conditiontype     = 'MWVS'
      WHERE i_purchaseorderitemapi01~purchasingdocumentdeletioncode IS INITIAL
        AND i_purchaseorderapi01~purchasingorganization IN @lr_purorg
        AND i_purchaseorderitemapi01~material           IN @lr_mahang
        AND i_purchaseorderitemapi01~materialgroup      IN @lr_matgrp
        AND i_purchaseorderitemapi01~materialtype       IN @lr_material_type
        AND i_purchaseorderapi01~supplier               IN @lr_mancc
        AND i_purchaseorderitemapi01~purchaserequisition IN @lr_sodnmh
        AND i_purchaseorderapi01~purchaseorder          IN @lr_sodh
        AND i_purchaseorderapi01~createdbyuser          IN @lr_usercrtpo
        AND i_purchaseorderapi01~creationdate           IN @lr_ngaydh
      INTO TABLE @DATA(lt_po).
    CHECK sy-subrc EQ 0.
    SORT lt_po BY sodh itemdh.

    SELECT
      zi_address~addressid,
      zi_address~streetname,
      zi_address~streetprefixname1,
      zi_address~streetprefixname2,
      zi_address~streetsuffixname1,
      zi_address~streetsuffixname2
      FROM @lt_po AS po
      INNER JOIN zi_address
      ON zi_address~addressid = po~addressid
      OR zi_address~addressid = po~manualdeliveryaddressid
      OR zi_address~addressid = po~referencedeliveryaddressid
      INTO TABLE @DATA(lt_po_address).
    IF sy-subrc EQ 0.
      SORT lt_po_address BY addressid.
    ENDIF.

    SELECT
      i_purchaserequisitionitemapi01~purchaserequisition            AS sodnmh,
      i_purchaserequisitionitemapi01~purchaserequisitionitem        AS itemdnmh,

      i_purchaserequisitionitemapi01~purchaserequisitiontype        AS prtype,
      i_purchasingdocumenttypetext~purchasingdocumenttypename       AS prtypedes,
      bophandnmhdes~description                                     AS bophandnmh,
*      ~
*      personpr~personfullname                                       AS nguoidnmh,
      i_purchaserequisitionitemapi01~requisitionername              AS nguoidnmh,
      i_purchaserequisitionitemapi01~creationdate                   AS ngaydnmh,
      i_purchaserequisitionitemapi01~purchaserequisitionreleasedate AS ngaynhandnmh,
      i_purchaserequisitionitemapi01~deliverydate                   AS ngaynhanhangkh,
      i_purchaserequisitionitemapi01~yy1_lydosudung_pri             AS lydosudung,
      i_purchaserequisitionitemapi01~yy1_ghichu_pri                 AS ghichu
      FROM @lt_po AS po
      INNER JOIN i_purchaserequisitionitemapi01
      ON  i_purchaserequisitionitemapi01~purchaserequisition     = po~sodnmh
      AND i_purchaserequisitionitemapi01~purchaserequisitionitem = po~itemdnmh
      LEFT OUTER JOIN i_purchasingdocumenttypetext
      ON  i_purchasingdocumenttypetext~purchasingdocumenttype     = i_purchaserequisitionitemapi01~purchaserequisitiontype
      AND i_purchasingdocumenttypetext~purchasingdocumentcategory = i_purchaserequisitionitemapi01~purchasingdocumentcategory
      AND i_purchasingdocumenttypetext~language                   = @sy-langu
      LEFT JOIN i_customfieldcodelisttext AS bophandnmhdes
                                          ON bophandnmhdes~customfieldid = 'YY1_PR_BOPHANYEUCAU'
                                         AND bophandnmhdes~code          = i_purchaserequisitionitemapi01~yy1_pr_bophanyeucau_pri
      LEFT OUTER JOIN i_businesspartner  AS personpr
      ON personpr~businesspartner  = i_purchaserequisitionitemapi01~createdbyuser
      WHERE i_purchaserequisitionitemapi01~deliverydate IN @lr_ngaynhankh
      INTO TABLE @DATA(lt_pr).
    IF sy-subrc EQ 0.
      SORT lt_pr BY sodnmh itemdnmh.
    ENDIF.

    SELECT
      i_deliverydocumentitem~purchaseorder                          AS sodh,
      i_deliverydocumentitem~purchaseorderitem                      AS itemdh,

      i_deliverydocumentitem~deliverydocument                       AS inbounddelivery,
      i_deliverydocumentitem~deliverydocumentitem                   AS inbounddeliveryitem,
      i_deliverydocumentitem~higherlvlitmofbatspltitm               AS higherlvlitmofbatspltitm,
      i_deliverydocumentitem~deliveryquantityunit,
*      i_deliverydocumentitem~originaldeliveryquantity               AS soluonginb
*      i_deliverydocumentitem~iscompletelydelivered                  AS iscompletelydelivered,
      i_deliverydocumentitem~actualdeliveryquantity                 AS soluonginb
*      i_deliverydocument~deliverydocumentbysupplier
      FROM @lt_po AS po
      INNER JOIN i_deliverydocumentitem
      ON  i_deliverydocumentitem~purchaseorder      =  po~sodh
      AND i_deliverydocumentitem~purchaseorderitem  =  po~itemdh
      INNER JOIN i_deliverydocument
              ON i_deliverydocument~deliverydocument = i_deliverydocumentitem~deliverydocument
      WHERE i_deliverydocumentitem~deliverydocument IN @lr_inbound
        AND i_deliverydocumentitem~createdbyuser    IN @lr_usercrdinb
*        AND i_deliverydocument~deliverydocumentbysupplier <> ''
        AND i_deliverydocumentitem~actualdeliveryquantity <> 0
*        AND NOT EXISTS ( SELECT * FROM i_deliverydocument AS b
*                         WHERE ltrim( b~deliverydocumentbysupplier, '0' ) = ltrim( i_deliverydocument~deliverydocument, '0' ) )
      INTO TABLE @DATA(lt_inb).
    IF sy-subrc EQ 0.
      SORT lt_inb BY sodh itemdh inbounddelivery inbounddeliveryitem.
    ENDIF.

    SELECT
      i_materialdocumentitem_2~deliverydocument                     AS inbounddelivery,
      i_materialdocumentitem_2~deliverydocumentitem                 AS inbounddeliveryitem,

      i_materialdocumentitem_2~materialdocumentyear,
      i_materialdocumentitem_2~materialdocument,
      i_materialdocumentitem_2~materialdocumentitem,
      i_materialdocumentitem_2~entryunit,
      i_materialdocumentitem_2~quantityinentryunit                  AS soluongdank,
      i_materialdocumentheader_2~postingdate                        AS ngaynhanhangtt,
      i_materialdocumentitem_2~totalgoodsmvtamtincccrcy
      FROM @lt_inb AS inb
      INNER JOIN i_materialdocumentitem_2
      ON  i_materialdocumentitem_2~deliverydocument                 EQ inb~inbounddelivery
      AND i_materialdocumentitem_2~deliverydocumentitem             EQ inb~inbounddeliveryitem
      INNER JOIN i_materialdocumentheader_2
      ON i_materialdocumentheader_2~materialdocument                 EQ i_materialdocumentitem_2~materialdocument
      AND i_materialdocumentheader_2~materialdocumentyear            EQ i_materialdocumentitem_2~materialdocumentyear
      WHERE i_materialdocumentitem_2~goodsmovementiscancelled       IS INITIAL
        AND i_materialdocumentitem_2~reversedmaterialdocumentyear   IS INITIAL
        AND i_materialdocumentitem_2~reversedmaterialdocument       IS INITIAL
        AND i_materialdocumentitem_2~reversedmaterialdocumentitem   IS INITIAL
        AND i_materialdocumentheader_2~postingdate                  IN @lr_ngaynhantt
      INTO TABLE @DATA(lt_matdoc).
    IF sy-subrc EQ 0.
      SORT lt_matdoc BY inbounddelivery inbounddeliveryitem materialdocumentyear materialdocument materialdocumentitem.
    ENDIF.

    SELECT
      i_materialdocumentitem_2~purchaseorder                        AS sodh,
      i_materialdocumentitem_2~purchaseorderitem                    AS itemdh,

      i_materialdocumentitem_2~materialdocumentyear,
      i_materialdocumentitem_2~materialdocument,
      i_materialdocumentitem_2~materialdocumentitem,
      i_materialdocumentitem_2~entryunit,
      i_materialdocumentitem_2~quantityinentryunit                  AS soluongdank,
      i_materialdocumentheader_2~postingdate                        AS ngaynhanhangtt,
      i_materialdocumentitem_2~totalgoodsmvtamtincccrcy
      FROM @lt_po AS po
      INNER JOIN i_materialdocumentitem_2
      ON  i_materialdocumentitem_2~purchaseorder                    EQ po~sodh
      AND i_materialdocumentitem_2~purchaseorderitem                EQ po~itemdh
      INNER JOIN i_materialdocumentheader_2
      ON i_materialdocumentheader_2~materialdocument                EQ i_materialdocumentitem_2~materialdocument
      AND i_materialdocumentheader_2~materialdocumentyear           EQ i_materialdocumentitem_2~materialdocumentyear
      WHERE i_materialdocumentitem_2~goodsmovementiscancelled       IS INITIAL
        AND i_materialdocumentitem_2~reversedmaterialdocumentyear   IS INITIAL
        AND i_materialdocumentitem_2~reversedmaterialdocument       IS INITIAL
        AND i_materialdocumentitem_2~reversedmaterialdocumentitem   IS INITIAL
        AND i_materialdocumentheader_2~postingdate                  IN @lr_ngaynhantt
      INTO TABLE @DATA(lt_matdoc_po).
    IF sy-subrc EQ 0.
      SORT lt_matdoc_po BY sodh itemdh materialdocumentyear materialdocument materialdocumentitem.
    ENDIF.

    SELECT
      i_suplrinvcitempurordrefapi01~purchaseorder                   AS sodh,
      i_suplrinvcitempurordrefapi01~purchaseorderitem               AS itemdh,

      i_suplrinvcitempurordrefapi01~referencedocument,
      i_suplrinvcitempurordrefapi01~referencedocumentfiscalyear,
      i_suplrinvcitempurordrefapi01~referencedocumentitem,

      i_suplrinvcitempurordrefapi01~fiscalyear,
      i_suplrinvcitempurordrefapi01~supplierinvoice                 AS invoicedocument,
      i_suplrinvcitempurordrefapi01~supplierinvoiceitem             AS invoicedocumentitem,
      i_suplrinvcitempurordrefapi01~purchaseorderpriceunit          AS unitinv,
      i_supplierinvoiceapi01~supplierinvoicestatus                  AS invoicestatus,
      i_suplrinvcitempurordrefapi01~qtyinpurchaseorderpriceunit,
      i_suplrinvcitempurordrefapi01~supplierinvoiceitemamount
      FROM @lt_po AS po
      INNER JOIN i_suplrinvcitempurordrefapi01
      ON  i_suplrinvcitempurordrefapi01~purchaseorder     = po~sodh
      AND i_suplrinvcitempurordrefapi01~purchaseorderitem = po~itemdh
      LEFT JOIN i_supplierinvoiceapi01
      ON i_supplierinvoiceapi01~supplierinvoice = i_suplrinvcitempurordrefapi01~supplierinvoice
      WHERE i_supplierinvoiceapi01~supplierinvoicestatus <> '8'
        AND i_supplierinvoiceapi01~supplierinvoicestatus <> '7'
        AND i_suplrinvcitempurordrefapi01~issubsequentdebitcredit <> 'X'
        AND i_supplierinvoiceapi01~reversedocument IS INITIAL
      INTO TABLE @DATA(lt_inv).
    IF sy-subrc EQ 0.
*      SELECT c_supplierinvoiceitemdex~purchaseorder,
*             c_supplierinvoiceitemdex~purchaseorderitem,
*             c_supplierinvoiceitemdex~prmthbreferencedocument,
*             c_supplierinvoiceitemdex~prmthbreferencedocumentitem,
*             c_supplierinvoiceitemdex~issubsequentdebitcredit
*      FROM   c_supplierinvoiceitemdex
*      FOR ALL ENTRIES IN @lt_inv
*      WHERE c_supplierinvoiceitemdex~purchaseorder               = @lt_inv-sodh
*        AND c_supplierinvoiceitemdex~purchaseorderitem           = @lt_inv-itemdh
*        AND c_supplierinvoiceitemdex~prmthbreferencedocument     = @lt_inv-referencedocument
*        AND c_supplierinvoiceitemdex~prmthbreferencedocumentitem = @lt_inv-referencedocumentitem
**        and c_supplierinvoiceitemdex~issubsequentdebitcredit <> 'X'
*     INTO TABLE @DATA(lt_c_supplierinvoiceitemdex).
*      IF sy-subrc = 0.
*        LOOP AT lt_inv INTO DATA(ls_inv_tmp).
*          READ TABLE lt_c_supplierinvoiceitemdex INTO DATA(ls_c_supplierinvoiceitemdex) WITH KEY purchaseorder = ls_inv_tmp-sodh
*                                                                                 purchaseorderitem = ls_inv_tmp-itemdh
*                                                                                 prmthbreferencedocument = ls_inv_tmp-referencedocument
*                                                                                 prmthbreferencedocumentitem = ls_inv_tmp-referencedocumentitem.
*          IF sy-subrc = 0 AND ls_c_supplierinvoiceitemdex-issubsequentdebitcredit IS NOT INITIAL.
*            DELETE lt_inv.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
      SORT lt_inv BY sodh itemdh referencedocument referencedocumentfiscalyear referencedocumentitem fiscalyear invoicedocument invoicedocumentitem.
    ENDIF.

    SELECT
      i_purordschedulelineapi01~purchaseorder,
      i_purordschedulelineapi01~purchaseorderitem,
      i_purordschedulelineapi01~schedulelinedeliverydate
      FROM @lt_po AS po
      INNER JOIN i_purordschedulelineapi01
      ON i_purordschedulelineapi01~purchaseorder = po~sodh
      AND i_purordschedulelineapi01~purchaseorderitem = po~itemdh
      WHERE i_purordschedulelineapi01~schedulelinedeliverydate IN @lr_ngaynhanyc
      INTO TABLE @DATA(lt_po_schdl).
    IF sy-subrc EQ 0.
      SORT lt_po_schdl BY purchaseorder purchaseorderitem schedulelinedeliverydate DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_po_schdl COMPARING purchaseorder purchaseorderitem.
    ENDIF.

    DATA: lt_poitem_input TYPE zcore_cl_get_long_text=>ty_purchaseorder_text,
          lt_pro_po_input TYPE zcore_cl_get_long_text=>ty_product_pur_text,
          lt_pro_bs_input TYPE zcore_cl_get_long_text=>ty_product,
          lt_po_input     TYPE zcore_cl_get_long_text=>ty_purchaseorder_note_text.

    LOOP AT lt_po INTO DATA(ls_po).
      INSERT VALUE #( purchaseorder     = ls_po-sodh
                      purchaseorderitem = ls_po-itemdh ) INTO TABLE lt_poitem_input.
      READ TABLE lt_po_input TRANSPORTING NO FIELDS WITH KEY
         purchaseorder  = ls_po-sodh
         textobjecttype = 'F01'
         language       = 'EN' BINARY SEARCH.
      IF sy-subrc <> 0.
        INSERT VALUE #( purchaseorder     = ls_po-sodh
                        textobjecttype    = 'F01'
                        language          = 'EN'  ) INTO TABLE lt_po_input.
      ENDIF.
      IF ls_po-mahang IS NOT INITIAL.
        INSERT VALUE #( product  = ls_po-mahang
                        language = sy-langu ) INTO TABLE lt_pro_po_input.
        INSERT VALUE #( product  = ls_po-mahang
                        language = sy-langu ) INTO TABLE lt_pro_bs_input.
      ENDIF.
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM lt_pro_po_input COMPARING ALL FIELDS.
    DELETE ADJACENT DUPLICATES FROM lt_pro_bs_input COMPARING ALL FIELDS.
    DELETE ADJACENT DUPLICATES FROM lt_po_input COMPARING ALL FIELDS.

    DATA(lt_poitem_text) = zcore_cl_get_long_text=>get_multi_purchaseorder_text( it_purchaseorder_text = lt_poitem_input ).

    DATA(lt_pro_po_text) = zcore_cl_get_long_text=>get_multi_pro_pur_text( it_product_pur_text = lt_pro_po_input ).

    DATA(lt_pro_bs_text) = zcore_cl_get_long_text=>get_multi_material_basic_text( it_material = lt_pro_bs_input ).

    DATA(lt_pro_note_text) = zcore_cl_get_long_text=>get_purchaseorder_note_text( it_purchaseorder_note_text = lt_po_input ).

    LOOP AT lt_po INTO ls_po.
      CLEAR: ls_data.
      MOVE-CORRESPONDING ls_po TO ls_data.
      READ TABLE lt_pr INTO DATA(ls_pr)
        WITH KEY sodnmh   = ls_po-sodnmh
                 itemdnmh = ls_po-itemdnmh BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_pr TO ls_data.
      ELSEIF lr_ngaynhankh IS NOT INITIAL.
        CLEAR: ls_data.
        CONTINUE.
      ENDIF.
      IF ls_po-manualdeliveryaddressid IS NOT INITIAL.
        READ TABLE lt_po_address INTO DATA(ls_po_address)
          WITH KEY addressid = ls_po-manualdeliveryaddressid BINARY SEARCH.
      ELSEIF ls_po-referencedeliveryaddressid IS NOT INITIAL.
        READ TABLE lt_po_address INTO ls_po_address
          WITH KEY addressid = ls_po-referencedeliveryaddressid BINARY SEARCH.
      ELSEIF ls_po-addressid IS NOT INITIAL.
        READ TABLE lt_po_address INTO ls_po_address
          WITH KEY addressid = ls_po-addressid BINARY SEARCH.
      ENDIF.
      IF ls_po_address IS NOT INITIAL.
        CONCATENATE ls_po_address-streetname
                    ls_po_address-streetprefixname1
                    ls_po_address-streetprefixname2
                    ls_po_address-streetsuffixname1
                    ls_po_address-streetsuffixname2
                    INTO ls_data-giaotai SEPARATED BY space.
      ENDIF.
*      READ TABLE lt_poitem_text INTO DATA(ls_poitem_text)
*        WITH KEY purchaseorder     = ls_po-sodh
*                 purchaseorderitem = ls_po-itemdh
*                 textobjecttype    = 'F03'
*                 language          = sy-langu BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        ls_data-tenhang = ls_poitem_text-plainlongtext.
**      ENDIF.
*      IF ls_data-tenhang IS INITIAL.
*        READ TABLE lt_pro_po_text INTO DATA(ls_pro_po_text)
*          WITH KEY product  = ls_po-mahang
*                   language = sy-langu BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          ls_data-tenhang = ls_pro_po_text-long_text.
*        ENDIF.
*      ENDIF.
*      IF ls_data-tenhang IS INITIAL.
      READ TABLE lt_pro_bs_text INTO DATA(ls_pro_bs_text)
        WITH KEY product  = ls_po-mahang BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_data-tenhang = ls_pro_bs_text-long_text.
      ENDIF.
*      ENDIF.
      IF ls_data-tenhang IS INITIAL.
        ls_data-tenhang = ls_po-purchaseorderitemtext.
      ENDIF.

      READ TABLE lt_inb TRANSPORTING NO FIELDS
        WITH KEY sodh   = ls_po-sodh
                 itemdh = ls_po-itemdh BINARY SEARCH.
      IF sy-subrc EQ 0.
        LOOP AT lt_inb INTO DATA(ls_inb) FROM sy-tabix.
          IF NOT ( ls_inb-sodh   = ls_po-sodh
               AND ls_inb-itemdh = ls_po-itemdh ).
            EXIT.
          ENDIF.
          MOVE-CORRESPONDING ls_inb TO ls_data.


          READ TABLE lt_matdoc TRANSPORTING NO FIELDS
            WITH KEY inbounddelivery     = ls_inb-inbounddelivery
                     inbounddeliveryitem = ls_inb-inbounddeliveryitem BINARY SEARCH.
          IF sy-subrc EQ 0.
            LOOP AT lt_matdoc INTO DATA(ls_matdoc) FROM sy-tabix.
              IF NOT ( ls_matdoc-inbounddelivery     = ls_inb-inbounddelivery
                   AND ls_matdoc-inbounddeliveryitem = ls_inb-inbounddeliveryitem ).
                EXIT.
              ENDIF.
              MOVE-CORRESPONDING ls_matdoc TO ls_data.
              APPEND ls_data TO lt_data.
              CLEAR: ls_data.
              " stored key
              ls_data-sodh     = ls_po-sodh.
              ls_data-itemdh   = ls_po-itemdh.
              ls_data-sodnmh   = ls_po-sodnmh.
              ls_data-itemdnmh = ls_po-itemdnmh.
              ls_data-inbounddelivery       = ls_inb-inbounddelivery.
              ls_data-inbounddeliveryitem   = ls_inb-inbounddeliveryitem.
*              ls_data-iscompletelydelivered = ls_inb-iscompletelydelivered.

              IF ls_data-sodnmh IS NOT INITIAL.
                ls_data-prtype     = ls_pr-prtype.
                ls_data-prtypedes  = ls_pr-prtypedes.
                ls_data-bophandnmh = ls_pr-bophandnmh.
              ENDIF.
            ENDLOOP.
          ELSEIF lr_ngaynhantt IS INITIAL.
            APPEND ls_data TO lt_data.
*                CLEAR: ls_data.
*
*                " stored key
*                ls_data-sodh     = ls_po-sodh.
*                ls_data-itemdh   = ls_po-itemdh.
*                ls_data-sodnmh   = ls_po-sodnmh.
*                ls_data-itemdnmh = ls_po-itemdnmh.
*
            " only clear quantity and amount
*            CLEAR: ls_data-soluonginb.
          ENDIF.
        ENDLOOP.
      ELSE.
        READ TABLE lt_matdoc_po TRANSPORTING NO FIELDS
          WITH KEY sodh   = ls_po-sodh
                   itemdh = ls_po-itemdh BINARY SEARCH.
        IF sy-subrc EQ 0.
          LOOP AT lt_matdoc_po INTO DATA(ls_matdoc_po) FROM sy-tabix.
            IF NOT ( ls_matdoc_po-sodh   = ls_po-sodh
                 AND ls_matdoc_po-itemdh = ls_po-itemdh ).
              EXIT.
            ENDIF.
            MOVE-CORRESPONDING ls_matdoc_po TO ls_data.
            APPEND ls_data TO lt_data.
            CLEAR: ls_data.
            " stored key
            ls_data-sodh     = ls_po-sodh.
            ls_data-itemdh   = ls_po-itemdh.
            ls_data-sodnmh   = ls_po-sodnmh.
            ls_data-itemdnmh = ls_po-itemdnmh.
            ls_data-inbounddelivery       = ls_inb-inbounddelivery.
            ls_data-inbounddeliveryitem   = ls_inb-inbounddeliveryitem.
*            ls_data-iscompletelydelivered = ls_inb-iscompletelydelivered.
            IF ls_data-sodnmh IS NOT INITIAL.
              ls_data-prtype     = ls_pr-prtype.
              ls_data-prtypedes  = ls_pr-prtypedes.
              ls_data-bophandnmh = ls_pr-bophandnmh.
            ENDIF.
          ENDLOOP.
        ELSE.
          IF lr_inbound    IS INITIAL AND
             lr_usercrdinb IS INITIAL AND
             lr_ngaynhantt IS INITIAL.
            APPEND ls_data TO lt_data.
            CLEAR: ls_data.
          ELSEIF lr_ngaynhantt IS INITIAL.
            APPEND ls_data TO lt_data.
            " only clear quantity and amount
*            CLEAR: ls_data-soluonginb.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DATA(lt_data_tmp) = lt_data.
    CLEAR: lt_data.


    LOOP AT lt_data_tmp INTO DATA(ls_data_tmp).
      IF ls_data_tmp-sodnmh IS INITIAL.
        CLEAR: ls_data_tmp-prtype,
               ls_data_tmp-prtypedes,
               ls_data_tmp-bophandnmh.
      ENDIF.
      MOVE-CORRESPONDING ls_data_tmp TO ls_data.
      READ TABLE lt_inv TRANSPORTING NO FIELDS
        WITH KEY sodh                           = ls_data_tmp-sodh
                 itemdh                         = ls_data_tmp-itemdh
                 referencedocument              = ls_data_tmp-materialdocument
                 referencedocumentfiscalyear    = ls_data_tmp-materialdocumentyear
                 referencedocumentitem          = ls_data_tmp-materialdocumentitem BINARY SEARCH.
      IF sy-subrc EQ 0.
        LOOP AT lt_inv INTO DATA(ls_inv) FROM sy-tabix.
          IF NOT ( ls_inv-sodh                          = ls_data_tmp-sodh
               AND ls_inv-itemdh                        = ls_data_tmp-itemdh
               AND ls_inv-referencedocument             = ls_data_tmp-materialdocument
               AND ls_inv-referencedocumentfiscalyear   = ls_data_tmp-materialdocumentyear
               AND ls_inv-referencedocumentitem         = ls_data_tmp-materialdocumentitem ).
            EXIT.
          ENDIF.
          MOVE-CORRESPONDING ls_inv TO ls_data.
          IF ls_inv-invoicestatus EQ 'A' OR
             ls_inv-invoicestatus EQ 'B' OR
             ls_inv-invoicestatus EQ 'C' OR
             ls_inv-invoicestatus EQ 'D' OR
             ls_inv-invoicestatus EQ 'E'.
            ls_data-invoicestatustext = 'Parked'.
            ls_data-soluongpark   = ls_inv-qtyinpurchaseorderpriceunit.
            ls_data-thanhtienpark = ls_inv-supplierinvoiceitemamount.
          ELSEIF ls_inv-invoicestatus EQ '5' OR
                 ls_inv-invoicestatus EQ '4'.
            ls_data-invoicestatustext = 'Posted'.
            ls_data-soluonginv    = ls_inv-qtyinpurchaseorderpriceunit.
            ls_data-thanhtieninv  = ls_inv-supplierinvoiceitemamount.
          ENDIF.

          APPEND ls_data TO lt_data.
*          CLEAR: ls_data.
*
*          " stored key
*          ls_data-sodh     = ls_data_tmp-sodh.
*          ls_data-itemdh   = ls_data_tmp-itemdh.
*          ls_data-sodnmh   = ls_data_tmp-sodnmh.
*          ls_data-itemdnmh = ls_data_tmp-itemdnmh.
*          ls_data-inbounddelivery     = ls_data_tmp-inbounddelivery.
*          ls_data-inbounddeliveryitem = ls_data_tmp-inbounddeliveryitem.
*
          " only clear quantity and amount
          CLEAR: ls_data-soluongdank,ls_data-soluonginv, ls_data-soluongpark, ls_data-thanhtieninv, ls_data-thanhtienpark, ls_data-totalgoodsmvtamtincccrcy.
        ENDLOOP.
      ELSE.
        APPEND ls_data TO lt_data.
        CLEAR: ls_data.
      ENDIF.
    ENDLOOP.

    "SL chưa Park
    TYPES: BEGIN OF lty_sl_chua_park,
             sodh                 TYPE vbeln_va,
             itemdh               TYPE n LENGTH 5,
             materialdocument     TYPE mblnr,
             materialdocumentyear TYPE mjahr,
             materialdocumentitem TYPE mblpo,
             sl_chua_park         TYPE menge_d,
             thanhtienchuapark    TYPE zi_mm_bclctmh-thanhtienchuapark,
           END OF lty_sl_chua_park.
    DATA: lt_sl_chua_park TYPE TABLE OF lty_sl_chua_park.

    SELECT lt_data~sodh,
           lt_data~itemdh,
           lt_data~materialdocument,
           lt_data~materialdocumentyear,
           lt_data~materialdocumentitem,
           SUM( lt_data~soluonginv + lt_data~soluongpark ) AS sl_chua_park,
           SUM( i_journalentryitem~creditamountincocodecrcy -
                lt_data~thanhtieninv  -
                lt_data~thanhtienpark ) AS thanhtienchuapark
    FROM @lt_data AS lt_data
    LEFT JOIN i_journalentryitem ON i_journalentryitem~referencedocument     = lt_data~materialdocument
                                AND i_journalentryitem~referencedocumentitem = lt_data~materialdocumentitem
    GROUP BY sodh, itemdh, materialdocument,  materialdocumentyear, materialdocumentitem
    ORDER BY sodh, itemdh, materialdocument,  materialdocumentyear, materialdocumentitem
    INTO CORRESPONDING FIELDS OF TABLE @lt_sl_chua_park.

    "Thành tiền chưa Park


    TYPES: BEGIN OF lty_inb_clear_qty,
             inbounddelivery     TYPE zde_vbeln,
             inbounddeliveryitem TYPE zde_posnr,
           END OF lty_inb_clear_qty.
    DATA: lt_inb_clear_qty  TYPE TABLE OF lty_inb_clear_qty,
          lv_check_netamout TYPE abap_boolean.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_original_data>).
      IF <lfs_original_data>-higherlvlitmofbatspltitm IS NOT INITIAL.
        <lfs_original_data>-inbounddeliveryitem = <lfs_original_data>-higherlvlitmofbatspltitm.
        APPEND VALUE #( inbounddelivery = <lfs_original_data>-inbounddelivery
                        inbounddeliveryitem = <lfs_original_data>-higherlvlitmofbatspltitm
                        ) TO lt_inb_clear_qty.
      ENDIF.
      " 12 TenHang      lay longtext
      " 26 NgayNhanHangTT = POSTINGDATE của MatDoc xa nhất (MatDoc)

      " 32 NgayNhanHangYC = DELIVERYDATE PurOrdSchedulelineAPI01 xa nhất (ItemPO)
      READ TABLE lt_po_schdl INTO DATA(ls_po_schdl)
        WITH KEY purchaseorder     = <lfs_original_data>-sodh
                 purchaseorderitem = <lfs_original_data>-itemdh BINARY SEARCH.
      IF sy-subrc EQ 0.
        <lfs_original_data>-ngaynhanhangyc = ls_po_schdl-schedulelinedeliverydate.
      ENDIF.
      " 33 GiaoTai = logic (PO)
    ENDLOOP.

    IF lr_ngaynhanyc IS NOT INITIAL.
      DELETE lt_data WHERE ngaynhanhangyc NOT IN lr_ngaynhanyc.
    ENDIF.

    SORT lt_inb_clear_qty BY inbounddelivery inbounddeliveryitem.

    SELECT
      inbounddelivery,
      inbounddeliveryitem,
      entryunit,
      SUM( soluongdank ) AS soluongdank
      FROM @lt_data AS data
      WHERE
      inbounddelivery IS NOT INITIAL
      GROUP BY
      inbounddelivery,
      inbounddeliveryitem,
      entryunit
      INTO TABLE @DATA(lt_data_ib_sum).
    IF sy-subrc EQ 0.
      SORT lt_data_ib_sum BY inbounddelivery inbounddeliveryitem.
    ENDIF.

    SELECT
      sodh,
      itemdh,
      entryunit,
      SUM( soluongdank ) AS soluongdank
      FROM @lt_data AS data
      WHERE
      inbounddelivery IS INITIAL
      GROUP BY
      sodh,
      itemdh,
      entryunit
      INTO TABLE @DATA(lt_data_wthout_ib_sum).
    IF sy-subrc EQ 0.
      SORT lt_data_wthout_ib_sum BY sodh itemdh.
    ENDIF.

    SELECT lt_data~sodh,
           lt_data~itemdh,
           lt_data~inbounddelivery,
           lt_data~inbounddeliveryitem,
           SUM( soluonginb ) AS soluonginb
    FROM @lt_data AS lt_data
    GROUP BY lt_data~sodh, lt_data~itemdh, lt_data~inbounddelivery, lt_data~inbounddeliveryitem
    ORDER BY lt_data~sodh, lt_data~itemdh, lt_data~inbounddelivery, lt_data~inbounddeliveryitem
    INTO TABLE @DATA(lt_sum_soluonginb).

    "Giá trị chi phí khác
    DATA(lt_tmp_cp_park) = lt_data.
    SORT lt_tmp_cp_park BY sodh itemdh .
    DELETE ADJACENT DUPLICATES FROM lt_tmp_cp_park COMPARING sodh itemdh .
    "Giá trị chi phí khác
    SELECT
        c_supplierinvoiceitemdex~purchaseorder,
        c_supplierinvoiceitemdex~purchaseorderitem,
        c_supplierinvoiceitemdex~documentcurrency,
        lt_tmp_cp_park~invoicestatus,
        SUM( c_supplierinvoiceitemdex~unplanneddeliverycost + supplierinvoiceitemamount ) AS amout
    FROM c_supplierinvoiceitemdex
    INNER JOIN @lt_tmp_cp_park AS lt_tmp_cp_park
                               ON lt_tmp_cp_park~sodh   = c_supplierinvoiceitemdex~purchaseorder
                              AND lt_tmp_cp_park~itemdh =  c_supplierinvoiceitemdex~purchaseorderitem
    WHERE c_supplierinvoiceitemdex~supplierinvoicestatus   IN ( 'A','B','C','D' ,'E' )
      AND c_supplierinvoiceitemdex~prmthbreferencedocument IS INITIAL
    GROUP BY c_supplierinvoiceitemdex~purchaseorder, c_supplierinvoiceitemdex~purchaseorderitem, c_supplierinvoiceitemdex~documentcurrency,lt_tmp_cp_park~invoicestatus
    ORDER BY c_supplierinvoiceitemdex~purchaseorder, c_supplierinvoiceitemdex~purchaseorderitem, c_supplierinvoiceitemdex~documentcurrency,lt_tmp_cp_park~invoicestatus
    INTO TABLE @DATA(lt_cp_k1).

    SELECT
       c_supplierinvoiceitemdex~purchaseorder,
       c_supplierinvoiceitemdex~purchaseorderitem,
       c_supplierinvoiceitemdex~documentcurrency,
       lt_tmp_cp_park~invoicestatus,
       SUM( c_supplierinvoiceitemdex~unplanneddeliverycost ) AS amout
   FROM c_supplierinvoiceitemdex
   INNER JOIN @lt_tmp_cp_park AS lt_tmp_cp_park
                              ON lt_tmp_cp_park~sodh   = c_supplierinvoiceitemdex~purchaseorder
                             AND lt_tmp_cp_park~itemdh =  c_supplierinvoiceitemdex~purchaseorderitem
   WHERE c_supplierinvoiceitemdex~issubsequentdebitcredit <> 'X'
     AND c_supplierinvoiceitemdex~supplierinvoicestatus   IN ( 'A','B','C','D' ,'E' )
     AND c_supplierinvoiceitemdex~prmthbreferencedocument IS INITIAL
   GROUP BY c_supplierinvoiceitemdex~purchaseorder, c_supplierinvoiceitemdex~purchaseorderitem,c_supplierinvoiceitemdex~documentcurrency,lt_tmp_cp_park~invoicestatus
   ORDER BY c_supplierinvoiceitemdex~purchaseorder, c_supplierinvoiceitemdex~purchaseorderitem,c_supplierinvoiceitemdex~documentcurrency,lt_tmp_cp_park~invoicestatus
   INTO TABLE @DATA(lt_cp_k2).

    SELECT
        c_supplierinvoiceitemdex~purchaseorder,
        c_supplierinvoiceitemdex~purchaseorderitem,
        c_supplierinvoiceitemdex~documentcurrency,
        lt_tmp_cp_park~invoicestatus,
        SUM( c_supplierinvoiceitemdex~unplanneddeliverycost + supplierinvoiceitemamount ) AS amout
    FROM c_supplierinvoiceitemdex
    INNER JOIN @lt_tmp_cp_park AS lt_tmp_cp_park
                               ON lt_tmp_cp_park~sodh   = c_supplierinvoiceitemdex~purchaseorder
                              AND lt_tmp_cp_park~itemdh =  c_supplierinvoiceitemdex~purchaseorderitem
                              AND lt_tmp_cp_park~materialdocument     = c_supplierinvoiceitemdex~prmthbreferencedocument
                              AND lt_tmp_cp_park~materialdocumentitem = c_supplierinvoiceitemdex~prmthbreferencedocumentitem
    WHERE c_supplierinvoiceitemdex~issubsequentdebitcredit = 'X'
      AND c_supplierinvoiceitemdex~supplierinvoicestatus   IN ( 'A','B','C', 'D','E' )
    GROUP BY c_supplierinvoiceitemdex~purchaseorder, c_supplierinvoiceitemdex~purchaseorderitem,c_supplierinvoiceitemdex~documentcurrency,lt_tmp_cp_park~invoicestatus
    ORDER BY c_supplierinvoiceitemdex~purchaseorder, c_supplierinvoiceitemdex~purchaseorderitem,c_supplierinvoiceitemdex~documentcurrency,lt_tmp_cp_park~invoicestatus
    INTO TABLE @DATA(lt_cp_k3).

    "Giá trị chi phí Invoice

    SELECT
        c_supplierinvoiceitemdex~purchaseorder,
        c_supplierinvoiceitemdex~purchaseorderitem,
        c_supplierinvoiceitemdex~documentcurrency,
        lt_tmp_cp_park~invoicestatus,
        SUM( c_supplierinvoiceitemdex~unplanneddeliverycost + supplierinvoiceitemamount ) AS amout
    FROM c_supplierinvoiceitemdex
    INNER JOIN @lt_tmp_cp_park AS lt_tmp_cp_park
                               ON lt_tmp_cp_park~sodh   = c_supplierinvoiceitemdex~purchaseorder
                              AND lt_tmp_cp_park~itemdh =  c_supplierinvoiceitemdex~purchaseorderitem
    WHERE c_supplierinvoiceitemdex~supplierinvoicestatus   IN ( '4', '5' )
      AND c_supplierinvoiceitemdex~prmthbreferencedocument IS INITIAL
    GROUP BY c_supplierinvoiceitemdex~purchaseorder, c_supplierinvoiceitemdex~purchaseorderitem, c_supplierinvoiceitemdex~documentcurrency, lt_tmp_cp_park~invoicestatus
    ORDER BY c_supplierinvoiceitemdex~purchaseorder, c_supplierinvoiceitemdex~purchaseorderitem, c_supplierinvoiceitemdex~documentcurrency, lt_tmp_cp_park~invoicestatus
    INTO TABLE @DATA(lt_cp_inv_k1).

    SELECT
       c_supplierinvoiceitemdex~purchaseorder,
       c_supplierinvoiceitemdex~purchaseorderitem,
       c_supplierinvoiceitemdex~documentcurrency,
       lt_tmp_cp_park~invoicestatus,
       SUM( c_supplierinvoiceitemdex~unplanneddeliverycost ) AS amout
   FROM c_supplierinvoiceitemdex
   INNER JOIN @lt_tmp_cp_park AS lt_tmp_cp_park
                              ON lt_tmp_cp_park~sodh   = c_supplierinvoiceitemdex~purchaseorder
                             AND lt_tmp_cp_park~itemdh =  c_supplierinvoiceitemdex~purchaseorderitem
                             AND lt_tmp_cp_park~materialdocument  = c_supplierinvoiceitemdex~prmthbreferencedocument
   WHERE c_supplierinvoiceitemdex~issubsequentdebitcredit <> 'X'
     AND c_supplierinvoiceitemdex~supplierinvoicestatus   IN ( '4', '5' )
     AND c_supplierinvoiceitemdex~prmthbreferencedocument IS INITIAL
   GROUP BY c_supplierinvoiceitemdex~purchaseorder, c_supplierinvoiceitemdex~purchaseorderitem,c_supplierinvoiceitemdex~documentcurrency,lt_tmp_cp_park~invoicestatus
   ORDER BY c_supplierinvoiceitemdex~purchaseorder, c_supplierinvoiceitemdex~purchaseorderitem,c_supplierinvoiceitemdex~documentcurrency,lt_tmp_cp_park~invoicestatus
   INTO TABLE @DATA(lt_cp_inv_k2).

    SELECT
        c_supplierinvoiceitemdex~purchaseorder,
        c_supplierinvoiceitemdex~purchaseorderitem,
        c_supplierinvoiceitemdex~documentcurrency,
        lt_tmp_cp_park~invoicestatus,
        SUM( c_supplierinvoiceitemdex~unplanneddeliverycost + supplierinvoiceitemamount ) AS amout
    FROM c_supplierinvoiceitemdex
    INNER JOIN @lt_tmp_cp_park AS lt_tmp_cp_park
                               ON lt_tmp_cp_park~sodh   = c_supplierinvoiceitemdex~purchaseorder
                              AND lt_tmp_cp_park~itemdh =  c_supplierinvoiceitemdex~purchaseorderitem
                              AND lt_tmp_cp_park~materialdocument     = c_supplierinvoiceitemdex~prmthbreferencedocument
                              AND lt_tmp_cp_park~materialdocumentitem = c_supplierinvoiceitemdex~prmthbreferencedocumentitem
    WHERE c_supplierinvoiceitemdex~issubsequentdebitcredit = 'X'
      AND c_supplierinvoiceitemdex~supplierinvoicestatus   IN ( '4', '5' )
    GROUP BY c_supplierinvoiceitemdex~purchaseorder, c_supplierinvoiceitemdex~purchaseorderitem,c_supplierinvoiceitemdex~documentcurrency,lt_tmp_cp_park~invoicestatus
    ORDER BY c_supplierinvoiceitemdex~purchaseorder, c_supplierinvoiceitemdex~purchaseorderitem,c_supplierinvoiceitemdex~documentcurrency,lt_tmp_cp_park~invoicestatus
    INTO TABLE @DATA(lt_cp_inv_k3).



    LOOP AT lt_data ASSIGNING <lfs_original_data>.
      "Giá trị chi phí Park
      READ TABLE lt_cp_k1 INTO DATA(ls_cp_k1) WITH KEY purchaseorder     = <lfs_original_data>-sodh
                                                       purchaseorderitem = <lfs_original_data>-itemdh.
*                                                       invoicestatus     = <lfs_original_data>-invoicestatus.
      READ TABLE lt_cp_k2 INTO DATA(ls_cp_k2) WITH KEY purchaseorder     = <lfs_original_data>-sodh
                                                       purchaseorderitem = <lfs_original_data>-itemdh.
*                                                       invoicestatus     = <lfs_original_data>-invoicestatus.
      READ TABLE lt_cp_k3 INTO DATA(ls_cp_k3) WITH KEY purchaseorder     = <lfs_original_data>-sodh
                                                       purchaseorderitem = <lfs_original_data>-itemdh.
*                                                       invoicestatus     = <lfs_original_data>-invoicestatus.
      <lfs_original_data>-giatrichiphipark    = ls_cp_k1-amout + ls_cp_k2-amout + ls_cp_k3-amout.
      <lfs_original_data>-curgiatrichiphipark = ls_cp_k1-documentcurrency.
      IF <lfs_original_data>-curgiatrichiphipark IS INITIAL.
        <lfs_original_data>-curgiatrichiphipark = ls_cp_k2-documentcurrency.
      ENDIF.
      IF <lfs_original_data>-curgiatrichiphipark IS INITIAL.
        <lfs_original_data>-curgiatrichiphipark = ls_cp_k3-documentcurrency.
      ENDIF.

      "Giá trị chi phí Invoice
      READ TABLE lt_cp_inv_k1 INTO DATA(ls_cp_inv_k1) WITH KEY purchaseorder     = <lfs_original_data>-sodh
                                                               purchaseorderitem = <lfs_original_data>-itemdh.
      READ TABLE lt_cp_inv_k2 INTO DATA(ls_cp_inv_k2) WITH KEY purchaseorder     = <lfs_original_data>-sodh
                                                               purchaseorderitem = <lfs_original_data>-itemdh.
      READ TABLE lt_cp_inv_k3 INTO DATA(ls_cp_inv_k3) WITH KEY purchaseorder     = <lfs_original_data>-sodh
                                                               purchaseorderitem = <lfs_original_data>-itemdh.
      <lfs_original_data>-giatrichiphiinvoice        = ls_cp_inv_k1-amout + ls_cp_inv_k2-amout + ls_cp_inv_k3-amout.
      <lfs_original_data>-curgiatrichiphiinvoicepark = ls_cp_inv_k1-documentcurrency.
      IF <lfs_original_data>-curgiatrichiphiinvoicepark IS INITIAL.
        <lfs_original_data>-curgiatrichiphiinvoicepark = ls_cp_inv_k2-documentcurrency.
      ENDIF.
      IF <lfs_original_data>-curgiatrichiphiinvoicepark IS INITIAL.
        <lfs_original_data>-curgiatrichiphiinvoicepark = ls_cp_inv_k3-documentcurrency.
      ENDIF.

      READ TABLE lt_sum_soluonginb REFERENCE INTO DATA(ls_sum_soluonginb) WITH KEY sodh   = <lfs_original_data>-sodh
                                                                                   itemdh = <lfs_original_data>-itemdh
                                                                                   inbounddelivery = <lfs_original_data>-inbounddelivery
                                                                                   inbounddeliveryitem = <lfs_original_data>-inbounddeliveryitem BINARY SEARCH.
      IF sy-subrc = 0.
        <lfs_original_data>-soluonginb = ls_sum_soluonginb->soluonginb.
      ENDIF.
      IF <lfs_original_data>-higherlvlitmofbatspltitm IS INITIAL.
        READ TABLE lt_inb_clear_qty TRANSPORTING NO FIELDS
          WITH KEY inbounddelivery = <lfs_original_data>-inbounddelivery
                   inbounddeliveryitem = <lfs_original_data>-inbounddeliveryitem BINARY SEARCH.
        IF sy-subrc EQ 0.
          CLEAR: <lfs_original_data>-soluonginb.
        ENDIF.
      ENDIF.
      READ TABLE lt_data_ib_sum INTO DATA(ls_data_ib_sum)
        WITH KEY inbounddelivery     = <lfs_original_data>-inbounddelivery
                 inbounddeliveryitem = <lfs_original_data>-inbounddeliveryitem BINARY SEARCH.
      IF sy-subrc EQ 0.
        " 41 SoLuongChuaNK = SoLuongINB - SoLuongDaNK(sum theo INB item)
        <lfs_original_data>-soluongchuank = <lfs_original_data>-soluonginb - ls_data_ib_sum-soluongdank.
        READ TABLE lt_sl_chua_park INTO DATA(ls_sl_chua_park) WITH KEY sodh = <lfs_original_data>-sodh
                                                                    itemdh  = <lfs_original_data>-itemdh
                                                                    materialdocument = <lfs_original_data>-materialdocument
                                                                    materialdocumentyear = <lfs_original_data>-materialdocumentyear
                                                                    materialdocumentitem = <lfs_original_data>-materialdocumentitem BINARY SEARCH.
        IF sy-subrc = 0.
          <lfs_original_data>-soluongchuapark   = ls_data_ib_sum-soluongdank - ls_sl_chua_park-sl_chua_park.
          <lfs_original_data>-thanhtienchuapark = ls_sl_chua_park-thanhtienchuapark.
        ELSE.
          <lfs_original_data>-soluongchuapark = ls_data_ib_sum-soluongdank.
        ENDIF.
      ELSE.
        READ TABLE lt_data_wthout_ib_sum INTO DATA(ls_data_wthout_ib_sum)
          WITH KEY sodh   = <lfs_original_data>-sodh
                   itemdh = <lfs_original_data>-itemdh BINARY SEARCH.
        IF sy-subrc EQ 0.
          " 41 SoLuongChuaNK = SoLuongKH - SoLuongDaNK(sum theo PO item)
          <lfs_original_data>-soluongchuank = <lfs_original_data>-soluongkh - ls_data_wthout_ib_sum-soluongdank.
          READ TABLE lt_sl_chua_park INTO ls_sl_chua_park WITH KEY sodh = <lfs_original_data>-sodh
                                                                    itemdh  = <lfs_original_data>-itemdh
                                                                    materialdocument = <lfs_original_data>-materialdocument
                                                                    materialdocumentyear = <lfs_original_data>-materialdocumentyear
                                                                    materialdocumentitem = <lfs_original_data>-materialdocumentitem BINARY SEARCH.
          IF sy-subrc = 0.
            <lfs_original_data>-soluongchuapark   = ls_data_wthout_ib_sum-soluongdank - ls_sl_chua_park-sl_chua_park.
            <lfs_original_data>-thanhtienchuapark = ls_sl_chua_park-thanhtienchuapark.
          ELSE.
            <lfs_original_data>-soluongchuapark = ls_data_wthout_ib_sum-soluongdank.
          ENDIF.
        ENDIF.
      ENDIF.
      IF lv_prev_poitem <> <lfs_original_data>-sodh && <lfs_original_data>-itemdh.
        IF <lfs_original_data>-netamount = 0 .
          lv_check_netamout = abap_true.
          <lfs_original_data>-soluongchuapark   = 0.
          <lfs_original_data>-thanhtienchuapark = 0.
          <lfs_original_data>-hangtang          = 'X'.
        ELSE.
          lv_check_netamout = abap_false.
        ENDIF.
      ENDIF.
      IF lv_prev_poitem EQ <lfs_original_data>-sodh && <lfs_original_data>-itemdh.
        IF lv_check_netamout = abap_true.
          <lfs_original_data>-soluongchuapark   = 0.
          <lfs_original_data>-thanhtienchuapark = 0.
          <lfs_original_data>-hangtang          = 'X'.
        ENDIF.
        CLEAR:
           <lfs_original_data>-giatrichiphiinvoice,
           <lfs_original_data>-giatrichiphipark,
*        // Kế hoạch
            <lfs_original_data>-soluongkh,
*        // Thực tế
            <lfs_original_data>-soluongtt,
*        // Còn lại
            <lfs_original_data>-soluongttconlai,
*        // Đơn giá
            <lfs_original_data>-dongia,
*        // VAT
            <lfs_original_data>-vat,
*        // Kế hoạch
            <lfs_original_data>-thanhtienkh,
*        // Thực tế
            <lfs_original_data>-thanhtientt,
*        // Còn lại
            <lfs_original_data>-thanhtienttconlai,
*        // Số lượng chưa tạo INB
            <lfs_original_data>-soluongchuataoinb.
**        // SL Park
*            <lfs_original_data>-soluongpark,
**        // Thành tiền Park
*            <lfs_original_data>-thanhtienpark,
**        // SL Invoice
*            <lfs_original_data>-soluonginv,
**        // Thành tiền Invoice
*            <lfs_original_data>-thanhtieninv.
      ENDIF.
      IF <lfs_original_data>-inbounddelivery IS NOT INITIAL.
        IF lv_prev_inbitem EQ <lfs_original_data>-inbounddelivery && <lfs_original_data>-inbounddeliveryitem.
          CLEAR:
*        // Số lượng INB
              <lfs_original_data>-soluonginb,
*        // SL chưa nhập kho
              <lfs_original_data>-soluongchuank.
        ENDIF.
      ENDIF.
      IF lv_prev_matdoc EQ <lfs_original_data>-materialdocument.
        CLEAR:
*        // Thực tế
            <lfs_original_data>-ngaynhanhangtt.
      ENDIF.
      IF lv_prev_matdocitem EQ <lfs_original_data>-materialdocument && <lfs_original_data>-materialdocumentyear && <lfs_original_data>-materialdocumentitem.
        CLEAR:
*        // SL chưa Park
            <lfs_original_data>-soluongchuapark,
*        // Thành tiền chưa Park
            <lfs_original_data>-thanhtienchuapark,
*        // SL chưa Invoice
            <lfs_original_data>-soluongchuainv,
*        // Thành tiền chưa Invoice
            <lfs_original_data>-thanhtienchuainv.
      ENDIF.
      IF lv_prev_inv EQ <lfs_original_data>-invoicedocument.
        CLEAR:
*        // Invoice Status
            <lfs_original_data>-invoicestatus,
            <lfs_original_data>-invoicestatustext.
      ENDIF.
      lv_prev_poitem = <lfs_original_data>-sodh && <lfs_original_data>-itemdh.
      lv_prev_inbitem = <lfs_original_data>-inbounddelivery && <lfs_original_data>-inbounddeliveryitem.
      lv_prev_matdoc = <lfs_original_data>-materialdocument.
      lv_prev_matdocitem = <lfs_original_data>-materialdocument && <lfs_original_data>-materialdocumentyear && <lfs_original_data>-materialdocumentitem.
      lv_prev_inv = <lfs_original_data>-invoicedocument.

      CLEAR: ls_cp_k1, ls_cp_k2, ls_cp_k3, ls_cp_inv_k1, ls_cp_inv_k2, ls_cp_inv_k3.
    ENDLOOP.

    SELECT
      sodh,
      itemdh,
*      entryunit,
*      deliveryquantityunit,
*      unitinv,
*      currencyinv,
      SUM( soluonginb ) AS soluonginb,
      SUM( soluongdank ) AS soluongdank,
      SUM( soluongpark ) AS soluongpark,
      SUM( thanhtienpark ) AS thanhtienpark,
      SUM( soluonginv ) AS soluonginv,
      SUM( thanhtieninv ) AS thanhtieninv
      FROM @lt_data AS data
      GROUP BY
      sodh,
      itemdh
*      entryunit,
*      deliveryquantityunit,
*      unitinv,
*      currencyinv
      INTO TABLE @DATA(lt_data_po_sum).
    IF sy-subrc EQ 0.
      SORT lt_data_po_sum BY sodh itemdh.
    ENDIF.



    DATA: lv_tax    TYPE n LENGTH 2,
          lv_dongia TYPE p DECIMALS 4.
    "Lấy thông tin Tax Code
    CLEAR: ls_data.
    LOOP AT lt_data ASSIGNING <lfs_original_data>.
      READ TABLE lt_data_po_sum INTO DATA(ls_data_po_sum)
        WITH KEY sodh   = <lfs_original_data>-sodh
                 itemdh = <lfs_original_data>-itemdh BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR: ls_data_po_sum.
      ENDIF.
      READ TABLE lt_pro_note_text INTO DATA(ls_pro_note_text) WITH KEY
          purchaseorder  = <lfs_original_data>-sodh
          textobjecttype = 'F01'
          language       = 'EN' BINARY SEARCH.
      IF sy-subrc = 0.
        <lfs_original_data>-hop_dong = ls_pro_note_text-plainlongtext.
      ENDIF.

      "Status
      CASE <lfs_original_data>-purchasingdocumentstatus.
        WHEN '10'.
          <lfs_original_data>-purchasingdocumentstatusdes = 'Deleted'.
        WHEN '01'.
          <lfs_original_data>-purchasingdocumentstatusdes = 'Draft'.
        WHEN '02'.
          <lfs_original_data>-purchasingdocumentstatusdes = 'In Approval'.
        WHEN '38'.
          <lfs_original_data>-purchasingdocumentstatusdes = 'Rejected'.
        WHEN '05' OR '37' OR '03' OR '04'.
          <lfs_original_data>-purchasingdocumentstatusdes = 'Approved'.
        WHEN OTHERS.
          <lfs_original_data>-purchasingdocumentstatusdes = ' '.
      ENDCASE.

      "Đơn giá
      IF <lfs_original_data>-netpricequantity <> 0.
        lv_dongia                  = ( <lfs_original_data>-dongia * 1000 ) / ( <lfs_original_data>-netpricequantity * 1000 ).
        <lfs_original_data>-dongia = ( <lfs_original_data>-dongia * 1000 ) / ( <lfs_original_data>-netpricequantity * 1000 ).
      ENDIF.
      "VAT
      lv_tax                  =  <lfs_original_data>-vatcalc.
      <lfs_original_data>-vat = |{ lv_tax ALPHA = OUT }%|.
      " 18 SoLuongTT = 41.SoLuongDaNK (sum theo PO item)
      <lfs_original_data>-soluongtt         = ls_data_po_sum-soluongdank.
      " 22 ThanhTienKH tinh theo flag VAT
      " 23 ThanhTienTT tinh theo flag VAT = chỉ tiêu (18) Thực tế (Số lượng) * (20) Đơn giá
      IF lv_calvat EQ 'N'.
        <lfs_original_data>-thanhtienkh = <lfs_original_data>-netamount.
        <lfs_original_data>-thanhtientt = <lfs_original_data>-soluongtt * lv_dongia.
      ELSE.
        <lfs_original_data>-thanhtienkh = <lfs_original_data>-netamount * ( 100 + <lfs_original_data>-vatcalc ) / 100.
        <lfs_original_data>-thanhtientt = <lfs_original_data>-soluongtt * lv_dongia * ( 100 + <lfs_original_data>-vatcalc ) / 100.
      ENDIF.

      " 37 SoLuongChuaTaoINB = SoLuongKH - SoLuongINB(sum theo PO item)
      IF <lfs_original_data>-inbounddelivery IS NOT INITIAL.
        <lfs_original_data>-soluongchuataoinb = <lfs_original_data>-soluongkh - ls_data_po_sum-soluonginb.
      ENDIF.
      IF <lfs_original_data>-currencypo IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh AND <lfs_original_data>-itemdh = ls_data-itemdh.
          <lfs_original_data>-currencypo = ls_data-currencypo.
        ENDIF.
      ENDIF.
      " 45 SoLuongChuaPark = SoLuongKH - SoLuongPark(sum theo PO item)
*      <lfs_original_data>-soluongchuapark   = <lfs_original_data>-soluongkh - ls_data_po_sum-soluongpark.
*      new => SL chưa park = SL thưc tế - SL invoice - SL park
*      <lfs_original_data>-soluongchuapark   = <lfs_original_data>-soluongtt - ls_data_po_sum-soluonginv - ls_data_po_sum-soluongpark.
*      new => SL chưa park = SL đã nhập kho - SL invoice - SL park
*      <lfs_original_data>-soluongchuapark   = <lfs_original_data>-soluongdank - <lfs_original_data>-soluonginv - <lfs_original_data>-soluongpark.
      " 47 ThanhTienChuaPark = ThanhTienTT - ThanhTienPark(sum theo PO item)
*      <lfs_original_data>-thanhtienchuapark = <lfs_original_data>-thanhtientt - ls_data_po_sum-thanhtienpark.
*      new => 47 ThanhTienChuaPark = TT TT - TT park - TT invoice
*      <lfs_original_data>-thanhtienchuapark = <lfs_original_data>-thanhtientt - ls_data_po_sum-thanhtieninv - ls_data_po_sum-thanhtienpark.
*      new => 47 ThanhTienChuaPark = TotalGoodsMvtAmtInccCrcy - TT park - TT invoice
*      <lfs_original_data>-thanhtienchuapark = <lfs_original_data>-totalgoodsmvtamtincccrcy - <lfs_original_data>-thanhtieninv - <lfs_original_data>-thanhtienpark.
      IF <lfs_original_data>-currencyinv IS INITIAL.
        <lfs_original_data>-currencyinv = <lfs_original_data>-currencypo.
      ENDIF.
      " 49 SoLuongChuaInv = SoLuongPark - SoLuongInv(sum theo PO item)
*      <lfs_original_data>-soluongchuainv    = <lfs_original_data>-soluongpark - ls_data_po_sum-soluonginv.
*      new => 49 SoLuongChuaInv = SoLuongPark
      <lfs_original_data>-soluongchuainv    = <lfs_original_data>-soluongpark.
      " 51 ThanhTienChuaInv = ThanhTienPark - ThanhTienInv(sum theo PO item)
*      <lfs_original_data>-thanhtienchuainv  = <lfs_original_data>-thanhtienpark - ls_data_po_sum-thanhtieninv.
*      new => 51 ThanhTienChuaInv = ThanhTienPark
      <lfs_original_data>-thanhtienchuainv  = <lfs_original_data>-thanhtienpark.
      " 19 SoLuongTTConLai = SoLuongKH - 18.SoLuongTT
      <lfs_original_data>-soluongttconlai   = <lfs_original_data>-soluongkh - <lfs_original_data>-soluongtt.
      " 24 ThanhTienTTConLai = ThanhTienKH - ThanhTienTT
      <lfs_original_data>-thanhtienttconlai = <lfs_original_data>-thanhtienkh - <lfs_original_data>-thanhtientt.

      " fill value
      IF <lfs_original_data>-iscompletelydelivered IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh AND <lfs_original_data>-itemdh = ls_data-itemdh.
          <lfs_original_data>-iscompletelydelivered = ls_data-iscompletelydelivered.
        ENDIF.
      ENDIF.
      IF <lfs_original_data>-iscompletelydelivered = abap_true.
        <lfs_original_data>-iscompletelydelivereddes = 'Hoàn thành'.
      ELSE.
        <lfs_original_data>-iscompletelydelivereddes = 'Chưa hoàn thành'.
      ENDIF.

      IF <lfs_original_data>-sodh = ls_data-sodh.
        <lfs_original_data>-purchasingdocumentstatusdes   = ls_data-purchasingdocumentstatusdes.
      ENDIF.
      IF <lfs_original_data>-sodh = ls_data-sodh AND <lfs_original_data>-itemdh = ls_data-itemdh.
        <lfs_original_data>-purchasingdocumentdeletioncode = ls_data-purchasingdocumentdeletioncode.
      ENDIF.
      IF <lfs_original_data>-nguoidnmh IS INITIAL.
        IF <lfs_original_data>-sodnmh = ls_data-sodnmh AND <lfs_original_data>-itemdnmh = ls_data-itemdnmh.
          <lfs_original_data>-nguoidnmh  = ls_data-nguoidnmh.
        ENDIF.
      ENDIF.
      IF <lfs_original_data>-nguoimuahang IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh.
          <lfs_original_data>-nguoimuahang    = ls_data-nguoimuahang.
        ENDIF.
      ENDIF.
      IF <lfs_original_data>-mahang IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh AND <lfs_original_data>-itemdh = ls_data-itemdh.
          <lfs_original_data>-mahang = ls_data-mahang.
        ENDIF.
      ENDIF.
      IF  <lfs_original_data>-dvt IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh AND <lfs_original_data>-itemdh = ls_data-itemdh.
          <lfs_original_data>-dvt             = ls_data-dvt.
        ENDIF.
      ENDIF.
      IF <lfs_original_data>-ngaynhanhangkh IS INITIAL.
        IF <lfs_original_data>-sodnmh = ls_data-sodnmh AND <lfs_original_data>-itemdnmh = ls_data-itemdnmh.
          <lfs_original_data>-ngaynhanhangkh  = ls_data-ngaynhanhangkh.
        ENDIF.
      ENDIF.
      IF <lfs_original_data>-lydosudung IS INITIAL.
        IF <lfs_original_data>-sodnmh = ls_data-sodnmh AND <lfs_original_data>-itemdnmh = ls_data-itemdnmh.
          <lfs_original_data>-lydosudung      = ls_data-lydosudung.
        ENDIF.
      ENDIF.

      IF <lfs_original_data>-ngaynhandnmh IS INITIAL.
        IF <lfs_original_data>-sodnmh = ls_data-sodnmh AND <lfs_original_data>-itemdnmh = ls_data-itemdnmh.
          <lfs_original_data>-ngaynhandnmh  = ls_data-ngaynhandnmh.
        ENDIF.
      ENDIF.

      IF <lfs_original_data>-ghichu IS INITIAL.
        IF <lfs_original_data>-sodnmh = ls_data-sodnmh AND <lfs_original_data>-itemdnmh = ls_data-itemdnmh.
          <lfs_original_data>-ghichu  = ls_data-ghichu.
        ENDIF.
      ENDIF.
      IF <lfs_original_data>-ngaydh IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh.
          <lfs_original_data>-ngaydh          = ls_data-ngaydh.
        ENDIF.
      ENDIF.
      IF  <lfs_original_data>-giaotai IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh AND <lfs_original_data>-itemdh = ls_data-itemdh.
          <lfs_original_data>-giaotai         = ls_data-giaotai.
        ENDIF.
      ENDIF.
      IF  <lfs_original_data>-isreturnsitem IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh AND <lfs_original_data>-itemdh = ls_data-itemdh.
          <lfs_original_data>-isreturnsitem   = ls_data-isreturnsitem.
        ENDIF.
      ENDIF.
      IF <lfs_original_data>-nhomhang IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh AND <lfs_original_data>-itemdh = ls_data-itemdh.
          <lfs_original_data>-nhomhang        = ls_data-nhomhang.
        ENDIF.
      ENDIF.
      IF  <lfs_original_data>-mancc IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh.
          <lfs_original_data>-mancc           = ls_data-mancc.
          <lfs_original_data>-tenncc          = ls_data-tenncc.
        ENDIF.
      ENDIF.
      IF <lfs_original_data>-tenhang IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh AND <lfs_original_data>-itemdh = ls_data-itemdh.
          <lfs_original_data>-tenhang = ls_data-tenhang.
        ENDIF.
      ENDIF.
      IF  <lfs_original_data>-ngaydnmh IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh .
          <lfs_original_data>-ngaydnmh        = ls_data-ngaydnmh.
        ENDIF.
      ENDIF.
      IF <lfs_original_data>-ngaynhanhangkh IS INITIAL.
        IF <lfs_original_data>-sodnmh = ls_data-sodnmh AND <lfs_original_data>-itemdnmh = ls_data-itemdnmh.
          <lfs_original_data>-ngaynhanhangkh  = ls_data-ngaynhanhangkh.
        ENDIF.
      ENDIF.
      IF <lfs_original_data>-ngaytaopo IS INITIAL.
        IF <lfs_original_data>-sodh = ls_data-sodh AND <lfs_original_data>-itemdh = ls_data-itemdh.
          <lfs_original_data>-ngaytaopo    = ls_data-ngaytaopo.
        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING <lfs_original_data> TO ls_data.

      IF <lfs_original_data>-isreturnsitem IS NOT INITIAL.
        <lfs_original_data>-soluongchuainv *= -1.
        <lfs_original_data>-soluongchuank *= -1.
        <lfs_original_data>-soluongchuapark *= -1.
        <lfs_original_data>-soluongchuataoinb *= -1.
        <lfs_original_data>-soluongdank *= -1.
        <lfs_original_data>-soluonginb *= -1.
        <lfs_original_data>-soluonginv *= -1.
        <lfs_original_data>-soluongkh *= -1.
        <lfs_original_data>-soluongpark *= -1.
        <lfs_original_data>-soluongtt *= -1.
        <lfs_original_data>-soluongttconlai *= -1.
        <lfs_original_data>-thanhtienchuainv *= -1.
        <lfs_original_data>-thanhtienchuapark *= -1.
        <lfs_original_data>-thanhtieninv *= -1.
        <lfs_original_data>-thanhtienkh *= -1.
        <lfs_original_data>-thanhtienpark *= -1.
        <lfs_original_data>-thanhtientt *= -1.
        <lfs_original_data>-thanhtienttconlai *= -1.
      ENDIF.

    ENDLOOP.

    et_data = lt_data.
  ENDMETHOD.


  METHOD get_instance.
    IF instance IS INITIAL.
      CREATE OBJECT instance.
    ENDIF.
    ro_instance = instance.
  ENDMETHOD.
ENDCLASS.
